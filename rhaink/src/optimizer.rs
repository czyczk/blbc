//! Module implementing the [`AST`] optimizer.
#![cfg(not(feature = "no_optimize"))]

use crate::ast::{ASTFlags, Expr, OpAssignment, Stmt, StmtBlock, StmtBlockContainer, SwitchCases};
use crate::engine::{KEYWORD_DEBUG, KEYWORD_EVAL, KEYWORD_FN_PTR, KEYWORD_PRINT, KEYWORD_TYPE_OF};
use crate::eval::{Caches, GlobalRuntimeState};
use crate::func::builtin::get_builtin_binary_op_fn;
use crate::func::hashing::get_hasher;
use crate::tokenizer::{Span, Token};
use crate::types::dynamic::AccessMode;
use crate::{
    calc_fn_hash, calc_fn_params_hash, combine_hashes, Dynamic, Engine, FnPtr, Identifier,
    Position, Scope, StaticVec, AST, INT, INT_BITS,
};
#[cfg(feature = "no_std")]
use std::prelude::v1::*;
use std::{
    any::TypeId,
    convert::TryFrom,
    hash::{Hash, Hasher},
    mem,
};

/// Level of optimization performed.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
#[non_exhaustive]
pub enum OptimizationLevel {
    /// No optimization performed.
    None,
    /// Only perform simple optimizations without evaluating functions.
    Simple,
    /// Full optimizations performed, including evaluating functions.
    /// Take care that this may cause side effects as it essentially assumes that all functions are pure.
    Full,
}

impl Default for OptimizationLevel {
    #[inline(always)]
    fn default() -> Self {
        Self::Simple
    }
}

/// Mutable state throughout an optimization pass.
#[derive(Debug, Clone)]
struct OptimizerState<'a> {
    /// Has the [`AST`] been changed during this pass?
    changed: bool,
    /// Collection of constants to use for eager function evaluations.
    variables: StaticVec<(Identifier, AccessMode, Option<Dynamic>)>,
    /// Activate constants propagation?
    propagate_constants: bool,
    /// An [`Engine`] instance for eager function evaluation.
    engine: &'a Engine,
    /// The global runtime state.
    global: GlobalRuntimeState<'a>,
    /// Function resolution caches.
    caches: Caches,
    /// [Module][crate::Module] containing script-defined functions.
    #[cfg(not(feature = "no_function"))]
    lib: &'a [&'a crate::Module],
    /// Optimization level.
    optimization_level: OptimizationLevel,
}

impl<'a> OptimizerState<'a> {
    /// Create a new State.
    #[inline(always)]
    pub fn new(
        engine: &'a Engine,
        #[cfg(not(feature = "no_function"))] lib: &'a [&'a crate::Module],
        optimization_level: OptimizationLevel,
    ) -> Self {
        Self {
            changed: false,
            variables: StaticVec::new_const(),
            propagate_constants: true,
            engine,
            global: GlobalRuntimeState::new(engine),
            caches: Caches::new(),
            #[cfg(not(feature = "no_function"))]
            lib,
            optimization_level,
        }
    }
    /// Set the [`AST`] state to be dirty (i.e. changed).
    #[inline(always)]
    pub fn set_dirty(&mut self) {
        self.changed = true;
    }
    /// Set the [`AST`] state to be not dirty (i.e. unchanged).
    #[inline(always)]
    pub fn clear_dirty(&mut self) {
        self.changed = false;
    }
    /// Is the [`AST`] dirty (i.e. changed)?
    #[inline(always)]
    pub const fn is_dirty(&self) -> bool {
        self.changed
    }
    /// Prune the list of constants back to a specified size.
    #[inline(always)]
    pub fn restore_var(&mut self, len: usize) {
        self.variables.truncate(len)
    }
    /// Add a new variable to the list.
    #[inline(always)]
    pub fn push_var(
        &mut self,
        name: impl Into<Identifier>,
        access: AccessMode,
        value: Option<Dynamic>,
    ) {
        self.variables.push((name.into(), access, value))
    }
    /// Look up a constant from the list.
    #[inline]
    pub fn find_constant(&self, name: &str) -> Option<&Dynamic> {
        if !self.propagate_constants {
            return None;
        }

        for (n, access, value) in self.variables.iter().rev() {
            if n == name {
                return match access {
                    AccessMode::ReadWrite => None,
                    AccessMode::ReadOnly => value.as_ref(),
                };
            }
        }

        None
    }
    /// Call a registered function
    #[inline]
    pub fn call_fn_with_constant_arguments(
        &mut self,
        fn_name: &str,
        arg_values: &mut [Dynamic],
    ) -> Option<Dynamic> {
        #[cfg(not(feature = "no_function"))]
        let lib = self.lib;
        #[cfg(feature = "no_function")]
        let lib = &[];

        self.engine
            .call_native_fn(
                &mut self.global,
                &mut self.caches,
                lib,
                fn_name,
                calc_fn_hash(&fn_name, arg_values.len()),
                &mut arg_values.iter_mut().collect::<StaticVec<_>>(),
                false,
                false,
                Position::NONE,
                0,
            )
            .ok()
            .map(|(v, ..)| v)
    }
}

// Has a system function a Rust-native override?
fn has_native_fn_override(
    engine: &Engine,
    hash_script: u64,
    arg_types: impl AsRef<[TypeId]>,
) -> bool {
    let hash_params = calc_fn_params_hash(arg_types.as_ref().iter().cloned());
    let hash = combine_hashes(hash_script, hash_params);

    // First check the global namespace and packages, but skip modules that are standard because
    // they should never conflict with system functions.
    let result = engine
        .global_modules
        .iter()
        .filter(|m| !m.standard)
        .any(|m| m.contains_fn(hash));

    #[cfg(not(feature = "no_module"))]
    // Then check sub-modules
    let result = result
        || engine
            .global_sub_modules
            .values()
            .any(|m| m.contains_qualified_fn(hash));

    result
}

/// Optimize a block of [statements][Stmt].
fn optimize_stmt_block(
    mut statements: StmtBlockContainer,
    state: &mut OptimizerState,
    preserve_result: bool,
    is_internal: bool,
    reduce_return: bool,
) -> StmtBlockContainer {
    if statements.is_empty() {
        return statements;
    }

    let mut is_dirty = state.is_dirty();

    let is_pure = if is_internal {
        Stmt::is_internally_pure
    } else {
        Stmt::is_pure
    };

    // Flatten blocks
    loop {
        if let Some(n) = statements.iter().enumerate().find_map(|(i, s)| match s {
            Stmt::Block(block, ..) if !block.iter().any(Stmt::is_block_dependent) => Some(i),
            _ => None,
        }) {
            let (first, second) = statements.split_at_mut(n);
            let stmt = mem::take(&mut second[0]);
            let mut stmts = match stmt {
                Stmt::Block(block, ..) => block,
                stmt => unreachable!("Stmt::Block expected but gets {:?}", stmt),
            };
            statements = first
                .iter_mut()
                .map(mem::take)
                .chain(stmts.iter_mut().map(mem::take))
                .chain(second.iter_mut().skip(1).map(mem::take))
                .collect();
        } else {
            break;
        }

        is_dirty = true;
    }

    // Optimize
    loop {
        state.clear_dirty();

        let orig_constants_len = state.variables.len(); // Original number of constants in the state, for restore later
        let orig_propagate_constants = state.propagate_constants;

        // Remove everything following control flow breaking statements
        let mut dead_code = false;

        statements.retain(|stmt| {
            if dead_code {
                state.set_dirty();
                false
            } else if stmt.is_control_flow_break() {
                dead_code = true;
                true
            } else {
                true
            }
        });

        // Optimize each statement in the block
        for stmt in statements.iter_mut() {
            match stmt {
                Stmt::Var(x, options, ..) => {
                    if options.contains(ASTFlags::CONSTANT) {
                        // Add constant literals into the state
                        optimize_expr(&mut x.1, state, false);

                        if x.1.is_constant() {
                            state.push_var(
                                x.0.as_str(),
                                AccessMode::ReadOnly,
                                x.1.get_literal_value(),
                            );
                        }
                    } else {
                        // Add variables into the state
                        optimize_expr(&mut x.1, state, false);
                        state.push_var(x.0.as_str(), AccessMode::ReadWrite, None);
                    }
                }
                // Optimize the statement
                _ => optimize_stmt(stmt, state, preserve_result),
            }
        }

        // Remove all pure statements except the last one
        let mut index = 0;
        let mut first_non_constant = statements
            .iter()
            .rev()
            .enumerate()
            .find_map(|(i, stmt)| match stmt {
                stmt if !is_pure(stmt) => Some(i),

                Stmt::Var(x, ..) if x.1.is_constant() => Some(i),
                Stmt::Expr(e) if !e.is_constant() => Some(i),

                #[cfg(not(feature = "no_module"))]
                Stmt::Import(x, ..) if !x.0.is_constant() => Some(i),

                _ => None,
            })
            .map_or(0, |n| statements.len() - n - 1);

        while index < statements.len() {
            if preserve_result && index >= statements.len() - 1 {
                break;
            } else {
                match statements[index] {
                    ref stmt if is_pure(stmt) && index >= first_non_constant => {
                        state.set_dirty();
                        statements.remove(index);
                    }
                    ref stmt if stmt.is_pure() => {
                        state.set_dirty();
                        if index < first_non_constant {
                            first_non_constant -= 1;
                        }
                        statements.remove(index);
                    }
                    _ => index += 1,
                }
            }
        }

        // Remove all pure statements that do not return values at the end of a block.
        // We cannot remove anything for non-pure statements due to potential side-effects.
        if preserve_result {
            loop {
                match statements[..] {
                    // { return; } -> {}
                    [Stmt::Return(None, options, ..)]
                        if reduce_return && !options.contains(ASTFlags::BREAK) =>
                    {
                        state.set_dirty();
                        statements.clear();
                    }
                    [ref stmt] if !stmt.returns_value() && is_pure(stmt) => {
                        state.set_dirty();
                        statements.clear();
                    }
                    // { ...; return; } -> { ... }
                    [.., ref last_stmt, Stmt::Return(None, options, ..)]
                        if reduce_return
                            && !options.contains(ASTFlags::BREAK)
                            && !last_stmt.returns_value() =>
                    {
                        state.set_dirty();
                        statements.pop().unwrap();
                    }
                    // { ...; return val; } -> { ...; val }
                    [.., Stmt::Return(ref mut expr, options, pos)]
                        if reduce_return && !options.contains(ASTFlags::BREAK) =>
                    {
                        state.set_dirty();
                        *statements.last_mut().unwrap() = expr
                            .as_mut()
                            .map_or_else(|| Stmt::Noop(pos), |e| Stmt::Expr(mem::take(e)));
                    }
                    // { ...; stmt; noop } -> done
                    [.., ref second_last_stmt, Stmt::Noop(..)]
                        if second_last_stmt.returns_value() =>
                    {
                        break
                    }
                    // { ...; stmt_that_returns; pure_non_value_stmt } -> { ...; stmt_that_returns; noop }
                    // { ...; stmt; pure_non_value_stmt } -> { ...; stmt }
                    [.., ref second_last_stmt, ref last_stmt]
                        if !last_stmt.returns_value() && is_pure(last_stmt) =>
                    {
                        state.set_dirty();
                        if second_last_stmt.returns_value() {
                            *statements.last_mut().unwrap() = Stmt::Noop(last_stmt.position());
                        } else {
                            statements.pop().unwrap();
                        }
                    }
                    _ => break,
                }
            }
        } else {
            loop {
                match statements[..] {
                    [ref stmt] if is_pure(stmt) => {
                        state.set_dirty();
                        statements.clear();
                    }
                    // { ...; return; } -> { ... }
                    [.., Stmt::Return(None, options, ..)]
                        if reduce_return && !options.contains(ASTFlags::BREAK) =>
                    {
                        state.set_dirty();
                        statements.pop().unwrap();
                    }
                    // { ...; return pure_val; } -> { ... }
                    [.., Stmt::Return(Some(ref expr), options, ..)]
                        if reduce_return
                            && !options.contains(ASTFlags::BREAK)
                            && expr.is_pure() =>
                    {
                        state.set_dirty();
                        statements.pop().unwrap();
                    }
                    [.., ref last_stmt] if is_pure(last_stmt) => {
                        state.set_dirty();
                        statements.pop().unwrap();
                    }
                    _ => break,
                }
            }
        }

        // Pop the stack and remove all the local constants
        state.restore_var(orig_constants_len);
        state.propagate_constants = orig_propagate_constants;

        if !state.is_dirty() {
            break;
        }

        is_dirty = true;
    }

    if is_dirty {
        state.set_dirty();
    }

    statements.shrink_to_fit();
    statements
}

/// Optimize a [statement][Stmt].
fn optimize_stmt(stmt: &mut Stmt, state: &mut OptimizerState, preserve_result: bool) {
    match stmt {
        // var = var op expr => var op= expr
        Stmt::Assignment(x, ..)
            if !x.0.is_op_assignment()
                && x.1.lhs.is_variable_access(true)
                && matches!(&x.1.rhs, Expr::FnCall(x2, ..)
                        if Token::lookup_from_syntax(&x2.name).map(|t| t.has_op_assignment()).unwrap_or(false)
                        && x2.args.len() == 2
                        && x2.args[0].get_variable_name(true) == x.1.lhs.get_variable_name(true)
                ) =>
        {
            match x.1.rhs {
                Expr::FnCall(ref mut x2, ..) => {
                    state.set_dirty();
                    x.0 = OpAssignment::new_op_assignment_from_base(&x2.name, x2.pos);
                    x.1.rhs = mem::take(&mut x2.args[1]);
                }
                ref expr => unreachable!("Expr::FnCall expected but gets {:?}", expr),
            }
        }

        // expr op= expr
        Stmt::Assignment(x, ..) => {
            if !x.1.lhs.is_variable_access(false) {
                optimize_expr(&mut x.1.lhs, state, false);
            }
            optimize_expr(&mut x.1.rhs, state, false);
        }

        // if expr {}
        Stmt::If(x, ..) if x.1.is_empty() && x.2.is_empty() => {
            let condition = &mut x.0;
            state.set_dirty();

            let pos = condition.start_position();
            let mut expr = mem::take(condition);
            optimize_expr(&mut expr, state, false);

            *stmt = if preserve_result {
                // -> { expr, Noop }
                (
                    [Stmt::Expr(expr.into()), Stmt::Noop(pos)],
                    pos,
                    Position::NONE,
                )
                    .into()
            } else {
                // -> expr
                Stmt::Expr(expr.into())
            };
        }
        // if false { if_block } -> Noop
        Stmt::If(x, ..) if matches!(x.0, Expr::BoolConstant(false, ..)) && x.2.is_empty() => {
            if let Expr::BoolConstant(false, pos) = x.0 {
                state.set_dirty();
                *stmt = Stmt::Noop(pos);
            } else {
                unreachable!("`Expr::BoolConstant`");
            }
        }
        // if false { if_block } else { else_block } -> else_block
        Stmt::If(x, ..) if matches!(x.0, Expr::BoolConstant(false, ..)) => {
            state.set_dirty();
            *stmt =
                match optimize_stmt_block(mem::take(&mut *x.2), state, preserve_result, true, false)
                {
                    statements if statements.is_empty() => Stmt::Noop(x.2.position()),
                    statements => (statements, x.2.span()).into(),
                }
        }
        // if true { if_block } else { else_block } -> if_block
        Stmt::If(x, ..) if matches!(x.0, Expr::BoolConstant(true, ..)) => {
            state.set_dirty();
            *stmt =
                match optimize_stmt_block(mem::take(&mut *x.1), state, preserve_result, true, false)
                {
                    statements if statements.is_empty() => Stmt::Noop(x.1.position()),
                    statements => (statements, x.1.span()).into(),
                }
        }
        // if expr { if_block } else { else_block }
        Stmt::If(x, ..) => {
            let (condition, body, other) = x.as_mut();
            optimize_expr(condition, state, false);
            **body =
                optimize_stmt_block(mem::take(&mut **body), state, preserve_result, true, false);
            **other =
                optimize_stmt_block(mem::take(&mut **other), state, preserve_result, true, false);
        }

        // switch const { ... }
        Stmt::Switch(x, pos) if x.0.is_constant() => {
            let (
                match_expr,
                SwitchCases {
                    cases,
                    ranges,
                    def_case,
                },
            ) = x.as_mut();

            let value = match_expr.get_literal_value().unwrap();
            let hasher = &mut get_hasher();
            value.hash(hasher);
            let hash = hasher.finish();

            // First check hashes
            if let Some(block) = cases.get_mut(&hash) {
                match mem::take(&mut block.condition) {
                    Expr::BoolConstant(true, ..) => {
                        // Promote the matched case
                        let statements = optimize_stmt_block(
                            mem::take(&mut block.statements),
                            state,
                            true,
                            true,
                            false,
                        );
                        *stmt = (statements, block.statements.span()).into();
                    }
                    mut condition => {
                        // switch const { case if condition => stmt, _ => def } => if condition { stmt } else { def }
                        optimize_expr(&mut condition, state, false);

                        let def_stmt =
                            optimize_stmt_block(mem::take(def_case), state, true, true, false);

                        *stmt = Stmt::If(
                            (
                                condition,
                                mem::take(&mut block.statements),
                                StmtBlock::new_with_span(
                                    def_stmt,
                                    def_case.span_or_else(*pos, Position::NONE),
                                ),
                            )
                                .into(),
                            match_expr.start_position(),
                        );
                    }
                }

                state.set_dirty();
                return;
            }

            // Then check ranges
            if value.is::<INT>() && !ranges.is_empty() {
                let value = value.as_int().expect("`INT`");

                // Only one range or all ranges without conditions
                if ranges.len() == 1
                    || ranges
                        .iter()
                        .all(|(.., c)| matches!(c.condition, Expr::BoolConstant(true, ..)))
                {
                    for (.., block) in
                        ranges
                            .iter_mut()
                            .filter(|&&mut (start, end, inclusive, ..)| {
                                (!inclusive && (start..end).contains(&value))
                                    || (inclusive && (start..=end).contains(&value))
                            })
                    {
                        match mem::take(&mut block.condition) {
                            Expr::BoolConstant(true, ..) => {
                                // Promote the matched case
                                let statements = mem::take(&mut *block.statements);
                                let statements =
                                    optimize_stmt_block(statements, state, true, true, false);
                                *stmt = (statements, block.statements.span()).into();
                            }
                            mut condition => {
                                // switch const { range if condition => stmt, _ => def } => if condition { stmt } else { def }
                                optimize_expr(&mut condition, state, false);

                                let def_stmt = optimize_stmt_block(
                                    mem::take(def_case),
                                    state,
                                    true,
                                    true,
                                    false,
                                );
                                *stmt = Stmt::If(
                                    (
                                        condition,
                                        mem::take(&mut block.statements),
                                        StmtBlock::new_with_span(
                                            def_stmt,
                                            def_case.span_or_else(*pos, Position::NONE),
                                        ),
                                    )
                                        .into(),
                                    match_expr.start_position(),
                                );
                            }
                        }

                        state.set_dirty();
                        return;
                    }
                } else {
                    // Multiple ranges - clear the table and just keep the right ranges
                    if !cases.is_empty() {
                        state.set_dirty();
                        cases.clear();
                    }

                    let old_ranges_len = ranges.len();

                    ranges.retain(|&mut (start, end, inclusive, ..)| {
                        (!inclusive && (start..end).contains(&value))
                            || (inclusive && (start..=end).contains(&value))
                    });

                    if ranges.len() != old_ranges_len {
                        state.set_dirty();
                    }

                    for (.., block) in ranges.iter_mut() {
                        let statements = mem::take(&mut *block.statements);
                        *block.statements =
                            optimize_stmt_block(statements, state, preserve_result, true, false);

                        optimize_expr(&mut block.condition, state, false);

                        match block.condition {
                            Expr::Unit(pos) => {
                                block.condition = Expr::BoolConstant(true, pos);
                                state.set_dirty()
                            }
                            _ => (),
                        }
                    }
                    return;
                }
            }

            // Promote the default case
            state.set_dirty();
            let def_stmt = optimize_stmt_block(mem::take(def_case), state, true, true, false);
            *stmt = (def_stmt, def_case.span_or_else(*pos, Position::NONE)).into();
        }
        // switch
        Stmt::Switch(x, ..) => {
            let (
                match_expr,
                SwitchCases {
                    cases,
                    ranges,
                    def_case,
                    ..
                },
            ) = x.as_mut();

            optimize_expr(match_expr, state, false);

            // Optimize cases
            for block in cases.values_mut() {
                let statements = mem::take(&mut *block.statements);
                *block.statements =
                    optimize_stmt_block(statements, state, preserve_result, true, false);

                optimize_expr(&mut block.condition, state, false);

                match block.condition {
                    Expr::Unit(pos) => {
                        block.condition = Expr::BoolConstant(true, pos);
                        state.set_dirty();
                    }
                    _ => (),
                }
            }

            // Remove false cases
            cases.retain(|_, block| match block.condition {
                Expr::BoolConstant(false, ..) => {
                    state.set_dirty();
                    false
                }
                _ => true,
            });

            // Optimize ranges
            for (.., block) in ranges.iter_mut() {
                let statements = mem::take(&mut *block.statements);
                *block.statements =
                    optimize_stmt_block(statements, state, preserve_result, true, false);

                optimize_expr(&mut block.condition, state, false);

                match block.condition {
                    Expr::Unit(pos) => {
                        block.condition = Expr::BoolConstant(true, pos);
                        state.set_dirty();
                    }
                    _ => (),
                }
            }

            // Remove false ranges
            ranges.retain(|(.., block)| match block.condition {
                Expr::BoolConstant(false, ..) => {
                    state.set_dirty();
                    false
                }
                _ => true,
            });

            let def_block = mem::take(&mut ***def_case);
            ***def_case = optimize_stmt_block(def_block, state, preserve_result, true, false);
        }

        // while false { block } -> Noop
        Stmt::While(x, ..) if matches!(x.0, Expr::BoolConstant(false, ..)) => match x.0 {
            Expr::BoolConstant(false, pos) => {
                state.set_dirty();
                *stmt = Stmt::Noop(pos)
            }
            _ => unreachable!("`Expr::BoolConstant"),
        },
        // while expr { block }
        Stmt::While(x, ..) => {
            let (condition, body) = x.as_mut();
            optimize_expr(condition, state, false);
            if let Expr::BoolConstant(true, pos) = condition {
                *condition = Expr::Unit(*pos);
            }
            **body = optimize_stmt_block(mem::take(&mut **body), state, false, true, false);

            if body.len() == 1 {
                match body[0] {
                    // while expr { break; } -> { expr; }
                    Stmt::BreakLoop(options, pos) if options.contains(ASTFlags::BREAK) => {
                        // Only a single break statement - turn into running the guard expression once
                        state.set_dirty();
                        if !condition.is_unit() {
                            let mut statements = vec![Stmt::Expr(mem::take(condition).into())];
                            if preserve_result {
                                statements.push(Stmt::Noop(pos))
                            }
                            *stmt = (statements, Span::new(pos, Position::NONE)).into();
                        } else {
                            *stmt = Stmt::Noop(pos);
                        };
                    }
                    _ => (),
                }
            }
        }
        // do { block } until true -> { block }
        Stmt::Do(x, options, ..)
            if matches!(x.0, Expr::BoolConstant(true, ..))
                && options.contains(ASTFlags::NEGATED) =>
        {
            state.set_dirty();
            *stmt = (
                optimize_stmt_block(mem::take(&mut *x.1), state, false, true, false),
                x.1.span(),
            )
                .into();
        }
        // do { block } while false -> { block }
        Stmt::Do(x, options, ..)
            if matches!(x.0, Expr::BoolConstant(false, ..))
                && !options.contains(ASTFlags::NEGATED) =>
        {
            state.set_dirty();
            *stmt = (
                optimize_stmt_block(mem::take(&mut *x.1), state, false, true, false),
                x.1.span(),
            )
                .into();
        }
        // do { block } while|until expr
        Stmt::Do(x, ..) => {
            optimize_expr(&mut x.0, state, false);
            *x.1 = optimize_stmt_block(mem::take(&mut *x.1), state, false, true, false);
        }
        // for id in expr { block }
        Stmt::For(x, ..) => {
            optimize_expr(&mut x.2, state, false);
            *x.3 = optimize_stmt_block(mem::take(&mut *x.3), state, false, true, false);
        }
        // let id = expr;
        Stmt::Var(x, options, ..) if !options.contains(ASTFlags::CONSTANT) => {
            optimize_expr(&mut x.1, state, false)
        }
        // import expr as var;
        #[cfg(not(feature = "no_module"))]
        Stmt::Import(x, ..) => optimize_expr(&mut x.0, state, false),
        // { block }
        Stmt::Block(block) => {
            let span = block.span();
            let statements = block.take_statements().into_vec().into();
            let mut block = optimize_stmt_block(statements, state, preserve_result, true, false);

            match block.as_mut_slice() {
                [] => {
                    state.set_dirty();
                    *stmt = Stmt::Noop(span.start());
                }
                // Only one statement which is not block-dependent - promote
                [s] if !s.is_block_dependent() => {
                    state.set_dirty();
                    *stmt = mem::take(s);
                }
                _ => *stmt = (block, span).into(),
            }
        }
        // try { pure try_block } catch ( var ) { catch_block } -> try_block
        Stmt::TryCatch(x, ..) if x.try_block.iter().all(Stmt::is_pure) => {
            // If try block is pure, there will never be any exceptions
            state.set_dirty();
            *stmt = (
                optimize_stmt_block(mem::take(&mut *x.try_block), state, false, true, false),
                x.try_block.span(),
            )
                .into();
        }
        // try { try_block } catch ( var ) { catch_block }
        Stmt::TryCatch(x, ..) => {
            *x.try_block =
                optimize_stmt_block(mem::take(&mut *x.try_block), state, false, true, false);
            *x.catch_block =
                optimize_stmt_block(mem::take(&mut *x.catch_block), state, false, true, false);
        }

        Stmt::Expr(expr) => {
            optimize_expr(expr, state, false);

            match expr.as_mut() {
                // func(...)
                Expr::FnCall(x, pos) => {
                    state.set_dirty();
                    *stmt = Stmt::FnCall(mem::take(x), *pos);
                }
                // {...};
                Expr::Stmt(x) => {
                    if x.is_empty() {
                        state.set_dirty();
                        *stmt = Stmt::Noop(x.position());
                    } else {
                        state.set_dirty();
                        *stmt = mem::take(&mut **x).into();
                    }
                }
                // expr;
                _ => (),
            }
        }

        // return expr;
        Stmt::Return(Some(ref mut expr), ..) => optimize_expr(expr, state, false),

        // All other statements - skip
        _ => (),
    }
}

/// Optimize an [expression][Expr].
fn optimize_expr(expr: &mut Expr, state: &mut OptimizerState, _chaining: bool) {
    // These keywords are handled specially
    const DONT_EVAL_KEYWORDS: &[&str] = &[
        KEYWORD_PRINT, // side effects
        KEYWORD_DEBUG, // side effects
        KEYWORD_EVAL,  // arbitrary scripts
    ];

    match expr {
        // {}
        Expr::Stmt(x) if x.is_empty() => { state.set_dirty(); *expr = Expr::Unit(x.position()) }
        // { stmt; ... } - do not count promotion as dirty because it gets turned back into an array
        Expr::Stmt(x) => {
            ***x = optimize_stmt_block(mem::take(&mut **x), state, true, true, false);

            // { Stmt(Expr) } - promote
            match x.as_mut().as_mut() {
                [ Stmt::Expr(e) ] => { state.set_dirty(); *expr = mem::take(e); }
                _ => ()
            }
        }
        // lhs.rhs
        #[cfg(not(feature = "no_object"))]
        Expr::Dot(x,_, ..) if !_chaining => match (&mut x.lhs, &mut x.rhs) {
            // map.string
            (Expr::Map(m, pos), Expr::Property(p, ..)) if m.0.iter().all(|(.., x)| x.is_pure()) => {
                let prop = p.2.as_str();
                // Map literal where everything is pure - promote the indexed item.
                // All other items can be thrown away.
                state.set_dirty();
                *expr = mem::take(&mut m.0).into_iter().find(|(x, ..)| x.as_str() == prop)
                            .map(|(.., mut expr)| { expr.set_position(*pos); expr })
                            .unwrap_or_else(|| Expr::Unit(*pos));
            }
            // var.rhs
            (Expr::Variable(..), rhs) => optimize_expr(rhs, state, true),
            // lhs.rhs
            (lhs, rhs) => { optimize_expr(lhs, state, false); optimize_expr(rhs, state, true); }
        }
        // ....lhs.rhs
        #[cfg(not(feature = "no_object"))]
        Expr::Dot(x,_, ..) => { optimize_expr(&mut x.lhs, state, false); optimize_expr(&mut x.rhs, state, _chaining); }

        // lhs[rhs]
        #[cfg(not(feature = "no_index"))]
        Expr::Index(x, ..) if !_chaining => match (&mut x.lhs, &mut x.rhs) {
            // array[int]
            (Expr::Array(a, pos), Expr::IntegerConstant(i, ..)) if *i >= 0 && (*i as usize) < a.len() && a.iter().all(Expr::is_pure) => {
                // Array literal where everything is pure - promote the indexed item.
                // All other items can be thrown away.
                state.set_dirty();
                let mut result = mem::take(&mut a[*i as usize]);
                result.set_position(*pos);
                *expr = result;
            }
            // array[-int]
            (Expr::Array(a, pos), Expr::IntegerConstant(i, ..)) if *i < 0 && i.checked_abs().map(|n| n as usize <= a.len()).unwrap_or(false) && a.iter().all(Expr::is_pure) => {
                // Array literal where everything is pure - promote the indexed item.
                // All other items can be thrown away.
                state.set_dirty();
                let index = a.len() - i.abs() as usize;
                let mut result = mem::take(&mut a[index]);
                result.set_position(*pos);
                *expr = result;
            }
            // map[string]
            (Expr::Map(m, pos), Expr::StringConstant(s, ..)) if m.0.iter().all(|(.., x)| x.is_pure()) => {
                // Map literal where everything is pure - promote the indexed item.
                // All other items can be thrown away.
                state.set_dirty();
                *expr = mem::take(&mut m.0).into_iter().find(|(x, ..)| x.as_str() == s.as_str())
                            .map(|(.., mut expr)| { expr.set_position(*pos); expr })
                            .unwrap_or_else(|| Expr::Unit(*pos));
            }
            // int[int]
            (Expr::IntegerConstant(n, pos), Expr::IntegerConstant(i, ..)) if *i >= 0 && (*i as usize) < INT_BITS => {
                // Bit-field literal indexing - get the bit
                state.set_dirty();
                *expr = Expr::BoolConstant((*n & (1 << (*i as usize))) != 0, *pos);
            }
            // int[-int]
            (Expr::IntegerConstant(n, pos), Expr::IntegerConstant(i, ..)) if *i < 0 && i.checked_abs().map(|i| i as usize <= INT_BITS).unwrap_or(false) => {
                // Bit-field literal indexing - get the bit
                state.set_dirty();
                *expr = Expr::BoolConstant((*n & (1 << (INT_BITS - i.abs() as usize))) != 0, *pos);
            }
            // string[int]
            (Expr::StringConstant(s, pos), Expr::IntegerConstant(i, ..)) if *i >= 0 && (*i as usize) < s.chars().count() => {
                // String literal indexing - get the character
                state.set_dirty();
                *expr = Expr::CharConstant(s.chars().nth(*i as usize).unwrap(), *pos);
            }
            // string[-int]
            (Expr::StringConstant(s, pos), Expr::IntegerConstant(i, ..)) if *i < 0 && i.checked_abs().map(|n| n as usize <= s.chars().count()).unwrap_or(false) => {
                // String literal indexing - get the character
                state.set_dirty();
                *expr = Expr::CharConstant(s.chars().rev().nth(i.abs() as usize - 1).unwrap(), *pos);
            }
            // var[rhs]
            (Expr::Variable(..), rhs) => optimize_expr(rhs, state, true),
            // lhs[rhs]
            (lhs, rhs) => { optimize_expr(lhs, state, false); optimize_expr(rhs, state, true); }
        },
        // ...[lhs][rhs]
        #[cfg(not(feature = "no_index"))]
        Expr::Index(x, ..) => { optimize_expr(&mut x.lhs, state, false); optimize_expr(&mut x.rhs, state, _chaining); }
        // ``
        Expr::InterpolatedString(x, pos) if x.is_empty() => {
            state.set_dirty();
            *expr = Expr::StringConstant(state.engine.const_empty_string(), *pos);
        }
        // `...`
        Expr::InterpolatedString(x, ..) if x.len() == 1 && matches!(x[0], Expr::StringConstant(..)) => {
            state.set_dirty();
            *expr = mem::take(&mut x[0]);
        }
        // `... ${ ... } ...`
        Expr::InterpolatedString(x, ..) => {
            x.iter_mut().for_each(|expr| optimize_expr(expr, state, false));

            let mut n = 0;

            // Merge consecutive strings
            while n < x.len() - 1 {
                match (mem::take(&mut x[n]), mem::take(&mut x[n+1])) {
                    (Expr::StringConstant(mut s1, pos), Expr::StringConstant(s2, ..)) => { s1 += s2; x[n] = Expr::StringConstant(s1, pos); x.remove(n+1); state.set_dirty(); }
                    (expr1, Expr::Unit(..)) => { x[n] = expr1; x.remove(n+1); state.set_dirty(); }
                    (Expr::Unit(..), expr2) => { x[n+1] = expr2; x.remove(n); state.set_dirty(); }
                    (expr1, Expr::StringConstant(s, ..)) if s.is_empty() => { x[n] = expr1; x.remove(n+1); state.set_dirty(); }
                    (Expr::StringConstant(s, ..), expr2) if s.is_empty()=> { x[n+1] = expr2; x.remove(n); state.set_dirty(); }
                    (expr1, expr2) => { x[n] = expr1; x[n+1] = expr2; n += 1; }
                }
            }

            x.shrink_to_fit();
        }
        // [ constant .. ]
        #[cfg(not(feature = "no_index"))]
        Expr::Array(..) if expr.is_constant() => {
            state.set_dirty();
            *expr = Expr::DynamicConstant(expr.get_literal_value().unwrap().into(), expr.position());
        }
        // [ items .. ]
        #[cfg(not(feature = "no_index"))]
        Expr::Array(x, ..) => x.iter_mut().for_each(|expr| optimize_expr(expr, state, false)),
        // #{ key:constant, .. }
        #[cfg(not(feature = "no_object"))]
        Expr::Map(..) if expr.is_constant() => {
            state.set_dirty();
            *expr = Expr::DynamicConstant(expr.get_literal_value().unwrap().into(), expr.position());
        }
        // #{ key:value, .. }
        #[cfg(not(feature = "no_object"))]
        Expr::Map(x, ..) => x.0.iter_mut().for_each(|(.., expr)| optimize_expr(expr, state, false)),
        // lhs && rhs
        Expr::And(x, ..) => match (&mut x.lhs, &mut x.rhs) {
            // true && rhs -> rhs
            (Expr::BoolConstant(true, ..), rhs) => { state.set_dirty(); optimize_expr(rhs, state, false); *expr = mem::take(rhs); }
            // false && rhs -> false
            (Expr::BoolConstant(false, pos), ..) => { state.set_dirty(); *expr = Expr::BoolConstant(false, *pos); }
            // lhs && true -> lhs
            (lhs, Expr::BoolConstant(true, ..)) => { state.set_dirty(); optimize_expr(lhs, state, false); *expr = mem::take(lhs); }
            // lhs && rhs
            (lhs, rhs) => { optimize_expr(lhs, state, false); optimize_expr(rhs, state, false); }
        },
        // lhs || rhs
        Expr::Or(ref mut x, ..) => match (&mut x.lhs, &mut x.rhs) {
            // false || rhs -> rhs
            (Expr::BoolConstant(false, ..), rhs) => { state.set_dirty(); optimize_expr(rhs, state, false); *expr = mem::take(rhs); }
            // true || rhs -> true
            (Expr::BoolConstant(true, pos), ..) => { state.set_dirty(); *expr = Expr::BoolConstant(true, *pos); }
            // lhs || false
            (lhs, Expr::BoolConstant(false, ..)) => { state.set_dirty(); optimize_expr(lhs, state, false); *expr = mem::take(lhs); }
            // lhs || rhs
            (lhs, rhs) => { optimize_expr(lhs, state, false); optimize_expr(rhs, state, false); }
        },

        // eval!
        Expr::FnCall(x, ..) if x.name == KEYWORD_EVAL => {
            state.propagate_constants = false;
        }
        // Fn
        Expr::FnCall(x, pos)
            if !x.is_qualified() // Non-qualified
            && state.optimization_level == OptimizationLevel::Simple // simple optimizations
            && x.args.len() == 1
            && x.name == KEYWORD_FN_PTR
            && x.args[0].is_constant()
        => {
            let fn_name = match x.args[0] {
                Expr::StringConstant(ref s, ..) => s.clone().into(),
                _ => Dynamic::UNIT
            };

            if let Ok(fn_ptr) = fn_name.into_immutable_string().map_err(|err| err.into()).and_then(FnPtr::try_from) {
                state.set_dirty();
                *expr = Expr::DynamicConstant(Box::new(fn_ptr.into()), *pos);
            } else {
                optimize_expr(&mut x.args[0], state, false);
            }
        }

        // Do not call some special keywords
        Expr::FnCall(x, ..) if DONT_EVAL_KEYWORDS.contains(&x.name.as_str()) => {
            x.args.iter_mut().for_each(|a| optimize_expr(a, state, false));
        }

        // Call built-in operators
        Expr::FnCall(x, pos)
                if !x.is_qualified() // Non-qualified
                && state.optimization_level == OptimizationLevel::Simple // simple optimizations
                && x.args.iter().all(Expr::is_constant) // all arguments are constants
                //&& !is_valid_identifier(x.chars()) // cannot be scripted
        => {
            let arg_values = &mut x.args.iter().map(|e| e.get_literal_value().unwrap()).collect::<StaticVec<_>>();
            let arg_types: StaticVec<_> = arg_values.iter().map(Dynamic::type_id).collect();

            match x.name.as_str() {
                KEYWORD_TYPE_OF if arg_values.len() == 1 => {
                    state.set_dirty();
                    let typ = state.engine.map_type_name(arg_values[0].type_name()).into();
                    *expr = Expr::from_dynamic(typ, *pos);
                    return;
                }
                #[cfg(not(feature = "no_closure"))]
                crate::engine::KEYWORD_IS_SHARED if arg_values.len() == 1 => {
                    state.set_dirty();
                    *expr = Expr::from_dynamic(Dynamic::FALSE, *pos);
                    return;
                }
                // Overloaded operators can override built-in.
                _ if x.args.len() == 2 && !has_native_fn_override(state.engine, x.hashes.native, arg_types.as_ref()) => {
                    if let Some(result) = get_builtin_binary_op_fn(&x.name, &arg_values[0], &arg_values[1])
                        .and_then(|f| {
                            #[cfg(not(feature = "no_function"))]
                            let lib = state.lib;
                            #[cfg(feature = "no_function")]
                            let lib = &[];

                            let context = (state.engine, &x.name, lib).into();
                            let (first, second) = arg_values.split_first_mut().unwrap();
                            (f)(context, &mut [ first, &mut second[0] ]).ok()
                        }) {
                            state.set_dirty();
                            *expr = Expr::from_dynamic(result, *pos);
                            return;
                        }
                }
                _ => ()
            }

            x.args.iter_mut().for_each(|a| optimize_expr(a, state, false));

            // Move constant arguments
            for arg in x.args.iter_mut() {
                match arg {
                    Expr::DynamicConstant(..) | Expr::Unit(..)
                    | Expr::StringConstant(..) | Expr::CharConstant(..)
                    | Expr::BoolConstant(..) | Expr::IntegerConstant(..) => (),

                    #[cfg(not(feature = "no_float"))]
                    Expr:: FloatConstant(..) => (),

                    _ => if let Some(value) = arg.get_literal_value() {
                        state.set_dirty();
                        *arg = Expr::DynamicConstant(value.into(), arg.start_position());
                    },
                }
            }
        }

        // Eagerly call functions
        Expr::FnCall(x, pos)
                if !x.is_qualified() // non-qualified
                && state.optimization_level == OptimizationLevel::Full // full optimizations
                && x.args.iter().all(Expr::is_constant) // all arguments are constants
        => {
            // First search for script-defined functions (can override built-in)
            #[cfg(not(feature = "no_function"))]
            let has_script_fn = state.lib.iter().any(|&m| m.get_script_fn(&x.name, x.args.len()).is_some());
            #[cfg(feature = "no_function")]
            let has_script_fn = false;

            if !has_script_fn {
                let arg_values = &mut x.args.iter().map(|e| e.get_literal_value().unwrap()).collect::<StaticVec<_>>();

                let result = match x.name.as_str() {
                    KEYWORD_TYPE_OF if arg_values.len() == 1 => Some(state.engine.map_type_name(arg_values[0].type_name()).into()),
                    #[cfg(not(feature = "no_closure"))]
                    crate::engine::KEYWORD_IS_SHARED if arg_values.len() == 1 => Some(Dynamic::FALSE),
                    _ => state.call_fn_with_constant_arguments(&x.name, arg_values)
                };

                if let Some(result) = result {
                    state.set_dirty();
                    *expr = Expr::from_dynamic(result, *pos);
                    return;
                }
            }

            x.args.iter_mut().for_each(|a| optimize_expr(a, state, false));
        }

        // id(args ..) or xxx.id(args ..) -> optimize function call arguments
        Expr::FnCall(x, ..) | Expr::MethodCall(x, ..) => for arg in x.args.iter_mut() {
            optimize_expr(arg, state, false);

            // Move constant arguments
            match arg {
                Expr::DynamicConstant(..) | Expr::Unit(..)
                | Expr::StringConstant(..) | Expr::CharConstant(..)
                | Expr::BoolConstant(..) | Expr::IntegerConstant(..) => (),

                #[cfg(not(feature = "no_float"))]
                Expr:: FloatConstant(..) => (),

                _ => if let Some(value) = arg.get_literal_value() {
                    state.set_dirty();
                    *arg = Expr::DynamicConstant(value.into(), arg.start_position());
                },
            }
        },

        // constant-name
        #[cfg(not(feature = "no_module"))]
        Expr::Variable(x, ..) if !x.1.is_empty() => (),
        Expr::Variable(x, .., pos) if state.find_constant(&x.3).is_some() => {
            // Replace constant with value
            *expr = Expr::from_dynamic(state.find_constant(&x.3).unwrap().clone(), *pos);
            state.set_dirty();
        }

        // Custom syntax
        Expr::Custom(x, ..) => {
            if x.scope_may_be_changed {
                state.propagate_constants = false;
            }
            x.inputs.iter_mut().for_each(|expr| optimize_expr(expr, state, false));
        }

        // All other expressions - skip
        _ => (),
    }
}

/// Optimize a block of [statements][Stmt] at top level.
///
/// Constants and variables from the scope are added.
fn optimize_top_level(
    statements: StmtBlockContainer,
    engine: &Engine,
    scope: &Scope,
    #[cfg(not(feature = "no_function"))] lib: &[&crate::Module],
    optimization_level: OptimizationLevel,
) -> StmtBlockContainer {
    let mut statements = statements;

    // If optimization level is None then skip optimizing
    if optimization_level == OptimizationLevel::None {
        statements.shrink_to_fit();
        return statements;
    }

    // Set up the state
    let mut state = OptimizerState::new(
        engine,
        #[cfg(not(feature = "no_function"))]
        lib,
        optimization_level,
    );

    // Add constants from global modules
    for (name, value) in engine
        .global_modules
        .iter()
        .rev()
        .flat_map(|m| m.iter_var())
    {
        state.push_var(name, AccessMode::ReadOnly, Some(value.clone()));
    }

    // Add constants and variables from the scope
    for (name, constant, value) in scope.iter() {
        if !constant {
            state.push_var(name, AccessMode::ReadWrite, None);
        } else {
            state.push_var(name, AccessMode::ReadOnly, Some(value));
        }
    }

    optimize_stmt_block(statements, &mut state, true, false, true)
}

/// Optimize an [`AST`].
pub fn optimize_into_ast(
    engine: &Engine,
    scope: &Scope,
    statements: StmtBlockContainer,
    #[cfg(not(feature = "no_function"))] functions: StaticVec<
        crate::Shared<crate::ast::ScriptFnDef>,
    >,
    optimization_level: OptimizationLevel,
) -> AST {
    let mut statements = statements;

    #[cfg(not(feature = "no_function"))]
    let lib = {
        let mut module = crate::Module::new();

        if optimization_level != OptimizationLevel::None {
            // We only need the script library's signatures for optimization purposes
            let mut lib2 = crate::Module::new();

            for fn_def in &functions {
                lib2.set_script_fn(crate::ast::ScriptFnDef {
                    name: fn_def.name.clone(),
                    access: fn_def.access,
                    body: crate::ast::StmtBlock::NONE,
                    params: fn_def.params.clone(),
                    #[cfg(not(feature = "no_module"))]
                    environ: None,
                    #[cfg(not(feature = "no_function"))]
                    #[cfg(feature = "metadata")]
                    comments: Box::default(),
                });
            }

            let lib2 = &[&lib2];

            for fn_def in functions {
                let mut fn_def = crate::func::native::shared_take_or_clone(fn_def);

                // Optimize the function body
                let body = mem::take(&mut *fn_def.body);

                *fn_def.body = optimize_top_level(body, engine, scope, lib2, optimization_level);

                module.set_script_fn(fn_def);
            }
        } else {
            for fn_def in functions {
                module.set_script_fn(fn_def);
            }
        }

        module
    };

    statements.shrink_to_fit();

    AST::new(
        match optimization_level {
            OptimizationLevel::None => statements,
            OptimizationLevel::Simple | OptimizationLevel::Full => optimize_top_level(
                statements,
                engine,
                &scope,
                #[cfg(not(feature = "no_function"))]
                &[&lib],
                optimization_level,
            ),
        },
        #[cfg(not(feature = "no_function"))]
        lib,
    )
}
