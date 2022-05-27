//! Module defining script statements.

use super::{ASTFlags, ASTNode, BinaryExpr, Expr, FnCallExpr, Ident};
use crate::engine::KEYWORD_EVAL;
use crate::tokenizer::{Span, Token};
use crate::{calc_fn_hash, Position, StaticVec, INT};
use core::{
    fmt,
    hash::Hash,
    mem,
    num::NonZeroUsize,
    ops::{Deref, DerefMut},
};

use ink_prelude::boxed::Box;
use ink_prelude::collections::{BTreeMap};
use ink_prelude::vec;
use ink_prelude::vec::Vec;

/// _(internals)_ An op-assignment operator.
/// Exported under the `internals` feature only.
///
/// This type may hold a straight assignment (i.e. not an op-assignment).
#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct OpAssignment<'a> {
    /// Hash of the op-assignment call.
    pub hash_op_assign: u64,
    /// Hash of the underlying operator call (for fallback).
    pub hash_op: u64,
    /// Op-assignment operator.
    pub op_assign: &'a str,
    /// Underlying operator.
    pub op: &'a str,
    /// [Position] of the op-assignment operator.
    pub pos: Position,
}

impl OpAssignment<'_> {
    /// Create a new [`OpAssignment`] that is only a straight assignment.
    #[must_use]
    #[inline(always)]
    pub const fn new_assignment(pos: Position) -> Self {
        Self {
            hash_op_assign: 0,
            hash_op: 0,
            op_assign: "=",
            op: "=",
            pos,
        }
    }
    /// Is this an op-assignment?
    #[must_use]
    #[inline(always)]
    pub const fn is_op_assignment(&self) -> bool {
        self.hash_op_assign != 0 || self.hash_op != 0
    }
    /// Create a new [`OpAssignment`].
    ///
    /// # Panics
    ///
    /// Panics if the name is not an op-assignment operator.
    #[must_use]
    #[inline(always)]
    pub fn new_op_assignment(name: &str, pos: Position) -> Self {
        Self::new_op_assignment_from_token(Token::lookup_from_syntax(name).expect("operator"), pos)
    }
    /// Create a new [`OpAssignment`] from a [`Token`].
    ///
    /// # Panics
    ///
    /// Panics if the token is not an op-assignment operator.
    #[must_use]
    pub fn new_op_assignment_from_token(op: Token, pos: Position) -> Self {
        let op_raw = op
            .get_base_op_from_assignment()
            .expect("op-assignment operator")
            .literal_syntax();
        Self {
            hash_op_assign: calc_fn_hash(op.literal_syntax(), 2),
            hash_op: calc_fn_hash(op_raw, 2),
            op_assign: op.literal_syntax(),
            op: op_raw,
            pos,
        }
    }
    /// Create a new [`OpAssignment`] from a base operator.
    ///
    /// # Panics
    ///
    /// Panics if the name is not an operator that can be converted into an op-operator.
    #[must_use]
    #[inline(always)]
    pub fn new_op_assignment_from_base(name: &str, pos: Position) -> Self {
        Self::new_op_assignment_from_base_token(
            Token::lookup_from_syntax(name).expect("operator"),
            pos,
        )
    }
    /// Convert a [`Token`] into a new [`OpAssignment`].
    ///
    /// # Panics
    ///
    /// Panics if the token is cannot be converted into an op-assignment operator.
    #[inline(always)]
    #[must_use]
    pub fn new_op_assignment_from_base_token(op: Token, pos: Position) -> Self {
        Self::new_op_assignment_from_token(op.convert_to_op_assignment().expect("operator"), pos)
    }
}

impl fmt::Debug for OpAssignment<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_op_assignment() {
            f.debug_struct("OpAssignment")
                .field("hash_op_assign", &self.hash_op_assign)
                .field("hash_op", &self.hash_op)
                .field("op_assign", &self.op_assign)
                .field("op", &self.op)
                .field("pos", &self.pos)
                .finish()
        } else {
            fmt::Debug::fmt(&self.pos, f)
        }
    }
}

/// A statements block with a condition.
///
/// The condition may simply be [`Expr::BoolConstant`] with `true` if there is actually no condition.
#[derive(Debug, Clone, Hash)]
pub struct ConditionalStmtBlock {
    /// Condition.
    pub condition: Expr,
    /// Statements block.
    pub statements: StmtBlock,
}

impl<B: Into<StmtBlock>> From<B> for ConditionalStmtBlock {
    #[inline(always)]
    fn from(value: B) -> Self {
        Self {
            condition: Expr::BoolConstant(true, Position::NONE),
            statements: value.into(),
        }
    }
}

impl<B: Into<StmtBlock>> From<(Expr, B)> for ConditionalStmtBlock {
    #[inline(always)]
    fn from(value: (Expr, B)) -> Self {
        Self {
            condition: value.0,
            statements: value.1.into(),
        }
    }
}

/// _(internals)_ A type containing all cases for a `switch` statement.
/// Exported under the `internals` feature only.
#[derive(Debug, Clone, Hash)]
pub struct SwitchCases {
    /// Dictionary mapping value hashes to [`ConditionalStmtBlock`]'s.
    pub cases: BTreeMap<u64, Box<ConditionalStmtBlock>>,
    /// Statements block for the default case (there can be no condition for the default case).
    pub def_case: Box<StmtBlock>,
    /// List of range cases.
    pub ranges: StaticVec<(INT, INT, bool, Box<ConditionalStmtBlock>)>,
}

/// _(internals)_ A `try-catch` block.
/// Exported under the `internals` feature only.
#[derive(Debug, Clone, Hash)]
pub struct TryCatchBlock {
    /// `try` block.
    pub try_block: StmtBlock,
    /// `catch` variable, if any.
    pub catch_var: Ident,
    /// `catch` block.
    pub catch_block: StmtBlock,
}

/// _(internals)_ The underlying container type for [`StmtBlock`].
/// Exported under the `internals` feature only.
///
/// A [`SmallVec`](https://crates.io/crates/smallvec) containing up to 8 items inline is used to
/// hold a statements block, with the assumption that most program blocks would container fewer than
/// 8 statements, and those that do have a lot more statements.
#[cfg(not(feature = "no_std"))]
pub type StmtBlockContainer = smallvec::SmallVec<[Stmt; 8]>;

/// _(internals)_ The underlying container type for [`StmtBlock`].
/// Exported under the `internals` feature only.
#[cfg(feature = "no_std")]
pub type StmtBlockContainer = StaticVec<Stmt>;

/// _(internals)_ A scoped block of statements.
/// Exported under the `internals` feature only.
#[derive(Clone, Hash, Default)]
pub struct StmtBlock(StmtBlockContainer, Span);

impl StmtBlock {
    /// A [`StmtBlock`] that does not exist.
    pub const NONE: Self = Self::empty(Position::NONE);

    /// Create a new [`StmtBlock`].
    #[inline(always)]
    #[must_use]
    pub fn new(
        statements: impl IntoIterator<Item = Stmt>,
        start_pos: Position,
        end_pos: Position,
    ) -> Self {
        Self::new_with_span(statements, Span::new(start_pos, end_pos))
    }
    /// Create a new [`StmtBlock`].
    #[must_use]
    pub fn new_with_span(statements: impl IntoIterator<Item = Stmt>, span: Span) -> Self {
        let mut statements: smallvec::SmallVec<_> = statements.into_iter().collect();
        statements.shrink_to_fit();
        Self(statements, span)
    }
    /// Create an empty [`StmtBlock`].
    #[inline(always)]
    #[must_use]
    pub const fn empty(pos: Position) -> Self {
        Self(StmtBlockContainer::new_const(), Span::new(pos, pos))
    }
    /// Is this statements block empty?
    #[inline(always)]
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    /// Number of statements in this statements block.
    #[inline(always)]
    #[must_use]
    pub fn len(&self) -> usize {
        self.0.len()
    }
    /// Get the statements of this statements block.
    #[inline(always)]
    #[must_use]
    pub fn statements(&self) -> &[Stmt] {
        &self.0
    }
    /// Extract the statements.
    #[inline(always)]
    #[must_use]
    pub(crate) fn take_statements(&mut self) -> StmtBlockContainer {
        mem::take(&mut self.0)
    }
    /// Get an iterator over the statements of this statements block.
    #[inline(always)]
    #[must_use]
    pub fn iter(&self) -> impl Iterator<Item = &Stmt> {
        self.0.iter()
    }
    /// Get the start position (location of the beginning `{`) of this statements block.
    #[inline(always)]
    #[must_use]
    pub const fn position(&self) -> Position {
        (self.1).start()
    }
    /// Get the end position (location of the ending `}`) of this statements block.
    #[inline(always)]
    #[must_use]
    pub const fn end_position(&self) -> Position {
        (self.1).end()
    }
    /// Get the positions (locations of the beginning `{` and ending `}`) of this statements block.
    #[inline(always)]
    #[must_use]
    pub const fn span(&self) -> Span {
        self.1
    }
    /// Get the positions (locations of the beginning `{` and ending `}`) of this statements block
    /// or a default.
    #[inline(always)]
    #[must_use]
    pub const fn span_or_else(&self, def_start_pos: Position, def_end_pos: Position) -> Span {
        Span::new(
            (self.1).start().or_else(def_start_pos),
            (self.1).end().or_else(def_end_pos),
        )
    }
    /// Set the positions of this statements block.
    #[inline(always)]
    pub fn set_position(&mut self, start_pos: Position, end_pos: Position) {
        self.1 = Span::new(start_pos, end_pos);
    }
}

impl Deref for StmtBlock {
    type Target = StmtBlockContainer;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for StmtBlock {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl AsRef<[Stmt]> for StmtBlock {
    #[inline(always)]
    fn as_ref(&self) -> &[Stmt] {
        &self.0
    }
}

impl AsMut<[Stmt]> for StmtBlock {
    #[inline(always)]
    fn as_mut(&mut self) -> &mut [Stmt] {
        &mut self.0
    }
}

impl fmt::Debug for StmtBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Block")?;
        fmt::Debug::fmt(&self.0, f)?;
        if !self.1.is_none() {
            write!(f, " @ {:?}", self.1)?;
        }
        Ok(())
    }
}

impl From<Stmt> for StmtBlock {
    #[inline]
    fn from(stmt: Stmt) -> Self {
        match stmt {
            Stmt::Block(block) => *block,
            Stmt::Noop(pos) => Self(StmtBlockContainer::new_const(), Span::new(pos, pos)),
            _ => {
                let pos = stmt.position();
                Self(vec![stmt].into(), Span::new(pos, Position::NONE))
            }
        }
    }
}

impl IntoIterator for StmtBlock {
    type Item = Stmt;
    #[cfg(not(feature = "no_std"))]
    type IntoIter = smallvec::IntoIter<[Stmt; 8]>;
    #[cfg(feature = "no_std")]
    type IntoIter = smallvec::IntoIter<[Stmt; 3]>;

    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl Extend<Stmt> for StmtBlock {
    #[inline(always)]
    fn extend<T: IntoIterator<Item = Stmt>>(&mut self, iter: T) {
        self.0.extend(iter)
    }
}

/// _(internals)_ A statement.
/// Exported under the `internals` feature only.
#[derive(Debug, Clone, Hash)]
#[non_exhaustive]
pub enum Stmt {
    /// No-op.
    Noop(Position),
    /// `if` expr `{` stmt `}` `else` `{` stmt `}`
    If(Box<(Expr, StmtBlock, StmtBlock)>, Position),
    /// `switch` expr `{` literal or range or _ `if` condition `=>` stmt `,` ... `}`
    ///
    /// ### Data Structure
    ///
    /// 0) Hash table for (condition, block)
    /// 1) Default block
    /// 2) List of ranges: (start, end, inclusive, condition, statement)
    Switch(Box<(Expr, SwitchCases)>, Position),
    /// `while` expr `{` stmt `}` | `loop` `{` stmt `}`
    ///
    /// If the guard expression is [`UNIT`][Expr::Unit], then it is a `loop` statement.
    While(Box<(Expr, StmtBlock)>, Position),
    /// `do` `{` stmt `}` `while`|`until` expr
    ///
    /// ### Flags
    ///
    /// * [`NONE`][ASTFlags::NONE] = `while`
    /// * [`NEGATED`][ASTFlags::NEGATED] = `until`
    Do(Box<(Expr, StmtBlock)>, ASTFlags, Position),
    /// `for` `(` id `,` counter `)` `in` expr `{` stmt `}`
    For(Box<(Ident, Ident, Expr, StmtBlock)>, Position),
    /// \[`export`\] `let`|`const` id `=` expr
    ///
    /// ### Flags
    ///
    /// * [`EXPORTED`][ASTFlags::EXPORTED] = `export`
    /// * [`CONSTANT`][ASTFlags::CONSTANT] = `const`
    Var(Box<(Ident, Expr, Option<NonZeroUsize>)>, ASTFlags, Position),
    /// expr op`=` expr
    Assignment(Box<(OpAssignment<'static>, BinaryExpr)>),
    /// func `(` expr `,` ... `)`
    ///
    /// Note - this is a duplicate of [`Expr::FnCall`] to cover the very common pattern of a single
    ///        function call forming one statement.
    FnCall(Box<FnCallExpr>, Position),
    /// `{` stmt`;` ... `}`
    Block(Box<StmtBlock>),
    /// `try` `{` stmt; ... `}` `catch` `(` var `)` `{` stmt; ... `}`
    TryCatch(Box<TryCatchBlock>, Position),
    /// [expression][Expr]
    Expr(Box<Expr>),
    /// `continue`/`break`
    ///
    /// ### Flags
    ///
    /// * [`NONE`][ASTFlags::NONE] = `continue`
    /// * [`BREAK`][ASTFlags::BREAK] = `break`
    BreakLoop(ASTFlags, Position),
    /// `return`/`throw`
    ///
    /// ### Flags
    ///
    /// * [`NONE`][ASTFlags::NONE] = `return`
    /// * [`BREAK`][ASTFlags::BREAK] = `throw`
    Return(Option<Box<Expr>>, ASTFlags, Position),
    /// `import` expr `as` alias
    ///
    /// Not available under `no_module`.
    #[cfg(not(feature = "no_module"))]
    Import(Box<(Expr, Ident)>, Position),
    /// `export` var `as` alias
    ///
    /// Not available under `no_module`.
    #[cfg(not(feature = "no_module"))]
    Export(Box<(Ident, Ident)>, Position),
    /// Convert a variable to shared.
    ///
    /// Not available under `no_closure`.
    ///
    /// # Notes
    ///
    /// This variant does not map to any language structure.  It is currently only used only to
    /// convert a normal variable into a shared variable when the variable is _captured_ by a closure.
    #[cfg(not(feature = "no_closure"))]
    Share(Box<crate::Identifier>, Position),
}

impl Default for Stmt {
    #[inline(always)]
    fn default() -> Self {
        Self::Noop(Position::NONE)
    }
}

impl From<StmtBlock> for Stmt {
    #[inline(always)]
    fn from(block: StmtBlock) -> Self {
        Self::Block(block.into())
    }
}

impl<T: IntoIterator<Item = Stmt>> From<(T, Position, Position)> for Stmt {
    #[inline(always)]
    fn from(value: (T, Position, Position)) -> Self {
        StmtBlock::new(value.0, value.1, value.2).into()
    }
}

impl<T: IntoIterator<Item = Stmt>> From<(T, Span)> for Stmt {
    #[inline(always)]
    fn from(value: (T, Span)) -> Self {
        StmtBlock::new_with_span(value.0, value.1).into()
    }
}

impl Stmt {
    /// Is this statement [`Noop`][Stmt::Noop]?
    #[inline(always)]
    #[must_use]
    pub const fn is_noop(&self) -> bool {
        matches!(self, Self::Noop(..))
    }
    /// Get the [position][Position] of this statement.
    #[must_use]
    pub fn position(&self) -> Position {
        match self {
            Self::Noop(pos)
            | Self::BreakLoop(.., pos)
            | Self::FnCall(.., pos)
            | Self::If(.., pos)
            | Self::Switch(.., pos)
            | Self::While(.., pos)
            | Self::Do(.., pos)
            | Self::For(.., pos)
            | Self::Return(.., pos)
            | Self::Var(.., pos)
            | Self::TryCatch(.., pos) => *pos,

            Self::Assignment(x) => x.0.pos,

            Self::Block(x) => x.position(),

            Self::Expr(x) => x.start_position(),

            #[cfg(not(feature = "no_module"))]
            Self::Import(.., pos) => *pos,
            #[cfg(not(feature = "no_module"))]
            Self::Export(.., pos) => *pos,

            #[cfg(not(feature = "no_closure"))]
            Self::Share(.., pos) => *pos,
        }
    }
    /// Override the [position][Position] of this statement.
    pub fn set_position(&mut self, new_pos: Position) -> &mut Self {
        match self {
            Self::Noop(pos)
            | Self::BreakLoop(.., pos)
            | Self::FnCall(.., pos)
            | Self::If(.., pos)
            | Self::Switch(.., pos)
            | Self::While(.., pos)
            | Self::Do(.., pos)
            | Self::For(.., pos)
            | Self::Return(.., pos)
            | Self::Var(.., pos)
            | Self::TryCatch(.., pos) => *pos = new_pos,

            Self::Assignment(x) => x.0.pos = new_pos,

            Self::Block(x) => x.set_position(new_pos, x.end_position()),

            Self::Expr(x) => {
                x.set_position(new_pos);
            }

            #[cfg(not(feature = "no_module"))]
            Self::Import(.., pos) => *pos = new_pos,
            #[cfg(not(feature = "no_module"))]
            Self::Export(.., pos) => *pos = new_pos,

            #[cfg(not(feature = "no_closure"))]
            Self::Share(.., pos) => *pos = new_pos,
        }

        self
    }
    /// Does this statement return a value?
    #[must_use]
    pub const fn returns_value(&self) -> bool {
        match self {
            Self::If(..)
            | Self::Switch(..)
            | Self::Block(..)
            | Self::Expr(..)
            | Self::FnCall(..) => true,

            Self::Noop(..)
            | Self::While(..)
            | Self::Do(..)
            | Self::For(..)
            | Self::TryCatch(..) => false,

            Self::Var(..) | Self::Assignment(..) | Self::BreakLoop(..) | Self::Return(..) => false,

            #[cfg(not(feature = "no_module"))]
            Self::Import(..) | Self::Export(..) => false,

            #[cfg(not(feature = "no_closure"))]
            Self::Share(..) => false,
        }
    }
    /// Is this statement self-terminated (i.e. no need for a semicolon terminator)?
    #[must_use]
    pub const fn is_self_terminated(&self) -> bool {
        match self {
            Self::If(..)
            | Self::Switch(..)
            | Self::While(..)
            | Self::For(..)
            | Self::Block(..)
            | Self::TryCatch(..) => true,

            // A No-op requires a semicolon in order to know it is an empty statement!
            Self::Noop(..) => false,

            Self::Expr(e) => match &**e {
                Expr::Custom(x, ..) if x.is_self_terminated() => true,
                _ => false,
            },

            Self::Var(..)
            | Self::Assignment(..)
            | Self::FnCall(..)
            | Self::Do(..)
            | Self::BreakLoop(..)
            | Self::Return(..) => false,

            #[cfg(not(feature = "no_module"))]
            Self::Import(..) | Self::Export(..) => false,

            #[cfg(not(feature = "no_closure"))]
            Self::Share(..) => false,
        }
    }
    /// Is this statement _pure_?
    ///
    /// A pure statement has no side effects.
    #[must_use]
    pub fn is_pure(&self) -> bool {
        match self {
            Self::Noop(..) => true,
            Self::Expr(expr) => expr.is_pure(),
            Self::If(x, ..) => {
                x.0.is_pure() && x.1.iter().all(Stmt::is_pure) && x.2.iter().all(Stmt::is_pure)
            }
            Self::Switch(x, ..) => {
                x.0.is_pure()
                    && x.1.cases.values().all(|block| {
                        block.condition.is_pure() && block.statements.iter().all(Stmt::is_pure)
                    })
                    && x.1.ranges.iter().all(|(.., block)| {
                        block.condition.is_pure() && block.statements.iter().all(Stmt::is_pure)
                    })
                    && x.1.def_case.iter().all(Stmt::is_pure)
            }

            // Loops that exit can be pure because it can never be infinite.
            Self::While(x, ..) if matches!(x.0, Expr::BoolConstant(false, ..)) => true,
            Self::Do(x, options, ..) if matches!(x.0, Expr::BoolConstant(..)) => match x.0 {
                Expr::BoolConstant(cond, ..) if cond == options.contains(ASTFlags::NEGATED) => {
                    x.1.iter().all(Stmt::is_pure)
                }
                _ => false,
            },

            // Loops are never pure since they can be infinite - and that's a side effect.
            Self::While(..) | Self::Do(..) => false,

            // For loops can be pure because if the iterable is pure, it is finite,
            // so infinite loops can never occur.
            Self::For(x, ..) => x.2.is_pure() && x.3.iter().all(Stmt::is_pure),

            Self::Var(..) | Self::Assignment(..) | Self::FnCall(..) => false,
            Self::Block(block, ..) => block.iter().all(|stmt| stmt.is_pure()),
            Self::BreakLoop(..) | Self::Return(..) => false,
            Self::TryCatch(x, ..) => {
                x.try_block.iter().all(Stmt::is_pure) && x.catch_block.iter().all(Stmt::is_pure)
            }

            #[cfg(not(feature = "no_module"))]
            Self::Import(..) => false,
            #[cfg(not(feature = "no_module"))]
            Self::Export(..) => false,

            #[cfg(not(feature = "no_closure"))]
            Self::Share(..) => false,
        }
    }
    /// Does this statement's behavior depend on its containing block?
    ///
    /// A statement that depends on its containing block behaves differently when promoted to an
    /// upper block.
    ///
    /// Currently only variable definitions (i.e. `let` and `const`), `import`/`export` statements,
    /// and `eval` calls (which may in turn define variables) fall under this category.
    #[inline]
    #[must_use]
    pub fn is_block_dependent(&self) -> bool {
        match self {
            Self::Var(..) => true,

            Self::Expr(e) => match &**e {
                Expr::Stmt(s) => s.iter().all(Stmt::is_block_dependent),
                Expr::FnCall(x, ..) => !x.is_qualified() && x.name == KEYWORD_EVAL,
                _ => false,
            },

            Self::FnCall(x, ..) => !x.is_qualified() && x.name == KEYWORD_EVAL,

            #[cfg(not(feature = "no_module"))]
            Self::Import(..) | Self::Export(..) => true,

            _ => false,
        }
    }
    /// Is this statement _pure_ within the containing block?
    ///
    /// An internally pure statement only has side effects that disappear outside the block.
    ///
    /// Currently only variable definitions (i.e. `let` and `const`) and `import`/`export`
    /// statements are internally pure, other than pure expressions.
    #[inline]
    #[must_use]
    pub fn is_internally_pure(&self) -> bool {
        match self {
            Self::Var(x, ..) => x.1.is_pure(),

            Self::Expr(e) => match e.as_ref() {
                Expr::Stmt(s) => s.iter().all(Stmt::is_internally_pure),
                _ => self.is_pure(),
            },

            #[cfg(not(feature = "no_module"))]
            Self::Import(x, ..) => x.0.is_pure(),
            #[cfg(not(feature = "no_module"))]
            Self::Export(..) => true,

            _ => self.is_pure(),
        }
    }
    /// Does this statement break the current control flow through the containing block?
    ///
    /// Currently this is only true for `return`, `throw`, `break` and `continue`.
    ///
    /// All statements following this statement will essentially be dead code.
    #[inline]
    #[must_use]
    pub const fn is_control_flow_break(&self) -> bool {
        match self {
            Self::Return(..) | Self::BreakLoop(..) => true,
            _ => false,
        }
    }
    /// Recursively walk this statement.
    /// Return `false` from the callback to terminate the walk.
    pub fn walk<'a>(
        &'a self,
        path: &mut Vec<ASTNode<'a>>,
        on_node: &mut impl FnMut(&[ASTNode]) -> bool,
    ) -> bool {
        // Push the current node onto the path
        path.push(self.into());

        if !on_node(path) {
            return false;
        }

        match self {
            Self::Var(x, ..) => {
                if !x.1.walk(path, on_node) {
                    return false;
                }
            }
            Self::If(x, ..) => {
                if !x.0.walk(path, on_node) {
                    return false;
                }
                for s in x.1.iter() {
                    if !s.walk(path, on_node) {
                        return false;
                    }
                }
                for s in x.2.iter() {
                    if !s.walk(path, on_node) {
                        return false;
                    }
                }
            }
            Self::Switch(x, ..) => {
                if !x.0.walk(path, on_node) {
                    return false;
                }
                for b in x.1.cases.values() {
                    if !b.condition.walk(path, on_node) {
                        return false;
                    }
                    for s in b.statements.iter() {
                        if !s.walk(path, on_node) {
                            return false;
                        }
                    }
                }
                for (.., b) in &x.1.ranges {
                    if !b.condition.walk(path, on_node) {
                        return false;
                    }
                    for s in b.statements.iter() {
                        if !s.walk(path, on_node) {
                            return false;
                        }
                    }
                }
                for s in x.1.def_case.iter() {
                    if !s.walk(path, on_node) {
                        return false;
                    }
                }
            }
            Self::While(x, ..) | Self::Do(x, ..) => {
                if !x.0.walk(path, on_node) {
                    return false;
                }
                for s in x.1.statements() {
                    if !s.walk(path, on_node) {
                        return false;
                    }
                }
            }
            Self::For(x, ..) => {
                if !x.2.walk(path, on_node) {
                    return false;
                }
                for s in x.3.iter() {
                    if !s.walk(path, on_node) {
                        return false;
                    }
                }
            }
            Self::Assignment(x, ..) => {
                if !x.1.lhs.walk(path, on_node) {
                    return false;
                }
                if !x.1.rhs.walk(path, on_node) {
                    return false;
                }
            }
            Self::FnCall(x, ..) => {
                for s in &x.args {
                    if !s.walk(path, on_node) {
                        return false;
                    }
                }
            }
            Self::Block(x, ..) => {
                for s in x.statements() {
                    if !s.walk(path, on_node) {
                        return false;
                    }
                }
            }
            Self::TryCatch(x, ..) => {
                for s in x.try_block.iter() {
                    if !s.walk(path, on_node) {
                        return false;
                    }
                }
                for s in x.catch_block.iter() {
                    if !s.walk(path, on_node) {
                        return false;
                    }
                }
            }
            Self::Expr(e) => {
                if !e.walk(path, on_node) {
                    return false;
                }
            }
            Self::Return(Some(e), ..) => {
                if !e.walk(path, on_node) {
                    return false;
                }
            }
            #[cfg(not(feature = "no_module"))]
            Self::Import(x, ..) => {
                if !x.0.walk(path, on_node) {
                    return false;
                }
            }
            _ => (),
        }

        path.pop().unwrap();

        true
    }
}
