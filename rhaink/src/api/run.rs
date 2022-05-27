//! Module that defines the public evaluation API of [`Engine`].

use crate::eval::{Caches, GlobalRuntimeState};
use crate::parser::ParseState;
use crate::{Engine, Module, RhaiResultOf, Scope, AST};

use ink_prelude::boxed::Box;

impl Engine {
    /// Evaluate a script, returning any error (if any).
    #[inline(always)]
    pub fn run(&self, script: &str) -> RhaiResultOf<()> {
        self.run_with_scope(&mut Scope::new(), script)
    }
    /// Evaluate a script with own scope, returning any error (if any).
    ///
    /// ## Constants Propagation
    ///
    /// If not [`OptimizationLevel::None`][crate::OptimizationLevel::None], constants defined within
    /// the scope are propagated throughout the script _including_ functions. This allows functions
    /// to be optimized based on dynamic global constants.
    #[inline]
    pub fn run_with_scope(&self, scope: &mut Scope, script: &str) -> RhaiResultOf<()> {
        let scripts = [script];
        let (stream, tokenizer_control) =
            self.lex_raw(&scripts, self.token_mapper.as_ref().map(Box::as_ref));
        let mut state = ParseState::new(self, scope, tokenizer_control);

        let ast = self.parse(
            &mut stream.peekable(),
            &mut state,
            self.options.optimization_level,
        )?;

        self.run_ast_with_scope(scope, &ast)
    }
    /// Evaluate an [`AST`], returning any error (if any).
    #[inline(always)]
    pub fn run_ast(&self, ast: &AST) -> RhaiResultOf<()> {
        self.run_ast_with_scope(&mut Scope::new(), ast)
    }
    /// Evaluate an [`AST`] with own scope, returning any error (if any).
    #[inline]
    pub fn run_ast_with_scope(&self, scope: &mut Scope, ast: &AST) -> RhaiResultOf<()> {
        let caches = &mut Caches::new();
        let global = &mut GlobalRuntimeState::new(self);
        global.source = ast.source_raw().clone();

        #[cfg(not(feature = "no_module"))]
        {
            global.embedded_module_resolver = ast.resolver().cloned();
        }

        let statements = ast.statements();
        if !statements.is_empty() {
            let lib = [
                #[cfg(not(feature = "no_function"))]
                ast.as_ref(),
            ];
            let lib = if lib.first().map(|m: &&Module| m.is_empty()).unwrap_or(true) {
                &lib[0..0]
            } else {
                &lib
            };
            self.eval_global_statements(scope, global, caches, statements, lib, 0)?;
        }

        #[cfg(feature = "debugging")]
        if self.debugger.is_some() {
            global.debugger.status = crate::eval::DebuggerStatus::Terminate;
            let lib = &[
                #[cfg(not(feature = "no_function"))]
                ast.as_ref(),
            ];
            let node = &crate::ast::Stmt::Noop(crate::Position::NONE);
            self.run_debugger(scope, global, lib, &mut None, node, 0)?;
        }

        Ok(())
    }
}
