//! Module defining the AST (abstract syntax tree).

use super::{ASTFlags, Expr, FnAccess, Stmt, StmtBlock, StmtBlockContainer};
use crate::{Dynamic, FnNamespace, Identifier, Position};
use core::{
    fmt,
    hash::Hash,
    ops::{Add, AddAssign},
    ptr,
};

/// Compiled AST (abstract syntax tree) of a Rhai script.
///
/// # Thread Safety
///
/// Currently, [`AST`] is neither `Send` nor `Sync`. Turn on the `sync` feature to make it `Send + Sync`.
#[derive(Clone)]
pub struct AST {
    /// Source of the [`AST`].
    /// No source if string is empty.
    source: Identifier,
    /// Global statements.
    body: StmtBlock,
    /// Script-defined functions.
    #[cfg(not(feature = "no_function"))]
    lib: crate::Shared<crate::Module>,
    /// Embedded module resolver, if any.
    #[cfg(not(feature = "no_module"))]
    resolver: Option<crate::Shared<crate::module::resolvers::StaticModuleResolver>>,
}

impl Default for AST {
    #[inline(always)]
    fn default() -> Self {
        Self::empty()
    }
}

impl fmt::Debug for AST {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut fp = f.debug_struct("AST");

        if !self.source.is_empty() {
            fp.field("source: ", &self.source);
        }
        #[cfg(not(feature = "no_module"))]
        if let Some(ref resolver) = self.resolver {
            fp.field("resolver: ", resolver);
        }

        fp.field("body", &self.body.as_slice());

        #[cfg(not(feature = "no_function"))]
        if !self.lib.is_empty() {
            for (.., ref fn_def) in self.lib.iter_script_fn() {
                let sig = fn_def.to_string();
                fp.field(&sig, &fn_def.body.as_slice());
            }
        }
        fp.finish()
    }
}

impl AST {
    /// Create a new [`AST`].
    #[cfg(not(feature = "internals"))]
    #[inline(always)]
    #[must_use]
    pub(crate) fn new(
        statements: impl IntoIterator<Item = Stmt>,
        #[cfg(not(feature = "no_function"))] functions: impl Into<crate::Shared<crate::Module>>,
    ) -> Self {
        Self {
            source: Identifier::new_const(),
            body: StmtBlock::new(statements, Position::NONE, Position::NONE),
            #[cfg(not(feature = "no_function"))]
            lib: functions.into(),
            #[cfg(not(feature = "no_module"))]
            resolver: None,
        }
    }
    /// _(internals)_ Create a new [`AST`].
    /// Exported under the `internals` feature only.
    #[cfg(feature = "internals")]
    #[inline(always)]
    #[must_use]
    pub fn new(
        statements: impl IntoIterator<Item = Stmt>,
        #[cfg(not(feature = "no_function"))] functions: impl Into<crate::Shared<crate::Module>>,
    ) -> Self {
        Self {
            source: Identifier::new_const(),
            body: StmtBlock::new(statements, Position::NONE, Position::NONE),
            #[cfg(not(feature = "no_function"))]
            lib: functions.into(),
            #[cfg(not(feature = "no_module"))]
            resolver: None,
        }
    }
    /// Create a new [`AST`] with a source name.
    #[cfg(not(feature = "internals"))]
    #[inline(always)]
    #[must_use]
    pub(crate) fn new_with_source(
        statements: impl IntoIterator<Item = Stmt>,
        #[cfg(not(feature = "no_function"))] functions: impl Into<crate::Shared<crate::Module>>,
        source: impl Into<Identifier>,
    ) -> Self {
        let mut ast = Self::new(
            statements,
            #[cfg(not(feature = "no_function"))]
            functions,
        );
        ast.set_source(source);
        ast
    }
    /// _(internals)_ Create a new [`AST`] with a source name.
    /// Exported under the `internals` feature only.
    #[cfg(feature = "internals")]
    #[inline(always)]
    #[must_use]
    pub fn new_with_source(
        statements: impl IntoIterator<Item = Stmt>,
        #[cfg(not(feature = "no_function"))] functions: impl Into<crate::Shared<crate::Module>>,
        source: impl Into<Identifier>,
    ) -> Self {
        let mut ast = Self::new(
            statements,
            #[cfg(not(feature = "no_function"))]
            functions,
        );
        ast.set_source(source);
        ast
    }
    /// Create an empty [`AST`].
    #[inline]
    #[must_use]
    pub fn empty() -> Self {
        Self {
            source: Identifier::new_const(),
            body: StmtBlock::NONE,
            #[cfg(not(feature = "no_function"))]
            lib: crate::Module::new().into(),
            #[cfg(not(feature = "no_module"))]
            resolver: None,
        }
    }
    /// Get the source, if any.
    #[inline(always)]
    #[must_use]
    pub fn source(&self) -> Option<&str> {
        if self.source.is_empty() {
            None
        } else {
            Some(self.source.as_str())
        }
    }
    /// Get a reference to the source.
    #[inline(always)]
    #[must_use]
    pub(crate) fn source_raw(&self) -> &Identifier {
        &self.source
    }
    /// Set the source.
    #[inline]
    pub fn set_source(&mut self, source: impl Into<Identifier>) -> &mut Self {
        let source = source.into();
        #[cfg(not(feature = "no_function"))]
        crate::Shared::get_mut(&mut self.lib)
            .as_mut()
            .map(|m| m.set_id(source.clone()));
        self.source = source;
        self
    }
    /// Clear the source.
    #[inline(always)]
    pub fn clear_source(&mut self) -> &mut Self {
        self.source.clear();
        self
    }
    /// Get the statements.
    #[cfg(not(feature = "internals"))]
    #[inline(always)]
    #[must_use]
    pub(crate) fn statements(&self) -> &[Stmt] {
        self.body.statements()
    }
    /// _(internals)_ Get the statements.
    /// Exported under the `internals` feature only.
    #[cfg(feature = "internals")]
    #[inline(always)]
    #[must_use]
    pub fn statements(&self) -> &[Stmt] {
        self.body.statements()
    }
    /// Extract the statements.
    #[allow(dead_code)]
    #[inline(always)]
    #[must_use]
    pub(crate) fn take_statements(&mut self) -> StmtBlockContainer {
        self.body.take_statements()
    }
    /// Does this [`AST`] contain script-defined functions?
    ///
    /// Not available under `no_function`.
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    #[must_use]
    pub fn has_functions(&self) -> bool {
        !self.lib.is_empty()
    }
    /// Get the internal shared [`Module`][crate::Module] containing all script-defined functions.
    #[cfg(not(feature = "internals"))]
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    #[must_use]
    pub(crate) fn shared_lib(&self) -> &crate::Shared<crate::Module> {
        &self.lib
    }
    /// _(internals)_ Get the internal shared [`Module`][crate::Module] containing all script-defined functions.
    /// Exported under the `internals` feature only.
    ///
    /// Not available under `no_function`.
    #[cfg(feature = "internals")]
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    #[must_use]
    pub fn shared_lib(&self) -> &crate::Shared<crate::Module> {
        &self.lib
    }
    /// Get the embedded [module resolver][`ModuleResolver`].
    #[cfg(not(feature = "internals"))]
    #[cfg(not(feature = "no_module"))]
    #[inline(always)]
    #[must_use]
    pub(crate) fn resolver(
        &self,
    ) -> Option<&crate::Shared<crate::module::resolvers::StaticModuleResolver>> {
        self.resolver.as_ref()
    }
    /// _(internals)_ Get the embedded [module resolver][crate::ModuleResolver].
    /// Exported under the `internals` feature only.
    ///
    /// Not available under `no_module`.
    #[cfg(feature = "internals")]
    #[cfg(not(feature = "no_module"))]
    #[inline(always)]
    #[must_use]
    pub fn resolver(
        &self,
    ) -> Option<&crate::Shared<crate::module::resolvers::StaticModuleResolver>> {
        self.resolver.as_ref()
    }
    /// Set the embedded [module resolver][`ModuleResolver`].
    #[cfg(not(feature = "no_module"))]
    #[inline(always)]
    pub(crate) fn set_resolver(
        &mut self,
        resolver: impl Into<crate::Shared<crate::module::resolvers::StaticModuleResolver>>,
    ) -> &mut Self {
        self.resolver = Some(resolver.into());
        self
    }
    /// Clone the [`AST`]'s functions into a new [`AST`].
    /// No statements are cloned.
    ///
    /// Not available under `no_function`.
    ///
    /// This operation is cheap because functions are shared.
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    #[must_use]
    pub fn clone_functions_only(&self) -> Self {
        self.clone_functions_only_filtered(|_, _, _, _, _| true)
    }
    /// Clone the [`AST`]'s functions into a new [`AST`] based on a filter predicate.
    /// No statements are cloned.
    ///
    /// Not available under `no_function`.
    ///
    /// This operation is cheap because functions are shared.
    #[cfg(not(feature = "no_function"))]
    #[inline]
    #[must_use]
    pub fn clone_functions_only_filtered(
        &self,
        filter: impl Fn(FnNamespace, FnAccess, bool, &str, usize) -> bool,
    ) -> Self {
        let mut lib = crate::Module::new();
        lib.merge_filtered(&self.lib, &filter);
        Self {
            source: self.source.clone(),
            body: StmtBlock::NONE,
            lib: lib.into(),
            #[cfg(not(feature = "no_module"))]
            resolver: self.resolver.clone(),
        }
    }
    /// Clone the [`AST`]'s script statements into a new [`AST`].
    /// No functions are cloned.
    #[inline(always)]
    #[must_use]
    pub fn clone_statements_only(&self) -> Self {
        Self {
            source: self.source.clone(),
            body: self.body.clone(),
            #[cfg(not(feature = "no_function"))]
            lib: crate::Module::new().into(),
            #[cfg(not(feature = "no_module"))]
            resolver: self.resolver.clone(),
        }
    }
    /// Merge two [`AST`] into one.  Both [`AST`]'s are untouched and a new, merged,
    /// version is returned.
    ///
    /// Statements in the second [`AST`] are simply appended to the end of the first _without any processing_.
    /// Thus, the return value of the first [`AST`] (if using expression-statement syntax) is buried.
    /// Of course, if the first [`AST`] uses a `return` statement at the end, then
    /// the second [`AST`] will essentially be dead code.
    ///
    /// All script-defined functions in the second [`AST`] overwrite similarly-named functions
    /// in the first [`AST`] with the same number of parameters.
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// # #[cfg(not(feature = "no_function"))]
    /// # {
    /// use rhai::Engine;
    ///
    /// let engine = Engine::new();
    ///
    /// let ast1 = engine.compile("
    ///     fn foo(x) { 42 + x }
    ///     foo(1)
    /// ")?;
    ///
    /// let ast2 = engine.compile(r#"
    ///     fn foo(n) { `hello${n}` }
    ///     foo("!")
    /// "#)?;
    ///
    /// let ast = ast1.merge(&ast2);    // Merge 'ast2' into 'ast1'
    ///
    /// // Notice that using the '+' operator also works:
    /// // let ast = &ast1 + &ast2;
    ///
    /// // 'ast' is essentially:
    /// //
    /// //    fn foo(n) { `hello${n}` } // <- definition of first 'foo' is overwritten
    /// //    foo(1)                    // <- notice this will be "hello1" instead of 43,
    /// //                              //    but it is no longer the return value
    /// //    foo("!")                  // returns "hello!"
    ///
    /// // Evaluate it
    /// assert_eq!(engine.eval_ast::<String>(&ast)?, "hello!");
    /// # }
    /// # Ok(())
    /// # }
    /// ```
    #[inline(always)]
    #[must_use]
    pub fn merge(&self, other: &Self) -> Self {
        self.merge_filtered_impl(other, |_, _, _, _, _| true)
    }
    /// Combine one [`AST`] with another.  The second [`AST`] is consumed.
    ///
    /// Statements in the second [`AST`] are simply appended to the end of the first _without any processing_.
    /// Thus, the return value of the first [`AST`] (if using expression-statement syntax) is buried.
    /// Of course, if the first [`AST`] uses a `return` statement at the end, then
    /// the second [`AST`] will essentially be dead code.
    ///
    /// All script-defined functions in the second [`AST`] overwrite similarly-named functions
    /// in the first [`AST`] with the same number of parameters.
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// # #[cfg(not(feature = "no_function"))]
    /// # {
    /// use rhai::Engine;
    ///
    /// let engine = Engine::new();
    ///
    /// let mut ast1 = engine.compile("
    ///     fn foo(x) { 42 + x }
    ///     foo(1)
    /// ")?;
    ///
    /// let ast2 = engine.compile(r#"
    ///     fn foo(n) { `hello${n}` }
    ///     foo("!")
    /// "#)?;
    ///
    /// ast1.combine(ast2);    // Combine 'ast2' into 'ast1'
    ///
    /// // Notice that using the '+=' operator also works:
    /// // ast1 += ast2;
    ///
    /// // 'ast1' is essentially:
    /// //
    /// //    fn foo(n) { `hello${n}` } // <- definition of first 'foo' is overwritten
    /// //    foo(1)                    // <- notice this will be "hello1" instead of 43,
    /// //                              //    but it is no longer the return value
    /// //    foo("!")                  // returns "hello!"
    ///
    /// // Evaluate it
    /// assert_eq!(engine.eval_ast::<String>(&ast1)?, "hello!");
    /// # }
    /// # Ok(())
    /// # }
    /// ```
    #[inline(always)]
    pub fn combine(&mut self, other: Self) -> &mut Self {
        self.combine_filtered_impl(other, |_, _, _, _, _| true)
    }
    /// Merge two [`AST`] into one.  Both [`AST`]'s are untouched and a new, merged, version
    /// is returned.
    ///
    /// Not available under `no_function`.
    ///
    /// Statements in the second [`AST`] are simply appended to the end of the first _without any processing_.
    /// Thus, the return value of the first [`AST`] (if using expression-statement syntax) is buried.
    /// Of course, if the first [`AST`] uses a `return` statement at the end, then
    /// the second [`AST`] will essentially be dead code.
    ///
    /// All script-defined functions in the second [`AST`] are first selected based on a filter
    /// predicate, then overwrite similarly-named functions in the first [`AST`] with the
    /// same number of parameters.
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::Engine;
    ///
    /// let engine = Engine::new();
    ///
    /// let ast1 = engine.compile("
    ///     fn foo(x) { 42 + x }
    ///     foo(1)
    /// ")?;
    ///
    /// let ast2 = engine.compile(r#"
    ///     fn foo(n) { `hello${n}` }
    ///     fn error() { 0 }
    ///     foo("!")
    /// "#)?;
    ///
    /// // Merge 'ast2', picking only 'error()' but not 'foo(..)', into 'ast1'
    /// let ast = ast1.merge_filtered(&ast2, |_, _, script, name, params|
    ///                                 script && name == "error" && params == 0);
    ///
    /// // 'ast' is essentially:
    /// //
    /// //    fn foo(n) { 42 + n }      // <- definition of 'ast1::foo' is not overwritten
    /// //                              //    because 'ast2::foo' is filtered away
    /// //    foo(1)                    // <- notice this will be 43 instead of "hello1",
    /// //                              //    but it is no longer the return value
    /// //    fn error() { 0 }          // <- this function passes the filter and is merged
    /// //    foo("!")                  // <- returns "42!"
    ///
    /// // Evaluate it
    /// assert_eq!(engine.eval_ast::<String>(&ast)?, "42!");
    /// # Ok(())
    /// # }
    /// ```
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    #[must_use]
    pub fn merge_filtered(
        &self,
        other: &Self,
        filter: impl Fn(FnNamespace, FnAccess, bool, &str, usize) -> bool,
    ) -> Self {
        self.merge_filtered_impl(other, filter)
    }
    /// Merge two [`AST`] into one.  Both [`AST`]'s are untouched and a new, merged, version
    /// is returned.
    #[inline]
    #[must_use]
    fn merge_filtered_impl(
        &self,
        other: &Self,
        _filter: impl Fn(FnNamespace, FnAccess, bool, &str, usize) -> bool,
    ) -> Self {
        let merged = match (self.body.is_empty(), other.body.is_empty()) {
            (false, false) => {
                let mut body = self.body.clone();
                body.extend(other.body.iter().cloned());
                body
            }
            (false, true) => self.body.clone(),
            (true, false) => other.body.clone(),
            (true, true) => StmtBlock::NONE,
        };

        #[cfg(not(feature = "no_function"))]
        let lib = {
            let mut lib = self.lib.as_ref().clone();
            lib.merge_filtered(&other.lib, &_filter);
            lib
        };

        if !other.source.is_empty() {
            Self::new_with_source(
                merged,
                #[cfg(not(feature = "no_function"))]
                lib,
                other.source.clone(),
            )
        } else {
            Self::new(
                merged,
                #[cfg(not(feature = "no_function"))]
                lib,
            )
        }
    }
    /// Combine one [`AST`] with another.  The second [`AST`] is consumed.
    ///
    /// Not available under `no_function`.
    ///
    /// Statements in the second [`AST`] are simply appended to the end of the first _without any processing_.
    /// Thus, the return value of the first [`AST`] (if using expression-statement syntax) is buried.
    /// Of course, if the first [`AST`] uses a `return` statement at the end, then
    /// the second [`AST`] will essentially be dead code.
    ///
    /// All script-defined functions in the second [`AST`] are first selected based on a filter
    /// predicate, then overwrite similarly-named functions in the first [`AST`] with the
    /// same number of parameters.
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::Engine;
    ///
    /// let engine = Engine::new();
    ///
    /// let mut ast1 = engine.compile("
    ///     fn foo(x) { 42 + x }
    ///     foo(1)
    /// ")?;
    ///
    /// let ast2 = engine.compile(r#"
    ///     fn foo(n) { `hello${n}` }
    ///     fn error() { 0 }
    ///     foo("!")
    /// "#)?;
    ///
    /// // Combine 'ast2', picking only 'error()' but not 'foo(..)', into 'ast1'
    /// ast1.combine_filtered(ast2, |_, _, script, name, params|
    ///                                 script && name == "error" && params == 0);
    ///
    /// // 'ast1' is essentially:
    /// //
    /// //    fn foo(n) { 42 + n }      // <- definition of 'ast1::foo' is not overwritten
    /// //                              //    because 'ast2::foo' is filtered away
    /// //    foo(1)                    // <- notice this will be 43 instead of "hello1",
    /// //                              //    but it is no longer the return value
    /// //    fn error() { 0 }          // <- this function passes the filter and is merged
    /// //    foo("!")                  // <- returns "42!"
    ///
    /// // Evaluate it
    /// assert_eq!(engine.eval_ast::<String>(&ast1)?, "42!");
    /// # Ok(())
    /// # }
    /// ```
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    pub fn combine_filtered(
        &mut self,
        other: Self,
        filter: impl Fn(FnNamespace, FnAccess, bool, &str, usize) -> bool,
    ) -> &mut Self {
        self.combine_filtered_impl(other, filter)
    }
    /// Combine one [`AST`] with another.  The second [`AST`] is consumed.
    #[inline]
    fn combine_filtered_impl(
        &mut self,
        other: Self,
        _filter: impl Fn(FnNamespace, FnAccess, bool, &str, usize) -> bool,
    ) -> &mut Self {
        self.body.extend(other.body.into_iter());

        #[cfg(not(feature = "no_function"))]
        if !other.lib.is_empty() {
            crate::func::native::shared_make_mut(&mut self.lib)
                .merge_filtered(&other.lib, &_filter);
        }
        self
    }
    /// Filter out the functions, retaining only some based on a filter predicate.
    ///
    /// Not available under `no_function`.
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// # #[cfg(not(feature = "no_function"))]
    /// # {
    /// use rhai::Engine;
    ///
    /// let engine = Engine::new();
    ///
    /// let mut ast = engine.compile(r#"
    ///     fn foo(n) { n + 1 }
    ///     fn bar() { print("hello"); }
    /// "#)?;
    ///
    /// // Remove all functions except 'foo(..)'
    /// ast.retain_functions(|_, _, name, params| name == "foo" && params == 1);
    /// # }
    /// # Ok(())
    /// # }
    /// ```
    #[cfg(not(feature = "no_function"))]
    #[inline]
    pub fn retain_functions(
        &mut self,
        filter: impl Fn(FnNamespace, FnAccess, &str, usize) -> bool,
    ) -> &mut Self {
        if !self.lib.is_empty() {
            crate::func::native::shared_make_mut(&mut self.lib).retain_script_functions(filter);
        }
        self
    }
    /// _(internals)_ Iterate through all function definitions.
    /// Exported under the `internals` feature only.
    ///
    /// Not available under `no_function`.
    #[cfg(feature = "internals")]
    #[cfg(not(feature = "no_function"))]
    #[inline]
    pub fn iter_fn_def(&self) -> impl Iterator<Item = &super::ScriptFnDef> {
        self.lib
            .iter_script_fn()
            .map(|(.., fn_def)| fn_def.as_ref())
    }
    /// Iterate through all function definitions.
    ///
    /// Not available under `no_function`.
    #[cfg(not(feature = "internals"))]
    #[cfg(not(feature = "no_function"))]
    #[allow(dead_code)]
    #[inline]
    pub(crate) fn iter_fn_def(&self) -> impl Iterator<Item = &super::ScriptFnDef> {
        self.lib
            .iter_script_fn()
            .map(|(.., fn_def)| fn_def.as_ref())
    }
    /// Iterate through all function definitions.
    ///
    /// Not available under `no_function`.
    #[cfg(not(feature = "no_function"))]
    #[inline]
    pub fn iter_functions<'a>(&'a self) -> impl Iterator<Item = super::ScriptFnMetadata> + 'a {
        self.lib
            .iter_script_fn()
            .map(|(.., fn_def)| fn_def.as_ref().into())
    }
    /// Clear all function definitions in the [`AST`].
    ///
    /// Not available under `no_function`.
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    pub fn clear_functions(&mut self) -> &mut Self {
        self.lib = crate::Module::new().into();
        self
    }
    /// Clear all statements in the [`AST`], leaving only function definitions.
    #[inline(always)]
    pub fn clear_statements(&mut self) -> &mut Self {
        self.body = StmtBlock::NONE;
        self
    }
    /// Extract all top-level literal constant and/or variable definitions.
    /// This is useful for extracting all global constants from a script without actually running it.
    ///
    /// A literal constant/variable definition takes the form of:
    /// `const VAR = `_value_`;` and `let VAR = `_value_`;`
    /// where _value_ is a literal expression or will be optimized into a literal.
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::{Engine, Scope};
    ///
    /// let engine = Engine::new();
    ///
    /// let ast = engine.compile(
    /// "
    ///     const A = 40 + 2;   // constant that optimizes into a literal
    ///     let b = 123;        // literal variable
    ///     const B = b * A;    // non-literal constant
    ///     const C = 999;      // literal constant
    ///     b = A + C;          // expression
    ///
    ///     {                   // <- new block scope
    ///         const Z = 0;    // <- literal constant not at top-level
    ///     }
    /// ")?;
    ///
    /// let mut iter = ast.iter_literal_variables(true, false)
    ///                   .map(|(name, is_const, value)| (name, is_const, value.as_int().unwrap()));
    ///
    /// # #[cfg(not(feature = "no_optimize"))]
    /// assert_eq!(iter.next(), Some(("A", true, 42)));
    /// assert_eq!(iter.next(), Some(("C", true, 999)));
    /// assert_eq!(iter.next(), None);
    ///
    /// let mut iter = ast.iter_literal_variables(false, true)
    ///                   .map(|(name, is_const, value)| (name, is_const, value.as_int().unwrap()));
    ///
    /// assert_eq!(iter.next(), Some(("b", false, 123)));
    /// assert_eq!(iter.next(), None);
    ///
    /// let mut iter = ast.iter_literal_variables(true, true)
    ///                   .map(|(name, is_const, value)| (name, is_const, value.as_int().unwrap()));
    ///
    /// # #[cfg(not(feature = "no_optimize"))]
    /// assert_eq!(iter.next(), Some(("A", true, 42)));
    /// assert_eq!(iter.next(), Some(("b", false, 123)));
    /// assert_eq!(iter.next(), Some(("C", true, 999)));
    /// assert_eq!(iter.next(), None);
    ///
    /// let scope: Scope = ast.iter_literal_variables(true, false).collect();
    ///
    /// # #[cfg(not(feature = "no_optimize"))]
    /// assert_eq!(scope.len(), 2);
    ///
    /// Ok(())
    /// # }
    /// ```
    pub fn iter_literal_variables(
        &self,
        include_constants: bool,
        include_variables: bool,
    ) -> impl Iterator<Item = (&str, bool, Dynamic)> {
        self.statements().iter().filter_map(move |stmt| match stmt {
            Stmt::Var(x, options, ..)
                if options.contains(ASTFlags::CONSTANT) && include_constants
                    || !options.contains(ASTFlags::CONSTANT) && include_variables =>
            {
                let (name, expr, ..) = x.as_ref();
                if let Some(value) = expr.get_literal_value() {
                    Some((name.as_str(), options.contains(ASTFlags::CONSTANT), value))
                } else {
                    None
                }
            }
            _ => None,
        })
    }
    /// Recursively walk the [`AST`], including function bodies (if any).
    /// Return `false` from the callback to terminate the walk.
    #[cfg(not(feature = "internals"))]
    #[cfg(not(feature = "no_module"))]
    #[inline]
    pub(crate) fn walk(&self, on_node: &mut impl FnMut(&[ASTNode]) -> bool) -> bool {
        let path = &mut Vec::new();

        for stmt in self.statements() {
            if !stmt.walk(path, on_node) {
                return false;
            }
        }
        #[cfg(not(feature = "no_function"))]
        for stmt in self.iter_fn_def().flat_map(|f| f.body.iter()) {
            if !stmt.walk(path, on_node) {
                return false;
            }
        }

        true
    }
    /// _(internals)_ Recursively walk the [`AST`], including function bodies (if any).
    /// Return `false` from the callback to terminate the walk.
    /// Exported under the `internals` feature only.
    #[cfg(feature = "internals")]
    #[inline]
    pub fn walk(&self, on_node: &mut impl FnMut(&[ASTNode]) -> bool) -> bool {
        let path = &mut Vec::new();

        for stmt in self.statements() {
            if !stmt.walk(path, on_node) {
                return false;
            }
        }
        #[cfg(not(feature = "no_function"))]
        for stmt in self.iter_fn_def().flat_map(|f| f.body.iter()) {
            if !stmt.walk(path, on_node) {
                return false;
            }
        }

        true
    }
}

impl<A: AsRef<AST>> Add<A> for &AST {
    type Output = AST;

    #[inline(always)]
    fn add(self, rhs: A) -> Self::Output {
        self.merge(rhs.as_ref())
    }
}

impl<A: Into<AST>> AddAssign<A> for AST {
    #[inline(always)]
    fn add_assign(&mut self, rhs: A) {
        self.combine(rhs.into());
    }
}

impl AsRef<[Stmt]> for AST {
    #[inline(always)]
    fn as_ref(&self) -> &[Stmt] {
        self.statements()
    }
}

#[cfg(not(feature = "no_function"))]
impl AsRef<crate::Module> for AST {
    #[inline(always)]
    fn as_ref(&self) -> &crate::Module {
        self.shared_lib().as_ref()
    }
}

#[cfg(not(feature = "no_function"))]
impl AsRef<crate::Shared<crate::Module>> for AST {
    #[inline(always)]
    fn as_ref(&self) -> &crate::Shared<crate::Module> {
        self.shared_lib()
    }
}

/// _(internals)_ An [`AST`] node, consisting of either an [`Expr`] or a [`Stmt`].
/// Exported under the `internals` feature only.
#[derive(Debug, Clone, Copy, Hash)]
#[non_exhaustive]
pub enum ASTNode<'a> {
    /// A statement ([`Stmt`]).
    Stmt(&'a Stmt),
    /// An expression ([`Expr`]).
    Expr(&'a Expr),
}

impl<'a> From<&'a Stmt> for ASTNode<'a> {
    fn from(stmt: &'a Stmt) -> Self {
        Self::Stmt(stmt)
    }
}

impl<'a> From<&'a Expr> for ASTNode<'a> {
    fn from(expr: &'a Expr) -> Self {
        Self::Expr(expr)
    }
}

impl PartialEq for ASTNode<'_> {
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Stmt(x), Self::Stmt(y)) => ptr::eq(*x, *y),
            (Self::Expr(x), Self::Expr(y)) => ptr::eq(*x, *y),
            _ => false,
        }
    }
}

impl Eq for ASTNode<'_> {}

impl ASTNode<'_> {
    /// Get the [`Position`] of this [`ASTNode`].
    pub fn position(&self) -> Position {
        match self {
            ASTNode::Stmt(stmt) => stmt.position(),
            ASTNode::Expr(expr) => expr.position(),
        }
    }
}

impl AST {
    /// _(internals)_ Get the internal [`Module`][crate::Module] containing all script-defined functions.
    /// Exported under the `internals` feature only.
    ///
    /// Not available under `no_function`.
    ///
    /// # Deprecated
    ///
    /// This method is deprecated. Use [`shared_lib`][AST::shared_lib] instead.
    ///
    /// This method will be removed in the next major version.
    #[deprecated(since = "1.3.0", note = "use `shared_lib` instead")]
    #[cfg(feature = "internals")]
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    #[must_use]
    pub fn lib(&self) -> &crate::Module {
        &self.lib
    }
}
