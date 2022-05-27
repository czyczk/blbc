//! Module containing all deprecated API that will be removed in the next major version.

use crate::{
    Dynamic, Engine, EvalAltResult, Expression, FnPtr, ImmutableString, NativeCallContext,
    Position, RhaiResult, RhaiResultOf, Scope, AST,
};

impl Engine {
    /// Evaluate a string, but throw away the result and only return error (if any).
    /// Useful for when you don't need the result, but still need to keep track of possible errors.
    ///
    /// # Deprecated
    ///
    /// This method is deprecated. Use [`run`][Engine::run] instead.
    ///
    /// This method will be removed in the next major version.
    #[deprecated(since = "1.1.0", note = "use `run` instead")]
    #[inline(always)]
    pub fn consume(&self, script: &str) -> RhaiResultOf<()> {
        self.run(script)
    }

    /// Evaluate a string with own scope, but throw away the result and only return error (if any).
    /// Useful for when you don't need the result, but still need to keep track of possible errors.
    ///
    /// # Deprecated
    ///
    /// This method is deprecated. Use [`run_with_scope`][Engine::run_with_scope] instead.
    ///
    /// This method will be removed in the next major version.
    #[deprecated(since = "1.1.0", note = "use `run_with_scope` instead")]
    #[inline(always)]
    pub fn consume_with_scope(&self, scope: &mut Scope, script: &str) -> RhaiResultOf<()> {
        self.run_with_scope(scope, script)
    }

    /// Evaluate an [`AST`], but throw away the result and only return error (if any).
    /// Useful for when you don't need the result, but still need to keep track of possible errors.
    ///
    /// # Deprecated
    ///
    /// This method is deprecated. Use [`run_ast`][Engine::run_ast] instead.
    ///
    /// This method will be removed in the next major version.
    #[deprecated(since = "1.1.0", note = "use `run_ast` instead")]
    #[inline(always)]
    pub fn consume_ast(&self, ast: &AST) -> RhaiResultOf<()> {
        self.run_ast(ast)
    }

    /// Evaluate an [`AST`] with own scope, but throw away the result and only return error (if any).
    /// Useful for when you don't need the result, but still need to keep track of possible errors.
    ///
    /// # Deprecated
    ///
    /// This method is deprecated. Use [`run_ast_with_scope`][Engine::run_ast_with_scope] instead.
    ///
    /// This method will be removed in the next major version.
    #[deprecated(since = "1.1.0", note = "use `run_ast_with_scope` instead")]
    #[inline(always)]
    pub fn consume_ast_with_scope(&self, scope: &mut Scope, ast: &AST) -> RhaiResultOf<()> {
        self.run_ast_with_scope(scope, ast)
    }
    /// Call a script function defined in an [`AST`] with multiple [`Dynamic`] arguments
    /// and optionally a value for binding to the `this` pointer.
    ///
    /// Not available under `no_function`.
    ///
    /// There is an option to evaluate the [`AST`] to load necessary modules before calling the function.
    ///
    /// # Deprecated
    ///
    /// This method is deprecated. Use [`run_ast_with_scope`][Engine::run_ast_with_scope] instead.
    ///
    /// This method will be removed in the next major version.
    ///
    /// # WARNING - Low Level API
    ///
    /// This function is very low level.
    ///
    /// # Arguments
    ///
    /// All the arguments are _consumed_, meaning that they're replaced by `()`.
    /// This is to avoid unnecessarily cloning the arguments.
    ///
    /// Do not use the arguments after this call. If they are needed afterwards,
    /// clone them _before_ calling this function.
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// # #[cfg(not(feature = "no_function"))]
    /// # {
    /// use rhai::{Engine, Scope, Dynamic};
    ///
    /// let engine = Engine::new();
    ///
    /// let ast = engine.compile("
    ///     fn add(x, y) { len(x) + y + foo }
    ///     fn add1(x)   { len(x) + 1 + foo }
    ///     fn bar()     { foo/2 }
    ///     fn action(x) { this += x; }         // function using 'this' pointer
    /// ")?;
    ///
    /// let mut scope = Scope::new();
    /// scope.push("foo", 42_i64);
    ///
    /// // Call the script-defined function
    /// let result = engine.call_fn_dynamic(&mut scope, &ast, true, "add", None, [ "abc".into(), 123_i64.into() ])?;
    /// //                                                                 ^^^^ no 'this' pointer
    /// assert_eq!(result.cast::<i64>(), 168);
    ///
    /// let result = engine.call_fn_dynamic(&mut scope, &ast, true, "add1", None, [ "abc".into() ])?;
    /// assert_eq!(result.cast::<i64>(), 46);
    ///
    /// let result = engine.call_fn_dynamic(&mut scope, &ast, true, "bar", None, [])?;
    /// assert_eq!(result.cast::<i64>(), 21);
    ///
    /// let mut value: Dynamic = 1_i64.into();
    /// let result = engine.call_fn_dynamic(&mut scope, &ast, true, "action", Some(&mut value), [ 41_i64.into() ])?;
    /// //                                                                    ^^^^^^^^^^^^^^^^ binding the 'this' pointer
    /// assert_eq!(value.as_int().expect("value should be INT"), 42);
    /// # }
    /// # Ok(())
    /// # }
    /// ```
    #[deprecated(since = "1.1.0", note = "use `call_fn_raw` instead")]
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    pub fn call_fn_dynamic(
        &self,
        scope: &mut Scope,
        ast: &AST,
        eval_ast: bool,
        name: impl AsRef<str>,
        this_ptr: Option<&mut Dynamic>,
        arg_values: impl AsMut<[Dynamic]>,
    ) -> RhaiResult {
        self.call_fn_raw(scope, ast, eval_ast, true, name, this_ptr, arg_values)
    }
}

impl Dynamic {
    /// Convert the [`Dynamic`] into a [`String`] and return it.
    /// If there are other references to the same string, a cloned copy is returned.
    /// Returns the name of the actual type if the cast fails.
    ///
    /// # Deprecated
    ///
    /// This method is deprecated. Use [`into_string`][Dynamic::into_string] instead.
    ///
    /// This method will be removed in the next major version.
    #[deprecated(since = "1.1.0", note = "use `into_string` instead")]
    #[inline(always)]
    pub fn as_string(self) -> Result<String, &'static str> {
        self.into_string()
    }

    /// Convert the [`Dynamic`] into an [`ImmutableString`] and return it.
    /// Returns the name of the actual type if the cast fails.
    ///
    /// # Deprecated
    ///
    /// This method is deprecated. Use [`into_immutable_string`][Dynamic::into_immutable_string] instead.
    ///
    /// This method will be removed in the next major version.
    #[deprecated(since = "1.1.0", note = "use `into_immutable_string` instead")]
    #[inline(always)]
    pub fn as_immutable_string(self) -> Result<ImmutableString, &'static str> {
        self.into_immutable_string()
    }
}

impl NativeCallContext<'_> {
    /// Call a function inside the call context.
    ///
    /// # WARNING - Low Level API
    ///
    /// This function is very low level.
    ///
    /// # Arguments
    ///
    /// All arguments may be _consumed_, meaning that they may be replaced by `()`. This is to avoid
    /// unnecessarily cloning the arguments.
    ///
    /// Do not use the arguments after this call. If they are needed afterwards, clone them _before_
    /// calling this function.
    ///
    /// If `is_method` is [`true`], the first argument is assumed to be passed by reference and is
    /// not consumed.
    ///
    /// # Deprecated
    ///
    /// This method is deprecated. Use [`call_fn_raw`][NativeCallContext::call_fn_raw] instead.
    ///
    /// This method will be removed in the next major version.
    #[deprecated(since = "1.2.0", note = "use `call_fn_raw` instead")]
    #[inline(always)]
    pub fn call_fn_dynamic_raw(
        &self,
        fn_name: impl AsRef<str>,
        is_method_call: bool,
        args: &mut [&mut Dynamic],
    ) -> RhaiResult {
        self.call_fn_raw(fn_name.as_ref(), is_method_call, is_method_call, args)
    }
}

#[allow(useless_deprecated)]
#[deprecated(since = "1.2.0", note = "explicitly wrap `EvalAltResult` in `Err`")]
impl<T> From<EvalAltResult> for RhaiResultOf<T> {
    #[inline(always)]
    fn from(err: EvalAltResult) -> Self {
        Err(err.into())
    }
}

impl FnPtr {
    /// Call the function pointer with curried arguments (if any).
    /// The function may be script-defined (not available under `no_function`) or native Rust.
    ///
    /// This method is intended for calling a function pointer that is passed into a native Rust
    /// function as an argument.  Therefore, the [`AST`] is _NOT_ evaluated before calling the
    /// function.
    ///
    /// # Deprecated
    ///
    /// This method is deprecated. Use [`call_within_context`][FnPtr::call_within_context] or
    /// [`call_raw`][FnPtr::call_raw] instead.
    ///
    /// This method will be removed in the next major version.
    ///
    /// # WARNING - Low Level API
    ///
    /// This function is very low level.
    ///
    /// # Arguments
    ///
    /// All the arguments are _consumed_, meaning that they're replaced by `()`.
    /// This is to avoid unnecessarily cloning the arguments.
    ///
    /// Do not use the arguments after this call. If they are needed afterwards,
    /// clone them _before_ calling this function.
    #[deprecated(
        since = "1.3.0",
        note = "use `call_within_context` or `call_raw` instead"
    )]
    #[inline(always)]
    pub fn call_dynamic(
        &self,
        context: &NativeCallContext,
        this_ptr: Option<&mut Dynamic>,
        arg_values: impl AsMut<[Dynamic]>,
    ) -> RhaiResult {
        self.call_raw(context, this_ptr, arg_values)
    }
}

impl Expression<'_> {
    /// If this expression is a variable name, return it.  Otherwise [`None`].
    ///
    /// # Deprecated
    ///
    /// This method is deprecated. Use [`get_string_value`][Expression::get_string_value] instead.
    ///
    /// This method will be removed in the next major version.
    #[deprecated(since = "1.4.0", note = "use `get_string_value` instead")]
    #[inline(always)]
    #[must_use]
    pub fn get_variable_name(&self) -> Option<&str> {
        self.get_string_value()
    }
}

impl Position {
    /// Create a new [`Position`].
    ///
    /// If `line` is zero, then [`None`] is returned.
    ///
    /// If `position` is zero, then it is at the beginning of a line.
    ///
    /// # Deprecated
    ///
    /// This function is deprecated. Use [`new`][Position::new] (which panics when `line` is zero) instead.
    ///
    /// This method will be removed in the next major version.
    #[deprecated(since = "1.6.0", note = "use `new` instead")]
    #[inline(always)]
    #[must_use]
    pub const fn new_const(line: u16, position: u16) -> Option<Self> {
        if line == 0 {
            None
        } else {
            Some(Self::new(line, position))
        }
    }
}
