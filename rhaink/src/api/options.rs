//! Settings for [`Engine`]'s language options.

use crate::{Engine, OptimizationLevel};

/// A type containing all language options for the [`Engine`].
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct LanguageOptions {
    /// Script optimization level.
    pub optimization_level: OptimizationLevel,
    /// Is `if`-expression allowed?
    pub allow_if_expr: bool,
    /// Is `switch` expression allowed?
    pub allow_switch_expr: bool,
    /// Is statement-expression allowed?
    pub allow_stmt_expr: bool,
    /// Is anonymous function allowed?
    #[cfg(not(feature = "no_function"))]
    pub allow_anonymous_fn: bool,
    /// Is looping allowed?
    pub allow_looping: bool,
    /// Is variables shadowing allowed?
    pub allow_shadowing: bool,
    /// Strict variables mode?
    pub strict_var: bool,
    /// Raise error if an object map property does not exist?
    /// Returns `()` if `false`.
    #[cfg(not(feature = "no_object"))]
    pub fail_on_invalid_map_property: bool,
}

impl LanguageOptions {
    /// Create a new [`Options`] with default values.
    #[inline(always)]
    pub const fn new() -> Self {
        Self {
            #[cfg(not(feature = "no_optimize"))]
            optimization_level: OptimizationLevel::Simple,
            #[cfg(feature = "no_optimize")]
            optimization_level: (),

            allow_if_expr: true,
            allow_switch_expr: true,
            allow_stmt_expr: true,
            #[cfg(not(feature = "no_function"))]
            allow_anonymous_fn: true,
            allow_looping: true,
            strict_var: false,
            allow_shadowing: true,
            #[cfg(not(feature = "no_object"))]
            fail_on_invalid_map_property: false,
        }
    }
}

impl Default for LanguageOptions {
    fn default() -> Self {
        Self::new()
    }
}

impl Engine {
    /// Is `if`-expression allowed?
    /// Default is `true`.
    #[inline(always)]
    pub const fn allow_if_expression(&self) -> bool {
        self.options.allow_if_expr
    }
    /// Set whether `if`-expression is allowed.
    #[inline(always)]
    pub fn set_allow_if_expression(&mut self, enable: bool) {
        self.options.allow_if_expr = enable;
    }
    /// Is `switch` expression allowed?
    /// Default is `true`.
    #[inline(always)]
    pub const fn allow_switch_expression(&self) -> bool {
        self.options.allow_switch_expr
    }
    /// Set whether `switch` expression is allowed.
    #[inline(always)]
    pub fn set_allow_switch_expression(&mut self, enable: bool) {
        self.options.allow_switch_expr = enable;
    }
    /// Is statement-expression allowed?
    /// Default is `true`.
    #[inline(always)]
    pub const fn allow_statement_expression(&self) -> bool {
        self.options.allow_stmt_expr
    }
    /// Set whether statement-expression is allowed.
    #[inline(always)]
    pub fn set_allow_statement_expression(&mut self, enable: bool) {
        self.options.allow_stmt_expr = enable;
    }
    /// Is anonymous function allowed?
    /// Default is `true`.
    ///
    /// Not available under `no_function`.
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    pub const fn allow_anonymous_fn(&self) -> bool {
        self.options.allow_anonymous_fn
    }
    /// Set whether anonymous function is allowed.
    ///
    /// Not available under `no_function`.
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    pub fn set_allow_anonymous_fn(&mut self, enable: bool) {
        self.options.allow_anonymous_fn = enable;
    }
    /// Is looping allowed?
    /// Default is `true`.
    #[inline(always)]
    pub const fn allow_looping(&self) -> bool {
        self.options.allow_looping
    }
    /// Set whether looping is allowed.
    #[inline(always)]
    pub fn set_allow_looping(&mut self, enable: bool) {
        self.options.allow_looping = enable;
    }
    /// Is variables shadowing allowed?
    /// Default is `true`.
    #[inline(always)]
    pub const fn allow_shadowing(&self) -> bool {
        self.options.allow_shadowing
    }
    /// Set whether variables shadowing is allowed.
    #[inline(always)]
    pub fn set_allow_shadowing(&mut self, enable: bool) {
        self.options.allow_shadowing = enable;
    }
    /// Is strict variables mode enabled?
    /// Default is `false`.
    #[inline(always)]
    pub const fn strict_variables(&self) -> bool {
        self.options.strict_var
    }
    /// Set whether strict variables mode is enabled.
    #[inline(always)]
    pub fn set_strict_variables(&mut self, enable: bool) {
        self.options.strict_var = enable;
    }
    /// Raise error if an object map property does not exist?
    /// Default is `false`.
    ///
    /// Not available under `no_object`.
    #[cfg(not(feature = "no_object"))]
    #[inline(always)]
    pub const fn fail_on_invalid_map_property(&self) -> bool {
        self.options.fail_on_invalid_map_property
    }
    /// Set whether to raise error if an object map property does not exist.
    ///
    /// Not available under `no_object`.
    #[cfg(not(feature = "no_object"))]
    #[inline(always)]
    pub fn set_fail_on_invalid_map_property(&mut self, enable: bool) {
        self.options.fail_on_invalid_map_property = enable;
    }
}
