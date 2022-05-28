//! Settings for [`Engine`]'s limitations.
#![cfg(not(feature = "unchecked"))]

use super::default_limits;
use crate::Engine;
use core::num::{NonZeroU64, NonZeroUsize};

/// A type containing all the limits imposed by the [`Engine`].
///
/// Not available under `unchecked`.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Limits {
    /// Maximum levels of call-stack to prevent infinite recursion.
    ///
    /// Set to zero to effectively disable function calls.
    ///
    /// Not available under `no_function`.
    #[cfg(not(feature = "no_function"))]
    pub max_call_stack_depth: usize,
    /// Maximum depth of statements/expressions at global level.
    pub max_expr_depth: Option<NonZeroUsize>,
    /// Maximum depth of statements/expressions in functions.
    ///
    /// Not available under `no_function`.
    #[cfg(not(feature = "no_function"))]
    pub max_function_expr_depth: Option<NonZeroUsize>,
    /// Maximum number of operations allowed to run.
    pub max_operations: Option<NonZeroU64>,
    /// Maximum number of [modules][crate::Module] allowed to load.
    ///
    /// Set to zero to effectively disable loading any [module][crate::Module].
    ///
    /// Not available under `no_module`.
    #[cfg(not(feature = "no_module"))]
    pub max_modules: usize,
    /// Maximum length of a [string][crate::ImmutableString].
    pub max_string_size: Option<NonZeroUsize>,
    /// Maximum length of an [array][crate::Array].
    ///
    /// Not available under `no_index`.
    #[cfg(not(feature = "no_index"))]
    pub max_array_size: Option<NonZeroUsize>,
    /// Maximum number of properties in an [object map][crate::Map].
    ///
    /// Not available under `no_object`.
    #[cfg(not(feature = "no_object"))]
    pub max_map_size: Option<NonZeroUsize>,
}

impl Limits {
    /// Create a new [`Limits`] with default values.
    ///
    /// Not available under `unchecked`.
    #[inline]
    pub const fn new() -> Self {
        Self {
            #[cfg(not(feature = "no_function"))]
            max_call_stack_depth: default_limits::MAX_CALL_STACK_DEPTH,
            max_expr_depth: NonZeroUsize::new(default_limits::MAX_EXPR_DEPTH),
            #[cfg(not(feature = "no_function"))]
            max_function_expr_depth: NonZeroUsize::new(default_limits::MAX_FUNCTION_EXPR_DEPTH),
            max_operations: None,
            #[cfg(not(feature = "no_module"))]
            max_modules: usize::MAX,
            max_string_size: None,
            #[cfg(not(feature = "no_index"))]
            max_array_size: None,
            #[cfg(not(feature = "no_object"))]
            max_map_size: None,
        }
    }
}

impl Default for Limits {
    #[inline(always)]
    fn default() -> Self {
        Self::new()
    }
}

impl Engine {
    /// Set the maximum levels of function calls allowed for a script in order to avoid
    /// infinite recursion and stack overflows.
    ///
    /// Not available under `unchecked` or `no_function`.
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    pub fn set_max_call_levels(&mut self, levels: usize) -> &mut Self {
        self.limits.max_call_stack_depth = levels;
        self
    }
    /// The maximum levels of function calls allowed for a script.
    ///
    /// Not available under `unchecked` or `no_function`.
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    #[must_use]
    pub const fn max_call_levels(&self) -> usize {
        self.limits.max_call_stack_depth
    }
    /// Set the maximum number of operations allowed for a script to run to avoid
    /// consuming too much resources (0 for unlimited).
    ///
    /// Not available under `unchecked`.
    #[inline(always)]
    pub fn set_max_operations(&mut self, operations: u64) -> &mut Self {
        self.limits.max_operations = NonZeroU64::new(operations);
        self
    }
    /// The maximum number of operations allowed for a script to run (0 for unlimited).
    ///
    /// Not available under `unchecked`.
    #[inline]
    #[must_use]
    pub const fn max_operations(&self) -> u64 {
        if let Some(n) = self.limits.max_operations {
            n.get()
        } else {
            0
        }
    }
    /// Set the maximum number of imported [modules][crate::Module] allowed for a script.
    ///
    /// Not available under `unchecked` or `no_module`.
    #[cfg(not(feature = "no_module"))]
    #[inline(always)]
    pub fn set_max_modules(&mut self, modules: usize) -> &mut Self {
        self.limits.max_modules = modules;
        self
    }
    /// The maximum number of imported [modules][crate::Module] allowed for a script.
    ///
    /// Not available under `unchecked` or `no_module`.
    #[cfg(not(feature = "no_module"))]
    #[inline(always)]
    #[must_use]
    pub const fn max_modules(&self) -> usize {
        self.limits.max_modules
    }
    /// Set the depth limits for expressions (0 for unlimited).
    ///
    /// Not available under `unchecked`.
    #[inline(always)]
    pub fn set_max_expr_depths(
        &mut self,
        max_expr_depth: usize,
        #[cfg(not(feature = "no_function"))] max_function_expr_depth: usize,
    ) -> &mut Self {
        self.limits.max_expr_depth = NonZeroUsize::new(max_expr_depth);
        #[cfg(not(feature = "no_function"))]
        {
            self.limits.max_function_expr_depth = NonZeroUsize::new(max_function_expr_depth);
        }
        self
    }
    /// The depth limit for expressions (0 for unlimited).
    ///
    /// Not available under `unchecked`.
    #[inline]
    #[must_use]
    pub const fn max_expr_depth(&self) -> usize {
        if let Some(n) = self.limits.max_expr_depth {
            n.get()
        } else {
            0
        }
    }
    /// The depth limit for expressions in functions (0 for unlimited).
    ///
    /// Not available under `unchecked` or `no_function`.
    #[cfg(not(feature = "no_function"))]
    #[inline]
    #[must_use]
    pub const fn max_function_expr_depth(&self) -> usize {
        if let Some(n) = self.limits.max_function_expr_depth {
            n.get()
        } else {
            0
        }
    }
    /// Set the maximum length of [strings][crate::ImmutableString] (0 for unlimited).
    ///
    /// Not available under `unchecked`.
    #[inline(always)]
    pub fn set_max_string_size(&mut self, max_size: usize) -> &mut Self {
        self.limits.max_string_size = NonZeroUsize::new(max_size);
        self
    }
    /// The maximum length of [strings][crate::ImmutableString] (0 for unlimited).
    ///
    /// Not available under `unchecked`.
    #[inline]
    #[must_use]
    pub const fn max_string_size(&self) -> usize {
        if let Some(n) = self.limits.max_string_size {
            n.get()
        } else {
            0
        }
    }
    /// Set the maximum length of [arrays][crate::Array] (0 for unlimited).
    ///
    /// Not available under `unchecked` or `no_index`.
    #[cfg(not(feature = "no_index"))]
    #[inline(always)]
    pub fn set_max_array_size(&mut self, max_size: usize) -> &mut Self {
        self.limits.max_array_size = NonZeroUsize::new(max_size);
        self
    }
    /// The maximum length of [arrays][crate::Array] (0 for unlimited).
    ///
    /// Not available under `unchecked` or `no_index`.
    #[cfg(not(feature = "no_index"))]
    #[inline]
    #[must_use]
    pub const fn max_array_size(&self) -> usize {
        if let Some(n) = self.limits.max_array_size {
            n.get()
        } else {
            0
        }
    }
    /// Set the maximum size of [object maps][crate::Map] (0 for unlimited).
    ///
    /// Not available under `unchecked` or `no_object`.
    #[cfg(not(feature = "no_object"))]
    #[inline(always)]
    pub fn set_max_map_size(&mut self, max_size: usize) -> &mut Self {
        self.limits.max_map_size = NonZeroUsize::new(max_size);
        self
    }
    /// The maximum size of [object maps][crate::Map] (0 for unlimited).
    ///
    /// Not available under `unchecked` or `no_object`.
    #[cfg(not(feature = "no_object"))]
    #[inline]
    #[must_use]
    pub const fn max_map_size(&self) -> usize {
        if let Some(n) = self.limits.max_map_size {
            n.get()
        } else {
            0
        }
    }
}
