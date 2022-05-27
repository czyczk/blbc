use core::fmt;

use crate::{ast::flags::FnAccess, Shared};

use super::native::{FnAny, FnPlugin, IteratorFn};

extern crate alloc;

/// A type encapsulating a function callable by Rhai.
#[derive(Clone)]
#[non_exhaustive]
pub enum CallableFunction {
    /// A pure native Rust function with all arguments passed by value.
    Pure(Shared<FnAny>),
    /// A native Rust object method with the first argument passed by reference,
    /// and the rest passed by value.
    Method(Shared<FnAny>),
    /// An iterator function.
    Iterator(Shared<IteratorFn>),
    /// A plugin function,
    Plugin(Shared<FnPlugin>),
    /// A script-defined function.
    #[cfg(not(feature = "no_function"))]
    Script(Shared<crate::ast::ScriptFnDef>),
}

impl fmt::Debug for CallableFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Pure(..) => write!(f, "NativePureFunction"),
            Self::Method(..) => write!(f, "NativeMethod"),
            Self::Iterator(..) => write!(f, "NativeIterator"),
            Self::Plugin(..) => write!(f, "PluginFunction"),

            #[cfg(not(feature = "no_function"))]
            Self::Script(fn_def) => fmt::Debug::fmt(fn_def, f),
        }
    }
}

impl fmt::Display for CallableFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Pure(..) => write!(f, "NativePureFunction"),
            Self::Method(..) => write!(f, "NativeMethod"),
            Self::Iterator(..) => write!(f, "NativeIterator"),
            Self::Plugin(..) => write!(f, "PluginFunction"),

            #[cfg(not(feature = "no_function"))]
            Self::Script(s) => fmt::Display::fmt(s, f),
        }
    }
}

impl CallableFunction {
    /// Is this a pure native Rust function?
    #[inline]
    #[must_use]
    pub fn is_pure(&self) -> bool {
        match self {
            Self::Pure(..) => true,
            Self::Method(..) | Self::Iterator(..) => false,

            Self::Plugin(p) => !p.is_method_call(),

            #[cfg(not(feature = "no_function"))]
            Self::Script(..) => false,
        }
    }
    /// Is this a native Rust method function?
    #[inline]
    #[must_use]
    pub fn is_method(&self) -> bool {
        match self {
            Self::Method(..) => true,
            Self::Pure(..) | Self::Iterator(..) => false,

            Self::Plugin(p) => p.is_method_call(),

            #[cfg(not(feature = "no_function"))]
            Self::Script(..) => false,
        }
    }
    /// Is this an iterator function?
    #[inline]
    #[must_use]
    pub const fn is_iter(&self) -> bool {
        match self {
            Self::Iterator(..) => true,
            Self::Pure(..) | Self::Method(..) | Self::Plugin(..) => false,

            #[cfg(not(feature = "no_function"))]
            Self::Script(..) => false,
        }
    }
    /// Is this a script-defined function?
    #[inline]
    #[must_use]
    pub const fn is_script(&self) -> bool {
        #[cfg(feature = "no_function")]
        return false;

        #[cfg(not(feature = "no_function"))]
        match self {
            Self::Script(..) => true,
            Self::Pure(..) | Self::Method(..) | Self::Iterator(..) | Self::Plugin(..) => false,
        }
    }
    /// Is this a plugin function?
    #[inline]
    #[must_use]
    pub const fn is_plugin_fn(&self) -> bool {
        match self {
            Self::Plugin(..) => true,
            Self::Pure(..) | Self::Method(..) | Self::Iterator(..) => false,

            #[cfg(not(feature = "no_function"))]
            Self::Script(..) => false,
        }
    }
    /// Is this a native Rust function?
    #[inline]
    #[must_use]
    pub const fn is_native(&self) -> bool {
        #[cfg(feature = "no_function")]
        return true;

        #[cfg(not(feature = "no_function"))]
        match self {
            Self::Pure(..) | Self::Method(..) => true,
            Self::Plugin(..) => true,
            Self::Iterator(..) => true,
            Self::Script(..) => false,
        }
    }
    /// Get the access mode.
    #[inline]
    #[must_use]
    pub fn access(&self) -> FnAccess {
        #[cfg(feature = "no_function")]
        return FnAccess::Public;

        #[cfg(not(feature = "no_function"))]
        match self {
            Self::Plugin(..) => FnAccess::Public,
            Self::Pure(..) | Self::Method(..) | Self::Iterator(..) => FnAccess::Public,
            Self::Script(f) => f.access,
        }
    }
    /// Get a shared reference to a native Rust function.
    #[inline]
    #[must_use]
    pub fn get_native_fn(&self) -> Option<&Shared<FnAny>> {
        match self {
            Self::Pure(f) | Self::Method(f) => Some(f),
            Self::Iterator(..) | Self::Plugin(..) => None,

            #[cfg(not(feature = "no_function"))]
            Self::Script(..) => None,
        }
    }
    /// Get a shared reference to a script-defined function definition.
    ///
    /// Not available under `no_function`.
    #[cfg(not(feature = "no_function"))]
    #[inline]
    #[must_use]
    pub const fn get_script_fn_def(&self) -> Option<&Shared<crate::ast::ScriptFnDef>> {
        match self {
            Self::Pure(..) | Self::Method(..) | Self::Iterator(..) | Self::Plugin(..) => None,
            Self::Script(f) => Some(f),
        }
    }
    /// Get a reference to an iterator function.
    #[inline]
    #[must_use]
    pub fn get_iter_fn(&self) -> Option<&IteratorFn> {
        match self {
            Self::Iterator(f) => Some(f.as_ref()),
            Self::Pure(..) | Self::Method(..) | Self::Plugin(..) => None,

            #[cfg(not(feature = "no_function"))]
            Self::Script(..) => None,
        }
    }
    /// Get a shared reference to a plugin function.
    #[inline]
    #[must_use]
    pub fn get_plugin_fn(&self) -> Option<&Shared<FnPlugin>> {
        match self {
            Self::Plugin(f) => Some(f),
            Self::Pure(..) | Self::Method(..) | Self::Iterator(..) => None,

            #[cfg(not(feature = "no_function"))]
            Self::Script(..) => None,
        }
    }
    /// Create a new [`CallableFunction::Pure`].
    #[inline(always)]
    #[must_use]
    pub fn from_pure(func: Box<FnAny>) -> Self {
        Self::Pure(func.into())
    }
    /// Create a new [`CallableFunction::Method`].
    #[inline(always)]
    #[must_use]
    pub fn from_method(func: Box<FnAny>) -> Self {
        Self::Method(func.into())
    }
    /// Create a new [`CallableFunction::Plugin`].
    #[inline(always)]
    #[must_use]
    pub fn from_plugin(func: impl PluginFunction + 'static + SendSync) -> Self {
        Self::Plugin((Box::new(func) as Box<FnPlugin>).into())
    }
}

#[cfg(not(feature = "no_function"))]
impl From<crate::ast::ScriptFnDef> for CallableFunction {
    #[inline(always)]
    fn from(_func: crate::ast::ScriptFnDef) -> Self {
        Self::Script(_func.into())
    }
}

#[cfg(not(feature = "no_function"))]
impl From<Shared<crate::ast::ScriptFnDef>> for CallableFunction {
    #[inline(always)]
    fn from(_func: Shared<crate::ast::ScriptFnDef>) -> Self {
        Self::Script(_func)
    }
}

impl<T: PluginFunction + 'static + SendSync> From<T> for CallableFunction {
    #[inline(always)]
    fn from(func: T) -> Self {
        Self::from_plugin(func)
    }
}

impl From<Shared<FnPlugin>> for CallableFunction {
    #[inline(always)]
    fn from(func: Shared<FnPlugin>) -> Self {
        Self::Plugin(func)
    }
}
