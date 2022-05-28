//! Module defining macros for developing _plugins_.

pub use super::CallableFunction;
use super::FnCallArgs;
pub use crate::{
    Dynamic, Engine, EvalAltResult, FnAccess, FnNamespace, ImmutableString, Module,
    NativeCallContext, Position,
};
pub use core::{any::TypeId, mem};

pub type RhaiResult = crate::RhaiResult;

#[cfg(not(features = "no_module"))]
pub use rhaink_codegen::*;
#[cfg(features = "no_module")]
pub use rhaink_codegen::{export_fn, register_exported_fn};

/// Trait implemented by a _plugin function_.
///
/// This trait should not be used directly.
/// Use the `#[export_module]` and `#[export_fn]` procedural attributes instead.
pub trait PluginFunction {
    /// Call the plugin function with the arguments provided.
    fn call(&self, context: NativeCallContext, args: &mut FnCallArgs) -> RhaiResult;

    /// Is this plugin function a method?
    #[must_use]
    fn is_method_call(&self) -> bool;
}
