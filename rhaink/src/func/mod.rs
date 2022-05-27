pub mod args;
mod builtin;
mod call;
pub mod callable_function;
pub mod hashing;
pub mod native;
pub mod plugin;
pub mod register;

pub use args::FuncArgs;
pub use builtin::{get_builtin_binary_op_fn, get_builtin_op_assignment_fn};
pub use call::FnCallArgs;
pub use callable_function::CallableFunction;
#[cfg(not(feature = "no_function"))]
pub use func::Func;
pub use hashing::{
    calc_fn_hash, calc_fn_params_hash, calc_qualified_fn_hash, calc_qualified_var_hash,
    combine_hashes, get_hasher,
};
pub use native::{
    locked_write, shared_make_mut, shared_take, shared_take_or_clone, shared_try_take, FnAny,
    FnPlugin, IteratorFn, Locked, NativeCallContext, SendSync, Shared,
};
pub use plugin::PluginFunction;
pub use register::RegisterNativeFunction;
