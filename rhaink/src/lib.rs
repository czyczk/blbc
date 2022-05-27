mod api;
mod ast;
mod engine;
mod eval;
mod func;
mod module;
mod optimizer;
mod packages;
mod parser;
mod reify;
mod tokenizer;
mod types;

extern crate alloc;

use ink_prelude::vec::Vec;

/// Error encountered when parsing a script.
type PERR = ParseErrorType;
/// Evaluation result.
type ERR = EvalAltResult;
/// General evaluation error for Rhai scripts.
type RhaiError = Box<ERR>;
/// Generic [`Result`] type for Rhai functions.
type RhaiResultOf<T> = Result<T, RhaiError>;
/// General [`Result`] type for Rhai functions returning [`Dynamic`] values.
type RhaiResult = RhaiResultOf<Dynamic>;

/// The system integer type.
/// It is defined as [`i32`] since the `only_i32` feature is used.
///
/// If the `only_i32` feature is not used, this will be `i64` instead.
pub type INT = i32;

/// The unsigned system integer base type.
/// It is defined as [`u32`] since the `only_i32` feature is used.
///
/// If the `only_i32` feature is not used, this will be `u64` instead.
#[allow(non_camel_case_types)]
type UNSIGNED_INT = u32;

/// Number of bits in [`INT`].
///
/// It is 64 unless the `only_i32` feature is enabled when it will be 32.
const INT_BITS: usize = std::mem::size_of::<INT>() * 8;

/// Number of bytes that make up an [`INT`].
///
/// It is 8 unless the `only_i32` feature is enabled when it will be 4.
const INT_BYTES: usize = std::mem::size_of::<INT>();

/// An exclusive integer range.
type ExclusiveRange = std::ops::Range<INT>;

/// An inclusive integer range.
type InclusiveRange = std::ops::RangeInclusive<INT>;

pub use api::custom_syntax::Expression;
pub use api::events::VarDefInfo;
pub use ast::{FnAccess, AST};
pub use engine::{Engine, OP_CONTAINS, OP_EQUALS};
pub use eval::EvalContext;
pub use func::{NativeCallContext, RegisterNativeFunction};
pub use module::{FnNamespace, Module};
pub use tokenizer::Position;
#[cfg(not(feature = "no_std"))]
pub use types::Instant;
pub use types::{
    Dynamic, EvalAltResult, FnPtr, ImmutableString, LexError, ParseError, ParseErrorType, Scope,
};

/// _(debugging)_ Module containing types for debugging.
/// Exported under the `debugging` feature only.
#[cfg(feature = "debugging")]
pub mod debugger {
    #[cfg(not(feature = "no_function"))]
    pub use super::eval::CallStackFrame;
    pub use super::eval::{BreakPoint, Debugger, DebuggerCommand, DebuggerEvent};
}

/// An identifier in Rhai. [`SmartString`](https://crates.io/crates/smartstring) is used because most
/// identifiers are ASCII and short, fewer than 23 characters, so they can be stored inline.
#[cfg(not(feature = "internals"))]
pub(crate) type Identifier = SmartString;

/// An identifier in Rhai. [`SmartString`](https://crates.io/crates/smartstring) is used because most
/// identifiers are ASCII and short, fewer than 23 characters, so they can be stored inline.
#[cfg(feature = "internals")]
pub type Identifier = SmartString;

/// Alias to [`Rc`][std::rc::Rc] or [`Arc`][std::sync::Arc] depending on the `sync` feature flag.
pub use func::Shared;

/// Alias to [`RefCell`][std::cell::RefCell] or [`RwLock`][std::sync::RwLock] depending on the `sync` feature flag.
pub use func::Locked;

#[allow(dead_code)]
pub(crate) use func::{
    calc_fn_hash, calc_fn_params_hash, calc_qualified_fn_hash, calc_qualified_var_hash,
    combine_hashes,
};

pub use rhaink_codegen::*;

pub use func::{plugin, FuncArgs};

/// Inline arguments storage for function calls.
///
/// # Notes
///
/// Since most usage of this is during a function call to gather up arguments, this is mostly
/// allocated on the stack, so we can tolerate a larger number of values stored inline.
///
/// Most functions have few parameters, but closures with a lot of captured variables can
/// potentially have many.  Having a larger inline storage for arguments reduces allocations in
/// scripts with heavy closure usage.
///
/// Under `no_closure`, this type aliases to [`StaticVec`][crate::StaticVec] instead.
#[cfg(not(feature = "no_closure"))]
type FnArgsVec<T> = smallvec::SmallVec<[T; 5]>;

/// Inline arguments storage for function calls.
/// This type aliases to [`StaticVec`][crate::StaticVec].
#[cfg(feature = "no_closure")]
type FnArgsVec<T> = crate::StaticVec<T>;

pub(crate) type SmartString = smartstring::SmartString<smartstring::LazyCompact>;

/// Alias to [`smallvec::SmallVec<[T; 3]>`](https://crates.io/crates/smallvec), which is a
/// specialized [`Vec`] backed by a small, inline, fixed-size array when there are ≤ 3 items stored.
///
/// # History
///
/// And Saint Attila raised the `SmallVec` up on high, saying, "O Lord, bless this Thy `SmallVec`
/// that, with it, Thou mayest blow Thine allocation costs to tiny bits in Thy mercy."
///
/// And the Lord did grin, and the people did feast upon the lambs and sloths and carp and anchovies
/// and orangutans and breakfast cereals and fruit bats and large chu...
///
/// And the Lord spake, saying, "First shalt thou depend on the [`smallvec`](https://crates.io/crates/smallvec) crate.
/// Then, shalt thou keep three inline. No more. No less. Three shalt be the number thou shalt keep inline,
/// and the number to keep inline shalt be three. Four shalt thou not keep inline, nor either keep inline
/// thou two, excepting that thou then proceed to three. Five is right out. Once the number three,
/// being the third number, be reached, then, lobbest thou thy `SmallVec` towards thy heap, who,
/// being slow and cache-naughty in My sight, shall snuff it."
///
/// # Why Three
///
/// `StaticVec` is used frequently to keep small lists of items in inline (non-heap) storage in
/// order to improve cache friendliness and reduce indirections.
///
/// The number 3, other than being the holy number, is carefully chosen for a balance between
/// storage space and reduce allocations. That is because most function calls (and most functions,
/// for that matter) contain fewer than 4 arguments, the exception being closures that capture a
/// large number of external variables.
///
/// In addition, most script blocks either contain many statements, or just one or two lines;
/// most scripts load fewer than 4 external modules; most module paths contain fewer than 4 levels
/// (e.g. `std::collections::map::HashMap` is 4 levels and it is just about as long as they get).
type StaticVec<T> = smallvec::SmallVec<[T; 3]>;

/// Variable-sized array of [`Dynamic`] values.
pub type Array = Vec<Dynamic>;

/// Variable-sized array of [`u8`] values (byte array).
pub type Blob = Vec<u8>;

/// A dictionary of [`Dynamic`] values with string keys.
/// [`SmartString`](https://crates.io/crates/smartstring) is used as the key type because most
/// property names are ASCII and short, fewer than 23 characters, so they can be stored inline.
pub type Map = ink_prelude::collections::BTreeMap<Identifier, Dynamic>;

#[cfg(not(feature = "no_optimize"))]
pub use optimizer::OptimizationLevel;

/// Placeholder for the optimization level.
#[cfg(feature = "no_optimize")]
pub type OptimizationLevel = ();
