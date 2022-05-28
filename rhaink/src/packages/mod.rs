//! Module containing all built-in _packages_ available to Rhai, plus facilities to define custom packages.

use crate::{Module, Shared};

pub(crate) mod arithmetic;
pub(crate) mod array_basic;
mod bit_field;
pub(crate) mod blob_basic;
// mod debugging;
mod fn_basic;
pub(crate) mod iter_basic;
mod lang_core;
mod logic;
mod map_basic;
mod math_basic;
mod pkg_core;
mod pkg_std;
mod string_basic;
mod string_more;
mod time_basic;

pub use arithmetic::ArithmeticPackage;
#[cfg(not(feature = "no_index"))]
pub use array_basic::BasicArrayPackage;
pub use bit_field::BitFieldPackage;
#[cfg(not(feature = "no_index"))]
pub use blob_basic::BasicBlobPackage;
#[cfg(feature = "debugging")]
pub use debugging::DebuggingPackage;
pub use fn_basic::BasicFnPackage;
pub use iter_basic::BasicIteratorPackage;
pub use lang_core::LanguageCorePackage;
pub use logic::LogicPackage;
#[cfg(not(feature = "no_object"))]
pub use map_basic::BasicMapPackage;
pub use math_basic::BasicMathPackage;
pub use pkg_core::CorePackage;
pub use pkg_std::StandardPackage;
pub use string_basic::BasicStringPackage;
pub use string_more::MoreStringPackage;
#[cfg(not(feature = "no_std"))]
pub use time_basic::BasicTimePackage;

/// Trait that all packages must implement.
pub trait Package {
    /// Initialize the package.
    /// Functions should be registered into `module` here.
    fn init(module: &mut Module);

    /// Get a reference to a shared module from this package.
    #[must_use]
    fn as_shared_module(&self) -> Shared<Module>;
}

/// Macro that makes it easy to define a _package_ (which is basically a shared [module][Module])
/// and register functions into it.
///
/// Functions can be added to the package using [`Module::set_native_fn`].
///
/// # Example
///
/// Define a package named `MyPackage` with a single function named `my_add`:
///
/// ```
/// use rhai::{Dynamic, EvalAltResult};
/// use rhai::def_package;
///
/// fn add(x: i64, y: i64) -> Result<i64, Box<EvalAltResult>> { Ok(x + y) }
///
/// def_package! {
///     /// My super-duper package.
///     pub MyPackage(module) {
///         // Load a binary function with all value parameters.
///         module.set_native_fn("my_add", add);
///     }
/// }
/// ```
#[macro_export]
macro_rules! def_package {
    ($($(#[$outer:meta])* $mod:vis $package:ident($lib:ident) $block:block)+) => { $(
        $(#[$outer])*
        $mod struct $package($crate::Shared<$crate::Module>);

        impl $crate::packages::Package for $package {
            fn as_shared_module(&self) -> $crate::Shared<$crate::Module> {
                self.0.clone()
            }
            fn init($lib: &mut $crate::Module) {
                $block
            }
        }

        impl Default for $package {
            fn default() -> Self {
                Self::new()
            }
        }

        impl $package {
            pub fn new() -> Self {
                let mut module = $crate::Module::new();
                <Self as $crate::packages::Package>::init(&mut module);
                module.build_index();
                Self(module.into())
            }
        }
    )* };
    ($($(#[$outer:meta])* $root:ident :: $package:ident => | $lib:ident | $block:block)+) => { $(
        $(#[$outer])*
        /// # Deprecated
        ///
        /// This old syntax of `def_package!` is deprecated. Use the new syntax instead.
        ///
        /// This syntax will be removed in the next major version.
        #[deprecated(since = "1.5.0", note = "this is an old syntax of `def_package!` and is deprecated; use the new syntax of `def_package!` instead")]
        pub struct $package($root::Shared<$root::Module>);

        impl $root::packages::Package for $package {
            fn as_shared_module(&self) -> $root::Shared<$root::Module> {
                self.0.clone()
            }
            fn init($lib: &mut $root::Module) {
                $block
            }
        }

        impl Default for $package {
            fn default() -> Self {
                Self::new()
            }
        }

        impl $package {
            pub fn new() -> Self {
                let mut module = $root::Module::new();
                <Self as $root::packages::Package>::init(&mut module);
                module.build_index();
                Self(module.into())
            }
        }
    )* };
    ($root:ident : $package:ident : $comment:expr , $lib:ident , $block:stmt) => {
        #[doc=$comment]
        ///
        /// # Deprecated
        ///
        /// This old syntax of `def_package!` is deprecated. Use the new syntax instead.
        ///
        /// This syntax will be removed in the next major version.
        #[deprecated(since = "1.4.0", note = "this is an old syntax of `def_package!` and is deprecated; use the new syntax of `def_package!` instead")]
        pub struct $package($root::Shared<$root::Module>);

        impl $root::packages::Package for $package {
            fn as_shared_module(&self) -> $root::Shared<$root::Module> {
                #[allow(deprecated)]
                self.0.clone()
            }
            fn init($lib: &mut $root::Module) {
                $block
            }
        }

        impl Default for $package {
            fn default() -> Self {
                Self::new()
            }
        }

        impl $package {
            pub fn new() -> Self {
                let mut module = $root::Module::new();
                <Self as $root::packages::Package>::init(&mut module);
                module.build_index();
                Self(module.into())
            }
        }
    };
}
