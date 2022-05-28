use crate::eval::GlobalRuntimeState;
use crate::func::native::SendSync;
use crate::{Engine, Module, Position, RhaiResultOf, Shared, AST};

mod collection;
mod dummy;
mod file;
mod stat;

pub use collection::ModuleResolversCollection;
pub use dummy::DummyModuleResolver;
#[cfg(not(feature = "no_std"))]
#[cfg(not(target_family = "wasm"))]
pub use file::FileModuleResolver;
pub use stat::StaticModuleResolver;

/// Trait that encapsulates a module resolution service.
pub trait ModuleResolver: SendSync {
    /// Resolve a module based on a path string.
    fn resolve(
        &self,
        engine: &Engine,
        source: Option<&str>,
        path: &str,
        pos: Position,
    ) -> RhaiResultOf<Shared<Module>>;

    /// Resolve a module based on a path string, given a [`GlobalRuntimeState`].
    ///
    /// # WARNING - Low Level API
    ///
    /// This function is very low level.
    fn resolve_raw(
        &self,
        engine: &Engine,
        global: &mut GlobalRuntimeState,
        path: &str,
        pos: Position,
    ) -> RhaiResultOf<Shared<Module>> {
        self.resolve(engine, global.source(), path, pos)
    }

    /// Resolve an `AST` based on a path string.
    ///
    /// Returns [`None`] (default) if such resolution is not supported
    /// (e.g. if the module is Rust-based).
    ///
    /// # WARNING - Low Level API
    ///
    /// Override the default implementation of this method if the module resolver
    /// serves modules based on compiled Rhai scripts.
    #[allow(unused_variables)]
    #[must_use]
    fn resolve_ast(
        &self,
        engine: &Engine,
        source: Option<&str>,
        path: &str,
        pos: Position,
    ) -> Option<RhaiResultOf<AST>> {
        None
    }
}
