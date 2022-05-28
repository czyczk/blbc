use crate::{Engine, Module, ModuleResolver, Position, RhaiResultOf, Shared, ERR};
use core::ops::AddAssign;

use ink_prelude::boxed::Box;
use ink_prelude::vec::IntoIter;
use ink_prelude::vec::Vec;

/// [Module] resolution service that holds a collection of module resolvers,
/// to be searched in sequential order.
///
/// # Example
///
/// ```
/// use rhai::{Engine, Module};
/// use rhai::module_resolvers::{StaticModuleResolver, ModuleResolversCollection};
///
/// let mut collection = ModuleResolversCollection::new();
///
/// let resolver = StaticModuleResolver::new();
/// collection.push(resolver);
///
/// let mut engine = Engine::new();
/// engine.set_module_resolver(collection);
/// ```
pub struct ModuleResolversCollection(Vec<Box<dyn ModuleResolver>>);

impl ModuleResolversCollection {
    /// Create a new [`ModuleResolversCollection`].
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::{Engine, Module};
    /// use rhai::module_resolvers::{StaticModuleResolver, ModuleResolversCollection};
    ///
    /// let mut collection = ModuleResolversCollection::new();
    ///
    /// let resolver = StaticModuleResolver::new();
    /// collection.push(resolver);
    ///
    /// let mut engine = Engine::new();
    /// engine.set_module_resolver(collection);
    /// ```
    #[inline(always)]
    #[must_use]
    pub fn new() -> Self {
        Self(Vec::new())
    }
    /// Append a [module resolver][ModuleResolver] to the end.
    #[inline(always)]
    pub fn push(&mut self, resolver: impl ModuleResolver + 'static) -> &mut Self {
        self.0.push(Box::new(resolver));
        self
    }
    /// Insert a [module resolver][ModuleResolver] to an offset index.
    ///
    /// # Panics
    ///
    /// Panics if the index is out of bounds.
    #[inline(always)]
    pub fn insert(&mut self, index: usize, resolver: impl ModuleResolver + 'static) -> &mut Self {
        self.0.insert(index, Box::new(resolver));
        self
    }
    /// Remove the last [module resolver][ModuleResolver] from the end, if any.
    #[inline(always)]
    pub fn pop(&mut self) -> Option<Box<dyn ModuleResolver>> {
        self.0.pop()
    }
    /// Remove a [module resolver][ModuleResolver] at an offset index.
    ///
    /// # Panics
    ///
    /// Panics if the index is out of bounds.
    #[inline(always)]
    pub fn remove(&mut self, index: usize) -> Box<dyn ModuleResolver> {
        self.0.remove(index)
    }
    /// Get an iterator of all the [module resolvers][ModuleResolver].
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = &dyn ModuleResolver> {
        self.0.iter().map(<_>::as_ref)
    }
    /// Remove all [module resolvers][ModuleResolver].
    #[inline(always)]
    pub fn clear(&mut self) -> &mut Self {
        self.0.clear();
        self
    }
    /// Is this [`ModuleResolversCollection`] empty?
    #[inline(always)]
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    /// Get the number of [module resolvers][ModuleResolver] in this [`ModuleResolversCollection`].
    #[inline(always)]
    #[must_use]
    pub fn len(&self) -> usize {
        self.0.len()
    }
    /// Add another [`ModuleResolversCollection`] to the end of this collection.
    /// The other [`ModuleResolversCollection`] is consumed.
    #[inline]
    pub fn append(&mut self, other: Self) -> &mut Self {
        self.0.extend(other.0.into_iter());
        self
    }
}

impl IntoIterator for ModuleResolversCollection {
    type Item = Box<dyn ModuleResolver>;
    type IntoIter = IntoIter<Box<dyn ModuleResolver>>;

    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl ModuleResolver for ModuleResolversCollection {
    fn resolve(
        &self,
        engine: &Engine,
        source_path: Option<&str>,
        path: &str,
        pos: Position,
    ) -> RhaiResultOf<Shared<Module>> {
        for resolver in &self.0 {
            match resolver.resolve(engine, source_path, path, pos) {
                Ok(module) => return Ok(module),
                Err(err) => match *err {
                    ERR::ErrorModuleNotFound(..) => continue,
                    ERR::ErrorInModule(_, err, _) => return Err(err),
                    _ => panic!("ModuleResolver::resolve returns error that is not ErrorModuleNotFound or ErrorInModule"),
                },
            }
        }

        Err(ERR::ErrorModuleNotFound(path.into(), pos).into())
    }
}

impl<M: ModuleResolver + 'static> AddAssign<M> for ModuleResolversCollection {
    #[inline(always)]
    fn add_assign(&mut self, rhs: M) {
        self.push(rhs);
    }
}
