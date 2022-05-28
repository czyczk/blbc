//! Module that defines the [`Scope`] type representing a function call-stack scope.

use super::dynamic::{AccessMode, Variant};
use crate::{Dynamic, Identifier};
use core::{
    fmt,
    iter::{Extend, FromIterator},
    marker::PhantomData,
};
use smallvec::SmallVec;

use ink_prelude::boxed::Box;
use ink_prelude::string::String;
use ink_prelude::vec::Vec;

/// Keep a number of entries inline (since [`Dynamic`] is usually small enough).
const SCOPE_ENTRIES_INLINED: usize = 8;

/// Type containing information about the current scope. Useful for keeping state between
/// [`Engine`][crate::Engine] evaluation runs.
///
/// # Lifetime
///
/// Currently the lifetime parameter is not used, but it is not guaranteed to remain unused for
/// future versions. Until then, `'static` can be used.
///
/// # Thread Safety
///
/// Currently, [`Scope`] is neither [`Send`] nor [`Sync`]. Turn on the `sync` feature to make it
/// [`Send`] `+` [`Sync`].
///
/// # Example
///
/// ```
/// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
/// use rhai::{Engine, Scope};
///
/// let engine = Engine::new();
/// let mut my_scope = Scope::new();
///
/// my_scope.push("z", 40_i64);
///
/// engine.run_with_scope(&mut my_scope, "let x = z + 1; z = 0;")?;
///
/// let result: i64 = engine.eval_with_scope(&mut my_scope, "x + 1")?;
///
/// assert_eq!(result, 42);
/// assert_eq!(my_scope.get_value::<i64>("x").expect("x should exist"), 41);
/// assert_eq!(my_scope.get_value::<i64>("z").expect("z should exist"), 0);
/// # Ok(())
/// # }
/// ```
///
/// When searching for entries, newly-added entries are found before similarly-named but older
/// entries, allowing for automatic _shadowing_.
//
// # Implementation Notes
//
// [`Scope`] is implemented as three arrays of exactly the same length. That's because variable
// names take up the most space, with [`Identifier`] being three words long, but in the vast
// majority of cases the name is NOT used to look up a variable.  Variable lookup is usually via
// direct indexing, by-passing the name altogether.
//
// [`Dynamic`] is reasonably small so packing it tightly improves cache performance.
#[derive(Debug, Hash, Default)]
pub struct Scope<'a> {
    /// Current value of the entry.
    values: SmallVec<[Dynamic; SCOPE_ENTRIES_INLINED]>,
    /// Name of the entry.
    names: SmallVec<[Identifier; SCOPE_ENTRIES_INLINED]>,
    /// Aliases of the entry.
    aliases: SmallVec<[Vec<Identifier>; SCOPE_ENTRIES_INLINED]>,
    /// Phantom to keep the lifetime parameter in order not to break existing code.
    phantom: PhantomData<&'a ()>,
}

impl fmt::Display for Scope<'_> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, (name, constant, value)) in self.iter_raw().enumerate() {
            #[cfg(not(feature = "no_closure"))]
            let value_is_shared = if value.is_shared() { " (shared)" } else { "" };
            #[cfg(feature = "no_closure")]
            let value_is_shared = "";

            write!(
                f,
                "[{}] {}{}{} = {:?}\n",
                i + 1,
                if constant { "const " } else { "" },
                name,
                value_is_shared,
                *value.read_lock::<Dynamic>().unwrap(),
            )?;
        }

        Ok(())
    }
}

impl Clone for Scope<'_> {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            values: self
                .values
                .iter()
                .map(|v| {
                    // Also copy the value's access mode (otherwise will turn to read-write)
                    let mut v2 = v.clone();
                    v2.set_access_mode(v.access_mode());
                    v2
                })
                .collect(),
            names: self.names.clone(),
            aliases: self.aliases.clone(),
            phantom: self.phantom.clone(),
        }
    }
}

impl IntoIterator for Scope<'_> {
    type Item = (String, Dynamic, Vec<Identifier>);
    type IntoIter = Box<dyn Iterator<Item = Self::Item>>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        Box::new(
            self.values
                .into_iter()
                .zip(self.names.into_iter().zip(self.aliases.into_iter()))
                .map(|(value, (name, alias))| (name.into(), value, alias)),
        )
    }
}

impl Scope<'_> {
    /// Create a new [`Scope`].
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Scope;
    ///
    /// let mut my_scope = Scope::new();
    ///
    /// my_scope.push("x", 42_i64);
    /// assert_eq!(my_scope.get_value::<i64>("x").expect("x should exist"), 42);
    /// ```
    #[inline(always)]
    #[must_use]
    pub const fn new() -> Self {
        Self {
            values: SmallVec::new_const(),
            names: SmallVec::new_const(),
            aliases: SmallVec::new_const(),
            phantom: PhantomData,
        }
    }
    /// Empty the [`Scope`].
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Scope;
    ///
    /// let mut my_scope = Scope::new();
    ///
    /// my_scope.push("x", 42_i64);
    /// assert!(my_scope.contains("x"));
    /// assert_eq!(my_scope.len(), 1);
    /// assert!(!my_scope.is_empty());
    ///
    /// my_scope.clear();
    /// assert!(!my_scope.contains("x"));
    /// assert_eq!(my_scope.len(), 0);
    /// assert!(my_scope.is_empty());
    /// ```
    #[inline(always)]
    pub fn clear(&mut self) -> &mut Self {
        self.names.clear();
        self.values.clear();
        self.aliases.clear();
        self
    }
    /// Get the number of entries inside the [`Scope`].
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Scope;
    ///
    /// let mut my_scope = Scope::new();
    /// assert_eq!(my_scope.len(), 0);
    ///
    /// my_scope.push("x", 42_i64);
    /// assert_eq!(my_scope.len(), 1);
    /// ```
    #[inline(always)]
    #[must_use]
    pub fn len(&self) -> usize {
        self.values.len()
    }
    /// Is the [`Scope`] empty?
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Scope;
    ///
    /// let mut my_scope = Scope::new();
    /// assert!(my_scope.is_empty());
    ///
    /// my_scope.push("x", 42_i64);
    /// assert!(!my_scope.is_empty());
    /// ```
    #[inline(always)]
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }
    /// Add (push) a new entry to the [`Scope`].
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Scope;
    ///
    /// let mut my_scope = Scope::new();
    ///
    /// my_scope.push("x", 42_i64);
    /// assert_eq!(my_scope.get_value::<i64>("x").expect("x should exist"), 42);
    /// ```
    #[inline(always)]
    pub fn push(&mut self, name: impl Into<Identifier>, value: impl Variant + Clone) -> &mut Self {
        self.push_entry(name, AccessMode::ReadWrite, Dynamic::from(value))
    }
    /// Add (push) a new [`Dynamic`] entry to the [`Scope`].
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::{Dynamic,  Scope};
    ///
    /// let mut my_scope = Scope::new();
    ///
    /// my_scope.push_dynamic("x", Dynamic::from(42_i64));
    /// assert_eq!(my_scope.get_value::<i64>("x").expect("x should exist"), 42);
    /// ```
    #[inline(always)]
    pub fn push_dynamic(&mut self, name: impl Into<Identifier>, value: Dynamic) -> &mut Self {
        self.push_entry(name, value.access_mode(), value)
    }
    /// Add (push) a new constant to the [`Scope`].
    ///
    /// Constants are immutable and cannot be assigned to.  Their values never change.
    /// Constants propagation is a technique used to optimize an [`AST`][crate::AST].
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Scope;
    ///
    /// let mut my_scope = Scope::new();
    ///
    /// my_scope.push_constant("x", 42_i64);
    /// assert_eq!(my_scope.get_value::<i64>("x").expect("x should exist"), 42);
    /// ```
    #[inline(always)]
    pub fn push_constant(
        &mut self,
        name: impl Into<Identifier>,
        value: impl Variant + Clone,
    ) -> &mut Self {
        self.push_entry(name, AccessMode::ReadOnly, Dynamic::from(value))
    }
    /// Add (push) a new constant with a [`Dynamic`] value to the Scope.
    ///
    /// Constants are immutable and cannot be assigned to.  Their values never change.
    /// Constants propagation is a technique used to optimize an [`AST`][crate::AST].
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::{Dynamic, Scope};
    ///
    /// let mut my_scope = Scope::new();
    ///
    /// my_scope.push_constant_dynamic("x", Dynamic::from(42_i64));
    /// assert_eq!(my_scope.get_value::<i64>("x").expect("x should exist"), 42);
    /// ```
    #[inline(always)]
    pub fn push_constant_dynamic(
        &mut self,
        name: impl Into<Identifier>,
        value: Dynamic,
    ) -> &mut Self {
        self.push_entry(name, AccessMode::ReadOnly, value)
    }
    /// Add (push) a new entry with a [`Dynamic`] value to the [`Scope`].
    #[inline]
    pub(crate) fn push_entry(
        &mut self,
        name: impl Into<Identifier>,
        access: AccessMode,
        mut value: Dynamic,
    ) -> &mut Self {
        self.names.push(name.into());
        self.aliases.push(Vec::new());
        value.set_access_mode(access);
        self.values.push(value);
        self
    }
    /// Truncate (rewind) the [`Scope`] to a previous size.
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Scope;
    ///
    /// let mut my_scope = Scope::new();
    ///
    /// my_scope.push("x", 42_i64);
    /// my_scope.push("y", 123_i64);
    /// assert!(my_scope.contains("x"));
    /// assert!(my_scope.contains("y"));
    /// assert_eq!(my_scope.len(), 2);
    ///
    /// my_scope.rewind(1);
    /// assert!(my_scope.contains("x"));
    /// assert!(!my_scope.contains("y"));
    /// assert_eq!(my_scope.len(), 1);
    ///
    /// my_scope.rewind(0);
    /// assert!(!my_scope.contains("x"));
    /// assert!(!my_scope.contains("y"));
    /// assert_eq!(my_scope.len(), 0);
    /// assert!(my_scope.is_empty());
    /// ```
    #[inline(always)]
    pub fn rewind(&mut self, size: usize) -> &mut Self {
        self.names.truncate(size);
        self.values.truncate(size);
        self.aliases.truncate(size);
        self
    }
    /// Does the [`Scope`] contain the entry?
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Scope;
    ///
    /// let mut my_scope = Scope::new();
    ///
    /// my_scope.push("x", 42_i64);
    /// assert!(my_scope.contains("x"));
    /// assert!(!my_scope.contains("y"));
    /// ```
    #[inline]
    #[must_use]
    pub fn contains(&self, name: &str) -> bool {
        self.names.iter().any(|key| name == key)
    }
    /// Find an entry in the [`Scope`], starting from the last.
    #[inline]
    #[must_use]
    pub(crate) fn get_index(&self, name: &str) -> Option<(usize, AccessMode)> {
        let len = self.len();

        self.names
            .iter()
            .rev() // Always search a Scope in reverse order
            .enumerate()
            .find_map(|(i, key)| {
                if name == key {
                    let index = len - 1 - i;
                    Some((index, self.values[index].access_mode()))
                } else {
                    None
                }
            })
    }
    /// Get the value of an entry in the [`Scope`], starting from the last.
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Scope;
    ///
    /// let mut my_scope = Scope::new();
    ///
    /// my_scope.push("x", 42_i64);
    /// assert_eq!(my_scope.get_value::<i64>("x").expect("x should exist"), 42);
    /// ```
    #[inline]
    #[must_use]
    pub fn get_value<T: Variant + Clone>(&self, name: &str) -> Option<T> {
        let len = self.len();

        self.names
            .iter()
            .rev()
            .enumerate()
            .find(|(.., key)| &name == key)
            .and_then(|(index, ..)| self.values[len - 1 - index].flatten_clone().try_cast())
    }
    /// Check if the named entry in the [`Scope`] is constant.
    ///
    /// Search starts backwards from the last, stopping at the first entry matching the specified name.
    ///
    /// Returns [`None`] if no entry matching the specified name is found.
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Scope;
    ///
    /// let mut my_scope = Scope::new();
    ///
    /// my_scope.push_constant("x", 42_i64);
    /// assert_eq!(my_scope.is_constant("x"), Some(true));
    /// assert_eq!(my_scope.is_constant("y"), None);
    /// ```
    #[inline]
    pub fn is_constant(&self, name: &str) -> Option<bool> {
        self.get_index(name).and_then(|(.., access)| {
            Some(match access {
                AccessMode::ReadWrite => false,
                AccessMode::ReadOnly => true,
            })
        })
    }
    /// Update the value of the named entry in the [`Scope`] if it already exists and is not constant.
    /// Push a new entry with the value into the [`Scope`] if the name doesn't exist or if the
    /// existing entry is constant.
    ///
    /// Search starts backwards from the last, and only the first entry matching the specified name is updated.
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Scope;
    ///
    /// let mut my_scope = Scope::new();
    ///
    /// my_scope.set_or_push("x", 42_i64);
    /// assert_eq!(my_scope.get_value::<i64>("x").expect("x should exist"), 42);
    /// assert_eq!(my_scope.len(), 1);
    ///
    /// my_scope.set_or_push("x", 0_i64);
    /// assert_eq!(my_scope.get_value::<i64>("x").expect("x should exist"), 0);
    /// assert_eq!(my_scope.len(), 1);
    ///
    /// my_scope.set_or_push("y", 123_i64);
    /// assert_eq!(my_scope.get_value::<i64>("y").expect("y should exist"), 123);
    /// assert_eq!(my_scope.len(), 2);
    /// ```
    #[inline]
    pub fn set_or_push(
        &mut self,
        name: impl AsRef<str> + Into<Identifier>,
        value: impl Variant + Clone,
    ) -> &mut Self {
        match self.get_index(name.as_ref()) {
            None | Some((.., AccessMode::ReadOnly)) => {
                self.push(name, value);
            }
            Some((index, AccessMode::ReadWrite)) => {
                let value_ref = self.values.get_mut(index).unwrap();
                *value_ref = Dynamic::from(value);
            }
        }
        self
    }
    /// Update the value of the named entry in the [`Scope`].
    ///
    /// Search starts backwards from the last, and only the first entry matching the specified name is updated.
    /// If no entry matching the specified name is found, a new one is added.
    ///
    /// # Panics
    ///
    /// Panics when trying to update the value of a constant.
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Scope;
    ///
    /// let mut my_scope = Scope::new();
    ///
    /// my_scope.push("x", 42_i64);
    /// assert_eq!(my_scope.get_value::<i64>("x").expect("x should exist"), 42);
    ///
    /// my_scope.set_value("x", 0_i64);
    /// assert_eq!(my_scope.get_value::<i64>("x").expect("x should exist"), 0);
    /// ```
    #[inline]
    pub fn set_value(
        &mut self,
        name: impl AsRef<str> + Into<Identifier>,
        value: impl Variant + Clone,
    ) -> &mut Self {
        match self.get_index(name.as_ref()) {
            None => {
                self.push(name, value);
            }
            Some((.., AccessMode::ReadOnly)) => panic!("variable {} is constant", name.as_ref()),
            Some((index, AccessMode::ReadWrite)) => {
                let value_ref = self.values.get_mut(index).unwrap();
                *value_ref = Dynamic::from(value);
            }
        }
        self
    }
    /// Get a mutable reference to an entry in the [`Scope`].
    ///
    /// If the entry by the specified name is not found, of if it is read-only,
    /// [`None`] is returned.
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Scope;
    ///
    /// let mut my_scope = Scope::new();
    ///
    /// my_scope.push("x", 42_i64);
    /// assert_eq!(my_scope.get_value::<i64>("x").expect("x should exist"), 42);
    ///
    /// let ptr = my_scope.get_mut("x").expect("x should exist");
    /// *ptr = 123_i64.into();
    ///
    /// assert_eq!(my_scope.get_value::<i64>("x").expect("x should exist"), 123);
    ///
    /// my_scope.push_constant("Z", 1_i64);
    /// assert!(my_scope.get_mut("Z").is_none());
    /// ```
    #[inline]
    #[must_use]
    pub fn get_mut(&mut self, name: &str) -> Option<&mut Dynamic> {
        self.get_index(name)
            .and_then(move |(index, access)| match access {
                AccessMode::ReadWrite => Some(self.get_mut_by_index(index)),
                AccessMode::ReadOnly => None,
            })
    }
    /// Get a mutable reference to an entry in the [`Scope`] based on the index.
    ///
    /// # Panics
    ///
    /// Panics if the index is out of bounds.
    #[inline]
    #[must_use]
    pub(crate) fn get_mut_by_index(&mut self, index: usize) -> &mut Dynamic {
        self.values.get_mut(index).unwrap()
    }
    /// Add an alias to an entry in the [`Scope`].
    ///
    /// # Panics
    ///
    /// Panics if the index is out of bounds.
    #[cfg(not(feature = "no_module"))]
    #[inline]
    pub(crate) fn add_alias_by_index(&mut self, index: usize, alias: Identifier) -> &mut Self {
        let aliases = self.aliases.get_mut(index).unwrap();
        if aliases.is_empty() || !aliases.contains(&alias) {
            aliases.push(alias);
        }
        self
    }
    /// Add an alias to a variable in the [`Scope`] so that it is exported under that name.
    /// This is an advanced API.
    ///
    /// If the alias is empty, then the variable is exported under its original name.
    ///
    /// Multiple aliases can be added to any variable.
    ///
    /// Only the last variable matching the name (and not other shadowed versions) is aliased by
    /// this call.
    #[cfg(not(feature = "no_module"))]
    #[inline]
    pub fn set_alias(
        &mut self,
        name: impl AsRef<str> + Into<Identifier>,
        alias: impl Into<Identifier>,
    ) {
        if let Some((index, ..)) = self.get_index(name.as_ref()) {
            let alias = match alias.into() {
                x if x.is_empty() => name.into(),
                x => x,
            };
            self.add_alias_by_index(index, alias);
        }
    }
    /// Clone the [`Scope`], keeping only the last instances of each variable name.
    /// Shadowed variables are omitted in the copy.
    #[inline]
    #[must_use]
    pub fn clone_visible(&self) -> Self {
        let len = self.len();
        let mut scope = Self::new();

        self.names.iter().rev().enumerate().for_each(|(i, name)| {
            if scope.names.contains(name) {
                return;
            }

            let v1 = &self.values[len - 1 - i];
            let alias = &self.aliases[len - 1 - i];
            let mut v2 = v1.clone();
            v2.set_access_mode(v1.access_mode());

            scope.names.push(name.clone());
            scope.values.push(v2);
            scope.aliases.push(alias.clone());
        });

        scope
    }
    /// Get an iterator to entries in the [`Scope`].
    #[inline]
    #[allow(dead_code)]
    pub(crate) fn into_iter(self) -> impl Iterator<Item = (Identifier, Dynamic, Vec<Identifier>)> {
        self.names
            .into_iter()
            .zip(self.values.into_iter().zip(self.aliases.into_iter()))
            .map(|(name, (value, alias))| (name, value, alias))
    }
    /// Get an iterator to entries in the [`Scope`].
    /// Shared values are flatten-cloned.
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::{Dynamic, Scope};
    ///
    /// let mut my_scope = Scope::new();
    ///
    /// my_scope.push("x", 42_i64);
    /// my_scope.push_constant("foo", "hello");
    ///
    /// let mut iter = my_scope.iter();
    ///
    /// let (name, is_constant, value) = iter.next().expect("value should exist");
    /// assert_eq!(name, "x");
    /// assert!(!is_constant);
    /// assert_eq!(value.cast::<i64>(), 42);
    ///
    /// let (name, is_constant, value) = iter.next().expect("value should exist");
    /// assert_eq!(name, "foo");
    /// assert!(is_constant);
    /// assert_eq!(value.cast::<String>(), "hello");
    /// ```
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = (&str, bool, Dynamic)> {
        self.iter_raw()
            .map(|(name, constant, value)| (name, constant, value.flatten_clone()))
    }
    /// Get an iterator to entries in the [`Scope`].
    /// Shared values are not expanded.
    #[inline]
    pub fn iter_raw(&self) -> impl Iterator<Item = (&str, bool, &Dynamic)> {
        self.names
            .iter()
            .zip(self.values.iter())
            .map(|(name, value)| (name.as_ref(), value.is_read_only(), value))
    }
    /// Get a reverse iterator to entries in the [`Scope`].
    /// Shared values are not expanded.
    #[inline]
    pub(crate) fn iter_rev_raw(&self) -> impl Iterator<Item = (&str, bool, &Dynamic)> {
        self.names
            .iter()
            .rev()
            .zip(self.values.iter().rev())
            .map(|(name, value)| (name.as_ref(), value.is_read_only(), value))
    }
    /// Remove a range of entries within the [`Scope`].
    ///
    /// # Panics
    ///
    /// Panics if the range is out of bounds.
    #[inline]
    #[allow(dead_code)]
    pub(crate) fn remove_range(&mut self, start: usize, len: usize) {
        self.values.drain(start..start + len).for_each(|_| {});
        self.names.drain(start..start + len).for_each(|_| {});
        self.aliases.drain(start..start + len).for_each(|_| {});
    }
}

impl<K: Into<Identifier>> Extend<(K, Dynamic)> for Scope<'_> {
    #[inline]
    fn extend<T: IntoIterator<Item = (K, Dynamic)>>(&mut self, iter: T) {
        for (name, value) in iter {
            self.push_entry(name, AccessMode::ReadWrite, value);
        }
    }
}

impl<K: Into<Identifier>> FromIterator<(K, Dynamic)> for Scope<'_> {
    #[inline]
    fn from_iter<T: IntoIterator<Item = (K, Dynamic)>>(iter: T) -> Self {
        let mut scope = Self::new();
        scope.extend(iter);
        scope
    }
}

impl<K: Into<Identifier>> Extend<(K, bool, Dynamic)> for Scope<'_> {
    #[inline]
    fn extend<T: IntoIterator<Item = (K, bool, Dynamic)>>(&mut self, iter: T) {
        for (name, is_constant, value) in iter {
            self.push_entry(
                name,
                if is_constant {
                    AccessMode::ReadOnly
                } else {
                    AccessMode::ReadWrite
                },
                value,
            );
        }
    }
}

impl<K: Into<Identifier>> FromIterator<(K, bool, Dynamic)> for Scope<'_> {
    #[inline]
    fn from_iter<T: IntoIterator<Item = (K, bool, Dynamic)>>(iter: T) -> Self {
        let mut scope = Self::new();
        scope.extend(iter);
        scope
    }
}
