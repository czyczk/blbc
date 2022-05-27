//! System caches.

use crate::func::CallableFunction;
use crate::{Identifier, StaticVec};

use ink_prelude::boxed::Box;
use ink_prelude::collections::BTreeMap;

/// _(internals)_ An entry in a function resolution cache.
/// Exported under the `internals` feature only.
#[derive(Debug, Clone)]
pub struct FnResolutionCacheEntry {
    /// Function.
    pub func: CallableFunction,
    /// Optional source.
    /// No source if the string is empty.
    pub source: Identifier,
}

/// _(internals)_ A function resolution cache.
/// Exported under the `internals` feature only.
///
/// [`FnResolutionCacheEntry`] is [`Box`]ed in order to pack as many entries inside a single B-Tree
/// level as possible.
pub type FnResolutionCache = BTreeMap<u64, Option<Box<FnResolutionCacheEntry>>>;

/// _(internals)_ A type containing system-wide caches.
/// Exported under the `internals` feature only.
///
/// The following caches are contained inside this type:
/// * A stack of [function resolution caches][FnResolutionCache]
#[derive(Debug, Clone)]
pub struct Caches(StaticVec<FnResolutionCache>);

impl Caches {
    /// Create an empty [`Caches`].
    #[inline(always)]
    #[must_use]
    pub const fn new() -> Self {
        Self(StaticVec::new_const())
    }
    /// Get the number of function resolution cache(s) in the stack.
    #[inline(always)]
    #[must_use]
    pub fn fn_resolution_caches_len(&self) -> usize {
        self.0.len()
    }
    /// Get a mutable reference to the current function resolution cache.
    #[inline]
    #[must_use]
    pub fn fn_resolution_cache_mut(&mut self) -> &mut FnResolutionCache {
        if self.0.is_empty() {
            // Push a new function resolution cache if the stack is empty
            self.push_fn_resolution_cache();
        }
        self.0.last_mut().unwrap()
    }
    /// Push an empty function resolution cache onto the stack and make it current.
    #[allow(dead_code)]
    #[inline(always)]
    pub fn push_fn_resolution_cache(&mut self) {
        self.0.push(BTreeMap::new());
    }
    /// Rewind the function resolution caches stack to a particular size.
    #[inline(always)]
    pub fn rewind_fn_resolution_caches(&mut self, len: usize) {
        self.0.truncate(len);
    }
}
