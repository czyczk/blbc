use crate::{Identifier, ImmutableString};
use core::ops::AddAssign;

use ink_prelude::collections::BTreeMap;

/// _(internals)_ A factory of identifiers from text strings.
/// Exported under the `internals` feature only.
///
/// Normal identifiers, property getters and setters are interned separately.
#[derive(Debug, Clone, Default, Hash)]
pub struct StringsInterner {
    /// Normal strings.
    strings: BTreeMap<Identifier, ImmutableString>,
    /// Property getters.
    #[cfg(not(feature = "no_object"))]
    getters: BTreeMap<Identifier, ImmutableString>,
    /// Property setters.
    #[cfg(not(feature = "no_object"))]
    setters: BTreeMap<Identifier, ImmutableString>,
}

impl StringsInterner {
    /// Create a new [`StringsInterner`].
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Self {
            strings: BTreeMap::new(),
            #[cfg(not(feature = "no_object"))]
            getters: BTreeMap::new(),
            #[cfg(not(feature = "no_object"))]
            setters: BTreeMap::new(),
        }
    }
    /// Get an identifier from a text string and prefix, adding it to the interner if necessary.
    ///
    /// # Prefix
    ///
    /// Currently recognized prefixes are:
    ///
    /// * `""` - None (normal string)
    /// * `"get$"` - Property getter, not available under `no_object`
    /// * `"set$"` - Property setter, not available under `no_object`
    ///
    /// # Panics
    ///
    /// Panics if the prefix is not recognized.
    #[inline]
    #[must_use]
    pub fn get(&mut self, prefix: impl AsRef<str>, text: impl AsRef<str>) -> ImmutableString {
        let (dict, mapper): (_, fn(&str) -> Identifier) = match prefix.as_ref() {
            "" => (&mut self.strings, |s| s.into()),

            #[cfg(not(feature = "no_object"))]
            crate::engine::FN_GET => (&mut self.getters, crate::engine::make_getter),
            #[cfg(not(feature = "no_object"))]
            crate::engine::FN_SET => (&mut self.setters, crate::engine::make_setter),

            _ => unreachable!("unsupported prefix {}", prefix.as_ref()),
        };

        if !dict.is_empty() && dict.contains_key(text.as_ref()) {
            dict.get(text.as_ref()).unwrap().clone()
        } else {
            let value: ImmutableString = mapper(text.as_ref()).into();
            dict.insert(text.as_ref().into(), value.clone());
            value
        }
    }
}

impl AddAssign<Self> for StringsInterner {
    #[inline(always)]
    fn add_assign(&mut self, rhs: Self) {
        self.strings.extend(rhs.strings.into_iter());
        #[cfg(not(feature = "no_object"))]
        self.getters.extend(rhs.getters.into_iter());
        #[cfg(not(feature = "no_object"))]
        self.setters.extend(rhs.setters.into_iter());
    }
}

impl AddAssign<&Self> for StringsInterner {
    #[inline(always)]
    fn add_assign(&mut self, rhs: &Self) {
        self.strings
            .extend(rhs.strings.iter().map(|(k, v)| (k.clone(), v.clone())));
        #[cfg(not(feature = "no_object"))]
        self.getters
            .extend(rhs.getters.iter().map(|(k, v)| (k.clone(), v.clone())));
        #[cfg(not(feature = "no_object"))]
        self.setters
            .extend(rhs.setters.iter().map(|(k, v)| (k.clone(), v.clone())));
    }
}
