//! Module defining script identifiers.

use crate::{Identifier, Position};
use core::{
    fmt,
    hash::Hash,
    ops::{Deref, DerefMut},
};

/// _(internals)_ An identifier containing a name and a [position][Position].
/// Exported under the `internals` feature only.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Ident {
    /// Identifier name.
    pub name: Identifier,
    /// Position.
    pub pos: Position,
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.name)?;
        self.pos.debug_print(f)
    }
}

impl AsRef<str> for Ident {
    #[inline(always)]
    fn as_ref(&self) -> &str {
        self.name.as_ref()
    }
}

impl Deref for Ident {
    type Target = Identifier;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.name
    }
}

impl DerefMut for Ident {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.name
    }
}

impl Ident {
    /// An empty [`Ident`].
    pub const EMPTY: Self = Self {
        name: Identifier::new_const(),
        pos: Position::NONE,
    };

    /// Get the name of the identifier as a string slice.
    #[inline(always)]
    pub fn as_str(&self) -> &str {
        self.name.as_str()
    }
}
