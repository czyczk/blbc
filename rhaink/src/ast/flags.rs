//! Module defining script options.

use bitflags::bitflags;

/// A type representing the access mode of a function.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[non_exhaustive]
pub enum FnAccess {
    /// Private function.
    Private,
    /// Public function.
    Public,
}

bitflags! {
    /// _(internals)_ A type that holds a configuration option with bit-flags.
    /// Exported under the `internals` feature only.
    pub struct ASTFlags: u8 {
        /// No options for the [`AST`][crate::AST] node.
        const NONE = 0b_0000_0000;
        /// The [`AST`][crate::AST] node is read-only.
        const CONSTANT = 0b_0000_0001;
        /// The [`AST`][crate::AST] node is exposed to the outside (i.e. public).
        const EXPORTED = 0b_0000_0010;
        /// The [`AST`][crate::AST] node is negated (i.e. whatever information is the opposite).
        const NEGATED = 0b_0000_0100;
        /// The [`AST`][crate::AST] node breaks out of normal control flow.
        const BREAK = 0b_0000_1000;
    }
}
