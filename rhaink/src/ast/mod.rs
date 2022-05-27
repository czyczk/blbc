//! Module defining the AST (abstract syntax tree).

pub mod ast;
pub mod expr;
pub mod flags;
pub mod ident;
pub mod namespace;
pub mod script_fn;
pub mod stmt;

pub use ast::{ASTNode, AST};
pub use expr::{BinaryExpr, CustomExpr, Expr, FnCallExpr, FnCallHashes};
pub use flags::{ASTFlags, FnAccess};
pub use ident::Ident;
#[cfg(not(feature = "no_module"))]
pub use namespace::Namespace;
#[cfg(not(feature = "no_module"))]
#[cfg(not(feature = "no_function"))]
pub use script_fn::EncapsulatedEnviron;
#[cfg(not(feature = "no_function"))]
pub use script_fn::{ScriptFnDef, ScriptFnMetadata};
pub use stmt::{
    ConditionalStmtBlock, OpAssignment, Stmt, StmtBlock, StmtBlockContainer, SwitchCases,
    TryCatchBlock,
};

#[cfg(not(feature = "no_float"))]
pub use expr::FloatWrapper;

#[cfg(feature = "no_function")]
pub struct ScriptFnDef;
