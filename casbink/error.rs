extern crate alloc;

use core::fmt::{Debug, Display, Formatter};

use derive_more::From;
use ink_prelude::string::String;

/// ModelError represents any type of errors in model configuration
#[derive(Debug, PartialEq, Eq)]
pub enum ModelError {
    // #[error("Invalid request definition: `{0}`")]
    R(String),
    // #[error("Invalid policy definition: `{0}`")]
    P(String),
    // #[error("Unsupported effect: `{0}`")]
    E(String),
    // #[error("Invalid matcher: `{0}`")]
    M(String),
    // #[error("Other: `{0}`")]
    Other(String),
}

impl Display for ModelError {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match *self {
            Self::R(_) => write!(f, "Invalid request definition: `{0}`", self),
            Self::P(_) => write!(f, "Invalid policy definition: `{0}`", self),
            Self::E(_) => write!(f, "Unsupported effect: `{0}`", self),
            Self::M(_) => write!(f, "Invalid matcher: `{0}`", self),
            Self::Other(_) => write!(f, "Other: `{0}`", self),
        }
    }
}

/// RequestError represents any type of errors in coming request
#[derive(Debug, PartialEq, Eq)]
pub enum RequestError {
    // #[error("Request doesn't match request definition. expected length: {0}, found length {1}")]
    UnmatchRequestDefinition(usize, usize),
}

impl Display for RequestError {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "Request doesn't match request definition. expected length: {0}, found length {1}",
            self, self
        )
    }
}

/// PolicyError represents any type of errors in policy
#[derive(Debug, PartialEq, Eq)]
pub enum PolicyError {
    // #[error("Policy doesn't match policy definition. expected length: {0}, found length {1}")]
    UnmatchPolicyDefinition(usize, usize),
}

impl Display for PolicyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "Policy doesn't match policy definition. expected length: {0}, found length {1}",
            self, self
        )
    }
}

// Counterfeit a `std::io::IoError` to ensure `Error` can pass compilation.
#[derive(Debug, PartialEq, Eq)]
pub enum StdIoError {
    IoError(String),
}

impl From<String> for StdIoError {
    fn from(str: String) -> Self {
        Self::IoError(str)
    }
}

impl Display for StdIoError {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "{0}", self)
    }
}

// Counterfeit a `rhai::EvalAltResultError` to ensure `Error` can pass compilation.
#[derive(Debug, PartialEq, Eq)]
pub enum RhaiEvalAltResultError {
    EvalAltResult(String),
}

impl From<String> for RhaiEvalAltResultError {
    fn from(str: String) -> Self {
        Self::EvalAltResult(str)
    }
}

impl Display for RhaiEvalAltResultError {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "Casbin Evaluation Error: `{0}`", self)
    }
}

// Counterfeit a `rhai::ParseError` to ensure `Error` can pass compilation.
#[derive(Debug, PartialEq, Eq)]
pub enum RhaiParseError {
    ParseError(String),
}

impl From<String> for RhaiParseError {
    fn from(str: String) -> Self {
        Self::ParseError(str)
    }
}

impl Display for RhaiParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "Casbin Parse Error: `{0}`", self)
    }
}

/// General casbin error
#[derive(Debug, From, PartialEq, Eq)]
pub enum Error {
    // #[error("Casbin Io Error: `{0:?}`")]
    IoError(StdIoError),

    // #[error("Casbin Model Error: `{0:?}`")]
    ModelError(ModelError),
    // #[error("Casbin Policy Error: `{0:?}`")]
    PolicyError(PolicyError),

    // Originally from `EvalAltResult`
    // #[error("Casbin Evaluation Error: `{0:?}`")]
    RhaiError(RhaiEvalAltResultError),
    // #[error("Casbin Parse Error: `{0:?}`")]
    RhaiParseError(RhaiParseError),

    // #[error("Casbin Request Error: `{0:?}`")]
    RequestError(RequestError),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match *self {
            Self::IoError(_) => write!(f, "Casbin Io Error: `{0}`", self),
            Self::ModelError(_) => write!(f, "Casbin Model Error: `{0}`", self),
            Self::PolicyError(_) => write!(f, "Casbin Policy Error: `{0}`", self),
            Self::RhaiError(_) => write!(f, "Casbin Evaluation Error: `{0}`", self),
            Self::RhaiParseError(_) => write!(f, "Casbin Parse Error: `{0}`", self),
            Self::RequestError(_) => write!(f, "Casbin Request Error: `{0}`", self),
        }
    }
}
