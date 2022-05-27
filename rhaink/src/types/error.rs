//! Module containing error definitions for the evaluation process.

extern crate alloc;

use core::fmt;

use ink_prelude::boxed::Box;
use ink_prelude::string::String;

use crate::tokenizer::Position;
use crate::INT;

use super::dynamic::Dynamic;
use super::immutable_string::ImmutableString;
use super::parse_error::ParseErrorType;

/// Evaluation result.
///
/// All wrapped [`Position`] values represent the location in the script where the error occurs.
///
/// Some errors never appear when certain features are turned on.
/// They still exist so that the application can turn features on and off without going through
/// massive code changes to remove/add back enum variants in match statements.
///
/// # Thread Safety
///
/// Currently, [`EvalAltResult`] is neither [`Send`] nor [`Sync`].
/// Turn on the `sync` feature to make it [`Send`] `+` [`Sync`].
#[derive(Debug)]
#[non_exhaustive]
pub enum EvalAltResult {
    /// System error. Wrapped values are the error message and the internal error.
    #[cfg(not(feature = "sync"))]
    ErrorSystem(String, String),
    /// System error. Wrapped values are the error message and the internal error.
    #[cfg(feature = "sync")]
    ErrorSystem(String, Box<dyn Error + Send + Sync>),

    /// Syntax error.
    ErrorParsing(ParseErrorType, Position),

    /// Shadowing of an existing variable disallowed. Wrapped value is the variable name.
    ErrorVariableExists(String, Position),
    /// Forbidden variable name. Wrapped value is the variable name.
    ErrorForbiddenVariable(String, Position),
    /// Access of an unknown variable. Wrapped value is the variable name.
    ErrorVariableNotFound(String, Position),
    /// Access of an unknown object map property. Wrapped value is the property name.
    ErrorPropertyNotFound(String, Position),
    /// Call to an unknown function. Wrapped value is the function signature.
    ErrorFunctionNotFound(String, Position),
    /// Usage of an unknown [module][crate::Module]. Wrapped value is the [module][crate::Module] name.
    ErrorModuleNotFound(String, Position),

    /// An error has occurred inside a called function.
    /// Wrapped values are the function name, function source, and the interior error.
    ErrorInFunctionCall(String, String, Box<Self>, Position),
    /// An error has occurred while loading a [module][crate::Module].
    /// Wrapped value are the [module][crate::Module] name and the interior error.
    ErrorInModule(String, Box<Self>, Position),

    /// Access to `this` that is not bound.
    ErrorUnboundThis(Position),

    /// Data is not of the required type.
    /// Wrapped values are the type requested and type of the actual result.
    ErrorMismatchDataType(String, String, Position),
    /// Returned type is not the same as the required output type.
    /// Wrapped values are the type requested and type of the actual result.
    ErrorMismatchOutputType(String, String, Position),
    /// Trying to index into a type that has no indexer function defined. Wrapped value is the type name.
    ErrorIndexingType(String, Position),

    /// Array access out-of-bounds.
    /// Wrapped values are the current number of elements in the array and the index number.
    ErrorArrayBounds(usize, INT, Position),
    /// String indexing out-of-bounds.
    /// Wrapped values are the current number of characters in the string and the index number.
    ErrorStringBounds(usize, INT, Position),
    /// Bit-field indexing out-of-bounds.
    /// Wrapped values are the current number of bits in the bit-field and the index number.
    ErrorBitFieldBounds(usize, INT, Position),

    /// The `for` statement encounters a type that is not iterable.
    ErrorFor(Position),

    /// Data race detected when accessing a variable. Wrapped value is the variable name.
    ErrorDataRace(String, Position),
    /// Assignment to a constant variable. Wrapped value is the variable name.
    ErrorAssignmentToConstant(String, Position),
    /// Inappropriate property access. Wrapped value is the property name.
    ErrorDotExpr(String, Position),
    /// Arithmetic error encountered. Wrapped value is the error message.
    ErrorArithmetic(String, Position),

    /// Number of operations over maximum limit.
    ErrorTooManyOperations(Position),
    /// [Modules][crate::Module] over maximum limit.
    ErrorTooManyModules(Position),
    /// Call stack over maximum limit.
    ErrorStackOverflow(Position),
    /// Data value over maximum size limit. Wrapped value is the type name.
    ErrorDataTooLarge(String, Position),
    /// The script is prematurely terminated. Wrapped value is the termination token.
    ErrorTerminated(Dynamic, Position),

    /// Error encountered for a custom syntax. Wrapped values are the error message and
    /// custom syntax symbols stream.
    ///
    /// Normally this should never happen, unless an [`AST`][crate::AST] is compiled on one
    /// [`Engine`][crate::Engine] but evaluated on another unrelated [`Engine`][crate::Engine].
    ErrorCustomSyntax(String, Vec<String>, Position),

    /// Run-time error encountered. Wrapped value is the error token.
    ErrorRuntime(Dynamic, Position),

    /// Breaking out of loops - not an error if within a loop.
    /// The wrapped value, if true, means breaking clean out of the loop (i.e. a `break` statement).
    /// The wrapped value, if false, means breaking the current context (i.e. a `continue` statement).
    LoopBreak(bool, Position),
    /// Not an error: Value returned from a script via the `return` keyword.
    /// Wrapped value is the result value.
    Return(Dynamic, Position),
}

impl fmt::Display for EvalAltResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ErrorSystem(s, err) => match s.as_str() {
                "" => write!(f, "{}", err),
                s => write!(f, "{}: {}", s, err),
            }?,

            Self::ErrorParsing(p, ..) => write!(f, "Syntax error: {}", p)?,

            #[cfg(not(feature = "no_function"))]
            Self::ErrorInFunctionCall(s, src, err, ..) if crate::parser::is_anonymous_fn(s) => {
                write!(f, "{} in call to closure", err)?;
                if !src.is_empty() {
                    write!(f, " @ '{}'", src)?;
                }
            }
            Self::ErrorInFunctionCall(s, src, err, ..) => {
                write!(f, "{} in call to function {}", err, s)?;
                if !src.is_empty() {
                    write!(f, " @ '{}'", src)?;
                }
            }

            Self::ErrorInModule(s, err, ..) if s.is_empty() => {
                write!(f, "Error in module > {}", err)?
            }
            Self::ErrorInModule(s, err, ..) => write!(f, "Error in module '{}' > {}", s, err)?,

            Self::ErrorVariableExists(s, ..) => write!(f, "Variable is already defined: {}", s)?,
            Self::ErrorForbiddenVariable(s, ..) => write!(f, "Forbidden variable name: {}", s)?,
            Self::ErrorVariableNotFound(s, ..) => write!(f, "Variable not found: {}", s)?,
            Self::ErrorPropertyNotFound(s, ..) => write!(f, "Property not found: {}", s)?,
            Self::ErrorFunctionNotFound(s, ..) => write!(f, "Function not found: {}", s)?,
            Self::ErrorModuleNotFound(s, ..) => write!(f, "Module not found: {}", s)?,
            Self::ErrorDataRace(s, ..) => {
                write!(f, "Data race detected when accessing variable: {}", s)?
            }
            Self::ErrorDotExpr(s, ..) => match s.as_str() {
                "" => f.write_str("Malformed dot expression"),
                s => f.write_str(s),
            }?,
            Self::ErrorIndexingType(s, ..) => write!(f, "Indexer not registered: {}", s)?,
            Self::ErrorUnboundThis(..) => f.write_str("'this' is not bound")?,
            Self::ErrorFor(..) => f.write_str("For loop expects a type that is iterable")?,
            Self::ErrorTooManyOperations(..) => f.write_str("Too many operations")?,
            Self::ErrorTooManyModules(..) => f.write_str("Too many modules imported")?,
            Self::ErrorStackOverflow(..) => f.write_str("Stack overflow")?,
            Self::ErrorTerminated(..) => f.write_str("Script terminated")?,

            Self::ErrorRuntime(d, ..) if d.is::<()>() => f.write_str("Runtime error")?,
            Self::ErrorRuntime(d, ..)
                if d.read_lock::<ImmutableString>()
                    .map_or(false, |v| v.is_empty()) =>
            {
                write!(f, "Runtime error")?
            }
            Self::ErrorRuntime(d, ..) => write!(f, "Runtime error: {}", d)?,

            Self::ErrorAssignmentToConstant(s, ..) => write!(f, "Cannot modify constant: {}", s)?,
            Self::ErrorMismatchOutputType(e, a, ..) => match (a.as_str(), e.as_str()) {
                ("", e) => write!(f, "Output type is incorrect, expecting {}", e),
                (a, "") => write!(f, "Output type is incorrect: {}", a),
                (a, e) => write!(f, "Output type is incorrect: {} (expecting {})", a, e),
            }?,
            Self::ErrorMismatchDataType(e, a, ..) => match (a.as_str(), e.as_str()) {
                ("", e) => write!(f, "Data type is incorrect, expecting {}", e),
                (a, "") => write!(f, "Data type is incorrect: {}", a),
                (a, e) => write!(f, "Data type is incorrect: {} (expecting {})", a, e),
            }?,
            Self::ErrorArithmetic(s, ..) => match s.as_str() {
                "" => f.write_str("Arithmetic error"),
                s => f.write_str(s),
            }?,

            Self::LoopBreak(true, ..) => f.write_str("'break' not inside a loop")?,
            Self::LoopBreak(false, ..) => f.write_str("'continue' not inside a loop")?,

            Self::Return(..) => f.write_str("NOT AN ERROR - function returns value")?,

            Self::ErrorArrayBounds(max, index, ..) => match max {
                0 => write!(f, "Array index {} out of bounds: array is empty", index),
                1 => write!(
                    f,
                    "Array index {} out of bounds: only 1 element in the array",
                    index
                ),
                _ => write!(
                    f,
                    "Array index {} out of bounds: only {} elements in the array",
                    index, max
                ),
            }?,
            Self::ErrorStringBounds(max, index, ..) => match max {
                0 => write!(f, "String index {} out of bounds: string is empty", index),
                1 => write!(
                    f,
                    "String index {} out of bounds: only 1 character in the string",
                    index
                ),
                _ => write!(
                    f,
                    "String index {} out of bounds: only {} characters in the string",
                    index, max
                ),
            }?,
            Self::ErrorBitFieldBounds(max, index, ..) => write!(
                f,
                "Bit-field index {} out of bounds: only {} bits in the bit-field",
                index, max
            )?,
            Self::ErrorDataTooLarge(typ, ..) => write!(f, "{} exceeds maximum limit", typ)?,

            Self::ErrorCustomSyntax(s, tokens, ..) => write!(f, "{}: {}", s, tokens.join(" "))?,
        }

        // Do not write any position if None
        if !self.position().is_none() {
            write!(f, " ({})", self.position())?;
        }

        Ok(())
    }
}

impl<T: AsRef<str>> From<T> for EvalAltResult {
    #[inline(never)]
    fn from(err: T) -> Self {
        Self::ErrorRuntime(err.as_ref().to_string().into(), Position::NONE)
    }
}

impl<T: AsRef<str>> From<T> for Box<EvalAltResult> {
    #[inline(never)]
    fn from(err: T) -> Self {
        EvalAltResult::ErrorRuntime(err.as_ref().to_string().into(), Position::NONE).into()
    }
}

impl EvalAltResult {
    /// Is this a pseudo error?  A pseudo error is one that does not occur naturally.
    ///
    /// [`LoopBreak`][EvalAltResult::LoopBreak] and [`Return`][EvalAltResult::Return] are pseudo errors.
    #[must_use]
    pub const fn is_pseudo_error(&self) -> bool {
        match self {
            Self::LoopBreak(..) | Self::Return(..) => true,
            _ => false,
        }
    }
    /// Can this error be caught?
    #[must_use]
    pub const fn is_catchable(&self) -> bool {
        match self {
            Self::ErrorSystem(..) => false,
            Self::ErrorParsing(..) => false,

            Self::ErrorFunctionNotFound(..)
            | Self::ErrorInFunctionCall(..)
            | Self::ErrorInModule(..)
            | Self::ErrorUnboundThis(..)
            | Self::ErrorMismatchDataType(..)
            | Self::ErrorArrayBounds(..)
            | Self::ErrorStringBounds(..)
            | Self::ErrorBitFieldBounds(..)
            | Self::ErrorIndexingType(..)
            | Self::ErrorFor(..)
            | Self::ErrorVariableExists(..)
            | Self::ErrorForbiddenVariable(..)
            | Self::ErrorVariableNotFound(..)
            | Self::ErrorPropertyNotFound(..)
            | Self::ErrorModuleNotFound(..)
            | Self::ErrorDataRace(..)
            | Self::ErrorAssignmentToConstant(..)
            | Self::ErrorMismatchOutputType(..)
            | Self::ErrorDotExpr(..)
            | Self::ErrorArithmetic(..)
            | Self::ErrorRuntime(..) => true,

            // Custom syntax raises errors only when they are compiled by one
            // [`Engine`][crate::Engine] and run by another, causing a mismatch.
            //
            // Therefore, this error should not be catchable.
            Self::ErrorCustomSyntax(..) => false,

            Self::ErrorTooManyOperations(..)
            | Self::ErrorTooManyModules(..)
            | Self::ErrorStackOverflow(..)
            | Self::ErrorDataTooLarge(..)
            | Self::ErrorTerminated(..) => false,

            Self::LoopBreak(..) | Self::Return(..) => false,
        }
    }
    /// Is this error a system exception?
    #[must_use]
    pub const fn is_system_exception(&self) -> bool {
        match self {
            Self::ErrorSystem(..) => true,
            Self::ErrorParsing(..) => true,

            Self::ErrorCustomSyntax(..)
            | Self::ErrorTooManyOperations(..)
            | Self::ErrorTooManyModules(..)
            | Self::ErrorStackOverflow(..)
            | Self::ErrorDataTooLarge(..) => true,

            Self::ErrorTerminated(..) => true,

            _ => false,
        }
    }
    /// Get the [position][Position] of this error.
    #[cfg(not(feature = "no_object"))]
    pub(crate) fn dump_fields(&self, map: &mut crate::Map) {
        map.insert(
            "error".into(),
            format!("{:?}", self)
                .split('(')
                .next()
                .expect("`ErrorXXX(...)`")
                .into(),
        );

        match self {
            Self::LoopBreak(..) | Self::Return(..) => (),

            Self::ErrorSystem(..)
            | Self::ErrorParsing(..)
            | Self::ErrorUnboundThis(..)
            | Self::ErrorFor(..)
            | Self::ErrorArithmetic(..)
            | Self::ErrorTooManyOperations(..)
            | Self::ErrorTooManyModules(..)
            | Self::ErrorStackOverflow(..)
            | Self::ErrorRuntime(..) => (),

            Self::ErrorFunctionNotFound(f, ..) => {
                map.insert("function".into(), f.into());
            }
            Self::ErrorInFunctionCall(f, s, ..) => {
                map.insert("function".into(), f.into());
                map.insert("source".into(), s.into());
            }
            Self::ErrorInModule(m, ..) => {
                map.insert("module".into(), m.into());
            }
            Self::ErrorMismatchDataType(r, a, ..) | Self::ErrorMismatchOutputType(r, a, ..) => {
                map.insert("requested".into(), r.into());
                map.insert("actual".into(), a.into());
            }
            Self::ErrorArrayBounds(n, i, ..)
            | Self::ErrorStringBounds(n, i, ..)
            | Self::ErrorBitFieldBounds(n, i, ..) => {
                map.insert("length".into(), (*n as INT).into());
                map.insert("index".into(), (*i as INT).into());
            }
            Self::ErrorIndexingType(t, ..) => {
                map.insert("type".into(), t.into());
            }
            Self::ErrorVariableExists(v, ..)
            | Self::ErrorForbiddenVariable(v, ..)
            | Self::ErrorVariableNotFound(v, ..)
            | Self::ErrorPropertyNotFound(v, ..)
            | Self::ErrorDataRace(v, ..)
            | Self::ErrorAssignmentToConstant(v, ..) => {
                map.insert("variable".into(), v.into());
            }
            Self::ErrorModuleNotFound(m, ..) => {
                map.insert("module".into(), m.into());
            }
            Self::ErrorDotExpr(p, ..) => {
                map.insert("property".into(), p.into());
            }

            Self::ErrorDataTooLarge(t, ..) => {
                map.insert("type".into(), t.into());
            }
            Self::ErrorTerminated(t, ..) => {
                map.insert("token".into(), t.clone());
            }
            Self::ErrorCustomSyntax(_, tokens, _) => {
                map.insert(
                    "tokens".into(),
                    #[cfg(not(feature = "no_index"))]
                    Dynamic::from_array(tokens.iter().map(Into::into).collect()),
                    #[cfg(feature = "no_index")]
                    tokens
                        .iter()
                        .map(|s| s.as_str())
                        .collect::<Vec<_>>()
                        .join(" ")
                        .into(),
                );
            }
        };
    }
    /// Unwrap this error and get the very base error.
    #[must_use]
    pub fn unwrap_inner(&self) -> &Self {
        match self {
            Self::ErrorInFunctionCall(.., err, _) | Self::ErrorInModule(.., err, _) => {
                err.unwrap_inner()
            }
            _ => self,
        }
    }
    /// Get the [position][Position] of this error.
    #[must_use]
    pub const fn position(&self) -> Position {
        match self {
            Self::ErrorSystem(..) => Position::NONE,

            Self::ErrorParsing(.., pos)
            | Self::ErrorFunctionNotFound(.., pos)
            | Self::ErrorInFunctionCall(.., pos)
            | Self::ErrorInModule(.., pos)
            | Self::ErrorUnboundThis(pos)
            | Self::ErrorMismatchDataType(.., pos)
            | Self::ErrorArrayBounds(.., pos)
            | Self::ErrorStringBounds(.., pos)
            | Self::ErrorBitFieldBounds(.., pos)
            | Self::ErrorIndexingType(.., pos)
            | Self::ErrorFor(pos)
            | Self::ErrorVariableExists(.., pos)
            | Self::ErrorForbiddenVariable(.., pos)
            | Self::ErrorVariableNotFound(.., pos)
            | Self::ErrorPropertyNotFound(.., pos)
            | Self::ErrorModuleNotFound(.., pos)
            | Self::ErrorDataRace(.., pos)
            | Self::ErrorAssignmentToConstant(.., pos)
            | Self::ErrorMismatchOutputType(.., pos)
            | Self::ErrorDotExpr(.., pos)
            | Self::ErrorArithmetic(.., pos)
            | Self::ErrorTooManyOperations(pos)
            | Self::ErrorTooManyModules(pos)
            | Self::ErrorStackOverflow(pos)
            | Self::ErrorDataTooLarge(.., pos)
            | Self::ErrorTerminated(.., pos)
            | Self::ErrorCustomSyntax(.., pos)
            | Self::ErrorRuntime(.., pos)
            | Self::LoopBreak(.., pos)
            | Self::Return(.., pos) => *pos,
        }
    }
    /// Remove the [position][Position] information from this error.
    ///
    /// The [position][Position] of this error is set to [`NONE`][Position::NONE] afterwards.
    pub fn clear_position(&mut self) -> &mut Self {
        self.set_position(Position::NONE)
    }
    /// Remove the [position][Position] information from this error and return it.
    ///
    /// The [position][Position] of this error is set to [`NONE`][Position::NONE] afterwards.
    pub fn take_position(&mut self) -> Position {
        let pos = self.position();
        self.set_position(Position::NONE);
        pos
    }
    /// Override the [position][Position] of this error.
    pub fn set_position(&mut self, new_position: Position) -> &mut Self {
        match self {
            Self::ErrorSystem(..) => (),

            Self::ErrorParsing(.., pos)
            | Self::ErrorFunctionNotFound(.., pos)
            | Self::ErrorInFunctionCall(.., pos)
            | Self::ErrorInModule(.., pos)
            | Self::ErrorUnboundThis(pos)
            | Self::ErrorMismatchDataType(.., pos)
            | Self::ErrorArrayBounds(.., pos)
            | Self::ErrorStringBounds(.., pos)
            | Self::ErrorBitFieldBounds(.., pos)
            | Self::ErrorIndexingType(.., pos)
            | Self::ErrorFor(pos)
            | Self::ErrorVariableExists(.., pos)
            | Self::ErrorForbiddenVariable(.., pos)
            | Self::ErrorVariableNotFound(.., pos)
            | Self::ErrorPropertyNotFound(.., pos)
            | Self::ErrorModuleNotFound(.., pos)
            | Self::ErrorDataRace(.., pos)
            | Self::ErrorAssignmentToConstant(.., pos)
            | Self::ErrorMismatchOutputType(.., pos)
            | Self::ErrorDotExpr(.., pos)
            | Self::ErrorArithmetic(.., pos)
            | Self::ErrorTooManyOperations(pos)
            | Self::ErrorTooManyModules(pos)
            | Self::ErrorStackOverflow(pos)
            | Self::ErrorDataTooLarge(.., pos)
            | Self::ErrorTerminated(.., pos)
            | Self::ErrorCustomSyntax(.., pos)
            | Self::ErrorRuntime(.., pos)
            | Self::LoopBreak(.., pos)
            | Self::Return(.., pos) => *pos = new_position,
        }
        self
    }
    /// Consume the current [`EvalAltResult`] and return a new one with the specified [`Position`]
    /// if the current position is [`Position::None`].
    #[inline(never)]
    #[must_use]
    pub(crate) fn fill_position(mut self: Box<Self>, new_position: Position) -> Box<Self> {
        if self.position().is_none() {
            self.set_position(new_position);
        }
        self
    }
}
