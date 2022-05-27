//! Main module defining the script evaluation [`Engine`].

use crate::api::custom_syntax::CustomSyntax;
use crate::api::options::LanguageOptions;
use crate::func::native::{
    OnDebugCallback, OnDefVarCallback, OnParseTokenCallback, OnPrintCallback, OnVarCallback,
};
use crate::packages::{Package, StandardPackage};
use crate::tokenizer::Token;
use crate::types::dynamic::Union;
use crate::{
    Dynamic, Identifier, ImmutableString, Module, Position, RhaiResult, Shared, StaticVec,
};
use core::{fmt, num::NonZeroU8};

use ink_prelude::boxed::Box;
use ink_prelude::collections::{BTreeMap, BTreeSet};

pub type Precedence = NonZeroU8;

pub const KEYWORD_PRINT: &str = "print";
pub const KEYWORD_DEBUG: &str = "debug";
pub const KEYWORD_TYPE_OF: &str = "type_of";
pub const KEYWORD_EVAL: &str = "eval";
pub const KEYWORD_FN_PTR: &str = "Fn";
pub const KEYWORD_FN_PTR_CALL: &str = "call";
pub const KEYWORD_FN_PTR_CURRY: &str = "curry";
#[cfg(not(feature = "no_closure"))]
pub const KEYWORD_IS_SHARED: &str = "is_shared";
pub const KEYWORD_IS_DEF_VAR: &str = "is_def_var";
#[cfg(not(feature = "no_function"))]
pub const KEYWORD_IS_DEF_FN: &str = "is_def_fn";
pub const KEYWORD_THIS: &str = "this";
#[cfg(not(feature = "no_function"))]
#[cfg(not(feature = "no_module"))]
pub const KEYWORD_GLOBAL: &str = "global";
#[cfg(not(feature = "no_object"))]
pub const FN_GET: &str = "get$";
#[cfg(not(feature = "no_object"))]
pub const FN_SET: &str = "set$";
#[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
pub const FN_IDX_GET: &str = "index$get$";
#[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
pub const FN_IDX_SET: &str = "index$set$";
#[cfg(not(feature = "no_function"))]
pub const FN_ANONYMOUS: &str = "anon$";

/// Standard equality comparison operator.
///
/// Some standard functions (e.g. searching an [`Array`][crate::Array]) implicitly call this
/// function to compare two [`Dynamic`] values.
pub const OP_EQUALS: &str = Token::EqualsTo.literal_syntax();

/// Standard concatenation operator.
///
/// Used primarily to build up interpolated strings.
pub const OP_CONCAT: &str = Token::PlusAssign.literal_syntax();

/// Standard containment testing function.
///
/// The `in` operator is implemented as a call to this function.
pub const OP_CONTAINS: &str = "contains";

/// Standard exclusive range operator.
pub const OP_EXCLUSIVE_RANGE: &str = Token::ExclusiveRange.literal_syntax();

/// Standard inclusive range operator.
pub const OP_INCLUSIVE_RANGE: &str = Token::InclusiveRange.literal_syntax();

/// Rhai main scripting engine.
///
/// # Thread Safety
///
/// [`Engine`] is re-entrant.
///
/// Currently, [`Engine`] is neither [`Send`] nor [`Sync`].
/// Use the `sync` feature to make it [`Send`] `+` [`Sync`].
///
/// # Example
///
/// ```
/// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
/// use rhai::Engine;
///
/// let engine = Engine::new();
///
/// let result = engine.eval::<i64>("40 + 2")?;
///
/// println!("Answer: {}", result);  // prints 42
/// # Ok(())
/// # }
/// ```
pub struct Engine {
    /// A collection of all modules loaded into the global namespace of the Engine.
    pub(crate) global_modules: StaticVec<Shared<Module>>,
    /// A collection of all sub-modules directly loaded into the Engine.
    #[cfg(not(feature = "no_module"))]
    pub(crate) global_sub_modules: BTreeMap<Identifier, Shared<Module>>,

    /// A module resolution service.
    #[cfg(not(feature = "no_module"))]
    pub(crate) module_resolver: Box<dyn crate::ModuleResolver>,

    /// An empty [`ImmutableString`] for cloning purposes.
    pub(crate) empty_string: ImmutableString,

    /// A set of symbols to disable.
    pub(crate) disabled_symbols: BTreeSet<Identifier>,
    /// A map containing custom keywords and precedence to recognize.
    pub(crate) custom_keywords: BTreeMap<Identifier, Option<Precedence>>,
    /// Custom syntax.
    pub(crate) custom_syntax: BTreeMap<Identifier, Box<CustomSyntax>>,
    /// Callback closure for filtering variable definition.
    pub(crate) def_var_filter: Option<Box<OnDefVarCallback>>,
    /// Callback closure for resolving variable access.
    pub(crate) resolve_var: Option<Box<OnVarCallback>>,
    /// Callback closure to remap tokens during parsing.
    pub(crate) token_mapper: Option<Box<OnParseTokenCallback>>,

    /// Callback closure for implementing the `print` command.
    pub(crate) print: Box<OnPrintCallback>,
    /// Callback closure for implementing the `debug` command.
    pub(crate) debug: Box<OnDebugCallback>,
    /// Callback closure for progress reporting.
    #[cfg(not(feature = "unchecked"))]
    pub(crate) progress: Option<Box<crate::func::native::OnProgressCallback>>,

    /// Language options.
    pub(crate) options: LanguageOptions,

    /// Max limits.
    #[cfg(not(feature = "unchecked"))]
    pub(crate) limits: crate::api::limits::Limits,

    /// Callback closure for debugging.
    #[cfg(feature = "debugging")]
    pub(crate) debugger: Option<(
        Box<crate::eval::OnDebuggingInit>,
        Box<crate::eval::OnDebuggerCallback>,
    )>,
}

impl fmt::Debug for Engine {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut f = f.debug_struct("Engine");

        f.field("global_modules", &self.global_modules);

        #[cfg(not(feature = "no_module"))]
        f.field("global_sub_modules", &self.global_sub_modules);

        f.field("disabled_symbols", &self.disabled_symbols)
            .field("custom_keywords", &self.custom_keywords)
            .field("custom_syntax", &(!self.custom_syntax.is_empty()))
            .field("def_var_filter", &self.def_var_filter.is_some())
            .field("resolve_var", &self.resolve_var.is_some())
            .field("token_mapper", &self.token_mapper.is_some());

        #[cfg(not(feature = "unchecked"))]
        f.field("progress", &self.progress.is_some());

        f.field("options", &self.options);

        #[cfg(not(feature = "unchecked"))]
        f.field("limits", &self.limits);

        f.finish()
    }
}

impl Default for Engine {
    #[inline(always)]
    fn default() -> Self {
        Self::new()
    }
}

/// Make getter function
#[cfg(not(feature = "no_object"))]
#[inline(always)]
#[must_use]
pub fn make_getter(id: &str) -> Identifier {
    let mut buf = Identifier::new_const();
    buf.push_str(FN_GET);
    buf.push_str(id);
    buf
}

/// Make setter function
#[cfg(not(feature = "no_object"))]
#[inline(always)]
#[must_use]
pub fn make_setter(id: &str) -> Identifier {
    let mut buf = Identifier::new_const();
    buf.push_str(FN_SET);
    buf.push_str(id);
    buf
}

impl Engine {
    /// Create a new [`Engine`].
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        // Create the new scripting Engine
        let mut engine = Self::new_raw();

        #[cfg(not(feature = "no_module"))]
        #[cfg(not(feature = "no_std"))]
        #[cfg(not(target_family = "wasm"))]
        {
            engine.module_resolver = Box::new(crate::module::resolvers::FileModuleResolver::new());
        }

        // default print/debug implementations
        #[cfg(not(feature = "no_std"))]
        #[cfg(not(target_family = "wasm"))]
        {
            engine.print = Box::new(|s| println!("{}", s));
            engine.debug = Box::new(|s, source, pos| {
                if let Some(source) = source {
                    println!("{} @ {:?} | {}", source, pos, s);
                } else if pos.is_none() {
                    println!("{}", s);
                } else {
                    println!("{:?} | {}", pos, s);
                }
            });
        }

        engine.register_global_module(StandardPackage::new().as_shared_module());

        engine
    }

    /// Create a new [`Engine`] with minimal built-in functions.
    ///
    /// Use [`register_global_module`][Engine::register_global_module] to add packages of functions.
    #[inline]
    #[must_use]
    pub fn new_raw() -> Self {
        let mut engine = Self {
            global_modules: StaticVec::new_const(),

            #[cfg(not(feature = "no_module"))]
            global_sub_modules: BTreeMap::new(),

            #[cfg(not(feature = "no_module"))]
            module_resolver: Box::new(crate::module::resolvers::DummyModuleResolver::new()),

            empty_string: ImmutableString::new(),
            disabled_symbols: BTreeSet::new(),
            custom_keywords: BTreeMap::new(),
            custom_syntax: BTreeMap::new(),

            def_var_filter: None,
            resolve_var: None,
            token_mapper: None,

            print: Box::new(|_| {}),
            debug: Box::new(|_, _, _| {}),

            #[cfg(not(feature = "unchecked"))]
            progress: None,

            options: LanguageOptions::new(),

            #[cfg(not(feature = "unchecked"))]
            limits: crate::api::limits::Limits::new(),

            #[cfg(feature = "debugging")]
            debugger: None,
        };

        // Add the global namespace module
        let mut global_namespace = Module::new();
        global_namespace.internal = true;
        engine.global_modules.push(global_namespace.into());

        engine
    }

    /// Get an empty [`ImmutableString`].
    ///
    /// [`Engine`] keeps a single instance of an empty [`ImmutableString`] and uses this to create
    /// shared instances for subsequent uses. This minimizes unnecessary allocations for empty strings.
    #[inline(always)]
    #[must_use]
    pub fn const_empty_string(&self) -> ImmutableString {
        self.empty_string.clone()
    }

    /// Check a result to ensure that it is valid.
    pub(crate) fn check_return_value(&self, mut result: RhaiResult, _pos: Position) -> RhaiResult {
        match result {
            Ok(ref mut r) => {
                // Concentrate all empty strings into one instance to save memory
                if let Dynamic(Union::Str(s, ..)) = r {
                    if s.is_empty() {
                        if !s.ptr_eq(&self.empty_string) {
                            *s = self.const_empty_string();
                        }
                        return result;
                    }
                }

                #[cfg(not(feature = "unchecked"))]
                self.check_data_size(&r, _pos)?;
            }
            _ => (),
        }

        result
    }
}
