extern crate alloc;

use core::any::TypeId;
use core::cmp::Ordering;
use core::fmt;
use core::iter::once;
use core::ops::Add;
use core::ops::AddAssign;

use ink_prelude::boxed::Box;
use ink_prelude::collections::BTreeMap;
use ink_prelude::collections::BTreeSet;

use crate::ast::flags::FnAccess;
use crate::func::callable_function::CallableFunction;
use crate::func::IteratorFn;
use crate::StaticVec;

use super::types::custom_types::CustomTypesCollection;
use super::types::dynamic::Variant;
use super::{types::dynamic::Dynamic, Identifier, Shared};

/// A module which may contain variables, sub-modules, external Rust functions,
/// and/or script-defined functions.
#[derive(Clone)]
pub struct Module {
    /// ID identifying the module.
    /// No ID if string is empty.
    id: Identifier,
    /// Is this module internal?
    pub(crate) internal: bool,
    /// Is this module part of a standard library?
    pub(crate) standard: bool,
    /// Custom types.
    custom_types: CustomTypesCollection,
    /// Sub-modules.
    modules: BTreeMap<Identifier, Shared<Module>>,
    /// [`Module`] variables.
    variables: BTreeMap<Identifier, Dynamic>,
    /// Flattened collection of all [`Module`] variables, including those in sub-modules.
    all_variables: BTreeMap<u64, Dynamic>,
    /// External Rust functions.
    functions: BTreeMap<u64, Box<FuncInfo>>,
    /// Flattened collection of all external Rust functions, native or scripted.
    /// including those in sub-modules.
    all_functions: BTreeMap<u64, Shared<CallableFunction>>,
    /// Iterator functions, keyed by the type producing the iterator.
    type_iterators: BTreeMap<TypeId, Shared<IteratorFn>>,
    /// Flattened collection of iterator functions, including those in sub-modules.
    all_type_iterators: BTreeMap<TypeId, Shared<IteratorFn>>,
    /// Is the [`Module`] indexed?
    indexed: bool,
    /// Does the [`Module`] contain indexed functions that have been exposed to the global namespace?
    contains_indexed_global_functions: bool,
}

impl Default for Module {
    #[inline(always)]
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Debug for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut d = f.debug_struct("Module");

        if !self.id.is_empty() {
            d.field("id", &self.id);
        }
        if !self.modules.is_empty() {
            d.field(
                "modules",
                &self
                    .modules
                    .keys()
                    .map(|m| m.as_str())
                    .collect::<BTreeSet<_>>(),
            );
        }
        if !self.variables.is_empty() {
            d.field("vars", &self.variables);
        }
        if !self.functions.is_empty() {
            d.field(
                "functions",
                &self
                    .iter_fn()
                    .map(|f| f.func.to_string())
                    .collect::<BTreeSet<_>>(),
            );
        }
        d.finish()
    }
}

impl<M: AsRef<Module>> Add<M> for &Module {
    type Output = Module;

    #[inline]
    fn add(self, rhs: M) -> Self::Output {
        let mut module = self.clone();
        module.merge(rhs.as_ref());
        module
    }
}

impl<M: AsRef<Module>> Add<M> for Module {
    type Output = Self;

    #[inline(always)]
    fn add(mut self, rhs: M) -> Self::Output {
        self.merge(rhs.as_ref());
        self
    }
}

impl<M: Into<Module>> AddAssign<M> for Module {
    #[inline(always)]
    fn add_assign(&mut self, rhs: M) {
        self.combine(rhs.into());
    }
}

impl Module {
    /// Create a new [`Module`].
    ///
    /// # Example
    ///
    /// ```
    /// # use rhai::Module;
    /// let mut module = Module::new();
    /// module.set_var("answer", 42_i64);
    /// assert_eq!(module.get_var_value::<i64>("answer").expect("answer should exist"), 42);
    /// ```
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Self {
            id: Identifier::new_const(),
            internal: false,
            standard: false,
            custom_types: CustomTypesCollection::new(),
            modules: BTreeMap::new(),
            variables: BTreeMap::new(),
            all_variables: BTreeMap::new(),
            functions: BTreeMap::new(),
            all_functions: BTreeMap::new(),
            type_iterators: BTreeMap::new(),
            all_type_iterators: BTreeMap::new(),
            indexed: true,
            contains_indexed_global_functions: false,
        }
    }

    /// Get the ID of the [`Module`], if any.
    ///
    /// # Example
    ///
    /// ```
    /// # use rhai::Module;
    /// let mut module = Module::new();
    /// module.set_id("hello");
    /// assert_eq!(module.id(), Some("hello"));
    /// ```
    #[inline]
    #[must_use]
    pub fn id(&self) -> Option<&str> {
        if self.id_raw().is_empty() {
            None
        } else {
            Some(self.id_raw())
        }
    }

    /// Get the ID of the [`Module`] as an [`Identifier`], if any.
    #[inline(always)]
    #[must_use]
    pub(crate) const fn id_raw(&self) -> &Identifier {
        &self.id
    }

    /// Set the ID of the [`Module`].
    ///
    /// If the string is empty, it is equivalent to clearing the ID.
    ///
    /// # Example
    ///
    /// ```
    /// # use rhai::Module;
    /// let mut module = Module::new();
    /// module.set_id("hello");
    /// assert_eq!(module.id(), Some("hello"));
    /// ```
    #[inline(always)]
    pub fn set_id(&mut self, id: impl Into<Identifier>) -> &mut Self {
        self.id = id.into();
        self
    }
    /// Clear the ID of the [`Module`].
    ///
    /// # Example
    ///
    /// ```
    /// # use rhai::Module;
    /// let mut module = Module::new();
    /// module.set_id("hello");
    /// assert_eq!(module.id(), Some("hello"));
    /// module.clear_id();
    /// assert_eq!(module.id(), None);
    /// ```
    #[inline(always)]
    pub fn clear_id(&mut self) -> &mut Self {
        self.id.clear();
        self
    }

    /// Map a custom type to a friendly display name.
    ///
    /// # Example
    ///
    /// ```
    /// # use rhai::Module;
    /// #[derive(Clone)]
    /// struct TestStruct;
    ///
    /// let name = std::any::type_name::<TestStruct>();
    ///
    /// let mut module = Module::new();
    ///
    /// module.set_custom_type::<TestStruct>("MyType");
    ///
    /// assert_eq!(module.get_custom_type(name), Some("MyType"));
    /// ```
    #[inline(always)]
    pub fn set_custom_type<T>(&mut self, name: &str) -> &mut Self {
        self.custom_types.add_type::<T>(name);
        self
    }
    /// Map a custom type to a friendly display name.
    ///
    /// ```
    /// # use rhai::Module;
    /// #[derive(Clone)]
    /// struct TestStruct;
    ///
    /// let name = std::any::type_name::<TestStruct>();
    ///
    /// let mut module = Module::new();
    ///
    /// module.set_custom_type_raw(name, "MyType");
    ///
    /// assert_eq!(module.get_custom_type(name), Some("MyType"));
    /// ```
    #[inline(always)]
    pub fn set_custom_type_raw(
        &mut self,
        type_name: impl Into<Identifier>,
        name: impl Into<Identifier>,
    ) -> &mut Self {
        self.custom_types.add(type_name, name);
        self
    }
    /// Get the display name of a registered custom type.
    ///
    /// # Example
    ///
    /// ```
    /// # use rhai::Module;
    /// #[derive(Clone)]
    /// struct TestStruct;
    ///
    /// let name = std::any::type_name::<TestStruct>();
    ///
    /// let mut module = Module::new();
    ///
    /// module.set_custom_type::<TestStruct>("MyType");
    ///
    /// assert_eq!(module.get_custom_type(name), Some("MyType"));
    /// ```
    #[inline(always)]
    pub fn get_custom_type(&self, key: &str) -> Option<&str> {
        self.custom_types.get(key)
    }

    /// Is the [`Module`] empty?
    ///
    /// # Example
    ///
    /// ```
    /// # use rhai::Module;
    /// let module = Module::new();
    /// assert!(module.is_empty());
    /// ```
    #[inline]
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.indexed
            && !self.contains_indexed_global_functions
            && self.functions.is_empty()
            && self.all_functions.is_empty()
            && self.variables.is_empty()
            && self.all_variables.is_empty()
            && self.modules.is_empty()
            && self.type_iterators.is_empty()
            && self.all_type_iterators.is_empty()
    }

    /// Is the [`Module`] indexed?
    ///
    /// A module must be indexed before it can be used in an `import` statement.
    ///
    /// # Example
    ///
    /// ```
    /// # use rhai::Module;
    /// let mut module = Module::new();
    /// assert!(module.is_indexed());
    ///
    /// module.set_native_fn("foo", |x: &mut i64, y: i64| { *x = y; Ok(()) });
    /// assert!(!module.is_indexed());
    ///
    /// # #[cfg(not(feature = "no_module"))]
    /// # {
    /// module.build_index();
    /// assert!(module.is_indexed());
    /// # }
    /// ```
    #[inline(always)]
    #[must_use]
    pub const fn is_indexed(&self) -> bool {
        self.indexed
    }

    /// _(metadata)_ Generate signatures for all the non-private functions in the [`Module`].
    /// Exported under the `metadata` feature only.
    #[cfg(feature = "metadata")]
    #[inline]
    pub fn gen_fn_signatures(&self) -> impl Iterator<Item = String> + '_ {
        self.iter_fn()
            .filter(|&f| match f.metadata.access {
                FnAccess::Public => true,
                FnAccess::Private => false,
            })
            .map(|f| f.gen_signature())
    }

    /// Does a variable exist in the [`Module`]?
    ///
    /// # Example
    ///
    /// ```
    /// # use rhai::Module;
    /// let mut module = Module::new();
    /// module.set_var("answer", 42_i64);
    /// assert!(module.contains_var("answer"));
    /// ```
    #[inline(always)]
    #[must_use]
    pub fn contains_var(&self, name: &str) -> bool {
        if !self.variables.is_empty() {
            self.variables.contains_key(name)
        } else {
            false
        }
    }

    /// Get the value of a [`Module`] variable.
    ///
    /// # Example
    ///
    /// ```
    /// # use rhai::Module;
    /// let mut module = Module::new();
    /// module.set_var("answer", 42_i64);
    /// assert_eq!(module.get_var_value::<i64>("answer").expect("answer should exist"), 42);
    /// ```
    #[inline]
    #[must_use]
    pub fn get_var_value<T: Variant + Clone>(&self, name: &str) -> Option<T> {
        self.get_var(name).and_then(Dynamic::try_cast::<T>)
    }

    /// Get a [`Module`] variable as a [`Dynamic`].
    ///
    /// # Example
    ///
    /// ```
    /// # use rhai::Module;
    /// let mut module = Module::new();
    /// module.set_var("answer", 42_i64);
    /// assert_eq!(module.get_var("answer").expect("answer should exist").cast::<i64>(), 42);
    /// ```
    #[inline(always)]
    #[must_use]
    pub fn get_var(&self, name: &str) -> Option<Dynamic> {
        if !self.variables.is_empty() {
            self.variables.get(name).cloned()
        } else {
            None
        }
    }

    /// Set a variable into the [`Module`].
    ///
    /// If there is an existing variable of the same name, it is replaced.
    ///
    /// # Example
    ///
    /// ```
    /// # use rhai::Module;
    /// let mut module = Module::new();
    /// module.set_var("answer", 42_i64);
    /// assert_eq!(module.get_var_value::<i64>("answer").expect("answer should exist"), 42);
    /// ```
    #[inline]
    pub fn set_var(
        &mut self,
        name: impl Into<Identifier>,
        value: impl Variant + Clone,
    ) -> &mut Self {
        let ident = name.into();
        let value = Dynamic::from(value);

        if self.indexed {
            let hash_var = crate::calc_qualified_var_hash(once(""), &ident);
            self.all_variables.insert(hash_var, value.clone());
        }
        self.variables.insert(ident, value);
        self
    }
}

/// A type representing the namespace of a function.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[non_exhaustive]
pub enum FnNamespace {
    /// Module namespace only.
    ///
    /// Ignored under `no_module`.
    Internal,
    /// Expose to global namespace.
    Global,
}

/// A type containing a single registered function.
#[derive(Debug, Clone)]
pub struct FuncInfo {
    /// Function instance.
    pub func: Shared<CallableFunction>,
    /// Parameter types (if applicable).
    pub param_types: StaticVec<TypeId>,
    /// Function metadata.
    pub metadata: FnMetadata,
}

/// A type containing all metadata for a registered function.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
#[non_exhaustive]
pub struct FnMetadata {
    /// Function namespace.
    pub namespace: FnNamespace,
    /// Function access mode.
    pub access: FnAccess,
    /// Function name.
    pub name: Identifier,
    /// Number of parameters.
    pub params: usize,
    /// Parameter names and types (if available).
    #[cfg(feature = "metadata")]
    pub params_info: StaticVec<Identifier>,
    /// Return type name.
    #[cfg(feature = "metadata")]
    pub return_type: Identifier,
    /// Comments.
    #[cfg(feature = "metadata")]
    pub comments: Box<[Box<str>]>,
}

impl PartialOrd for FnMetadata {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FnMetadata {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.name.cmp(&other.name) {
            #[cfg(feature = "metadata")]
            Ordering::Equal => match self.params.cmp(&other.params) {
                Ordering::Equal => self.params_info.cmp(&other.params_info),
                cmp => cmp,
            },
            #[cfg(not(feature = "metadata"))]
            Ordering::Equal => self.params.cmp(&other.params),
            cmp => cmp,
        }
    }
}

impl FuncInfo {
    /// Format a return type to be display-friendly.
    ///
    /// `()` is cleared.  
    /// [`RhaiResult`][crate::RhaiResult] and [`RhaiResultOf<T>`] are expanded.
    #[cfg(feature = "metadata")]
    pub fn format_type(typ: &str, is_return_type: bool) -> std::borrow::Cow<str> {
        const RHAI_RESULT_TYPE: &str = "RhaiResult";
        const RHAI_RESULT_TYPE_EXPAND: &str = "Result<Dynamic, Box<EvalAltResult>>";
        const RHAI_RESULT_OF_TYPE: &str = "RhaiResultOf<";
        const RHAI_RESULT_OF_TYPE_EXPAND: &str = "Result<{}, Box<EvalAltResult>>";
        const RHAI_RANGE: &str = "ExclusiveRange";
        const RHAI_RANGE_TYPE: &str = "Range<";
        const RHAI_RANGE_EXPAND: &str = "Range<{}>";
        const RHAI_INCLUSIVE_RANGE: &str = "InclusiveRange";
        const RHAI_INCLUSIVE_RANGE_TYPE: &str = "RangeInclusive<";
        const RHAI_INCLUSIVE_RANGE_EXPAND: &str = "RangeInclusive<{}>";

        let typ = typ.trim();

        if typ.starts_with("rhai::") {
            return Self::format_type(&typ[6..], is_return_type);
        } else if typ.starts_with("&mut ") {
            let x = &typ[5..];
            let r = Self::format_type(x, false);
            return if r == x {
                typ.into()
            } else {
                format!("&mut {}", r).into()
            };
        }

        match typ {
            "" | "()" if is_return_type => "".into(),
            "INT" => std::any::type_name::<crate::INT>().into(),
            #[cfg(not(feature = "no_float"))]
            "FLOAT" => std::any::type_name::<crate::FLOAT>().into(),
            RHAI_RANGE => RHAI_RANGE_EXPAND
                .replace("{}", std::any::type_name::<crate::INT>())
                .into(),
            RHAI_INCLUSIVE_RANGE => RHAI_INCLUSIVE_RANGE_EXPAND
                .replace("{}", std::any::type_name::<crate::INT>())
                .into(),
            RHAI_RESULT_TYPE => RHAI_RESULT_TYPE_EXPAND.into(),
            ty if ty.starts_with(RHAI_RANGE_TYPE) && ty.ends_with('>') => {
                let inner = &ty[RHAI_RANGE_TYPE.len()..ty.len() - 1];
                RHAI_RANGE_EXPAND
                    .replace("{}", Self::format_type(inner, false).trim())
                    .into()
            }
            ty if ty.starts_with(RHAI_INCLUSIVE_RANGE_TYPE) && ty.ends_with('>') => {
                let inner = &ty[RHAI_INCLUSIVE_RANGE_TYPE.len()..ty.len() - 1];
                RHAI_INCLUSIVE_RANGE_EXPAND
                    .replace("{}", Self::format_type(inner, false).trim())
                    .into()
            }
            ty if ty.starts_with(RHAI_RESULT_OF_TYPE) && ty.ends_with('>') => {
                let inner = &ty[RHAI_RESULT_OF_TYPE.len()..ty.len() - 1];
                RHAI_RESULT_OF_TYPE_EXPAND
                    .replace("{}", Self::format_type(inner, false).trim())
                    .into()
            }
            ty => ty.into(),
        }
    }
    /// _(metadata)_ Generate a signature of the function.
    /// Exported under the `metadata` feature only.
    #[cfg(feature = "metadata")]
    #[must_use]
    pub fn gen_signature(&self) -> String {
        let mut sig = format!("{}(", self.metadata.name);

        let return_type = Self::format_type(&self.metadata.return_type, true);

        if !self.metadata.params_info.is_empty() {
            let params: StaticVec<_> = self
                .metadata
                .params_info
                .iter()
                .map(|s| {
                    let mut seg = s.splitn(2, ':');
                    let name = match seg.next().unwrap().trim() {
                        "" => "_",
                        s => s,
                    };
                    let result: std::borrow::Cow<str> = match seg.next() {
                        Some(typ) => {
                            format!("{}: {}", name, FuncInfo::format_type(typ, false)).into()
                        }
                        None => name.into(),
                    };
                    result
                })
                .collect();
            sig.push_str(&params.join(", "));
            sig.push(')');
        } else {
            for x in 0..self.metadata.params {
                sig.push('_');
                if x < self.metadata.params - 1 {
                    sig.push_str(", ");
                }
            }
            sig.push(')');
        }

        if !self.func.is_script() && !return_type.is_empty() {
            sig.push_str(" -> ");
            sig.push_str(&return_type);
        }

        sig
    }
}
