use crate::def_package;
use crate::plugin::*;
use crate::types::dynamic::Tag;
use crate::{Dynamic, RhaiResultOf, ERR, INT};

def_package! {
    /// Package of core language features.
    pub LanguageCorePackage(lib) {
        lib.standard = true;

        combine_with_exported_module!(lib, "core", core_functions);

        #[cfg(not(feature = "no_function"))]
        #[cfg(not(feature = "no_index"))]
        #[cfg(not(feature = "no_object"))]
        combine_with_exported_module!(lib, "reflection", reflection_functions);
    }
}

#[export_module]
mod core_functions {
    /// Return the _tag_ of a `Dynamic` value.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let x = "hello, world!";
    ///
    /// x.tag = 42;
    ///
    /// print(x.tag);           // prints 42
    /// ```
    #[rhai_fn(name = "tag", get = "tag", pure)]
    pub fn get_tag(value: &mut Dynamic) -> INT {
        value.tag() as INT
    }
    /// Set the _tag_ of a `Dynamic` value.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let x = "hello, world!";
    ///
    /// x.tag = 42;
    ///
    /// print(x.tag);           // prints 42
    /// ```
    #[rhai_fn(name = "set_tag", set = "tag", return_raw)]
    pub fn set_tag(value: &mut Dynamic, tag: INT) -> RhaiResultOf<()> {
        if tag < Tag::MIN as INT {
            Err(ERR::ErrorArithmetic(
                format!(
                    "{} is too small to fit into a tag (must be between {} and {})",
                    tag,
                    Tag::MIN,
                    Tag::MAX
                ),
                Position::NONE,
            )
            .into())
        } else if tag > Tag::MAX as INT {
            Err(ERR::ErrorArithmetic(
                format!(
                    "{} is too large to fit into a tag (must be between {} and {})",
                    tag,
                    Tag::MIN,
                    Tag::MAX
                ),
                Position::NONE,
            )
            .into())
        } else {
            value.set_tag(tag as Tag);
            Ok(())
        }
    }

    /// Block the current thread for a particular number of `seconds`.
    #[cfg(not(feature = "no_float"))]
    #[cfg(not(feature = "no_std"))]
    #[rhai_fn(name = "sleep")]
    pub fn sleep_float(seconds: crate::FLOAT) {
        if seconds <= 0.0 {
            return;
        }

        #[cfg(not(feature = "f32_float"))]
        std::thread::sleep(std::time::Duration::from_secs_f64(seconds));
        #[cfg(feature = "f32_float")]
        std::thread::sleep(std::time::Duration::from_secs_f32(seconds));
    }

    /// Block the current thread for a particular number of `seconds`.
    #[cfg(not(feature = "no_std"))]
    pub fn sleep(seconds: INT) {
        if seconds <= 0 {
            return;
        }
        std::thread::sleep(std::time::Duration::from_secs(seconds as u64));
    }
}

#[cfg(not(feature = "no_function"))]
#[cfg(not(feature = "no_index"))]
#[cfg(not(feature = "no_object"))]
#[export_module]
mod reflection_functions {
    pub fn get_fn_metadata_list(ctx: NativeCallContext) -> crate::Array {
        collect_fn_metadata(ctx, |_, _, _, _, _| true)
    }
    #[rhai_fn(name = "get_fn_metadata_list")]
    pub fn get_fn_metadata(ctx: NativeCallContext, name: &str) -> crate::Array {
        collect_fn_metadata(ctx, |_, _, n, _, _| n == name)
    }
    #[rhai_fn(name = "get_fn_metadata_list")]
    pub fn get_fn_metadata2(ctx: NativeCallContext, name: &str, params: INT) -> crate::Array {
        if params < 0 {
            crate::Array::new()
        } else {
            collect_fn_metadata(ctx, |_, _, n, p, _| p == (params as usize) && n == name)
        }
    }
}

#[cfg(not(feature = "no_function"))]
#[cfg(not(feature = "no_index"))]
#[cfg(not(feature = "no_object"))]
fn collect_fn_metadata(
    ctx: NativeCallContext,
    filter: impl Fn(FnNamespace, FnAccess, &str, usize, &crate::Shared<crate::ast::ScriptFnDef>) -> bool
        + Copy,
) -> crate::Array {
    use crate::{ast::ScriptFnDef, Array, Identifier, Map};
    use std::collections::BTreeSet;

    // Create a metadata record for a function.
    fn make_metadata(
        dict: &BTreeSet<Identifier>,
        #[cfg(not(feature = "no_module"))] namespace: Identifier,
        func: &ScriptFnDef,
    ) -> Map {
        const DICT: &str = "key exists";

        let mut map = Map::new();

        #[cfg(not(feature = "no_module"))]
        if !namespace.is_empty() {
            map.insert(dict.get("namespace").expect(DICT).clone(), namespace.into());
        }
        map.insert(
            dict.get("name").expect(DICT).clone(),
            func.name.clone().into(),
        );
        map.insert(
            dict.get("access").expect(DICT).clone(),
            match func.access {
                FnAccess::Public => dict.get("public").expect(DICT).clone(),
                FnAccess::Private => dict.get("private").expect(DICT).clone(),
            }
            .into(),
        );
        map.insert(
            dict.get("is_anonymous").expect(DICT).clone(),
            func.name.starts_with(crate::engine::FN_ANONYMOUS).into(),
        );
        map.insert(
            dict.get("params").expect(DICT).clone(),
            func.params
                .iter()
                .cloned()
                .map(Into::into)
                .collect::<Array>()
                .into(),
        );

        map
    }

    // Intern strings
    let dict: BTreeSet<Identifier> = [
        #[cfg(not(feature = "no_module"))]
        "namespace",
        "name",
        "access",
        "public",
        "private",
        "is_anonymous",
        "params",
    ]
    .iter()
    .map(|&s| s.into())
    .collect();

    let mut list = Array::new();

    ctx.iter_namespaces()
        .flat_map(Module::iter_script_fn)
        .filter(|(s, a, n, p, f)| filter(*s, *a, n, *p, f))
        .for_each(|(.., f)| {
            list.push(
                make_metadata(
                    &dict,
                    #[cfg(not(feature = "no_module"))]
                    Identifier::new_const(),
                    f,
                )
                .into(),
            )
        });

    ctx.engine()
        .global_modules
        .iter()
        .flat_map(|m| m.iter_script_fn())
        .filter(|(ns, a, n, p, f)| filter(*ns, *a, n, *p, f))
        .for_each(|(.., f)| {
            list.push(
                make_metadata(
                    &dict,
                    #[cfg(not(feature = "no_module"))]
                    Identifier::new_const(),
                    f,
                )
                .into(),
            )
        });

    #[cfg(not(feature = "no_module"))]
    ctx.engine()
        .global_sub_modules
        .values()
        .flat_map(|m| m.iter_script_fn())
        .filter(|(ns, a, n, p, f)| filter(*ns, *a, n, *p, f))
        .for_each(|(.., f)| {
            list.push(
                make_metadata(
                    &dict,
                    #[cfg(not(feature = "no_module"))]
                    Identifier::new_const(),
                    f,
                )
                .into(),
            )
        });

    #[cfg(not(feature = "no_module"))]
    {
        // Recursively scan modules for script-defined functions.
        fn scan_module(
            list: &mut Array,
            dict: &BTreeSet<Identifier>,
            namespace: Identifier,
            module: &Module,
            filter: impl Fn(
                    FnNamespace,
                    FnAccess,
                    &str,
                    usize,
                    &crate::Shared<crate::ast::ScriptFnDef>,
                ) -> bool
                + Copy,
        ) {
            module
                .iter_script_fn()
                .filter(|(s, a, n, p, f)| filter(*s, *a, n, *p, f))
                .for_each(|(.., f)| list.push(make_metadata(dict, namespace.clone(), f).into()));
            for (ns, m) in module.iter_sub_modules() {
                let ns = format!(
                    "{}{}{}",
                    namespace,
                    crate::tokenizer::Token::DoubleColon.literal_syntax(),
                    ns
                );
                scan_module(list, dict, ns.into(), m.as_ref(), filter)
            }
        }

        for (ns, m) in ctx.iter_imports_raw() {
            scan_module(&mut list, &dict, ns.clone(), m.as_ref(), filter)
        }
    }

    list
}
