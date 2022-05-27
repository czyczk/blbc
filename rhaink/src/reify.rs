/// Macro to cast an identifier or expression to another type with type checks.
///
/// Runs _code_ if _variable_ or _expression_ is of type _type_, otherwise run _fallback_.
///
/// # Syntax
///
/// * `reify!(`_variable_ or _expression_`,|`_temp-variable_`: `_type_`|` _code_`,` `||` _fallback_ `)`
/// * `reify!(`_variable_ or _expression_`,|`_temp-variable_`: `_type_`|` _code_ `)`
/// * `reify!(`_variable_ or _expression_ `=>` `Option<`_type_`>` `)`
/// * `reify!(`_variable_ or _expression_ `=>` _type_ `)`
extern crate alloc;

#[macro_export]
macro_rules! reify {
    ($old:ident, |$new:ident : $t:ty| $code:expr, || $fallback:expr) => {{
        if core::any::TypeId::of::<$t>() == core::any::Any::type_id(&$old) {
            // SAFETY: This is safe because we already checked to make sure the two types
            // are actually the same.
            let $new: $t =
                unsafe { core::mem::transmute_copy(&core::mem::ManuallyDrop::new($old)) };
            $code
        } else {
            $fallback
        }
    }};
    ($old:expr, |$new:ident : $t:ty| $code:expr, || $fallback:expr) => {{
        let old = $old;
        reify!(old, |$new: $t| $code, || $fallback)
    }};

    ($old:ident, |$new:ident : $t:ty| $code:expr) => {
        reify!($old, |$new: $t| $code, || ())
    };
    ($old:expr, |$new:ident : $t:ty| $code:expr) => {
        reify!($old, |$new: $t| $code, || ())
    };

    ($old:ident => Option<$t:ty>) => {
        reify!($old, |v: $t| Some(v), || None)
    };
    ($old:expr => Option<$t:ty>) => {
        reify!($old, |v: $t| Some(v), || None)
    };

    ($old:ident => $t:ty) => {
        reify!($old, |v: $t| v, || unreachable!())
    };
    ($old:expr => $t:ty) => {
        reify!($old, |v: $t| v, || unreachable!())
    };
}
