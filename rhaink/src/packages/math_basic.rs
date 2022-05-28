#![allow(non_snake_case)]

use crate::plugin::*;
use crate::{def_package, Position, RhaiResultOf, ERR, INT};

macro_rules! gen_conversion_as_functions {
    ($root:ident => $func_name:ident ( $($arg_type:ident),+ ) -> $result_type:ty) => {
        pub mod $root { $(pub mod $arg_type {
            use super::super::*;

            #[export_fn]
            pub fn $func_name(x: $arg_type) -> $result_type {
                x as $result_type
            }
        })* }
    }
}

macro_rules! reg_functions {
    ($mod_name:ident += $root:ident :: $func_name:ident ( $($arg_type:ident),+ ) ) => { $(
        set_exported_fn!($mod_name, stringify!($func_name), $root::$arg_type::$func_name);
    )* }
}

def_package! {
    /// Basic mathematical package.
    pub BasicMathPackage(lib) {
        lib.standard = true;

        // Integer functions
        combine_with_exported_module!(lib, "int", int_functions);

        reg_functions!(lib += basic_to_int::to_int(char));

        #[cfg(not(feature = "only_i32"))]
        #[cfg(not(feature = "only_i64"))]
        {
            reg_functions!(lib += numbers_to_int::to_int(i8, u8, i16, u16, i32, u32, i64, u64));

            #[cfg(not(target_family = "wasm"))]

            reg_functions!(lib += num_128_to_int::to_int(i128, u128));
        }

        #[cfg(not(feature = "no_float"))]
        {
            // Floating point functions
            combine_with_exported_module!(lib, "float", float_functions);

            // Trig functions
            combine_with_exported_module!(lib, "trig", trig_functions);

            reg_functions!(lib += basic_to_float::to_float(INT));

            #[cfg(not(feature = "only_i32"))]
            #[cfg(not(feature = "only_i64"))]
            {
                reg_functions!(lib += numbers_to_float::to_float(i8, u8, i16, u16, i32, u32, i64, u32));

                #[cfg(not(target_family = "wasm"))]

                reg_functions!(lib += num_128_to_float::to_float(i128, u128));
            }
        }

        // Decimal functions
        #[cfg(feature = "decimal")]
        {
            combine_with_exported_module!(lib, "decimal", decimal_functions);

            reg_functions!(lib += basic_to_decimal::to_decimal(INT));

            #[cfg(not(feature = "only_i32"))]
            #[cfg(not(feature = "only_i64"))]
            reg_functions!(lib += numbers_to_decimal::to_decimal(i8, u8, i16, u16, i32, u32, i64, u64));
        }
    }
}

#[export_module]
mod int_functions {
    /// Parse a string into an integer number.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let x = parse_int("123");
    ///
    /// print(x);       // prints 123
    /// ```
    #[rhai_fn(name = "parse_int", return_raw)]
    pub fn parse_int(string: &str) -> RhaiResultOf<INT> {
        parse_int_radix(string, 10)
    }
    /// Parse a string into an integer number of the specified `radix`.
    ///
    /// `radix` must be between 2 and 36.
    ///
    /// # Example
    ///
    /// ```rhai
    /// let x = parse_int("123");
    ///
    /// print(x);       // prints 123
    ///
    /// let y = parse_int("123abc", 16);
    ///
    /// print(y);       // prints 1194684 (0x123abc)
    /// ```
    #[rhai_fn(name = "parse_int", return_raw)]
    pub fn parse_int_radix(string: &str, radix: INT) -> RhaiResultOf<INT> {
        if !(2..=36).contains(&radix) {
            return Err(ERR::ErrorArithmetic(
                format!("Invalid radix: '{}'", radix),
                Position::NONE,
            )
            .into());
        }

        INT::from_str_radix(string.trim(), radix as u32).map_err(|err| {
            ERR::ErrorArithmetic(
                format!("Error parsing integer number '{}': {}", string, err),
                Position::NONE,
            )
            .into()
        })
    }
}

gen_conversion_as_functions!(basic_to_int => to_int (char) -> INT);

#[cfg(not(feature = "only_i32"))]
#[cfg(not(feature = "only_i64"))]
gen_conversion_as_functions!(numbers_to_int => to_int (i8, u8, i16, u16, i32, u32, i64, u64) -> INT);

#[cfg(not(feature = "only_i32"))]
#[cfg(not(feature = "only_i64"))]
#[cfg(not(target_family = "wasm"))]

gen_conversion_as_functions!(num_128_to_int => to_int (i128, u128) -> INT);
