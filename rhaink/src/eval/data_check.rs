//! Data size checks during evaluation.
#![cfg(not(feature = "unchecked"))]

use crate::types::dynamic::Union;
use crate::{Dynamic, Engine, Position, RhaiResultOf, ERR};
use core::num::NonZeroUsize;

impl Engine {
    /// Recursively calculate the sizes of a value.
    ///
    /// Sizes returned are `(` [`Array`][crate::Array], [`Map`][crate::Map] and [`String`] `)`.
    ///
    /// # Panics
    ///
    /// Panics if any interior data is shared (should never happen).
    #[cfg(not(feature = "unchecked"))]
    pub(crate) fn calc_data_sizes(value: &Dynamic, _top: bool) -> (usize, usize, usize) {
        match value.0 {
            #[cfg(not(feature = "no_index"))]
            Union::Array(ref arr, ..) => {
                arr.iter()
                    .fold((0, 0, 0), |(arrays, maps, strings), value| match value.0 {
                        Union::Array(..) => {
                            let (a, m, s) = Self::calc_data_sizes(value, false);
                            (arrays + a + 1, maps + m, strings + s)
                        }
                        Union::Blob(ref a, ..) => (arrays + 1 + a.len(), maps, strings),
                        #[cfg(not(feature = "no_object"))]
                        Union::Map(..) => {
                            let (a, m, s) = Self::calc_data_sizes(value, false);
                            (arrays + a + 1, maps + m, strings + s)
                        }
                        Union::Str(ref s, ..) => (arrays + 1, maps, strings + s.len()),
                        _ => (arrays + 1, maps, strings),
                    })
            }
            #[cfg(not(feature = "no_index"))]
            Union::Blob(ref arr, ..) => (arr.len(), 0, 0),
            #[cfg(not(feature = "no_object"))]
            Union::Map(ref map, ..) => {
                map.values()
                    .fold((0, 0, 0), |(arrays, maps, strings), value| match value.0 {
                        #[cfg(not(feature = "no_index"))]
                        Union::Array(..) => {
                            let (a, m, s) = Self::calc_data_sizes(value, false);
                            (arrays + a, maps + m + 1, strings + s)
                        }
                        #[cfg(not(feature = "no_index"))]
                        Union::Blob(ref a, ..) => (arrays + a.len(), maps, strings),
                        Union::Map(..) => {
                            let (a, m, s) = Self::calc_data_sizes(value, false);
                            (arrays + a, maps + m + 1, strings + s)
                        }
                        Union::Str(ref s, ..) => (arrays, maps + 1, strings + s.len()),
                        _ => (arrays, maps + 1, strings),
                    })
            }
            Union::Str(ref s, ..) => (0, 0, s.len()),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(..) if _top => {
                Self::calc_data_sizes(&*value.read_lock::<Dynamic>().unwrap(), true)
            }
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(..) => {
                unreachable!("shared values discovered within data: {}", value)
            }
            _ => (0, 0, 0),
        }
    }

    /// Is there a data size limit set?
    #[cfg(not(feature = "unchecked"))]
    pub(crate) fn has_data_size_limit(&self) -> bool {
        let mut _limited = self.limits.max_string_size.is_some();

        #[cfg(not(feature = "no_index"))]
        {
            _limited = _limited || self.limits.max_array_size.is_some();
        }
        #[cfg(not(feature = "no_object"))]
        {
            _limited = _limited || self.limits.max_map_size.is_some();
        }

        _limited
    }

    /// Raise an error if any data size exceeds limit.
    #[cfg(not(feature = "unchecked"))]
    pub(crate) fn raise_err_if_over_data_size_limit(
        &self,
        sizes: (usize, usize, usize),
        pos: Position,
    ) -> RhaiResultOf<()> {
        let (_arr, _map, s) = sizes;

        if s > self
            .limits
            .max_string_size
            .map_or(usize::MAX, NonZeroUsize::get)
        {
            return Err(ERR::ErrorDataTooLarge("Length of string".to_string(), pos).into());
        }

        #[cfg(not(feature = "no_index"))]
        if _arr
            > self
                .limits
                .max_array_size
                .map_or(usize::MAX, NonZeroUsize::get)
        {
            return Err(ERR::ErrorDataTooLarge("Size of array".to_string(), pos).into());
        }

        #[cfg(not(feature = "no_object"))]
        if _map
            > self
                .limits
                .max_map_size
                .map_or(usize::MAX, NonZeroUsize::get)
        {
            return Err(ERR::ErrorDataTooLarge("Size of object map".to_string(), pos).into());
        }

        Ok(())
    }

    /// Check whether the size of a [`Dynamic`] is within limits.
    #[cfg(not(feature = "unchecked"))]
    pub(crate) fn check_data_size(&self, value: &Dynamic, pos: Position) -> RhaiResultOf<()> {
        // If no data size limits, just return
        if !self.has_data_size_limit() {
            return Ok(());
        }

        let sizes = Self::calc_data_sizes(value, true);

        self.raise_err_if_over_data_size_limit(sizes, pos)
    }

    /// Raise an error if the size of a [`Dynamic`] is out of limits (if any).
    ///
    /// Not available under `unchecked`.
    #[cfg(not(feature = "unchecked"))]
    #[inline(always)]
    pub fn ensure_data_size_within_limits(&self, value: &Dynamic) -> RhaiResultOf<()> {
        self.check_data_size(value, Position::NONE)
    }

    /// Check if the number of operations stay within limit.
    #[cfg(not(feature = "unchecked"))]
    pub(crate) fn inc_operations(
        &self,
        num_operations: &mut u64,
        pos: Position,
    ) -> RhaiResultOf<()> {
        *num_operations += 1;

        // Guard against too many operations
        if self.max_operations() > 0 && *num_operations > self.max_operations() {
            return Err(ERR::ErrorTooManyOperations(pos).into());
        }

        // Report progress - only in steps
        if let Some(ref progress) = self.progress {
            if let Some(token) = progress(*num_operations) {
                // Terminate script if progress returns a termination token
                return Err(ERR::ErrorTerminated(token, pos).into());
            }
        }

        Ok(())
    }
}
