//! Type to hold a mutable reference to the target of an evaluation.

use crate::tokenizer::Position;
use crate::types::dynamic::Variant;
use crate::types::immutable_string::ImmutableString;
use crate::{Blob, Dynamic, RhaiResultOf};
use core::ops::{Deref, DerefMut};

// Calculate an offset+len pair given an actual length of the underlying array.
//
// Negative starting positions count from the end.
//
// Values going over bounds are limited to the actual length.
#[inline]
#[allow(dead_code)]
pub fn calc_offset_len(length: usize, start: crate::INT, len: crate::INT) -> (usize, usize) {
    let start = if start < 0 {
        start.checked_abs().map_or(0, |positive_start| {
            length - usize::min(positive_start as usize, length)
        })
    } else if start as usize >= length {
        return (length, 0);
    } else {
        start as usize
    };

    let len = if len <= 0 {
        0
    } else if len as usize > length - start {
        length - start
    } else {
        len as usize
    };

    (start, len)
}

// Calculate an offset+len pair given an actual length of the underlying array.
//
// Negative starting positions count from the end.
//
// Values going over bounds call the provided closure to return a default value or an error.
#[inline]
#[allow(dead_code)]
pub fn calc_index<E>(
    length: usize,
    start: crate::INT,
    negative_count_from_end: bool,
    err: impl Fn() -> Result<usize, E>,
) -> Result<usize, E> {
    if start < 0 {
        if negative_count_from_end {
            // Count from end if negative
            #[cfg(not(feature = "unchecked"))]
            return match start.checked_abs() {
                Some(positive_start) => {
                    if (positive_start as usize) > length {
                        err()
                    } else {
                        Ok(length - (positive_start as usize))
                    }
                }
                None => err(),
            };
            #[cfg(feature = "unchecked")]
            return Ok(length - (start.abs() as usize));
        } else {
            err()
        }
    } else if start as usize >= length {
        err()
    } else {
        Ok(start as usize)
    }
}

/// A type that encapsulates a mutation target for an expression with side effects.
#[derive(Debug)]
pub enum Target<'a> {
    /// The target is a mutable reference to a [`Dynamic`].
    RefMut(&'a mut Dynamic),
    /// The target is a mutable reference to a _shared_ [`Dynamic`].
    #[cfg(not(feature = "no_closure"))]
    SharedValue {
        /// Lock guard to the shared [`Dynamic`].
        source: crate::types::dynamic::DynamicWriteLock<'a, Dynamic>,
        /// Copy of the value.
        value: Dynamic,
    },
    /// The target is a temporary [`Dynamic`] value (i.e. its mutation can cause no side effects).
    TempValue(Dynamic),
    /// The target is a bit inside an [`INT`][crate::INT].
    /// This is necessary because directly pointing to a bit inside an [`INT`][crate::INT] is impossible.
    #[cfg(not(feature = "no_index"))]
    Bit {
        /// Mutable reference to the source [`Dynamic`].
        source: &'a mut Dynamic,
        /// Copy of the boolean bit, as a [`Dynamic`].
        value: Dynamic,
        /// Bit offset.
        bit: u8,
    },
    /// The target is a range of bits inside an [`INT`][crate::INT].
    /// This is necessary because directly pointing to a range of bits inside an [`INT`][crate::INT] is impossible.
    #[cfg(not(feature = "no_index"))]
    BitField {
        /// Mutable reference to the source [`Dynamic`].
        source: &'a mut Dynamic,
        /// Copy of the integer value of the bits, as a [`Dynamic`].
        value: Dynamic,
        /// Bitmask to apply to the source value (i.e. shifted)
        mask: crate::INT,
        /// Number of bits to right-shift the source value.
        shift: u8,
    },
    /// The target is a byte inside a [`Blob`][crate::Blob].
    /// This is necessary because directly pointing to a byte (in [`Dynamic`] form) inside a blob is impossible.
    #[cfg(not(feature = "no_index"))]
    BlobByte {
        /// Mutable reference to the source [`Dynamic`].
        source: &'a mut Dynamic,
        /// Copy of the byte at the index, as a [`Dynamic`].
        value: Dynamic,
        /// Offset index.
        index: usize,
    },
    /// The target is a character inside a string.
    /// This is necessary because directly pointing to a char inside a String is impossible.
    #[cfg(not(feature = "no_index"))]
    StringChar {
        /// Mutable reference to the source [`Dynamic`].
        source: &'a mut Dynamic,
        /// Copy of the character at the offset, as a [`Dynamic`].
        value: Dynamic,
        /// Offset index.
        index: usize,
    },
}

impl<'a> Target<'a> {
    /// Is the [`Target`] a reference pointing to other data?
    #[allow(dead_code)]
    #[inline]
    #[must_use]
    pub const fn is_ref(&self) -> bool {
        match self {
            Self::RefMut(..) => true,
            #[cfg(not(feature = "no_closure"))]
            Self::SharedValue { .. } => true,
            Self::TempValue(..) => false,
            #[cfg(not(feature = "no_index"))]
            Self::Bit { .. }
            | Self::BitField { .. }
            | Self::BlobByte { .. }
            | Self::StringChar { .. } => false,
        }
    }
    /// Is the [`Target`] a temp value?
    #[inline]
    #[must_use]
    pub const fn is_temp_value(&self) -> bool {
        match self {
            Self::RefMut(..) => false,
            #[cfg(not(feature = "no_closure"))]
            Self::SharedValue { .. } => false,
            Self::TempValue(..) => true,
            #[cfg(not(feature = "no_index"))]
            Self::Bit { .. }
            | Self::BitField { .. }
            | Self::BlobByte { .. }
            | Self::StringChar { .. } => false,
        }
    }
    /// Is the [`Target`] a shared value?
    #[cfg(not(feature = "no_closure"))]
    #[inline]
    #[must_use]
    pub fn is_shared(&self) -> bool {
        match self {
            Self::RefMut(r) => r.is_shared(),
            #[cfg(not(feature = "no_closure"))]
            Self::SharedValue { .. } => true,
            Self::TempValue(r) => r.is_shared(),
            #[cfg(not(feature = "no_index"))]
            Self::Bit { .. }
            | Self::BitField { .. }
            | Self::BlobByte { .. }
            | Self::StringChar { .. } => false,
        }
    }
    /// Is the [`Target`] a specific type?
    #[allow(dead_code)]
    #[inline]
    #[must_use]
    pub fn is<T: Variant + Clone>(&self) -> bool {
        #[allow(unused_imports)]
        use core::any::TypeId;

        match self {
            Self::RefMut(r) => r.is::<T>(),
            #[cfg(not(feature = "no_closure"))]
            Self::SharedValue { source, .. } => source.is::<T>(),
            Self::TempValue(r) => r.is::<T>(),
            #[cfg(not(feature = "no_index"))]
            Self::Bit { .. } => TypeId::of::<T>() == TypeId::of::<bool>(),
            #[cfg(not(feature = "no_index"))]
            Self::BitField { .. } => TypeId::of::<T>() == TypeId::of::<crate::INT>(),
            #[cfg(not(feature = "no_index"))]
            Self::BlobByte { .. } => TypeId::of::<T>() == TypeId::of::<crate::Blob>(),
            #[cfg(not(feature = "no_index"))]
            Self::StringChar { .. } => TypeId::of::<T>() == TypeId::of::<char>(),
        }
    }
    /// Get the value of the [`Target`] as a [`Dynamic`], cloning a referenced value if necessary.
    #[inline]
    #[must_use]
    pub fn take_or_clone(self) -> Dynamic {
        match self {
            Self::RefMut(r) => r.clone(), // Referenced value is cloned
            #[cfg(not(feature = "no_closure"))]
            Self::SharedValue { value, .. } => value, // Original shared value is simply taken
            Self::TempValue(v) => v,      // Owned value is simply taken
            #[cfg(not(feature = "no_index"))]
            Self::Bit { value, .. } => value, // boolean is taken
            #[cfg(not(feature = "no_index"))]
            Self::BitField { value, .. } => value, // INT is taken
            #[cfg(not(feature = "no_index"))]
            Self::BlobByte { value, .. } => value, // byte is taken
            #[cfg(not(feature = "no_index"))]
            Self::StringChar { value, .. } => value, // char is taken
        }
    }
    /// Take a `&mut Dynamic` reference from the `Target`.
    #[inline(always)]
    #[must_use]
    pub fn take_ref(self) -> Option<&'a mut Dynamic> {
        match self {
            Self::RefMut(r) => Some(r),
            _ => None,
        }
    }
    /// Convert a shared or reference [`Target`] into a target with an owned value.
    #[inline(always)]
    #[must_use]
    pub fn into_owned(self) -> Self {
        match self {
            Self::RefMut(r) => Self::TempValue(r.clone()),
            #[cfg(not(feature = "no_closure"))]
            Self::SharedValue { value, .. } => Self::TempValue(value),
            _ => self,
        }
    }
    /// Get the source [`Dynamic`] of the [`Target`].
    #[allow(dead_code)]
    #[inline]
    #[must_use]
    pub fn source(&self) -> &Dynamic {
        match self {
            Self::RefMut(r) => *r,
            #[cfg(not(feature = "no_closure"))]
            Self::SharedValue { source, .. } => source,
            Self::TempValue(v) => v,
            #[cfg(not(feature = "no_index"))]
            Self::Bit { source, .. } => source,
            #[cfg(not(feature = "no_index"))]
            Self::BitField { source, .. } => source,
            #[cfg(not(feature = "no_index"))]
            Self::BlobByte { source, .. } => source,
            #[cfg(not(feature = "no_index"))]
            Self::StringChar { source, .. } => source,
        }
    }
    /// Propagate a changed value back to the original source.
    /// This has no effect for direct references.
    ///
    /// [`Position`] in [`EvalAltResult`] is [`NONE`][Position::NONE] and should be set afterwards.
    #[inline]
    pub fn propagate_changed_value(&mut self) -> RhaiResultOf<()> {
        match self {
            Self::RefMut(..) | Self::TempValue(..) => (),
            #[cfg(not(feature = "no_closure"))]
            Self::SharedValue { .. } => (),
            #[cfg(not(feature = "no_index"))]
            Self::Bit { source, value, bit } => {
                // Replace the bit at the specified index position
                let new_bit = value.as_bool().map_err(|err| {
                    Box::new(crate::ERR::ErrorMismatchDataType(
                        "bool".to_string(),
                        err.to_string(),
                        Position::NONE,
                    ))
                })?;

                let value = &mut *source.write_lock::<crate::INT>().expect("`INT`");

                let index = *bit;

                let mask = 1 << index;
                if new_bit {
                    *value |= mask;
                } else {
                    *value &= !mask;
                }
            }
            #[cfg(not(feature = "no_index"))]
            Self::BitField {
                source,
                value,
                mask,
                shift,
            } => {
                let shift = *shift;
                let mask = *mask;

                // Replace the bit at the specified index position
                let new_value = value.as_int().map_err(|err| {
                    Box::new(crate::ERR::ErrorMismatchDataType(
                        "integer".to_string(),
                        err.to_string(),
                        Position::NONE,
                    ))
                })?;

                let new_value = (new_value << shift) & mask;
                let value = &mut *source.write_lock::<crate::INT>().expect("`INT`");

                *value &= !mask;
                *value |= new_value;
            }
            #[cfg(not(feature = "no_index"))]
            Self::BlobByte {
                source,
                value,
                index,
            } => {
                // Replace the byte at the specified index position
                let new_byte = value.as_int().map_err(|err| {
                    Box::new(crate::ERR::ErrorMismatchDataType(
                        "INT".to_string(),
                        err.to_string(),
                        Position::NONE,
                    ))
                })?;

                let value = &mut *source.write_lock::<Blob>().expect("`Blob`");

                let index = *index;

                if index < value.len() {
                    value[index] = (new_byte & 0x00ff) as u8;
                } else {
                    unreachable!("blob index out of bounds: {}", index);
                }
            }
            #[cfg(not(feature = "no_index"))]
            Self::StringChar {
                source,
                value,
                index,
            } => {
                // Replace the character at the specified index position
                let new_ch = value.as_char().map_err(|err| {
                    Box::new(crate::ERR::ErrorMismatchDataType(
                        "char".to_string(),
                        err.to_string(),
                        Position::NONE,
                    ))
                })?;

                let s = &mut *source
                    .write_lock::<ImmutableString>()
                    .expect("`ImmutableString`");

                let index = *index;

                *s = s
                    .chars()
                    .enumerate()
                    .map(|(i, ch)| if i == index { new_ch } else { ch })
                    .collect();
            }
        }

        Ok(())
    }
}

impl<'a> From<&'a mut Dynamic> for Target<'a> {
    #[inline]
    fn from(value: &'a mut Dynamic) -> Self {
        #[cfg(not(feature = "no_closure"))]
        if value.is_shared() {
            // Cloning is cheap for a shared value
            let val = value.clone();
            let source = value.write_lock::<Dynamic>().expect("`Dynamic`");
            return Self::SharedValue { source, value: val };
        }

        Self::RefMut(value)
    }
}

impl Deref for Target<'_> {
    type Target = Dynamic;

    #[inline]
    fn deref(&self) -> &Dynamic {
        match self {
            Self::RefMut(r) => *r,
            #[cfg(not(feature = "no_closure"))]
            Self::SharedValue { source, .. } => &**source,
            Self::TempValue(ref r) => r,
            #[cfg(not(feature = "no_index"))]
            Self::Bit { ref value, .. }
            | Self::BitField { ref value, .. }
            | Self::BlobByte { ref value, .. }
            | Self::StringChar { ref value, .. } => value,
        }
    }
}

impl AsRef<Dynamic> for Target<'_> {
    #[inline(always)]
    fn as_ref(&self) -> &Dynamic {
        self
    }
}

impl DerefMut for Target<'_> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Dynamic {
        match self {
            Self::RefMut(r) => *r,
            #[cfg(not(feature = "no_closure"))]
            Self::SharedValue { source, .. } => &mut *source,
            Self::TempValue(ref mut r) => r,
            #[cfg(not(feature = "no_index"))]
            Self::Bit { ref mut value, .. }
            | Self::BitField { ref mut value, .. }
            | Self::BlobByte { ref mut value, .. }
            | Self::StringChar { ref mut value, .. } => value,
        }
    }
}

impl AsMut<Dynamic> for Target<'_> {
    #[inline(always)]
    fn as_mut(&mut self) -> &mut Dynamic {
        self
    }
}

impl<T: Into<Dynamic>> From<T> for Target<'_> {
    #[inline(always)]
    #[must_use]
    fn from(value: T) -> Self {
        Self::TempValue(value.into())
    }
}
