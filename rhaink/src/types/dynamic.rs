//! Helper module which defines the [`Dynamic`] data type and the
//! [`Any`] trait to to allow custom type handling.

use crate::func::native::SendSync;
use crate::{reify, ExclusiveRange, FnPtr, ImmutableString, InclusiveRange, INT};
use core::{
    any::{type_name, Any, TypeId},
    convert::From,
    fmt,
    hash::{Hash, Hasher},
    mem,
    ops::{Deref, DerefMut},
    str::FromStr,
};

use ink_prelude::boxed::Box;
use ink_prelude::string::String;

pub use instant::Instant;

/// The message: data type was checked
const CHECKED: &str = "data type was checked";

mod private {
    use crate::func::native::SendSync;
    use std::any::Any;

    /// A sealed trait that prevents other crates from implementing [`Variant`].
    pub trait Sealed {}

    impl<T: Any + Clone + SendSync> Sealed for T {}
}

/// _(internals)_ Trait to represent any type.
/// Exported under the `internals` feature only.
///
/// This trait is sealed and cannot be implemented.
///
/// Currently, [`Variant`] is not [`Send`] nor [`Sync`], so it can practically be any type.
/// Turn on the `sync` feature to restrict it to only types that implement [`Send`] `+` [`Sync`].
#[cfg(not(feature = "sync"))]
pub trait Variant: Any + private::Sealed {
    /// Convert this [`Variant`] trait object to [`&dyn Any`][Any].
    #[must_use]
    fn as_any(&self) -> &dyn Any;

    /// Convert this [`Variant`] trait object to [`&mut dyn Any`][Any].
    #[must_use]
    fn as_any_mut(&mut self) -> &mut dyn Any;

    /// Convert this [`Variant`] trait object to [`Box<dyn Any>`].
    #[must_use]
    fn as_boxed_any(self: Box<Self>) -> Box<dyn Any>;

    /// Get the name of this type.
    #[must_use]
    fn type_name(&self) -> &'static str;

    /// Clone this [`Variant`] trait object.
    #[must_use]
    fn clone_object(&self) -> Box<dyn Variant>;
}

/// _(internals)_ Trait to represent any type.
/// Exported under the `internals` feature only.
///
/// This trait is sealed and cannot be implemented.
#[cfg(feature = "sync")]
pub trait Variant: Any + Send + Sync + private::Sealed {
    /// Convert this [`Variant`] trait object to [`&dyn Any`][Any].
    #[must_use]
    fn as_any(&self) -> &dyn Any;

    /// Convert this [`Variant`] trait object to [`&mut dyn Any`][Any].
    #[must_use]
    fn as_any_mut(&mut self) -> &mut dyn Any;

    /// Convert this [`Variant`] trait object to [`Box<dyn Any>`].
    #[must_use]
    fn as_boxed_any(self: Box<Self>) -> Box<dyn Any>;

    /// Get the name of this type.
    #[must_use]
    fn type_name(&self) -> &'static str;

    /// Clone this [`Variant`] trait object.
    #[must_use]
    fn clone_object(&self) -> Box<dyn Variant>;
}

impl<T: Any + Clone + SendSync> Variant for T {
    #[inline(always)]
    fn as_any(&self) -> &dyn Any {
        self
    }
    #[inline(always)]
    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
    #[inline(always)]
    fn as_boxed_any(self: Box<Self>) -> Box<dyn Any> {
        self
    }
    #[inline(always)]
    fn type_name(&self) -> &'static str {
        type_name::<T>()
    }
    #[inline(always)]
    fn clone_object(&self) -> Box<dyn Variant> {
        Box::new(self.clone()) as Box<dyn Variant>
    }
}

impl dyn Variant {
    /// Is this [`Variant`] a specific type?
    #[inline(always)]
    #[must_use]
    pub fn is<T: Any>(&self) -> bool {
        TypeId::of::<T>() == self.type_id()
    }
}

/// _(internals)_ Modes of access.
/// Exported under the `internals` feature only.
#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
#[non_exhaustive]
pub enum AccessMode {
    /// Mutable.
    ReadWrite,
    /// Immutable.
    ReadOnly,
}

/// Arbitrary data attached to a [`Dynamic`] value.
#[cfg(target_pointer_width = "64")]
pub type Tag = i32;

/// Arbitrary data attached to a [`Dynamic`] value.
#[cfg(target_pointer_width = "32")]
pub type Tag = i16;

/// Default tag value for [`Dynamic`].
const DEFAULT_TAG_VALUE: Tag = 0;

/// Dynamic type containing any value.
pub struct Dynamic(pub(crate) Union);

/// Internal [`Dynamic`] representation.
///
/// Most variants are boxed to reduce the size.
pub enum Union {
    /// The Unit value - ().
    Unit((), Tag, AccessMode),
    /// A boolean value.
    Bool(bool, Tag, AccessMode),
    /// An [`ImmutableString`] value.
    Str(ImmutableString, Tag, AccessMode),
    /// A character value.
    Char(char, Tag, AccessMode),
    /// An integer value.
    Int(INT, Tag, AccessMode),
    /// A floating-point value.
    #[cfg(not(feature = "no_float"))]
    Float(crate::ast::FloatWrapper<crate::FLOAT>, Tag, AccessMode),
    /// _(decimal)_ A fixed-precision decimal value.
    /// Exported under the `decimal` feature only.
    #[cfg(feature = "decimal")]
    Decimal(Box<rust_decimal::Decimal>, Tag, AccessMode),
    /// An array value.
    #[cfg(not(feature = "no_index"))]
    Array(Box<crate::Array>, Tag, AccessMode),
    /// An blob (byte array).
    #[cfg(not(feature = "no_index"))]
    Blob(Box<crate::Blob>, Tag, AccessMode),
    /// An object map value.
    #[cfg(not(feature = "no_object"))]
    Map(Box<crate::Map>, Tag, AccessMode),
    /// A function pointer.
    FnPtr(Box<FnPtr>, Tag, AccessMode),
    /// A timestamp value.
    #[cfg(not(feature = "no_std"))]
    TimeStamp(Box<Instant>, Tag, AccessMode),

    /// Any type as a trait object.
    #[allow(clippy::redundant_allocation)]
    Variant(Box<Box<dyn Variant>>, Tag, AccessMode),

    /// A _shared_ value of any type.
    #[cfg(not(feature = "no_closure"))]
    Shared(crate::Shared<crate::Locked<Dynamic>>, Tag, AccessMode),
}

/// _(internals)_ Lock guard for reading a [`Dynamic`].
/// Exported under the `internals` feature only.
///
/// This type provides transparent interoperability between normal [`Dynamic`] and shared
/// [`Dynamic`] values.
#[derive(Debug)]
pub struct DynamicReadLock<'d, T: Clone>(DynamicReadLockInner<'d, T>);

/// Different types of read guards for [`DynamicReadLock`].
#[derive(Debug)]
enum DynamicReadLockInner<'d, T: Clone> {
    /// A simple reference to a non-shared value.
    Reference(&'d T),

    /// A read guard to a shared value.
    #[cfg(not(feature = "no_closure"))]
    Guard(crate::func::native::LockGuard<'d, Dynamic>),
}

impl<'d, T: Any + Clone> Deref for DynamicReadLock<'d, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        match self.0 {
            DynamicReadLockInner::Reference(ref reference) => *reference,
            #[cfg(not(feature = "no_closure"))]
            DynamicReadLockInner::Guard(ref guard) => guard.downcast_ref().expect(CHECKED),
        }
    }
}

/// _(internals)_ Lock guard for writing a [`Dynamic`].
/// Exported under the `internals` feature only.
///
/// This type provides transparent interoperability between normal [`Dynamic`] and shared
/// [`Dynamic`] values.
#[derive(Debug)]
pub struct DynamicWriteLock<'d, T: Clone>(DynamicWriteLockInner<'d, T>);

/// Different types of write guards for [`DynamicReadLock`].
#[derive(Debug)]
enum DynamicWriteLockInner<'d, T: Clone> {
    /// A simple mutable reference to a non-shared value.
    Reference(&'d mut T),

    /// A write guard to a shared value.
    #[cfg(not(feature = "no_closure"))]
    Guard(crate::func::native::LockGuardMut<'d, Dynamic>),
}

impl<'d, T: Any + Clone> Deref for DynamicWriteLock<'d, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        match self.0 {
            DynamicWriteLockInner::Reference(ref reference) => *reference,
            #[cfg(not(feature = "no_closure"))]
            DynamicWriteLockInner::Guard(ref guard) => guard.downcast_ref().expect(CHECKED),
        }
    }
}

impl<'d, T: Any + Clone> DerefMut for DynamicWriteLock<'d, T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self.0 {
            DynamicWriteLockInner::Reference(ref mut reference) => *reference,
            #[cfg(not(feature = "no_closure"))]
            DynamicWriteLockInner::Guard(ref mut guard) => guard.downcast_mut().expect(CHECKED),
        }
    }
}

impl Dynamic {
    /// Get the arbitrary data attached to this [`Dynamic`].
    #[must_use]
    pub const fn tag(&self) -> Tag {
        match self.0 {
            Union::Unit(_, tag, _)
            | Union::Bool(_, tag, _)
            | Union::Str(_, tag, _)
            | Union::Char(_, tag, _)
            | Union::Int(_, tag, _)
            | Union::FnPtr(_, tag, _)
            | Union::Variant(_, tag, _) => tag,

            #[cfg(not(feature = "no_float"))]
            Union::Float(_, tag, _) => tag,
            #[cfg(feature = "decimal")]
            Union::Decimal(_, tag, _) => tag,
            #[cfg(not(feature = "no_index"))]
            Union::Array(_, tag, _) | Union::Blob(_, tag, _) => tag,
            #[cfg(not(feature = "no_object"))]
            Union::Map(_, tag, _) => tag,
            #[cfg(not(feature = "no_std"))]
            Union::TimeStamp(_, tag, _) => tag,
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(_, tag, _) => tag,
        }
    }
    /// Attach arbitrary data to this [`Dynamic`].
    pub fn set_tag(&mut self, value: Tag) -> &mut Self {
        match self.0 {
            Union::Unit(_, ref mut tag, _)
            | Union::Bool(_, ref mut tag, _)
            | Union::Str(_, ref mut tag, _)
            | Union::Char(_, ref mut tag, _)
            | Union::Int(_, ref mut tag, _)
            | Union::FnPtr(_, ref mut tag, _)
            | Union::Variant(_, ref mut tag, _) => *tag = value,

            #[cfg(not(feature = "no_float"))]
            Union::Float(_, ref mut tag, _) => *tag = value,
            #[cfg(feature = "decimal")]
            Union::Decimal(_, ref mut tag, _) => *tag = value,
            #[cfg(not(feature = "no_index"))]
            Union::Array(_, ref mut tag, _) | Union::Blob(_, ref mut tag, _) => *tag = value,
            #[cfg(not(feature = "no_object"))]
            Union::Map(_, ref mut tag, _) => *tag = value,
            #[cfg(not(feature = "no_std"))]
            Union::TimeStamp(_, ref mut tag, _) => *tag = value,
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(_, ref mut tag, _) => *tag = value,
        }
        self
    }
    /// Does this [`Dynamic`] hold a variant data type instead of one of the supported system
    /// primitive types?
    #[inline(always)]
    #[must_use]
    pub const fn is_variant(&self) -> bool {
        matches!(self.0, Union::Variant(..))
    }
    /// Is the value held by this [`Dynamic`] shared?
    ///
    /// Not available under `no_closure`.
    #[cfg(not(feature = "no_closure"))]
    #[inline(always)]
    #[must_use]
    pub const fn is_shared(&self) -> bool {
        #[cfg(not(feature = "no_closure"))]
        return matches!(self.0, Union::Shared(..));
        #[cfg(feature = "no_closure")]
        return false;
    }
    /// Is the value held by this [`Dynamic`] a particular type?
    ///
    /// If the [`Dynamic`] is a shared variant checking is performed on top of its internal value.
    #[inline]
    #[must_use]
    pub fn is<T: Any + Clone>(&self) -> bool {
        if TypeId::of::<T>() == TypeId::of::<String>() {
            self.type_id() == TypeId::of::<ImmutableString>()
        } else {
            self.type_id() == TypeId::of::<T>()
        }
    }
    /// Get the [`TypeId`] of the value held by this [`Dynamic`].
    ///
    /// # Panics or Deadlocks When Value is Shared
    ///
    /// Under the `sync` feature, this call may deadlock, or [panic](https://doc.rust-lang.org/std/sync/struct.RwLock.html#panics-1).
    /// Otherwise, this call panics if the data is currently borrowed for write.
    #[must_use]
    pub fn type_id(&self) -> TypeId {
        match self.0 {
            Union::Unit(..) => TypeId::of::<()>(),
            Union::Bool(..) => TypeId::of::<bool>(),
            Union::Str(..) => TypeId::of::<ImmutableString>(),
            Union::Char(..) => TypeId::of::<char>(),
            Union::Int(..) => TypeId::of::<INT>(),
            #[cfg(not(feature = "no_float"))]
            Union::Float(..) => TypeId::of::<crate::FLOAT>(),
            #[cfg(feature = "decimal")]
            Union::Decimal(..) => TypeId::of::<rust_decimal::Decimal>(),
            #[cfg(not(feature = "no_index"))]
            Union::Array(..) => TypeId::of::<crate::Array>(),
            #[cfg(not(feature = "no_index"))]
            Union::Blob(..) => TypeId::of::<crate::Blob>(),
            #[cfg(not(feature = "no_object"))]
            Union::Map(..) => TypeId::of::<crate::Map>(),
            Union::FnPtr(..) => TypeId::of::<FnPtr>(),
            #[cfg(not(feature = "no_std"))]
            Union::TimeStamp(..) => TypeId::of::<Instant>(),

            Union::Variant(ref v, ..) => (***v).type_id(),

            #[cfg(not(feature = "no_closure"))]
            #[cfg(not(feature = "sync"))]
            Union::Shared(ref cell, ..) => (*cell.borrow()).type_id(),

            #[cfg(not(feature = "no_closure"))]
            #[cfg(feature = "sync")]
            Union::Shared(ref cell, ..) => (*cell.read().unwrap()).type_id(),
        }
    }
    /// Get the name of the type of the value held by this [`Dynamic`].
    ///
    /// # Panics or Deadlocks When Value is Shared
    ///
    /// Under the `sync` feature, this call may deadlock, or [panic](https://doc.rust-lang.org/std/sync/struct.RwLock.html#panics-1).
    /// Otherwise, this call panics if the data is currently borrowed for write.
    #[must_use]
    pub fn type_name(&self) -> &'static str {
        match self.0 {
            Union::Unit(..) => "()",
            Union::Bool(..) => "bool",
            Union::Str(..) => "string",
            Union::Char(..) => "char",
            Union::Int(..) => type_name::<INT>(),
            #[cfg(not(feature = "no_float"))]
            Union::Float(..) => type_name::<crate::FLOAT>(),
            #[cfg(feature = "decimal")]
            Union::Decimal(..) => "decimal",
            #[cfg(not(feature = "no_index"))]
            Union::Array(..) => "array",
            #[cfg(not(feature = "no_index"))]
            Union::Blob(..) => "blob",
            #[cfg(not(feature = "no_object"))]
            Union::Map(..) => "map",
            Union::FnPtr(..) => "Fn",
            #[cfg(not(feature = "no_std"))]
            Union::TimeStamp(..) => "timestamp",

            Union::Variant(ref v, ..) => (***v).type_name(),

            #[cfg(not(feature = "no_closure"))]
            #[cfg(not(feature = "sync"))]
            Union::Shared(ref cell, ..) => cell
                .try_borrow()
                .map(|v| (*v).type_name())
                .unwrap_or("<shared>"),
            #[cfg(not(feature = "no_closure"))]
            #[cfg(feature = "sync")]
            Union::Shared(ref cell, ..) => (*cell.read().unwrap()).type_name(),
        }
    }
}

impl Hash for Dynamic {
    /// Hash the [`Dynamic`] value.
    ///
    /// # Panics
    ///
    /// Panics if the [`Dynamic`] value contains an unrecognized trait object.
    fn hash<H: Hasher>(&self, state: &mut H) {
        mem::discriminant(&self.0).hash(state);

        match self.0 {
            Union::Unit(..) => ().hash(state),
            Union::Bool(ref b, ..) => b.hash(state),
            Union::Str(ref s, ..) => s.hash(state),
            Union::Char(ref c, ..) => c.hash(state),
            Union::Int(ref i, ..) => i.hash(state),
            #[cfg(not(feature = "no_float"))]
            Union::Float(ref f, ..) => f.hash(state),
            #[cfg(feature = "decimal")]
            Union::Decimal(ref d, ..) => d.hash(state),
            #[cfg(not(feature = "no_index"))]
            Union::Array(ref a, ..) => a.as_ref().hash(state),
            #[cfg(not(feature = "no_index"))]
            Union::Blob(ref a, ..) => a.as_ref().hash(state),
            #[cfg(not(feature = "no_object"))]
            Union::Map(ref m, ..) => m.as_ref().hash(state),
            Union::FnPtr(ref f, ..) => f.hash(state),

            #[cfg(not(feature = "no_closure"))]
            #[cfg(not(feature = "sync"))]
            Union::Shared(ref cell, ..) => (*cell.borrow()).hash(state),

            #[cfg(not(feature = "no_closure"))]
            #[cfg(feature = "sync")]
            Union::Shared(ref cell, ..) => (*cell.read().unwrap()).hash(state),

            Union::Variant(ref _v, ..) => {
                #[cfg(not(feature = "only_i32"))]
                #[cfg(not(feature = "only_i64"))]
                {
                    let value_any = (***_v).as_any();
                    let type_id = value_any.type_id();

                    if type_id == TypeId::of::<u8>() {
                        TypeId::of::<u8>().hash(state);
                        value_any.downcast_ref::<u8>().expect(CHECKED).hash(state);
                    } else if type_id == TypeId::of::<u16>() {
                        TypeId::of::<u16>().hash(state);
                        value_any.downcast_ref::<u16>().expect(CHECKED).hash(state);
                    } else if type_id == TypeId::of::<u32>() {
                        TypeId::of::<u32>().hash(state);
                        value_any.downcast_ref::<u32>().expect(CHECKED).hash(state);
                    } else if type_id == TypeId::of::<u64>() {
                        TypeId::of::<u64>().hash(state);
                        value_any.downcast_ref::<u64>().expect(CHECKED).hash(state);
                    } else if type_id == TypeId::of::<i8>() {
                        TypeId::of::<i8>().hash(state);
                        value_any.downcast_ref::<i8>().expect(CHECKED).hash(state);
                    } else if type_id == TypeId::of::<i16>() {
                        TypeId::of::<i16>().hash(state);
                        value_any.downcast_ref::<i16>().expect(CHECKED).hash(state);
                    } else if type_id == TypeId::of::<i32>() {
                        TypeId::of::<i32>().hash(state);
                        value_any.downcast_ref::<i32>().expect(CHECKED).hash(state);
                    } else if type_id == TypeId::of::<i64>() {
                        TypeId::of::<i64>().hash(state);
                        value_any.downcast_ref::<i64>().expect(CHECKED).hash(state);
                    }

                    #[cfg(not(target_family = "wasm"))]
                    if type_id == TypeId::of::<u128>() {
                        TypeId::of::<u128>().hash(state);
                        value_any.downcast_ref::<u128>().expect(CHECKED).hash(state);
                    } else if type_id == TypeId::of::<i128>() {
                        TypeId::of::<i128>().hash(state);
                        value_any.downcast_ref::<i128>().expect(CHECKED).hash(state);
                    }
                }

                unimplemented!("a custom type cannot be hashed")
            }

            #[cfg(not(feature = "no_std"))]
            Union::TimeStamp(..) => unimplemented!("{} cannot be hashed", self.type_name()),
        }
    }
}

impl fmt::Display for Dynamic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            Union::Unit(..) => write!(f, ""),
            Union::Bool(ref v, ..) => fmt::Display::fmt(v, f),
            Union::Str(ref v, ..) => fmt::Display::fmt(v, f),
            Union::Char(ref v, ..) => fmt::Display::fmt(v, f),
            Union::Int(ref v, ..) => fmt::Display::fmt(v, f),
            #[cfg(not(feature = "no_float"))]
            Union::Float(ref v, ..) => fmt::Display::fmt(v, f),
            #[cfg(feature = "decimal")]
            Union::Decimal(ref v, ..) => fmt::Display::fmt(v, f),
            #[cfg(not(feature = "no_index"))]
            Union::Array(..) => fmt::Debug::fmt(self, f),
            #[cfg(not(feature = "no_index"))]
            Union::Blob(..) => fmt::Debug::fmt(self, f),
            #[cfg(not(feature = "no_object"))]
            Union::Map(..) => fmt::Debug::fmt(self, f),
            Union::FnPtr(ref v, ..) => fmt::Display::fmt(v, f),
            #[cfg(not(feature = "no_std"))]
            Union::TimeStamp(..) => f.write_str("<timestamp>"),

            Union::Variant(ref v, ..) => {
                let _value_any = (***v).as_any();
                let _type_id = _value_any.type_id();

                #[cfg(not(feature = "only_i32"))]
                #[cfg(not(feature = "only_i64"))]
                if _type_id == TypeId::of::<u8>() {
                    return fmt::Display::fmt(_value_any.downcast_ref::<u8>().expect(CHECKED), f);
                } else if _type_id == TypeId::of::<u16>() {
                    return fmt::Display::fmt(_value_any.downcast_ref::<u16>().expect(CHECKED), f);
                } else if _type_id == TypeId::of::<u32>() {
                    return fmt::Display::fmt(_value_any.downcast_ref::<u32>().expect(CHECKED), f);
                } else if _type_id == TypeId::of::<u64>() {
                    return fmt::Display::fmt(_value_any.downcast_ref::<u64>().expect(CHECKED), f);
                } else if _type_id == TypeId::of::<i8>() {
                    return fmt::Display::fmt(_value_any.downcast_ref::<i8>().expect(CHECKED), f);
                } else if _type_id == TypeId::of::<i16>() {
                    return fmt::Display::fmt(_value_any.downcast_ref::<i16>().expect(CHECKED), f);
                } else if _type_id == TypeId::of::<i32>() {
                    return fmt::Display::fmt(_value_any.downcast_ref::<i32>().expect(CHECKED), f);
                } else if _type_id == TypeId::of::<i64>() {
                    return fmt::Display::fmt(_value_any.downcast_ref::<i64>().expect(CHECKED), f);
                }

                #[cfg(not(feature = "no_float"))]
                #[cfg(not(feature = "f32_float"))]
                if _type_id == TypeId::of::<f32>() {
                    return fmt::Display::fmt(_value_any.downcast_ref::<f32>().expect(CHECKED), f);
                }
                #[cfg(not(feature = "no_float"))]
                #[cfg(feature = "f32_float")]
                if _type_id == TypeId::of::<f64>() {
                    return fmt::Display::fmt(_value_any.downcast_ref::<f64>().expect(CHECKED), f);
                }

                #[cfg(not(feature = "only_i32"))]
                #[cfg(not(feature = "only_i64"))]
                #[cfg(not(target_family = "wasm"))]
                if _type_id == TypeId::of::<u128>() {
                    return fmt::Display::fmt(_value_any.downcast_ref::<u128>().expect(CHECKED), f);
                } else if _type_id == TypeId::of::<i128>() {
                    return fmt::Display::fmt(_value_any.downcast_ref::<i128>().expect(CHECKED), f);
                }

                if _type_id == TypeId::of::<ExclusiveRange>() {
                    let range = _value_any.downcast_ref::<ExclusiveRange>().expect(CHECKED);
                    return write!(f, "{}..{}", range.start, range.end);
                } else if _type_id == TypeId::of::<InclusiveRange>() {
                    let range = _value_any.downcast_ref::<InclusiveRange>().expect(CHECKED);
                    return write!(f, "{}..={}", range.start(), range.end());
                }

                f.write_str((***v).type_name())
            }

            #[cfg(not(feature = "no_closure"))]
            #[cfg(not(feature = "sync"))]
            Union::Shared(ref cell, ..) => {
                if let Ok(v) = cell.try_borrow() {
                    fmt::Display::fmt(&*v, f)
                } else {
                    f.write_str("<shared>")
                }
            }
            #[cfg(not(feature = "no_closure"))]
            #[cfg(feature = "sync")]
            Union::Shared(ref cell, ..) => fmt::Display::fmt(&*cell.read().unwrap(), f),
        }
    }
}

impl fmt::Debug for Dynamic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            Union::Unit(ref v, ..) => fmt::Debug::fmt(v, f),
            Union::Bool(ref v, ..) => fmt::Debug::fmt(v, f),
            Union::Str(ref v, ..) => fmt::Debug::fmt(v, f),
            Union::Char(ref v, ..) => fmt::Debug::fmt(v, f),
            Union::Int(ref v, ..) => fmt::Debug::fmt(v, f),
            #[cfg(not(feature = "no_float"))]
            Union::Float(ref v, ..) => fmt::Debug::fmt(v, f),
            #[cfg(feature = "decimal")]
            Union::Decimal(ref v, ..) => fmt::Debug::fmt(v, f),
            #[cfg(not(feature = "no_index"))]
            Union::Array(ref v, ..) => fmt::Debug::fmt(v, f),
            #[cfg(not(feature = "no_index"))]
            Union::Blob(ref v, ..) => {
                f.write_str("[")?;
                v.iter().enumerate().try_for_each(|(i, v)| {
                    if i > 0 && i % 8 == 0 {
                        f.write_str(" ")?;
                    }
                    write!(f, "{:02x}", v)
                })?;
                f.write_str("]")
            }
            #[cfg(not(feature = "no_object"))]
            Union::Map(ref v, ..) => {
                f.write_str("#")?;
                fmt::Debug::fmt(v, f)
            }
            Union::FnPtr(ref v, ..) => fmt::Debug::fmt(v, f),
            #[cfg(not(feature = "no_std"))]
            Union::TimeStamp(..) => write!(f, "<timestamp>"),

            Union::Variant(ref v, ..) => {
                let _value_any = (***v).as_any();
                let _type_id = _value_any.type_id();

                #[cfg(not(feature = "only_i32"))]
                #[cfg(not(feature = "only_i64"))]
                if _type_id == TypeId::of::<u8>() {
                    return fmt::Debug::fmt(_value_any.downcast_ref::<u8>().expect(CHECKED), f);
                } else if _type_id == TypeId::of::<u16>() {
                    return fmt::Debug::fmt(_value_any.downcast_ref::<u16>().expect(CHECKED), f);
                } else if _type_id == TypeId::of::<u32>() {
                    return fmt::Debug::fmt(_value_any.downcast_ref::<u32>().expect(CHECKED), f);
                } else if _type_id == TypeId::of::<u64>() {
                    return fmt::Debug::fmt(_value_any.downcast_ref::<u64>().expect(CHECKED), f);
                } else if _type_id == TypeId::of::<i8>() {
                    return fmt::Debug::fmt(_value_any.downcast_ref::<i8>().expect(CHECKED), f);
                } else if _type_id == TypeId::of::<i16>() {
                    return fmt::Debug::fmt(_value_any.downcast_ref::<i16>().expect(CHECKED), f);
                } else if _type_id == TypeId::of::<i32>() {
                    return fmt::Debug::fmt(_value_any.downcast_ref::<i32>().expect(CHECKED), f);
                } else if _type_id == TypeId::of::<i64>() {
                    return fmt::Debug::fmt(_value_any.downcast_ref::<i64>().expect(CHECKED), f);
                }

                #[cfg(not(feature = "no_float"))]
                #[cfg(not(feature = "f32_float"))]
                if _type_id == TypeId::of::<f32>() {
                    return fmt::Debug::fmt(_value_any.downcast_ref::<f32>().expect(CHECKED), f);
                }
                #[cfg(not(feature = "no_float"))]
                #[cfg(feature = "f32_float")]
                if _type_id == TypeId::of::<f64>() {
                    return fmt::Debug::fmt(_value_any.downcast_ref::<f64>().expect(CHECKED), f);
                }

                #[cfg(not(feature = "only_i32"))]
                #[cfg(not(feature = "only_i64"))]
                #[cfg(not(target_family = "wasm"))]
                if _type_id == TypeId::of::<u128>() {
                    return fmt::Debug::fmt(_value_any.downcast_ref::<u128>().expect(CHECKED), f);
                } else if _type_id == TypeId::of::<i128>() {
                    return fmt::Debug::fmt(_value_any.downcast_ref::<i128>().expect(CHECKED), f);
                }

                if _type_id == TypeId::of::<ExclusiveRange>() {
                    let range = _value_any.downcast_ref::<ExclusiveRange>().expect(CHECKED);
                    return write!(f, "{}..{}", range.start, range.end);
                } else if _type_id == TypeId::of::<InclusiveRange>() {
                    let range = _value_any.downcast_ref::<InclusiveRange>().expect(CHECKED);
                    return write!(f, "{}..={}", range.start(), range.end());
                }

                f.write_str((***v).type_name())
            }

            #[cfg(not(feature = "no_closure"))]
            #[cfg(not(feature = "sync"))]
            Union::Shared(ref cell, ..) => {
                if let Ok(v) = cell.try_borrow() {
                    write!(f, "{:?} (shared)", *v)
                } else {
                    f.write_str("<shared>")
                }
            }
            #[cfg(not(feature = "no_closure"))]
            #[cfg(feature = "sync")]
            Union::Shared(ref cell, ..) => fmt::Debug::fmt(&*cell.read().unwrap(), f),
        }
    }
}

use AccessMode::*;

impl Clone for Dynamic {
    /// Clone the [`Dynamic`] value.
    ///
    /// # WARNING
    ///
    /// The cloned copy is marked read-write even if the original is read-only.
    fn clone(&self) -> Self {
        match self.0 {
            Union::Unit(v, tag, ..) => Self(Union::Unit(v, tag, ReadWrite)),
            Union::Bool(v, tag, ..) => Self(Union::Bool(v, tag, ReadWrite)),
            Union::Str(ref v, tag, ..) => Self(Union::Str(v.clone(), tag, ReadWrite)),
            Union::Char(v, tag, ..) => Self(Union::Char(v, tag, ReadWrite)),
            Union::Int(v, tag, ..) => Self(Union::Int(v, tag, ReadWrite)),
            #[cfg(not(feature = "no_float"))]
            Union::Float(v, tag, ..) => Self(Union::Float(v, tag, ReadWrite)),
            #[cfg(feature = "decimal")]
            Union::Decimal(ref v, tag, ..) => Self(Union::Decimal(v.clone(), tag, ReadWrite)),
            #[cfg(not(feature = "no_index"))]
            Union::Array(ref v, tag, ..) => Self(Union::Array(v.clone(), tag, ReadWrite)),
            #[cfg(not(feature = "no_index"))]
            Union::Blob(ref v, tag, ..) => Self(Union::Blob(v.clone(), tag, ReadWrite)),
            #[cfg(not(feature = "no_object"))]
            Union::Map(ref v, tag, ..) => Self(Union::Map(v.clone(), tag, ReadWrite)),
            Union::FnPtr(ref v, tag, ..) => Self(Union::FnPtr(v.clone(), tag, ReadWrite)),
            #[cfg(not(feature = "no_std"))]
            Union::TimeStamp(ref v, tag, ..) => Self(Union::TimeStamp(v.clone(), tag, ReadWrite)),

            Union::Variant(ref v, tag, ..) => Self(Union::Variant(
                v.as_ref().as_ref().clone_object().into(),
                tag,
                ReadWrite,
            )),

            #[cfg(not(feature = "no_closure"))]
            Union::Shared(ref cell, tag, ..) => Self(Union::Shared(cell.clone(), tag, ReadWrite)),
        }
    }
}

impl Default for Dynamic {
    #[inline(always)]
    #[must_use]
    fn default() -> Self {
        Self::UNIT
    }
}

#[cfg(not(feature = "no_float"))]
#[cfg(feature = "f32_float")]
use std::f32::consts as FloatConstants;
#[cfg(not(feature = "no_float"))]
#[cfg(not(feature = "f32_float"))]
use std::f64::consts as FloatConstants;

impl Dynamic {
    /// A [`Dynamic`] containing a `()`.
    pub const UNIT: Self = Self(Union::Unit((), DEFAULT_TAG_VALUE, ReadWrite));
    /// A [`Dynamic`] containing a `true`.
    pub const TRUE: Self = Self::from_bool(true);
    /// A [`Dynamic`] containing a [`false`].
    pub const FALSE: Self = Self::from_bool(false);
    /// A [`Dynamic`] containing the integer zero.
    pub const ZERO: Self = Self::from_int(0);
    /// A [`Dynamic`] containing the integer 1.
    pub const ONE: Self = Self::from_int(1);
    /// A [`Dynamic`] containing the integer 2.
    pub const TWO: Self = Self::from_int(2);
    /// A [`Dynamic`] containing the integer 3.
    pub const THREE: Self = Self::from_int(3);
    /// A [`Dynamic`] containing the integer 10.
    pub const TEN: Self = Self::from_int(10);
    /// A [`Dynamic`] containing the integer 100.
    pub const HUNDRED: Self = Self::from_int(100);
    /// A [`Dynamic`] containing the integer 1,000.
    pub const THOUSAND: Self = Self::from_int(1000);
    /// A [`Dynamic`] containing the integer 1,000,000.
    pub const MILLION: Self = Self::from_int(1000000);
    /// A [`Dynamic`] containing the integer -1.
    pub const NEGATIVE_ONE: Self = Self::from_int(-1);
    /// A [`Dynamic`] containing the integer -2.
    pub const NEGATIVE_TWO: Self = Self::from_int(-2);
    /// A [`Dynamic`] containing `0.0`.
    ///
    /// Not available under `no_float`.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_ZERO: Self = Self::from_float(0.0);
    /// A [`Dynamic`] containing `1.0`.
    ///
    /// Not available under `no_float`.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_ONE: Self = Self::from_float(1.0);
    /// A [`Dynamic`] containing `2.0`.
    ///
    /// Not available under `no_float`.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_TWO: Self = Self::from_float(2.0);
    /// A [`Dynamic`] containing `10.0`.
    ///
    /// Not available under `no_float`.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_TEN: Self = Self::from_float(10.0);
    /// A [`Dynamic`] containing `100.0`.
    ///
    /// Not available under `no_float`.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_HUNDRED: Self = Self::from_float(100.0);
    /// A [`Dynamic`] containing `1000.0`.
    ///
    /// Not available under `no_float`.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_THOUSAND: Self = Self::from_float(1000.0);
    /// A [`Dynamic`] containing `1000000.0`.
    ///
    /// Not available under `no_float`.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_MILLION: Self = Self::from_float(1000000.0);
    /// A [`Dynamic`] containing `-1.0`.
    ///
    /// Not available under `no_float`.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_NEGATIVE_ONE: Self = Self::from_float(-1.0);
    /// A [`Dynamic`] containing `-2.0`.
    ///
    /// Not available under `no_float`.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_NEGATIVE_TWO: Self = Self::from_float(-2.0);
    /// A [`Dynamic`] containing `0.5`.
    ///
    /// Not available under `no_float`.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_HALF: Self = Self::from_float(0.5);
    /// A [`Dynamic`] containing `0.25`.
    ///
    /// Not available under `no_float`.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_QUARTER: Self = Self::from_float(0.25);
    /// A [`Dynamic`] containing `0.2`.
    ///
    /// Not available under `no_float`.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_FIFTH: Self = Self::from_float(0.2);
    /// A [`Dynamic`] containing `0.1`.
    ///
    /// Not available under `no_float`.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_TENTH: Self = Self::from_float(0.1);
    /// A [`Dynamic`] containing `0.01`.
    ///
    /// Not available under `no_float`.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_HUNDREDTH: Self = Self::from_float(0.01);
    /// A [`Dynamic`] containing `0.001`.
    ///
    /// Not available under `no_float`.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_THOUSANDTH: Self = Self::from_float(0.001);
    /// A [`Dynamic`] containing `0.000001`.
    ///
    /// Not available under `no_float`.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_MILLIONTH: Self = Self::from_float(0.000001);
    /// A [`Dynamic`] containing π.
    ///
    /// Not available under `no_float`.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_PI: Self = Self::from_float(FloatConstants::PI);
    /// A [`Dynamic`] containing π/2.
    ///
    /// Not available under `no_float`.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_HALF_PI: Self = Self::from_float(FloatConstants::FRAC_PI_2);
    /// A [`Dynamic`] containing π/4.
    ///
    /// Not available under `no_float`.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_QUARTER_PI: Self = Self::from_float(FloatConstants::FRAC_PI_4);
    /// A [`Dynamic`] containing 2π.
    ///
    /// Not available under `no_float`.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_TWO_PI: Self = Self::from_float(FloatConstants::TAU);
    /// A [`Dynamic`] containing 1/π.
    ///
    /// Not available under `no_float`.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_INVERSE_PI: Self = Self::from_float(FloatConstants::FRAC_1_PI);
    /// A [`Dynamic`] containing _e_.
    ///
    /// Not available under `no_float`.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_E: Self = Self::from_float(FloatConstants::E);
    /// A [`Dynamic`] containing `log` _e_.
    ///
    /// Not available under `no_float`.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_LOG_E: Self = Self::from_float(FloatConstants::LOG10_E);
    /// A [`Dynamic`] containing `ln 10`.
    ///
    /// Not available under `no_float`.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_LN_10: Self = Self::from_float(FloatConstants::LN_10);

    /// Create a new [`Dynamic`] from a [`bool`].
    #[inline(always)]
    pub const fn from_bool(value: bool) -> Self {
        Self(Union::Bool(value, DEFAULT_TAG_VALUE, ReadWrite))
    }
    /// Create a new [`Dynamic`] from an [`INT`].
    #[inline(always)]
    pub const fn from_int(value: INT) -> Self {
        Self(Union::Int(value, DEFAULT_TAG_VALUE, ReadWrite))
    }
    /// Create a new [`Dynamic`] from a [`char`].
    #[inline(always)]
    pub const fn from_char(value: char) -> Self {
        Self(Union::Char(value, DEFAULT_TAG_VALUE, ReadWrite))
    }
    /// Create a new [`Dynamic`] from a [`FLOAT`][crate::FLOAT].
    ///
    /// Not available under `no_float`.
    #[cfg(not(feature = "no_float"))]
    #[inline(always)]
    pub const fn from_float(value: crate::FLOAT) -> Self {
        Self(Union::Float(
            crate::ast::FloatWrapper::new_const(value),
            DEFAULT_TAG_VALUE,
            ReadWrite,
        ))
    }
    /// Create a new [`Dynamic`] from a [`Decimal`](https://docs.rs/rust_decimal).
    ///
    /// Exported under the `decimal` feature only.
    #[cfg(feature = "decimal")]
    #[inline(always)]
    pub fn from_decimal(value: rust_decimal::Decimal) -> Self {
        Self(Union::Decimal(value.into(), DEFAULT_TAG_VALUE, ReadWrite))
    }
    /// Create a [`Dynamic`] from an [`Array`][crate::Array].
    #[cfg(not(feature = "no_index"))]
    #[inline(always)]
    pub fn from_array(array: crate::Array) -> Self {
        Self(Union::Array(array.into(), DEFAULT_TAG_VALUE, ReadWrite))
    }
    /// Create a [`Dynamic`] from a [`Blob`][crate::Blob].
    #[cfg(not(feature = "no_index"))]
    #[inline(always)]
    pub fn from_blob(blob: crate::Blob) -> Self {
        Self(Union::Blob(blob.into(), DEFAULT_TAG_VALUE, ReadWrite))
    }
    /// Create a [`Dynamic`] from a [`Map`][crate::Map].
    #[cfg(not(feature = "no_object"))]
    #[inline(always)]
    pub fn from_map(map: crate::Map) -> Self {
        Self(Union::Map(map.into(), DEFAULT_TAG_VALUE, ReadWrite))
    }
    /// Create a new [`Dynamic`] from an [`Instant`].
    ///
    /// Not available under `no-std`.
    #[cfg(not(feature = "no_std"))]
    #[inline(always)]
    pub fn from_timestamp(value: Instant) -> Self {
        Self(Union::TimeStamp(value.into(), DEFAULT_TAG_VALUE, ReadWrite))
    }

    /// Get the [`AccessMode`] for this [`Dynamic`].
    #[must_use]
    pub(crate) const fn access_mode(&self) -> AccessMode {
        match self.0 {
            Union::Unit(.., access)
            | Union::Bool(.., access)
            | Union::Str(.., access)
            | Union::Char(.., access)
            | Union::Int(.., access)
            | Union::FnPtr(.., access)
            | Union::Variant(.., access) => access,

            #[cfg(not(feature = "no_float"))]
            Union::Float(.., access) => access,
            #[cfg(feature = "decimal")]
            Union::Decimal(.., access) => access,
            #[cfg(not(feature = "no_index"))]
            Union::Array(.., access) | Union::Blob(.., access) => access,
            #[cfg(not(feature = "no_object"))]
            Union::Map(.., access) => access,
            #[cfg(not(feature = "no_std"))]
            Union::TimeStamp(.., access) => access,
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(.., access) => access,
        }
    }
    /// Set the [`AccessMode`] for this [`Dynamic`].
    pub(crate) fn set_access_mode(&mut self, typ: AccessMode) -> &mut Self {
        match self.0 {
            Union::Unit(.., ref mut access)
            | Union::Bool(.., ref mut access)
            | Union::Str(.., ref mut access)
            | Union::Char(.., ref mut access)
            | Union::Int(.., ref mut access)
            | Union::FnPtr(.., ref mut access)
            | Union::Variant(.., ref mut access) => *access = typ,

            #[cfg(not(feature = "no_float"))]
            Union::Float(.., ref mut access) => *access = typ,
            #[cfg(feature = "decimal")]
            Union::Decimal(.., ref mut access) => *access = typ,
            #[cfg(not(feature = "no_index"))]
            Union::Array(ref mut a, _, ref mut access) => {
                *access = typ;
                for v in a.iter_mut() {
                    v.set_access_mode(typ);
                }
            }
            #[cfg(not(feature = "no_index"))]
            Union::Blob(.., ref mut access) => *access = typ,
            #[cfg(not(feature = "no_object"))]
            Union::Map(ref mut m, _, ref mut access) => {
                *access = typ;
                for v in m.values_mut() {
                    v.set_access_mode(typ);
                }
            }
            #[cfg(not(feature = "no_std"))]
            Union::TimeStamp(.., ref mut access) => *access = typ,
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(.., ref mut access) => *access = typ,
        }
        self
    }
    /// Make this [`Dynamic`] read-only (i.e. a constant).
    #[inline(always)]
    pub fn into_read_only(self) -> Self {
        let mut value = self;
        value.set_access_mode(AccessMode::ReadOnly);
        value
    }
    /// Is this [`Dynamic`] read-only?
    ///
    /// Constant [`Dynamic`] values are read-only.
    ///
    /// If a [`&mut Dynamic`][Dynamic] to such a constant is passed to a Rust function, the function
    /// can use this information to return an error of
    /// [`ErrorAssignmentToConstant`][crate::EvalAltResult::ErrorAssignmentToConstant] if its value
    /// is going to be modified.
    ///
    /// This safe-guards constant values from being modified from within Rust functions.
    #[must_use]
    pub fn is_read_only(&self) -> bool {
        #[cfg(not(feature = "no_closure"))]
        match self.0 {
            Union::Shared(.., ReadOnly) => return true,

            #[cfg(not(feature = "sync"))]
            Union::Shared(ref cell, ..) => {
                return match cell.borrow().access_mode() {
                    ReadWrite => false,
                    ReadOnly => true,
                }
            }
            #[cfg(feature = "sync")]
            Union::Shared(ref cell, ..) => {
                return match cell.read().unwrap().access_mode() {
                    ReadWrite => false,
                    ReadOnly => true,
                }
            }

            _ => (),
        }

        match self.access_mode() {
            ReadWrite => false,
            ReadOnly => true,
        }
    }
    /// Can this [`Dynamic`] be hashed?
    #[must_use]
    pub(crate) fn is_hashable(&self) -> bool {
        match self.0 {
            Union::Unit(..)
            | Union::Bool(..)
            | Union::Str(..)
            | Union::Char(..)
            | Union::Int(..) => true,

            #[cfg(not(feature = "no_float"))]
            Union::Float(..) => true,
            #[cfg(not(feature = "no_index"))]
            Union::Array(..) => true,
            #[cfg(not(feature = "no_object"))]
            Union::Map(..) => true,

            #[cfg(not(feature = "no_closure"))]
            #[cfg(not(feature = "sync"))]
            Union::Shared(ref cell, ..) => cell.borrow().is_hashable(),

            #[cfg(not(feature = "no_closure"))]
            #[cfg(feature = "sync")]
            Union::Shared(ref cell, ..) => cell.read().unwrap().is_hashable(),

            _ => false,
        }
    }
    /// Create a [`Dynamic`] from any type.  A [`Dynamic`] value is simply returned as is.
    ///
    /// # Notes
    ///
    /// Beware that you need to pass in an [`Array`][crate::Array] type for it to be recognized as
    /// an [`Array`][crate::Array]. A [`Vec<T>`][Vec] does not get automatically converted to an
    /// [`Array`][crate::Array], but will be a custom type instead (stored as a trait object).  Use
    /// `Into<Dynamic>` to convert a [`Vec<T>`][Vec] into a [`Dynamic`] as an
    /// [`Array`][crate::Array] value.
    ///
    /// Similarly, passing in a [`HashMap<String, T>`][std::collections::HashMap] or
    /// [`BTreeMap<String, T>`][std::collections::BTreeMap] will not get a [`Map`][crate::Map] but a
    /// custom type. Again, use `Into<Dynamic>` to get a [`Dynamic`] with a [`Map`][crate::Map]
    /// value.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::Dynamic;
    ///
    /// let result = Dynamic::from(42_i64);
    /// assert_eq!(result.type_name(), "i64");
    /// assert_eq!(result.to_string(), "42");
    ///
    /// let result = Dynamic::from("hello");
    /// assert_eq!(result.type_name(), "string");
    /// assert_eq!(result.to_string(), "hello");
    ///
    /// let new_result = Dynamic::from(result);
    /// assert_eq!(new_result.type_name(), "string");
    /// assert_eq!(new_result.to_string(), "hello");
    /// ```
    #[inline]
    #[must_use]
    pub fn from<T: Variant + Clone>(value: T) -> Self {
        // Coded this way in order to maximally leverage potentials for dead-code removal.

        reify!(value, |v: Dynamic| return v);
        reify!(value, |v: INT| return v.into());

        #[cfg(not(feature = "no_float"))]
        reify!(value, |v: crate::FLOAT| return v.into());

        #[cfg(feature = "decimal")]
        reify!(value, |v: rust_decimal::Decimal| return v.into());

        reify!(value, |v: bool| return v.into());
        reify!(value, |v: char| return v.into());
        reify!(value, |v: ImmutableString| return v.into());
        reify!(value, |v: String| return v.into());
        reify!(value, |v: &str| return v.into());
        reify!(value, |v: ()| return v.into());

        #[cfg(not(feature = "no_index"))]
        reify!(value, |v: crate::Array| return v.into());
        #[cfg(not(feature = "no_index"))]
        reify!(value, |v: crate::Blob| {
            // don't use blob.into() because it'll be converted into an Array
            return Dynamic::from_blob(v);
        });
        #[cfg(not(feature = "no_object"))]
        reify!(value, |v: crate::Map| return v.into());
        reify!(value, |v: FnPtr| return v.into());

        #[cfg(not(feature = "no_std"))]
        reify!(value, |v: Instant| return v.into());
        #[cfg(not(feature = "no_closure"))]
        reify!(value, |v: crate::Shared<crate::Locked<Dynamic>>| return v
            .into());

        Self(Union::Variant(
            Box::new(Box::new(value)),
            DEFAULT_TAG_VALUE,
            ReadWrite,
        ))
    }
    /// Turn the [`Dynamic`] value into a shared [`Dynamic`] value backed by an
    /// [`Rc<RefCell<Dynamic>>`][std::rc::Rc] or [`Arc<RwLock<Dynamic>>`][std::sync::Arc]
    /// depending on the `sync` feature.
    ///
    /// Not available under `no_closure`.
    ///
    /// Shared [`Dynamic`] values are relatively cheap to clone as they simply increment the
    /// reference counts.
    ///
    /// Shared [`Dynamic`] values can be converted seamlessly to and from ordinary [`Dynamic`]
    /// values.
    ///
    /// If the [`Dynamic`] value is already shared, this method returns itself.
    #[cfg(not(feature = "no_closure"))]
    #[inline]
    #[must_use]
    pub fn into_shared(self) -> Self {
        let _access = self.access_mode();

        match self.0 {
            Union::Shared(..) => self,
            _ => Self(Union::Shared(
                crate::Locked::new(self).into(),
                DEFAULT_TAG_VALUE,
                _access,
            )),
        }
    }
    /// Convert the [`Dynamic`] value into specific type.
    ///
    /// Casting to a [`Dynamic`] just returns as is, but if it contains a shared value,
    /// it is cloned into a [`Dynamic`] with a normal value.
    ///
    /// Returns [`None`] if types mismatched.
    ///
    /// # Panics or Deadlocks
    ///
    /// Under the `sync` feature, this call may deadlock, or [panic](https://doc.rust-lang.org/std/sync/struct.RwLock.html#panics-1).
    /// Otherwise, this call panics if the data is currently borrowed for write.
    ///
    /// These normally shouldn't occur since most operations in Rhai is single-threaded.
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Dynamic;
    ///
    /// let x = Dynamic::from(42_u32);
    ///
    /// assert_eq!(x.try_cast::<u32>().expect("x should be u32"), 42);
    /// ```
    #[inline]
    #[must_use]
    pub fn try_cast<T: Any>(self) -> Option<T> {
        // Coded this way in order to maximally leverage potentials for dead-code removal.

        #[cfg(not(feature = "no_closure"))]
        if let Union::Shared(..) = self.0 {
            return self.flatten().try_cast::<T>();
        }

        reify!(self, |v: T| return Some(v));

        match self.0 {
            Union::Int(v, ..) => reify!(v => Option<T>),
            #[cfg(not(feature = "no_float"))]
            Union::Float(v, ..) => reify!(*v => Option<T>),
            #[cfg(feature = "decimal")]
            Union::Decimal(v, ..) => reify!(*v => Option<T>),
            Union::Bool(v, ..) => reify!(v => Option<T>),
            Union::Str(v, ..) => {
                reify!(v, |v: T| Some(v), || reify!(v.to_string() => Option<T>))
            }
            Union::Char(v, ..) => reify!(v => Option<T>),
            #[cfg(not(feature = "no_index"))]
            Union::Array(v, ..) => reify!(*v => Option<T>),
            #[cfg(not(feature = "no_index"))]
            Union::Blob(v, ..) => reify!(*v => Option<T>),
            #[cfg(not(feature = "no_object"))]
            Union::Map(v, ..) => reify!(*v => Option<T>),
            Union::FnPtr(v, ..) => reify!(*v => Option<T>),
            #[cfg(not(feature = "no_std"))]
            Union::TimeStamp(v, ..) => reify!(*v => Option<T>),
            Union::Unit(v, ..) => reify!(v => Option<T>),
            Union::Variant(v, ..) => (*v).as_boxed_any().downcast().ok().map(|x| *x),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(..) => unreachable!("Union::Shared case should be already handled"),
        }
    }
    /// Convert the [`Dynamic`] value into a specific type.
    ///
    /// Casting to a [`Dynamic`] just returns as is, but if it contains a shared value,
    /// it is cloned into a [`Dynamic`] with a normal value.
    ///
    /// # Panics or Deadlocks
    ///
    /// Panics if the cast fails (e.g. the type of the actual value is not the same as the specified type).
    ///
    /// Under the `sync` feature, this call may deadlock, or [panic](https://doc.rust-lang.org/std/sync/struct.RwLock.html#panics-1).
    /// Otherwise, this call panics if the data is currently borrowed for write.
    ///
    /// These normally shouldn't occur since most operations in Rhai is single-threaded.
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Dynamic;
    ///
    /// let x = Dynamic::from(42_u32);
    ///
    /// assert_eq!(x.cast::<u32>(), 42);
    /// ```
    #[inline]
    #[must_use]
    pub fn cast<T: Any + Clone>(self) -> T {
        #[cfg(not(feature = "no_closure"))]
        let self_type_name = if self.is_shared() {
            // Avoid panics/deadlocks with shared values
            "<shared>"
        } else {
            self.type_name()
        };
        #[cfg(feature = "no_closure")]
        let self_type_name = self.type_name();

        self.try_cast::<T>()
            .unwrap_or_else(|| panic!("cannot cast {} to {}", self_type_name, type_name::<T>()))
    }
    /// Clone the [`Dynamic`] value and convert it into a specific type.
    ///
    /// Casting to a [`Dynamic`] just returns as is, but if it contains a shared value,
    /// it is cloned into a [`Dynamic`] with a normal value.
    ///
    /// Returns [`None`] if types mismatched.
    ///
    /// # Panics or Deadlocks
    ///
    /// Panics if the cast fails (e.g. the type of the actual value is not the
    /// same as the specified type).
    ///
    /// Under the `sync` feature, this call may deadlock, or [panic](https://doc.rust-lang.org/std/sync/struct.RwLock.html#panics-1).
    /// Otherwise, this call panics if the data is currently borrowed for write.
    ///
    /// These normally shouldn't occur since most operations in Rhai is single-threaded.
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Dynamic;
    ///
    /// let x = Dynamic::from(42_u32);
    /// let y = &x;
    ///
    /// assert_eq!(y.clone_cast::<u32>(), 42);
    /// ```
    #[inline(always)]
    #[must_use]
    pub fn clone_cast<T: Any + Clone>(&self) -> T {
        self.flatten_clone().cast::<T>()
    }
    /// Flatten the [`Dynamic`] and clone it.
    ///
    /// If the [`Dynamic`] is not a shared value, it returns a cloned copy.
    ///
    /// If the [`Dynamic`] is a shared value, it returns a cloned copy of the shared value.
    #[inline]
    #[must_use]
    pub fn flatten_clone(&self) -> Self {
        match self.0 {
            #[cfg(not(feature = "no_closure"))]
            #[cfg(not(feature = "sync"))]
            Union::Shared(ref cell, ..) => cell.borrow().clone(),
            #[cfg(not(feature = "no_closure"))]
            #[cfg(feature = "sync")]
            Union::Shared(ref cell, ..) => cell.read().unwrap().clone(),
            _ => self.clone(),
        }
    }
    /// Flatten the [`Dynamic`].
    ///
    /// If the [`Dynamic`] is not a shared value, it returns itself.
    ///
    /// If the [`Dynamic`] is a shared value, it returns the shared value if there are no
    /// outstanding references, or a cloned copy.
    #[inline]
    #[must_use]
    pub fn flatten(self) -> Self {
        match self.0 {
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(cell, ..) => crate::func::native::shared_try_take(cell).map_or_else(
                #[cfg(not(feature = "sync"))]
                |cell| cell.borrow().clone(),
                #[cfg(feature = "sync")]
                |cell| cell.read().unwrap().clone(),
                #[cfg(not(feature = "sync"))]
                |value| value.into_inner(),
                #[cfg(feature = "sync")]
                |value| value.into_inner().unwrap(),
            ),
            _ => self,
        }
    }
    /// Flatten the [`Dynamic`] in place.
    ///
    /// If the [`Dynamic`] is not a shared value, it does nothing.
    ///
    /// If the [`Dynamic`] is a shared value, it is set to the shared value if there are no
    /// outstanding references, or a cloned copy otherwise.
    #[inline]
    pub(crate) fn flatten_in_place(&mut self) -> &mut Self {
        match self.0 {
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(ref mut cell, ..) => {
                let cell = mem::take(cell);
                *self = crate::func::native::shared_try_take(cell).map_or_else(
                    #[cfg(not(feature = "sync"))]
                    |cell| cell.borrow().clone(),
                    #[cfg(feature = "sync")]
                    |cell| cell.read().unwrap().clone(),
                    #[cfg(not(feature = "sync"))]
                    |value| value.into_inner(),
                    #[cfg(feature = "sync")]
                    |value| value.into_inner().unwrap(),
                );
            }
            _ => (),
        }
        self
    }
    /// Is the [`Dynamic`] a shared value that is locked?
    ///
    /// Not available under `no_closure`.
    ///
    /// ## Note
    ///
    /// Under the `sync` feature, shared values use [`RwLock`][std::sync::RwLock] and they are never locked.
    /// Access just waits until the [`RwLock`][std::sync::RwLock] is released.
    /// So this method always returns [`false`] under [`Sync`].
    #[cfg(not(feature = "no_closure"))]
    #[inline]
    #[must_use]
    pub fn is_locked(&self) -> bool {
        #[cfg(not(feature = "no_closure"))]
        match self.0 {
            Union::Shared(ref _cell, ..) => {
                #[cfg(not(feature = "sync"))]
                return _cell.try_borrow().is_err();
                #[cfg(feature = "sync")]
                return false;
            }
            _ => (),
        }

        false
    }
    /// Get a reference of a specific type to the [`Dynamic`].
    /// Casting to [`Dynamic`] just returns a reference to it.
    ///
    /// Returns [`None`] if the cast fails.
    ///
    /// # Panics or Deadlocks When Value is Shared
    ///
    /// Under the `sync` feature, this call may deadlock, or [panic](https://doc.rust-lang.org/std/sync/struct.RwLock.html#panics-1).
    /// Otherwise, this call panics if the data is currently borrowed for write.
    #[inline]
    #[must_use]
    pub fn read_lock<T: Any + Clone>(&self) -> Option<DynamicReadLock<T>> {
        match self.0 {
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(ref cell, ..) => {
                #[cfg(not(feature = "sync"))]
                let value = cell.borrow();
                #[cfg(feature = "sync")]
                let value = cell.read().unwrap();

                if (*value).type_id() != TypeId::of::<T>()
                    && TypeId::of::<Dynamic>() != TypeId::of::<T>()
                {
                    return None;
                } else {
                    return Some(DynamicReadLock(DynamicReadLockInner::Guard(value)));
                }
            }
            _ => (),
        }

        self.downcast_ref()
            .map(DynamicReadLockInner::Reference)
            .map(DynamicReadLock)
    }
    /// Get a mutable reference of a specific type to the [`Dynamic`].
    /// Casting to [`Dynamic`] just returns a mutable reference to it.
    ///
    /// Returns [`None`] if the cast fails.
    ///
    /// # Panics or Deadlocks When Value is Shared
    ///
    /// Under the `sync` feature, this call may deadlock, or [panic](https://doc.rust-lang.org/std/sync/struct.RwLock.html#panics-1).
    /// Otherwise, this call panics if the data is currently borrowed for write.
    #[inline]
    #[must_use]
    pub fn write_lock<T: Any + Clone>(&mut self) -> Option<DynamicWriteLock<T>> {
        match self.0 {
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(ref cell, ..) => {
                let guard = crate::func::native::locked_write(cell);

                if (*guard).type_id() != TypeId::of::<T>()
                    && TypeId::of::<Dynamic>() != TypeId::of::<T>()
                {
                    return None;
                } else {
                    return Some(DynamicWriteLock(DynamicWriteLockInner::Guard(guard)));
                }
            }
            _ => (),
        }

        self.downcast_mut()
            .map(DynamicWriteLockInner::Reference)
            .map(DynamicWriteLock)
    }
    /// Get a reference of a specific type to the [`Dynamic`].
    /// Casting to [`Dynamic`] just returns a reference to it.
    ///
    /// Returns [`None`] if the cast fails, or if the value is shared.
    #[inline]
    #[must_use]
    pub(crate) fn downcast_ref<T: Any + Clone + ?Sized>(&self) -> Option<&T> {
        // Coded this way in order to maximally leverage potentials for dead-code removal.

        if TypeId::of::<T>() == TypeId::of::<INT>() {
            return match self.0 {
                Union::Int(ref v, ..) => v.as_any().downcast_ref::<T>(),
                _ => None,
            };
        }
        #[cfg(not(feature = "no_float"))]
        if TypeId::of::<T>() == TypeId::of::<crate::FLOAT>() {
            return match self.0 {
                Union::Float(ref v, ..) => v.as_ref().as_any().downcast_ref::<T>(),
                _ => None,
            };
        }
        #[cfg(feature = "decimal")]
        if TypeId::of::<T>() == TypeId::of::<rust_decimal::Decimal>() {
            return match self.0 {
                Union::Decimal(ref v, ..) => v.as_ref().as_any().downcast_ref::<T>(),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<bool>() {
            return match self.0 {
                Union::Bool(ref v, ..) => v.as_any().downcast_ref::<T>(),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<ImmutableString>() {
            return match self.0 {
                Union::Str(ref v, ..) => v.as_any().downcast_ref::<T>(),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<char>() {
            return match self.0 {
                Union::Char(ref v, ..) => v.as_any().downcast_ref::<T>(),
                _ => None,
            };
        }
        #[cfg(not(feature = "no_index"))]
        if TypeId::of::<T>() == TypeId::of::<crate::Array>() {
            return match self.0 {
                Union::Array(ref v, ..) => v.as_ref().as_any().downcast_ref::<T>(),
                _ => None,
            };
        }
        #[cfg(not(feature = "no_index"))]
        if TypeId::of::<T>() == TypeId::of::<crate::Blob>() {
            return match self.0 {
                Union::Blob(ref v, ..) => v.as_ref().as_any().downcast_ref::<T>(),
                _ => None,
            };
        }
        #[cfg(not(feature = "no_object"))]
        if TypeId::of::<T>() == TypeId::of::<crate::Map>() {
            return match self.0 {
                Union::Map(ref v, ..) => v.as_ref().as_any().downcast_ref::<T>(),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<FnPtr>() {
            return match self.0 {
                Union::FnPtr(ref v, ..) => v.as_ref().as_any().downcast_ref::<T>(),
                _ => None,
            };
        }
        #[cfg(not(feature = "no_std"))]
        if TypeId::of::<T>() == TypeId::of::<Instant>() {
            return match self.0 {
                Union::TimeStamp(ref v, ..) => v.as_ref().as_any().downcast_ref::<T>(),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<()>() {
            return match self.0 {
                Union::Unit(ref v, ..) => v.as_any().downcast_ref::<T>(),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<Dynamic>() {
            return self.as_any().downcast_ref::<T>();
        }

        match self.0 {
            Union::Variant(ref v, ..) => (***v).as_any().downcast_ref::<T>(),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(..) => None,
            _ => None,
        }
    }
    /// Get a mutable reference of a specific type to the [`Dynamic`].
    /// Casting to [`Dynamic`] just returns a mutable reference to it.
    ///
    /// Returns [`None`] if the cast fails, or if the value is shared.
    #[inline]
    #[must_use]
    pub(crate) fn downcast_mut<T: Any + Clone>(&mut self) -> Option<&mut T> {
        // Coded this way in order to maximally leverage potentials for dead-code removal.

        if TypeId::of::<T>() == TypeId::of::<INT>() {
            return match self.0 {
                Union::Int(ref mut v, ..) => v.as_any_mut().downcast_mut::<T>(),
                _ => None,
            };
        }
        #[cfg(not(feature = "no_float"))]
        if TypeId::of::<T>() == TypeId::of::<crate::FLOAT>() {
            return match self.0 {
                Union::Float(ref mut v, ..) => v.as_mut().as_any_mut().downcast_mut::<T>(),
                _ => None,
            };
        }
        #[cfg(feature = "decimal")]
        if TypeId::of::<T>() == TypeId::of::<rust_decimal::Decimal>() {
            return match self.0 {
                Union::Decimal(ref mut v, ..) => v.as_mut().as_any_mut().downcast_mut::<T>(),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<bool>() {
            return match self.0 {
                Union::Bool(ref mut v, ..) => v.as_any_mut().downcast_mut::<T>(),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<ImmutableString>() {
            return match self.0 {
                Union::Str(ref mut v, ..) => v.as_any_mut().downcast_mut::<T>(),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<char>() {
            return match self.0 {
                Union::Char(ref mut v, ..) => v.as_any_mut().downcast_mut::<T>(),
                _ => None,
            };
        }
        #[cfg(not(feature = "no_index"))]
        if TypeId::of::<T>() == TypeId::of::<crate::Array>() {
            return match self.0 {
                Union::Array(ref mut v, ..) => v.as_mut().as_any_mut().downcast_mut::<T>(),
                _ => None,
            };
        }
        #[cfg(not(feature = "no_index"))]
        if TypeId::of::<T>() == TypeId::of::<crate::Blob>() {
            return match self.0 {
                Union::Blob(ref mut v, ..) => v.as_mut().as_any_mut().downcast_mut::<T>(),
                _ => None,
            };
        }
        #[cfg(not(feature = "no_object"))]
        if TypeId::of::<T>() == TypeId::of::<crate::Map>() {
            return match self.0 {
                Union::Map(ref mut v, ..) => v.as_mut().as_any_mut().downcast_mut::<T>(),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<FnPtr>() {
            return match self.0 {
                Union::FnPtr(ref mut v, ..) => v.as_mut().as_any_mut().downcast_mut::<T>(),
                _ => None,
            };
        }
        #[cfg(not(feature = "no_std"))]
        if TypeId::of::<T>() == TypeId::of::<Instant>() {
            return match self.0 {
                Union::TimeStamp(ref mut v, ..) => v.as_mut().as_any_mut().downcast_mut::<T>(),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<()>() {
            return match self.0 {
                Union::Unit(ref mut v, ..) => v.as_any_mut().downcast_mut::<T>(),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<Dynamic>() {
            return self.as_any_mut().downcast_mut::<T>();
        }

        match self.0 {
            Union::Variant(ref mut v, ..) => (***v).as_any_mut().downcast_mut::<T>(),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(..) => None,
            _ => None,
        }
    }
    /// Cast the [`Dynamic`] as a unit `()`.
    /// Returns the name of the actual type if the cast fails.
    #[inline]
    pub fn as_unit(&self) -> Result<(), &'static str> {
        match self.0 {
            Union::Unit(v, ..) => Ok(v),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(..) => self.read_lock().map(|v| *v).ok_or_else(|| self.type_name()),
            _ => Err(self.type_name()),
        }
    }
    /// Cast the [`Dynamic`] as the system integer type [`INT`].
    /// Returns the name of the actual type if the cast fails.
    #[inline]
    pub fn as_int(&self) -> Result<INT, &'static str> {
        match self.0 {
            Union::Int(n, ..) => Ok(n),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(..) => self.read_lock().map(|v| *v).ok_or_else(|| self.type_name()),
            _ => Err(self.type_name()),
        }
    }
    /// Cast the [`Dynamic`] as the system floating-point type [`FLOAT`][crate::FLOAT].
    /// Returns the name of the actual type if the cast fails.
    ///
    /// Not available under `no_float`.
    #[cfg(not(feature = "no_float"))]
    #[inline]
    pub fn as_float(&self) -> Result<crate::FLOAT, &'static str> {
        match self.0 {
            Union::Float(n, ..) => Ok(*n),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(..) => self.read_lock().map(|v| *v).ok_or_else(|| self.type_name()),
            _ => Err(self.type_name()),
        }
    }
    /// _(decimal)_ Cast the [`Dynamic`] as a [`Decimal`][rust_decimal::Decimal].
    /// Returns the name of the actual type if the cast fails.
    ///
    /// Exported under the `decimal` feature only.
    #[cfg(feature = "decimal")]
    #[inline]
    pub fn as_decimal(&self) -> Result<rust_decimal::Decimal, &'static str> {
        match self.0 {
            Union::Decimal(ref n, ..) => Ok(**n),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(..) => self.read_lock().map(|v| *v).ok_or_else(|| self.type_name()),
            _ => Err(self.type_name()),
        }
    }
    /// Cast the [`Dynamic`] as a [`bool`].
    /// Returns the name of the actual type if the cast fails.
    #[inline]
    pub fn as_bool(&self) -> Result<bool, &'static str> {
        match self.0 {
            Union::Bool(b, ..) => Ok(b),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(..) => self.read_lock().map(|v| *v).ok_or_else(|| self.type_name()),
            _ => Err(self.type_name()),
        }
    }
    /// Cast the [`Dynamic`] as a [`char`].
    /// Returns the name of the actual type if the cast fails.
    #[inline]
    pub fn as_char(&self) -> Result<char, &'static str> {
        match self.0 {
            Union::Char(n, ..) => Ok(n),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(..) => self.read_lock().map(|v| *v).ok_or_else(|| self.type_name()),
            _ => Err(self.type_name()),
        }
    }
    /// Cast the [`Dynamic`] as a string slice.
    /// Returns the name of the actual type if the cast fails.
    ///
    /// # Panics
    ///
    /// Panics if the value is shared.
    #[inline]
    pub(crate) fn as_str_ref(&self) -> Result<&str, &'static str> {
        match self.0 {
            Union::Str(ref s, ..) => Ok(s),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(..) => panic!("as_str_ref() cannot be called on shared values"),
            _ => Err(self.type_name()),
        }
    }
    /// Convert the [`Dynamic`] into a [`String`].
    /// If there are other references to the same string, a cloned copy is returned.
    /// Returns the name of the actual type if the cast fails.
    #[inline]
    pub fn into_string(self) -> Result<String, &'static str> {
        self.into_immutable_string()
            .map(ImmutableString::into_owned)
    }
    /// Convert the [`Dynamic`] into an [`ImmutableString`].
    /// Returns the name of the actual type if the cast fails.
    #[inline]
    pub fn into_immutable_string(self) -> Result<ImmutableString, &'static str> {
        match self.0 {
            Union::Str(s, ..) => Ok(s),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(cell, ..) => {
                #[cfg(not(feature = "sync"))]
                let value = cell.borrow();
                #[cfg(feature = "sync")]
                let value = cell.read().unwrap();

                match value.0 {
                    Union::Str(ref s, ..) => Ok(s.clone()),
                    _ => Err((*value).type_name()),
                }
            }
            _ => Err(self.type_name()),
        }
    }
    /// Convert the [`Dynamic`] into an [`Array`][crate::Array].
    /// Returns the name of the actual type if the cast fails.
    #[cfg(not(feature = "no_index"))]
    #[inline(always)]
    pub fn into_array(self) -> Result<crate::Array, &'static str> {
        match self.0 {
            Union::Array(a, ..) => Ok(*a),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(cell, ..) => {
                #[cfg(not(feature = "sync"))]
                let value = cell.borrow();
                #[cfg(feature = "sync")]
                let value = cell.read().unwrap();

                match value.0 {
                    Union::Array(ref a, ..) => Ok(a.as_ref().clone()),
                    _ => Err((*value).type_name()),
                }
            }
            _ => Err(self.type_name()),
        }
    }
    /// Convert the [`Dynamic`] into a [`Vec`].
    /// Returns the name of the actual type if any cast fails.
    #[cfg(not(feature = "no_index"))]
    #[inline(always)]
    pub fn into_typed_array<T: Variant + Clone>(self) -> Result<Vec<T>, &'static str> {
        match self.0 {
            Union::Array(a, ..) => a
                .into_iter()
                .map(|v| {
                    #[cfg(not(feature = "no_closure"))]
                    let typ = if v.is_shared() {
                        // Avoid panics/deadlocks with shared values
                        "<shared>"
                    } else {
                        v.type_name()
                    };
                    #[cfg(feature = "no_closure")]
                    let typ = v.type_name();

                    v.try_cast::<T>().ok_or_else(|| typ)
                })
                .collect(),
            Union::Blob(..) if TypeId::of::<T>() == TypeId::of::<u8>() => Ok(self.cast::<Vec<T>>()),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(cell, ..) => {
                #[cfg(not(feature = "sync"))]
                let value = cell.borrow();
                #[cfg(feature = "sync")]
                let value = cell.read().unwrap();

                match value.0 {
                    Union::Array(ref a, ..) => {
                        a.iter()
                            .map(|v| {
                                #[cfg(not(feature = "no_closure"))]
                                let typ = if v.is_shared() {
                                    // Avoid panics/deadlocks with shared values
                                    "<shared>"
                                } else {
                                    v.type_name()
                                };
                                #[cfg(feature = "no_closure")]
                                let typ = v.type_name();

                                v.read_lock::<T>().ok_or_else(|| typ).map(|v| v.clone())
                            })
                            .collect()
                    }
                    Union::Blob(..) if TypeId::of::<T>() == TypeId::of::<u8>() => {
                        Ok((*value).clone().cast::<Vec<T>>())
                    }
                    _ => Err((*value).type_name()),
                }
            }
            _ => Err(self.type_name()),
        }
    }
    /// Convert the [`Dynamic`] into a [`Blob`][crate::Blob].
    /// Returns the name of the actual type if the cast fails.
    #[cfg(not(feature = "no_index"))]
    #[inline(always)]
    pub fn into_blob(self) -> Result<crate::Blob, &'static str> {
        match self.0 {
            Union::Blob(a, ..) => Ok(*a),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(cell, ..) => {
                #[cfg(not(feature = "sync"))]
                let value = cell.borrow();
                #[cfg(feature = "sync")]
                let value = cell.read().unwrap();

                match value.0 {
                    Union::Blob(ref a, ..) => Ok(a.as_ref().clone()),
                    _ => Err((*value).type_name()),
                }
            }
            _ => Err(self.type_name()),
        }
    }
}

impl From<()> for Dynamic {
    #[inline(always)]
    fn from(value: ()) -> Self {
        Self(Union::Unit(value, DEFAULT_TAG_VALUE, ReadWrite))
    }
}
impl From<bool> for Dynamic {
    #[inline(always)]
    fn from(value: bool) -> Self {
        Self(Union::Bool(value, DEFAULT_TAG_VALUE, ReadWrite))
    }
}
impl From<INT> for Dynamic {
    #[inline(always)]
    fn from(value: INT) -> Self {
        Self(Union::Int(value, DEFAULT_TAG_VALUE, ReadWrite))
    }
}
#[cfg(not(feature = "no_float"))]
impl From<crate::FLOAT> for Dynamic {
    #[inline(always)]
    fn from(value: crate::FLOAT) -> Self {
        Self(Union::Float(value.into(), DEFAULT_TAG_VALUE, ReadWrite))
    }
}
#[cfg(not(feature = "no_float"))]
impl From<crate::ast::FloatWrapper<crate::FLOAT>> for Dynamic {
    #[inline(always)]
    fn from(value: crate::ast::FloatWrapper<crate::FLOAT>) -> Self {
        Self(Union::Float(value, DEFAULT_TAG_VALUE, ReadWrite))
    }
}
#[cfg(feature = "decimal")]
impl From<rust_decimal::Decimal> for Dynamic {
    #[inline(always)]
    fn from(value: rust_decimal::Decimal) -> Self {
        Self(Union::Decimal(value.into(), DEFAULT_TAG_VALUE, ReadWrite))
    }
}
impl From<char> for Dynamic {
    #[inline(always)]
    fn from(value: char) -> Self {
        Self(Union::Char(value, DEFAULT_TAG_VALUE, ReadWrite))
    }
}
impl<S: Into<ImmutableString>> From<S> for Dynamic {
    #[inline(always)]
    fn from(value: S) -> Self {
        Self(Union::Str(value.into(), DEFAULT_TAG_VALUE, ReadWrite))
    }
}
impl From<&ImmutableString> for Dynamic {
    #[inline(always)]
    fn from(value: &ImmutableString) -> Self {
        value.clone().into()
    }
}
impl FromStr for Dynamic {
    type Err = ();

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        Ok(Self(Union::Str(value.into(), DEFAULT_TAG_VALUE, ReadWrite)))
    }
}
#[cfg(not(feature = "no_index"))]
impl<T: Variant + Clone> From<Vec<T>> for Dynamic {
    #[inline]
    fn from(value: Vec<T>) -> Self {
        Self(Union::Array(
            Box::new(value.into_iter().map(Dynamic::from).collect()),
            DEFAULT_TAG_VALUE,
            ReadWrite,
        ))
    }
}
#[cfg(not(feature = "no_index"))]
impl<T: Variant + Clone> From<&[T]> for Dynamic {
    #[inline]
    fn from(value: &[T]) -> Self {
        Self(Union::Array(
            Box::new(value.iter().cloned().map(Dynamic::from).collect()),
            DEFAULT_TAG_VALUE,
            ReadWrite,
        ))
    }
}
#[cfg(not(feature = "no_index"))]
impl<T: Variant + Clone> std::iter::FromIterator<T> for Dynamic {
    #[inline]
    fn from_iter<X: IntoIterator<Item = T>>(iter: X) -> Self {
        Self(Union::Array(
            Box::new(iter.into_iter().map(Dynamic::from).collect()),
            DEFAULT_TAG_VALUE,
            ReadWrite,
        ))
    }
}
#[cfg(not(feature = "no_object"))]
#[cfg(not(feature = "no_std"))]
impl<K: Into<crate::Identifier>, T: Variant + Clone> From<std::collections::HashMap<K, T>>
    for Dynamic
{
    #[inline]
    fn from(value: std::collections::HashMap<K, T>) -> Self {
        Self(Union::Map(
            Box::new(
                value
                    .into_iter()
                    .map(|(k, v)| (k.into(), Dynamic::from(v)))
                    .collect(),
            ),
            DEFAULT_TAG_VALUE,
            ReadWrite,
        ))
    }
}
#[cfg(not(feature = "no_object"))]
#[cfg(not(feature = "no_std"))]
impl<K: Into<crate::Identifier>> From<std::collections::HashSet<K>> for Dynamic {
    #[inline]
    fn from(value: std::collections::HashSet<K>) -> Self {
        Self(Union::Map(
            Box::new(
                value
                    .into_iter()
                    .map(|k| (k.into(), Dynamic::UNIT))
                    .collect(),
            ),
            DEFAULT_TAG_VALUE,
            ReadWrite,
        ))
    }
}
#[cfg(not(feature = "no_object"))]
impl<K: Into<crate::Identifier>, T: Variant + Clone> From<std::collections::BTreeMap<K, T>>
    for Dynamic
{
    #[inline]
    fn from(value: std::collections::BTreeMap<K, T>) -> Self {
        Self(Union::Map(
            Box::new(
                value
                    .into_iter()
                    .map(|(k, v)| (k.into(), Dynamic::from(v)))
                    .collect(),
            ),
            DEFAULT_TAG_VALUE,
            ReadWrite,
        ))
    }
}
#[cfg(not(feature = "no_object"))]
impl<K: Into<crate::Identifier>> From<std::collections::BTreeSet<K>> for Dynamic {
    #[inline]
    fn from(value: std::collections::BTreeSet<K>) -> Self {
        Self(Union::Map(
            Box::new(
                value
                    .into_iter()
                    .map(|k| (k.into(), Dynamic::UNIT))
                    .collect(),
            ),
            DEFAULT_TAG_VALUE,
            ReadWrite,
        ))
    }
}
impl From<FnPtr> for Dynamic {
    #[inline(always)]
    fn from(value: FnPtr) -> Self {
        Self(Union::FnPtr(value.into(), DEFAULT_TAG_VALUE, ReadWrite))
    }
}
#[cfg(not(feature = "no_std"))]
impl From<Instant> for Dynamic {
    #[inline(always)]
    fn from(value: Instant) -> Self {
        Self(Union::TimeStamp(value.into(), DEFAULT_TAG_VALUE, ReadWrite))
    }
}
#[cfg(not(feature = "no_closure"))]
impl From<crate::Shared<crate::Locked<Dynamic>>> for Dynamic {
    #[inline(always)]
    fn from(value: crate::Shared<crate::Locked<Self>>) -> Self {
        Self(Union::Shared(value, DEFAULT_TAG_VALUE, ReadWrite))
    }
}

impl From<ExclusiveRange> for Dynamic {
    #[inline(always)]
    fn from(value: ExclusiveRange) -> Self {
        Dynamic::from(value)
    }
}
impl From<InclusiveRange> for Dynamic {
    #[inline(always)]
    fn from(value: InclusiveRange) -> Self {
        Dynamic::from(value)
    }
}
