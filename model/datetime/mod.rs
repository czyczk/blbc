mod offset;

extern crate alloc;

use core::fmt::{Debug, Display, Formatter};

use chrono::{DateTime, FixedOffset, NaiveDateTime, Utc};
use ink_prelude::string::String;
use ink_storage::traits::{PackedLayout, SpreadLayout};
use scale::{Decode, Encode};

#[derive(Clone, Eq, Ord, Hash)]
pub struct ScaleDateTimeLocal {
    pub datetime: DateTime<FixedOffset>,
    pub rfc3339_str: String,
}

impl ScaleDateTimeLocal {
    pub fn datetime(&self) -> DateTime<FixedOffset> {
        self.datetime
    }
}

impl Debug for ScaleDateTimeLocal {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        // Debug formats only the datetime
        write!(f, "{:?}", self.datetime)
    }
}

impl Display for ScaleDateTimeLocal {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        // Display formats only the datetime
        write!(f, "{}", self.datetime)
    }
}

impl PartialEq for ScaleDateTimeLocal {
    fn eq(&self, other: &Self) -> bool {
        // Compare only the datetime
        self.datetime.eq(&other.datetime)
    }
}

impl PartialOrd for ScaleDateTimeLocal {
    fn partial_cmp(&self, rhs: &Self) -> Option<core::cmp::Ordering> {
        // Compare only the datetime
        self.datetime.partial_cmp(&rhs.datetime)
    }
}

impl Encode for ScaleDateTimeLocal {
    fn size_hint(&self) -> usize {
        // Estimate only the RFC3339 notation
        self.rfc3339_str.size_hint()
    }

    fn encode_to<T: scale::Output + ?Sized>(&self, dest: &mut T) {
        // Encode only the RFC3339 notation
        self.rfc3339_str.using_encoded(|buf| dest.write(buf));
    }
}

impl Decode for ScaleDateTimeLocal {
    fn decode<I: scale::Input>(input: &mut I) -> Result<Self, scale::Error> {
        let rfc3339_str: String = Decode::decode(input)?;

        // let datetime = DateTime::parse_from_rfc3339(&rfc3339_str).map_err(|e| {
        //     let leaked_str: &'static str =
        //         alloc::boxed::Box::leak(format!("{}", e).into_boxed_str());
        //     scale::Error::from(leaked_str)
        // })?;

        Ok(rfc3339_str.into())
    }
}

#[cfg(feature = "std")]
impl scale_info::TypeInfo for ScaleDateTimeLocal {
    type Identity = String;

    fn type_info() -> scale_info::Type {
        String::type_info()
    }
}

impl PackedLayout for ScaleDateTimeLocal {
    fn pull_packed(&mut self, at: &ink_primitives::Key) {
        self.rfc3339_str.pull_packed(at)
    }

    fn push_packed(&self, at: &ink_primitives::Key) {
        self.rfc3339_str.push_packed(at)
    }

    fn clear_packed(&self, at: &ink_primitives::Key) {
        self.rfc3339_str.clear_packed(at)
    }
}

impl SpreadLayout for ScaleDateTimeLocal {
    const FOOTPRINT: u64 = String::FOOTPRINT;

    const REQUIRES_DEEP_CLEAN_UP: bool = String::REQUIRES_DEEP_CLEAN_UP;

    fn pull_spread(ptr: &mut ink_primitives::KeyPtr) -> Self {
        let rfc3339_str = String::pull_spread(ptr);
        let datetime = DateTime::parse_from_rfc3339(&rfc3339_str).unwrap();
        Self {
            datetime: datetime.into(),
            rfc3339_str,
        }
    }

    fn push_spread(&self, ptr: &mut ink_primitives::KeyPtr) {
        self.rfc3339_str.push_spread(ptr)
    }

    fn clear_spread(&self, ptr: &mut ink_primitives::KeyPtr) {
        self.rfc3339_str.clear_spread(ptr)
    }
}

impl From<DateTime<Utc>> for ScaleDateTimeLocal {
    fn from(datetime: DateTime<Utc>) -> Self {
        let datetime_utc_e8 = datetime.with_timezone(&offset::create_utc_e8());

        Self {
            datetime: datetime_utc_e8,
            rfc3339_str: datetime_utc_e8.to_rfc3339(),
        }
    }
}

impl From<NaiveDateTime> for ScaleDateTimeLocal {
    fn from(naive_datetime: NaiveDateTime) -> Self {
        DateTime::<Utc>::from_utc(naive_datetime, Utc).into()
    }
}

impl From<u64> for ScaleDateTimeLocal {
    fn from(block_timestamp: u64) -> Self {
        // Convert to timestamp in nanoseconds
        let timestamp_in_nano = (block_timestamp * 1_000_000) as i64;
        let nano_unit = 1_000_000_000i64;
        let secs = (timestamp_in_nano / nano_unit) as i64;
        let nsecs = (timestamp_in_nano % nano_unit) as u32;

        let naive_datetime = NaiveDateTime::from_timestamp(secs, nsecs);
        naive_datetime.into()
    }
}

impl From<String> for ScaleDateTimeLocal {
    fn from(rfc3339_str: String) -> Self {
        // Parse the RFC3339 notation
        let datetime = DateTime::parse_from_rfc3339(&rfc3339_str).unwrap();
        let datetime_utc_e8 = datetime.with_timezone(&offset::create_utc_e8());

        Self {
            datetime: datetime_utc_e8,
            rfc3339_str: datetime_utc_e8.to_rfc3339(),
        }
    }
}
