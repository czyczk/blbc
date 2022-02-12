use chrono::FixedOffset;

pub fn create_utc_e8() -> FixedOffset {
    FixedOffset::east(8 * 3600)
}
