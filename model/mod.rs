pub mod auth;
pub mod data;
pub mod datetime;
pub mod document;
pub mod key_switch;
pub mod query;

use ink_prelude::string::String;

pub trait ManualJsonfiable {
    fn to_json_string(&self) -> String;
}
