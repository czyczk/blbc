mod assertion;
pub mod default_model;

use ink_prelude::collections::BTreeMap;
use ink_prelude::string::String;

use self::assertion::AssertionMap;

pub trait Model {
    fn add_def(&mut self, sec: &str, key: &str, value: &str) -> bool;
    fn get_model(&self) -> &BTreeMap<String, AssertionMap>;
}
