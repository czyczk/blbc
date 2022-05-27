extern crate alloc;

use ink_prelude::vec::Vec;
use ink_prelude::{string::String, vec};
use ritelinked::{LinkedHashMap, LinkedHashSet};

pub type AssertionMap = LinkedHashMap<String, Assertion>;

#[derive(Debug, Clone)]
pub struct Assertion {
    pub key: String,
    pub value: String,
    pub tokens: Vec<String>,
    pub policy: LinkedHashSet<Vec<String>>,
}

impl Default for Assertion {
    fn default() -> Self {
        Assertion {
            key: String::new(),
            value: String::new(),
            tokens: vec![],
            policy: LinkedHashSet::new(),
        }
    }
}

impl Assertion {
    #[inline]
    pub fn get_policy(&self) -> &LinkedHashSet<Vec<String>> {
        &self.policy
    }
}
