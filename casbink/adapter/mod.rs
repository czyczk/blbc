extern crate alloc;

pub mod null_adapter;

use crate::casbink::Result;

use ink_prelude::vec::Vec;

use super::model::Model;

#[derive(Debug, Clone)]
pub struct Filter<'a> {
    pub p: Vec<&'a str>,
    pub g: Vec<&'a str>,
}

pub trait Adapter {
    fn load_policy(&self, m: &mut dyn Model) -> Result<()>;

    fn load_filtered_policy<'a>(&mut self, m: &mut dyn Model, f: Filter<'a>) -> Result<()>;
}
