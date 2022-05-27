extern crate alloc;

use crate::casbink::{model::Model, Result};

use super::{Adapter, Filter};

pub struct NullAdapter;

impl Adapter for NullAdapter {
    fn load_policy(&self, _m: &mut dyn Model) -> Result<()> {
        Ok(())
    }

    fn load_filtered_policy<'a>(&mut self, _m: &mut dyn Model, _f: Filter<'a>) -> Result<()> {
        Ok(())
    }
}
