extern crate alloc;

use super::model::Model;

pub trait CoreApi {
    fn get_model(&self) -> &dyn Model;
}
