mod assertion;
pub mod default_model;

pub trait Model {
    fn add_def(&mut self, sec: &str, key: &str, value: &str) -> bool;
}
