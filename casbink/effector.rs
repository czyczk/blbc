extern crate alloc;

use ink_prelude::boxed::Box;
use ink_prelude::string::String;
use ink_prelude::vec::Vec;

#[derive(PartialEq, Clone, Copy)]
pub enum EffectKind {
    Allow = 0,
    Indeterminate = 1,
    Deny = 2,
}

pub trait Effector {
    fn new_stream(&self, expr: &str, cap: usize) -> Box<dyn EffectorStream>;
}

pub trait EffectorStream {
    fn next(&self) -> bool;
    fn explain(&self) -> Option<Vec<usize>>;
    fn push_effect(&mut self, eft: EffectKind) -> bool;
}

#[derive(Clone)]
pub struct DefaultEffectStream {
    done: bool,
    res: bool,
    expr: String,
    idx: usize,
    cap: usize,
    expl: Vec<usize>,
}

#[derive(Default)]
pub struct DefaultEffector;
