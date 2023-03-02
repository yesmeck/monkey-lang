use crate::{code::Instructions, object::Closure};

#[derive(Debug, Clone)]
pub struct Frame {
    pub closure: Closure,
    pub ip: isize,
    pub base_pointer: usize,
}

impl Frame {
    pub fn new(closure: Closure, base_pointer: usize) -> Self {
        Self { closure, ip: -1, base_pointer }
    }

    pub fn instructions(&self) -> &Instructions {
        &self.closure.func.instructions
    }
}
