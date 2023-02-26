use crate::{code::Instructions, object::CompiledFunction};

#[derive(Debug, Clone)]
pub struct Frame {
    func: CompiledFunction,
    pub ip: isize,
    pub base_pointer: usize,
}

impl Frame {
    pub fn new(func: CompiledFunction, base_pointer: usize) -> Self {
        Self { func, ip: -1, base_pointer }
    }

    pub fn instructions(&self) -> &Instructions {
        &self.func.instructions
    }
}
