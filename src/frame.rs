use crate::{code::Instructions, object::CompiledFunction};

#[derive(Debug, Clone)]
pub struct Frame {
    func: CompiledFunction,
    pub ip: isize,
}

impl Frame {
    pub fn new(func: CompiledFunction) -> Self {
        Self { func, ip: -1 }
    }

    pub fn instructions(&self) -> &Instructions {
        &self.func.instructions
    }
}
