use byteorder::{BigEndian, ByteOrder};

use crate::{
    code::{Instructions, OpCode},
    compiler::Bytecode,
    object::{Integer, Object},
};

const STACK_SIZE: usize = 2048;

#[derive(Debug)]
pub struct Vm {
    constants: Vec<Object>,
    instructions: Instructions,

    stack: Vec<Object>,
    sp: usize, // stack pointer
}

impl Vm {
    pub fn new(bytecode: Bytecode) -> Self {
        Self {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            stack: Vec::with_capacity(STACK_SIZE),
            sp: 0,
        }
    }

    pub fn run(&mut self) {
        let mut ip = 0;
        while ip < self.instructions.len() {
            let op = OpCode::from(self.instructions.0[ip]);
            match op {
                OpCode::OpConstant => {
                    let const_index = BigEndian::read_u16(&self.instructions.0[ip + 1..]);
                    ip += 2;
                    let constant = self.constants.get(const_index as usize).unwrap().to_owned();
                    self.push(constant);
                }
                OpCode::OpAdd => {
                    let right = self.pop();
                    let left = self.pop();
                    if let Object::Integer(right) = right {
                        if let Object::Integer(left) = left {
                            let result = left.value + right.value;
                            self.push(Object::Integer(Integer::new(result)));
                        }
                    }
                }
                OpCode::NoOp => {}
            }
            ip += 1;
        }
    }

    fn push(&mut self, object: Object) {
        if self.sp > STACK_SIZE {
            panic!("stack overflow");
        }
        self.stack.push(object);
        self.sp += 1;
    }

    fn pop(&mut self) -> Object {
        let o = self.stack.pop();
        self.sp -= 1;
        o.unwrap()
    }

    pub fn stack_top(&self) -> Option<&Object> {
        self.stack.get(self.sp - 1)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        compiler::Compiler,
        object::Object,
        test_helper::{parse, test_integer_object},
    };

    use super::Vm;

    #[derive(Debug)]
    enum ExpectedValue {
        Integer(i64),
    }

    #[derive(Debug)]
    struct VmTestCase<'a> {
        input: &'a str,
        expected: ExpectedValue,
    }

    fn run_vm_tests(tests: &[VmTestCase]) {
        for test in tests.iter() {
            let program = parse(test.input);
            let mut compiler = Compiler::default();
            compiler.compile(&program);
            let mut vm = Vm::new(compiler.bytecode());
            vm.run();

            test_expected_object(vm.stack_top().unwrap(), &test.expected)
        }
    }

    fn test_expected_object(actual: &Object, expected: &ExpectedValue) {
        match expected {
            ExpectedValue::Integer(e) => test_integer_object(actual, e.to_owned()),
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = [
            VmTestCase {
                input: "1",
                expected: ExpectedValue::Integer(1),
            },
            VmTestCase {
                input: "2",
                expected: ExpectedValue::Integer(2),
            },
            VmTestCase {
                input: "1 + 2",
                expected: ExpectedValue::Integer(3),
            },
        ];

        run_vm_tests(&tests);
    }
}
