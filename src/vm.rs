use std::rc::Rc;

use byteorder::{BigEndian, ByteOrder};

use crate::{
    code::{Instructions, OpCode},
    compiler::Bytecode,
    object::{Boolean, Integer, Null, Object},
};

const STACK_SIZE: usize = 2048;

#[derive(Debug)]
pub struct Vm {
    true_object: Rc<Object>,
    false_object: Rc<Object>,
    null_object: Rc<Object>,

    constants: Vec<Object>,
    instructions: Instructions,

    stack: Vec<Rc<Object>>,
    sp: usize, // stack pointer
}

impl Vm {
    pub fn new(bytecode: Bytecode) -> Self {
        let true_object = Rc::new(Object::Boolean(Boolean::new(true)));
        let false_object = Rc::new(Object::Boolean(Boolean::new(false)));
        let null_object = Rc::new(Object::Null(Null::default()));

        Self {
            true_object,
            false_object,
            null_object: Rc::clone(&null_object),

            constants: bytecode.constants,
            instructions: bytecode.instructions,
            stack: vec![Rc::clone(&null_object); STACK_SIZE],
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
                    self.push(constant.into());
                }
                OpCode::OpTrue => self.push(Rc::clone(&self.true_object)),
                OpCode::OpFalse => self.push(Rc::clone(&self.false_object)),
                OpCode::OpAdd | OpCode::OpSub | OpCode::OpMul | OpCode::OpDiv => {
                    self.execute_binary_operation(&op);
                }
                OpCode::OpEqual | OpCode::OpNotEqual | OpCode::OpGreaterThan => {
                    self.execute_comparison(&op);
                }
                OpCode::OpMinus => self.execute_minus_operator(),
                OpCode::OpBang => self.execute_bang_operator(),
                OpCode::OpPop => {
                    self.pop();
                }
                OpCode::NoOp => {}
            }
            ip += 1;
        }
    }

    fn execute_binary_operation(&mut self, op: &OpCode) {
        let right = self.pop().to_owned();
        let left = self.pop().to_owned();
        if let Object::Integer(ref right) = *right {
            if let Object::Integer(ref left) = *left {
                self.execute_binary_integer_operation(op, left, right);
            }
        }
    }

    fn execute_binary_integer_operation(&mut self, op: &OpCode, left: &Integer, right: &Integer) {
        let result = match op {
            OpCode::OpAdd => left.value + right.value,
            OpCode::OpSub => left.value - right.value,
            OpCode::OpMul => left.value * right.value,
            OpCode::OpDiv => left.value / right.value,
            _ => panic!("unknown integer operator: {:?}", op),
        };
        self.push(Object::Integer(Integer::new(result)).into());
    }

    fn execute_comparison(&mut self, op: &OpCode) {
        let right = self.pop().to_owned();
        let left = self.pop().to_owned();

        if let Object::Integer(ref right) = *right {
            if let Object::Integer(ref left) = *left {
                let result = self.execute_integer_comparison(op, left, right);
                self.push(result);
                return;
            }
        }

        let result = match op {
            OpCode::OpEqual => self.native_bool_to_boolean_object(left == right),
            OpCode::OpNotEqual => self.native_bool_to_boolean_object(left != right),
            _ => panic!(
                "unknown operator: {:?} ({:?} {:?})",
                op,
                left.kind(),
                right.kind()
            ),
        };
        self.push(result);
    }

    fn execute_integer_comparison(
        &mut self,
        op: &OpCode,
        left: &Integer,
        right: &Integer,
    ) -> Rc<Object> {
        match op {
            OpCode::OpEqual => self.native_bool_to_boolean_object(left.value == right.value),
            OpCode::OpNotEqual => self.native_bool_to_boolean_object(left.value != right.value),
            OpCode::OpGreaterThan => self.native_bool_to_boolean_object(left.value > right.value),
            _ => panic!("unknown operator: {:?}", op),
        }
    }

    fn execute_bang_operator(&mut self) {
        let operand = self.pop().to_owned();
        if operand == self.true_object {
            self.push(Rc::clone(&self.false_object));
        } else if operand == self.false_object {
            self.push(Rc::clone(&self.true_object));
        } else {
            self.push(Rc::clone(&self.false_object));
        }
    }

    fn execute_minus_operator(&mut self) {
        let operand = self.pop().to_owned();

        if let Object::Integer(ref integer) = *operand {
            self.push(
                Object::Integer(Integer::new(-integer.value)).into()
            );
        } else {
            panic!("unsupported type for negation: {}", operand.kind());
        }
    }

    fn native_bool_to_boolean_object(&self, native: bool) -> Rc<Object> {
        match native {
            true => Rc::clone(&self.true_object),
            false => Rc::clone(&self.false_object),
        }
    }

    fn push(&mut self, object: Rc<Object>) {
        self.stack[self.sp] = object;
        self.sp += 1;
    }

    fn pop(&mut self) -> &Rc<Object> {
        self.sp -= 1;
        &self.stack[self.sp]
    }

    pub fn last_popped_stack_elem(&self) -> &Object {
        &self.stack[self.sp]
    }

    pub fn stack_top(&self) -> &Object {
        &self.stack[self.sp - 1]
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        compiler::Compiler,
        object::Object,
        test_helper::{parse, test_boolean_object, test_integer_object, ExpectedValue},
    };

    use super::Vm;

    #[derive(Debug)]
    struct VmTestCase<'a>(&'a str, ExpectedValue);

    fn run_vm_tests(tests: &[VmTestCase]) {
        for test in tests.iter() {
            println!("{}", test.0);
            let program = parse(test.0);
            let mut compiler = Compiler::default();
            compiler.compile(&program);
            let mut vm = Vm::new(compiler.bytecode());
            vm.run();

            test_expected_object(vm.last_popped_stack_elem(), &test.1)
        }
    }

    fn test_expected_object(actual: &Object, expected: &ExpectedValue) {
        match expected {
            ExpectedValue::Integer(e) => test_integer_object(actual, e.to_owned()),
            ExpectedValue::Boolean(e) => test_boolean_object(actual, e.to_owned()),
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = [
            VmTestCase("1", ExpectedValue::Integer(1)),
            VmTestCase("2", ExpectedValue::Integer(2)),
            VmTestCase("1 + 2", ExpectedValue::Integer(3)),
            VmTestCase("1 - 2", ExpectedValue::Integer(-1)),
            VmTestCase("1 * 2", ExpectedValue::Integer(2)),
            VmTestCase("4 / 2", ExpectedValue::Integer(2)),
            VmTestCase("50 / 2 * 2 + 10 - 5", ExpectedValue::Integer(55)),
            VmTestCase("5 + 5 + 5 + 5 - 10", ExpectedValue::Integer(10)),
            VmTestCase("2 * 2 * 2 * 2 * 2", ExpectedValue::Integer(32)),
            VmTestCase("5 * 2 + 10", ExpectedValue::Integer(20)),
            VmTestCase("5 + 2 * 10", ExpectedValue::Integer(25)),
            VmTestCase("5 * (2 + 10)", ExpectedValue::Integer(60)),
            VmTestCase("-5", ExpectedValue::Integer(-5)),
            VmTestCase("-10", ExpectedValue::Integer(-10)),
            VmTestCase("-50 + 100 + -50", ExpectedValue::Integer(0)),
            VmTestCase(
                "(5 + 10 * 2 + 15 / 3) * 2 + -10",
                ExpectedValue::Integer(50),
            ),
        ];

        run_vm_tests(&tests);
    }

    #[test]
    fn test_boolean_expression() {
        let tests = [
            VmTestCase("true", ExpectedValue::Boolean(true)),
            VmTestCase("false", ExpectedValue::Boolean(false)),
            VmTestCase("1 < 2", ExpectedValue::Boolean(true)),
            VmTestCase("1 > 2", ExpectedValue::Boolean(false)),
            VmTestCase("1 < 1", ExpectedValue::Boolean(false)),
            VmTestCase("1 > 1", ExpectedValue::Boolean(false)),
            VmTestCase("1 == 1", ExpectedValue::Boolean(true)),
            VmTestCase("1 != 1", ExpectedValue::Boolean(false)),
            VmTestCase("1 == 2", ExpectedValue::Boolean(false)),
            VmTestCase("1 != 2", ExpectedValue::Boolean(true)),
            VmTestCase("true == true", ExpectedValue::Boolean(true)),
            VmTestCase("false == false", ExpectedValue::Boolean(true)),
            VmTestCase("true == false", ExpectedValue::Boolean(false)),
            VmTestCase("true != false", ExpectedValue::Boolean(true)),
            VmTestCase("false != true", ExpectedValue::Boolean(true)),
            VmTestCase("(1 < 2) == true", ExpectedValue::Boolean(true)),
            VmTestCase("(1 < 2) == false", ExpectedValue::Boolean(false)),
            VmTestCase("(1 > 2) == true", ExpectedValue::Boolean(false)),
            VmTestCase("(1 > 2) == false", ExpectedValue::Boolean(true)),
            VmTestCase("!true", ExpectedValue::Boolean(false)),
            VmTestCase("!false", ExpectedValue::Boolean(true)),
            VmTestCase("!5", ExpectedValue::Boolean(false)),
            VmTestCase("!!true", ExpectedValue::Boolean(true)),
            VmTestCase("!!false", ExpectedValue::Boolean(false)),
            VmTestCase("!!5", ExpectedValue::Boolean(true)),
        ];

        run_vm_tests(&tests);
    }
}
