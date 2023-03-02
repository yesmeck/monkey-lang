use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    builtin::{Builtin, BUILTINS},
    code::Opcode,
    compiler::Bytecode,
    frame::Frame,
    object::{
        Array, Boolean, BuiltinFunction, Closure, CompiledFunction, Hash, HashKeyable, Integer,
        Null, Object, RuntimeError, Str,
    },
};

const STACK_SIZE: usize = 2048;
pub const GLOBAL_SIZE: usize = 65536;
const MAX_FRAMES: usize = 1024;

#[derive(Debug)]
pub struct Vm {
    true_object: Rc<Object>,
    false_object: Rc<Object>,
    null_object: Rc<Object>,

    constants: Vec<Object>,

    stack: Vec<Rc<Object>>,
    sp: usize, // stack pointer

    builtin: Builtin,

    pub globals: Vec<Rc<Object>>,

    frames: Vec<Rc<RefCell<Frame>>>,
    frame_index: usize,
}

impl Vm {
    pub fn from(bytecode: Bytecode, other: Vm) -> Self {
        let mut vm = Self::new(bytecode);
        vm.globals = other.globals;
        vm
    }

    pub fn new(bytecode: Bytecode) -> Self {
        let true_object = Rc::new(Object::Boolean(Boolean::new(true)));
        let false_object = Rc::new(Object::Boolean(Boolean::new(false)));
        let null_object = Rc::new(Object::Null(Null::default()));

        let main_func = CompiledFunction::new(bytecode.instructions, 0, 0);
        let main_closure = Closure::new(main_func, vec![]);
        let main_frame = Rc::new(RefCell::new(Frame::new(main_closure, 0)));
        let mut frames = Vec::with_capacity(MAX_FRAMES);
        frames.push(main_frame);

        Self {
            true_object,
            false_object,
            null_object: Rc::clone(&null_object),

            constants: bytecode.constants,
            stack: vec![Rc::clone(&null_object); STACK_SIZE],
            sp: 0,

            builtin: Builtin::default(),

            globals: vec![Rc::clone(&null_object); GLOBAL_SIZE],

            frames,
            frame_index: 1,
        }
    }

    fn current_frame(&self) -> Rc<RefCell<Frame>> {
        Rc::clone(&self.frames[self.frame_index - 1])
    }

    fn push_frame(&mut self, f: Rc<RefCell<Frame>>) {
        self.frames.push(f);
        self.frame_index += 1;
    }

    fn pop_frame(&mut self) -> Rc<RefCell<Frame>> {
        let frame = self.frames.pop().unwrap();
        self.frame_index -= 1;
        frame
    }

    pub fn run(&mut self) {
        while self.current_frame().borrow().ip
            < self.current_frame().borrow().instructions().len() as isize - 1
        {
            let frame = self.current_frame();
            frame.borrow_mut().ip += 1;
            let op = frame
                .borrow()
                .instructions()
                .read_op_at(frame.borrow().ip as usize);
            match op {
                Opcode::Constant => {
                    let const_index = frame
                        .borrow()
                        .instructions()
                        .read_u16_from(frame.borrow().ip as usize + 1);
                    frame.borrow_mut().ip += 2;
                    let constant = self.constants.get(const_index as usize).unwrap().to_owned();
                    self.push(constant.into());
                }
                Opcode::True => self.push(Rc::clone(&self.true_object)),
                Opcode::False => self.push(Rc::clone(&self.false_object)),
                Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div => {
                    self.execute_binary_operation(&op);
                }
                Opcode::Equal | Opcode::NotEqual | Opcode::GreaterThan => {
                    self.execute_comparison(&op);
                }
                Opcode::Minus => self.execute_minus_operator(),
                Opcode::Bang => self.execute_bang_operator(),
                Opcode::Pop => {
                    self.pop();
                }
                Opcode::JumpNotTruth => {
                    let pos = frame
                        .borrow()
                        .instructions()
                        .read_u16_from(frame.borrow().ip as usize + 1);
                    frame.borrow_mut().ip += 2;

                    let confition = self.pop();

                    if !self.is_truthy(&confition) {
                        frame.borrow_mut().ip = pos as isize - 1;
                    }
                }
                Opcode::Jump => {
                    let pos = frame
                        .borrow()
                        .instructions()
                        .read_u16_from((frame.borrow().ip + 1) as usize);
                    frame.borrow_mut().ip = pos as isize - 1;
                }
                Opcode::Null => self.push(Rc::clone(&self.null_object)),
                Opcode::GetGlobal => {
                    let global_index = frame
                        .borrow()
                        .instructions()
                        .read_u16_from((frame.borrow().ip + 1) as usize);
                    frame.borrow_mut().ip += 2;
                    let var = Rc::clone(self.globals.get(global_index as usize).unwrap());
                    self.push(var);
                }
                Opcode::SetGlobal => {
                    let global_index = frame
                        .borrow()
                        .instructions()
                        .read_u16_from(frame.borrow().ip as usize + 1);
                    frame.borrow_mut().ip += 2;
                    self.globals[global_index as usize] = self.pop();
                }
                Opcode::Array => {
                    let num_elements = frame
                        .borrow()
                        .instructions()
                        .read_u16_from(frame.borrow().ip as usize + 1);
                    frame.borrow_mut().ip += 2;

                    let array = self.build_array(self.sp - num_elements as usize, self.sp);
                    self.sp -= num_elements as usize;
                    self.push(array);
                }
                Opcode::Hash => {
                    let num_elements = frame
                        .borrow()
                        .instructions()
                        .read_u16_from(frame.borrow().ip as usize + 1);
                    frame.borrow_mut().ip += 2;

                    let hash = self.build_hash(self.sp - num_elements as usize, self.sp);
                    self.sp -= num_elements as usize;
                    self.push(hash);
                }
                Opcode::Index => self.execute_index_expression(),
                Opcode::Call => {
                    let num_args = frame
                        .borrow()
                        .instructions()
                        .read_u8_from(frame.borrow().ip as usize + 1);
                    frame.borrow_mut().ip += 1;
                    self.execute_call(num_args);
                }
                Opcode::ReturnValue => {
                    let return_value = self.pop();
                    let frame2 = self.pop_frame();
                    self.sp = frame2.borrow().base_pointer - 1;
                    self.push(return_value);
                }
                Opcode::Return => {
                    let frame2 = self.pop_frame();
                    self.sp = frame2.borrow().base_pointer - 1;
                    self.push(Rc::clone(&self.null_object));
                }
                Opcode::GetLocal => {
                    let local_index = frame
                        .borrow()
                        .instructions()
                        .read_u8_from((frame.borrow().ip + 1) as usize);
                    frame.borrow_mut().ip += 1;
                    let var = Rc::clone(
                        self.stack
                            .get(frame.borrow().base_pointer + local_index as usize)
                            .unwrap(),
                    );
                    self.push(var);
                }
                Opcode::SetLocal => {
                    let local_index = frame
                        .borrow()
                        .instructions()
                        .read_u8_from((frame.borrow().ip + 1) as usize);
                    frame.borrow_mut().ip += 1;
                    self.stack[frame.borrow().base_pointer + local_index as usize] = self.pop();
                }
                Opcode::GetBuiltin => {
                    let builtin_index = frame
                        .borrow()
                        .instructions()
                        .read_u8_from(frame.borrow().ip as usize + 1);
                    frame.borrow_mut().ip += 1;
                    let name = BUILTINS[builtin_index as usize];
                    self.push(Object::BuiltinFunction(BuiltinFunction::new(name.into())).into());
                }
                Opcode::Closure => {
                    let const_index = frame
                        .borrow()
                        .instructions()
                        .read_u16_from(frame.borrow().ip as usize + 1);
                    let num_free = frame
                        .borrow()
                        .instructions()
                        .read_u8_from(frame.borrow().ip as usize + 3);
                    frame.borrow_mut().ip += 3;
                    self.push_closure(const_index, num_free as u16);
                }
                Opcode::GetFree => {
                    let free_index = frame
                        .borrow()
                        .instructions()
                        .read_u8_from(frame.borrow().ip as usize + 1);
                    frame.borrow_mut().ip += 1;
                    let current_closure = &frame.borrow().closure;
                    self.push(Rc::clone(&current_closure.free[free_index as usize]));
                }
                Opcode::CurrentClosure => {
                    let current_closure = frame.borrow().closure.to_owned();
                    self.push(Rc::new(Object::Closure(current_closure)));
                }
            }
        }
    }

    fn execute_binary_operation(&mut self, op: &Opcode) {
        let right = self.pop();
        let left = self.pop();
        if let Object::Integer(ref right) = *right {
            if let Object::Integer(ref left) = *left {
                return self.execute_binary_integer_operation(op, left, right);
            }
        }

        if let Object::Str(ref right) = *right {
            if let Object::Str(ref left) = *left {
                return self.execute_binary_string_operation(op, left, right);
            }
        }

        panic!(
            "unsupported types for binary operation: {} {}",
            left.kind(),
            right.kind()
        );
    }

    fn execute_binary_integer_operation(&mut self, op: &Opcode, left: &Integer, right: &Integer) {
        let result = match op {
            Opcode::Add => left.value + right.value,
            Opcode::Sub => left.value - right.value,
            Opcode::Mul => left.value * right.value,
            Opcode::Div => left.value / right.value,
            _ => panic!("unknown integer operator: {:?}", op),
        };
        self.push(Object::Integer(Integer::new(result)).into());
    }

    fn execute_binary_string_operation(&mut self, op: &Opcode, left: &Str, right: &Str) {
        let result = match op {
            Opcode::Add => format!("{}{}", left.value, right.value),
            _ => panic!("unknown string operator: {:?}", op),
        };
        self.push(Object::Str(Str::new(result)).into());
    }

    fn execute_comparison(&mut self, op: &Opcode) {
        let right = self.pop();
        let left = self.pop();

        if let Object::Integer(ref right) = *right {
            if let Object::Integer(ref left) = *left {
                let result = self.execute_integer_comparison(op, left, right);
                self.push(result);
                return;
            }
        }

        let result = match op {
            Opcode::Equal => self.native_bool_to_boolean_object(left == right),
            Opcode::NotEqual => self.native_bool_to_boolean_object(left != right),
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
        op: &Opcode,
        left: &Integer,
        right: &Integer,
    ) -> Rc<Object> {
        match op {
            Opcode::Equal => self.native_bool_to_boolean_object(left.value == right.value),
            Opcode::NotEqual => self.native_bool_to_boolean_object(left.value != right.value),
            Opcode::GreaterThan => self.native_bool_to_boolean_object(left.value > right.value),
            _ => panic!("unknown operator: {:?}", op),
        }
    }

    fn execute_bang_operator(&mut self) {
        let operand = self.pop();
        if operand == self.true_object {
            self.push(Rc::clone(&self.false_object));
        } else if operand == self.false_object || operand == self.null_object {
            self.push(Rc::clone(&self.true_object));
        } else {
            self.push(Rc::clone(&self.false_object));
        }
    }

    fn execute_minus_operator(&mut self) {
        let operand = self.pop();

        if let Object::Integer(ref integer) = *operand {
            self.push(Object::Integer(Integer::new(-integer.value)).into());
        } else {
            panic!("unsupported type for negation: {}", operand.kind());
        }
    }

    fn execute_index_expression(&mut self) {
        let index = self.pop();
        let left = self.pop();

        match *left {
            Object::Array(ref array) => self.execute_array_index(array, &index),
            Object::Hash(ref hash) => self.execute_hash_index(hash, &index),
            _ => panic!("index operator not supported: {}", left.kind()),
        }
    }

    fn execute_array_index(&mut self, left: &Array, index: &Object) {
        if let Object::Integer(integer) = index {
            let index = integer.value;
            if index < 0 || index > (left.elements.len() as i64 - 1) {
                self.push(Rc::clone(&self.null_object))
            } else {
                self.push(Rc::clone(&left.elements[index as usize]))
            }
        } else {
            self.push(Rc::clone(&self.null_object))
        }
    }

    fn execute_hash_index(&mut self, left: &Hash, key: &Object) {
        let hash_key = match *key {
            Object::Str(ref o) => o.hash_key(),
            Object::Integer(ref o) => o.hash_key(),
            Object::Boolean(ref o) => o.hash_key(),
            _ => return self.push(Rc::clone(&self.null_object)),
        };

        if let Some(object) = left.value.get(&hash_key) {
            self.push(Rc::clone(object));
        } else {
            self.push(Rc::clone(&self.null_object))
        }
    }

    fn execute_call(&mut self, num_args: u8) {
        let callee = Rc::clone(&self.stack[self.sp - 1 - num_args as usize]);
        match *callee {
            Object::Closure(ref closure) => self.call_closure(closure, num_args),
            Object::BuiltinFunction(ref func) => self.call_builtin(func, num_args),
            _ => panic!("calling non-closure and non-built-in"),
        }
    }

    fn call_closure(&mut self, closure: &Closure, num_args: u8) {
        if num_args != closure.func.num_parameters {
            panic!(
                "wrong number of arguments: want={}, got={}",
                closure.func.num_parameters, num_args
            );
        }
        let frame = Rc::new(RefCell::new(Frame::new(
            closure.to_owned(),
            self.sp - num_args as usize,
        )));
        self.push_frame(Rc::clone(&frame));
        self.sp = frame.borrow().base_pointer + closure.func.num_locals as usize;
    }

    fn call_builtin(&mut self, func: &BuiltinFunction, num_args: u8) {
        let mut args = vec![];
        let mut i = self.sp - num_args as usize;
        while i < self.sp {
            args.push(Rc::clone(&self.stack[i]));
            i += 1;
        }
        if let Some(result) = self.builtin.apply_function(&func.name, args) {
            self.push(result);
        } else {
            self.push(Object::Null(Null::default()).into());
        }
    }

    fn native_bool_to_boolean_object(&self, native: bool) -> Rc<Object> {
        match native {
            true => Rc::clone(&self.true_object),
            false => Rc::clone(&self.false_object),
        }
    }

    fn build_array(&self, start_index: usize, end_index: usize) -> Rc<Object> {
        let mut elements = Vec::with_capacity(end_index - start_index);

        let mut i = start_index;
        while i < end_index {
            elements.push(Rc::clone(&self.stack[i]));
            i += 1;
        }

        Object::Array(Array::new(elements)).into()
    }

    fn build_hash(&self, start_index: usize, end_index: usize) -> Rc<Object> {
        let mut members = HashMap::new();

        let mut i = start_index;
        while i < end_index {
            let key = &self.stack[i];
            let value = Rc::clone(&self.stack[i + 1]);

            let hash_key = match **key {
                Object::Str(ref o) => o.hash_key(),
                Object::Integer(ref o) => o.hash_key(),
                Object::Boolean(ref o) => o.hash_key(),
                _ => {
                    return Object::RuntimeError(RuntimeError::new(format!(
                        "only string, integer and boolean can be hash key, found {}",
                        key.kind()
                    )))
                    .into()
                }
            };

            members.insert(hash_key, value);

            i += 2;
        }

        Object::Hash(Hash::new(members)).into()
    }

    fn is_truthy(&self, object: &Rc<Object>) -> bool {
        match **object {
            Object::Boolean(ref o) => o.value.to_owned(),
            Object::Null(_) => false,
            _ => true,
        }
    }

    fn push_closure(&mut self, const_index: u16, num_free: u16) {
        let constant = self.constants.get(const_index as usize).unwrap().to_owned();
        if let Object::CompiledFunction(func) = constant {
            let mut free = vec![];
            let mut i = 0;
            while i < num_free {
                free.push(Rc::clone(
                    &self.stack[self.sp - num_free as usize + i as usize],
                ));
                i += 1;
            }
            self.sp -= num_free as usize;
            self.push(Object::Closure(Closure::new(func, free)).into());
        }
    }

    fn push(&mut self, object: Rc<Object>) {
        self.stack[self.sp] = object;
        self.sp += 1;
    }

    fn pop(&mut self) -> Rc<Object> {
        self.sp -= 1;
        Rc::clone(&self.stack[self.sp])
    }

    pub fn last_popped_stack_elem(&self) -> Rc<Object> {
        if let Some(object) = self.stack.get(self.sp) {
            Rc::clone(object)
        } else {
            Rc::clone(&self.null_object)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::panic;

    use crate::{
        compiler::Compiler,
        object::Object,
        test_helper::{
            parse, test_array_object, test_boolean_object, test_closure_object, test_error_object,
            test_hash_object, test_integer_object, test_null_object, test_string_object,
            ExpectedValue,
        },
    };

    use super::Vm;

    #[derive(Debug)]
    struct VmTestCase<'a>(&'a str, ExpectedValue<'a>);

    fn run_vm_tests(tests: &[VmTestCase]) {
        for test in tests.iter() {
            println!("{}", test.0);
            let program = parse(test.0);
            let mut compiler = Compiler::new();
            compiler.compile(&program);
            let mut vm = Vm::new(compiler.bytecode());
            vm.run();

            test_expected_object(&vm.last_popped_stack_elem(), &test.1)
        }
    }

    fn test_expected_object(actual: &Object, expected: &ExpectedValue) {
        match expected {
            ExpectedValue::Integer(e) => test_integer_object(actual, e.to_owned()),
            ExpectedValue::Boolean(e) => test_boolean_object(actual, e.to_owned()),
            ExpectedValue::String(e) => test_string_object(actual, e),
            ExpectedValue::Array(e) => test_array_object(actual, e),
            ExpectedValue::Hash(e) => test_hash_object(actual, e),
            ExpectedValue::Function(e) => test_closure_object(actual, e),
            ExpectedValue::Error(e) => test_error_object(actual, e),
            ExpectedValue::Null => test_null_object(actual),
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
            VmTestCase("!(if (false) { 5; })", ExpectedValue::Boolean(true)),
        ];

        run_vm_tests(&tests);
    }

    #[test]
    fn test_conditionals() {
        let tests = [
            VmTestCase("if (true) { 10 }", ExpectedValue::Integer(10)),
            VmTestCase("if (true) { 10 } else { 20 }", ExpectedValue::Integer(10)),
            VmTestCase("if (false) { 10 } else { 20 } ", ExpectedValue::Integer(20)),
            VmTestCase("if (1) { 10 }", ExpectedValue::Integer(10)),
            VmTestCase("if (1 < 2) { 10 }", ExpectedValue::Integer(10)),
            VmTestCase("if (1 < 2) { 10 } else { 20 }", ExpectedValue::Integer(10)),
            VmTestCase("if (1 > 2) { 10 } else { 20 }", ExpectedValue::Integer(20)),
            VmTestCase("if (1 > 2) { 10 }", ExpectedValue::Null),
            VmTestCase("if (false) { 10 }", ExpectedValue::Null),
            VmTestCase(
                "if ((if (false) { 10 })) { 10 } else { 20 }",
                ExpectedValue::Integer(20),
            ),
        ];

        run_vm_tests(&tests);
    }

    #[test]
    fn test_global_let_statements() {
        let tests = [
            VmTestCase("let one = 1; one", ExpectedValue::Integer(1)),
            VmTestCase(
                "let one = 1; let two = 2; one + two",
                ExpectedValue::Integer(3),
            ),
            VmTestCase(
                "let one = 1; let two = one + one; one + two",
                ExpectedValue::Integer(3),
            ),
        ];

        run_vm_tests(&tests);
    }

    #[test]
    fn test_string_expressions() {
        let tests = [
            VmTestCase(r#""monkey""#, ExpectedValue::String("monkey")),
            VmTestCase(r#""mon" + "key""#, ExpectedValue::String("monkey")),
            VmTestCase(
                r#""mon" + "key" + "banana""#,
                ExpectedValue::String("monkeybanana"),
            ),
        ];

        run_vm_tests(&tests);
    }

    #[test]
    fn test_array_literals() {
        let tests = [
            VmTestCase("[]", ExpectedValue::Array(vec![])),
            VmTestCase("[1, 2, 3]", ExpectedValue::Array(vec![1, 2, 3])),
            VmTestCase(
                "[1 + 2, 3 * 4 , 5 + 6]",
                ExpectedValue::Array(vec![3, 12, 11]),
            ),
        ];

        run_vm_tests(&tests);
    }

    #[test]
    fn test_hash_literals() {
        let tests = [
            VmTestCase("{}", ExpectedValue::Hash(vec![])),
            VmTestCase("{1: 2, 2: 3}", ExpectedValue::Hash(vec![(1, 2), (2, 3)])),
            VmTestCase(
                "{1 + 1: 2 * 2, 3 + 3: 4 * 4}",
                ExpectedValue::Hash(vec![(2, 4), (6, 16)]),
            ),
        ];

        run_vm_tests(&tests);
    }

    #[test]
    fn test_index_expressions() {
        let tests = [
            VmTestCase("[1, 2, 3][1]", ExpectedValue::Integer(2)),
            VmTestCase("[1, 2, 3][0 + 2]", ExpectedValue::Integer(3)),
            VmTestCase("[[1, 1, 1]][0][0]", ExpectedValue::Integer(1)),
            VmTestCase("[][0]", ExpectedValue::Null),
            VmTestCase("[1, 2, 3][99]", ExpectedValue::Null),
            VmTestCase("[1][-1]", ExpectedValue::Null),
            VmTestCase("{1: 1, 2: 2}[1]", ExpectedValue::Integer(1)),
            VmTestCase("{1: 1, 2: 2}[2]", ExpectedValue::Integer(2)),
            VmTestCase("{1: 1}[0]", ExpectedValue::Null),
            VmTestCase("{}[0]", ExpectedValue::Null),
        ];

        run_vm_tests(&tests);
    }

    #[test]
    fn test_calling_function_without_arguments() {
        let tests = [
            VmTestCase(
                "let fivePlusTen = fn() { 5 + 10; };fivePlusTen();",
                ExpectedValue::Integer(15),
            ),
            VmTestCase(
                "let a = fn() { 1 };
                let b = fn() { a() + 1 };
                let c = fn() { b() + 1 };
                c();",
                ExpectedValue::Integer(3),
            ),
        ];

        run_vm_tests(&tests);
    }

    #[test]
    fn test_functions_with_return_statment() {
        let tests = [
            VmTestCase(
                "let earlyExit = fn() { return 99; 100; };earlyExit();",
                ExpectedValue::Integer(99),
            ),
            VmTestCase(
                "let earlyExit = fn() { return 99; return 100; };earlyExit();",
                ExpectedValue::Integer(99),
            ),
        ];

        run_vm_tests(&tests);
    }

    #[test]
    fn test_function_without_return_value() {
        let tests = [
            VmTestCase("let noReturn = fn() { };noReturn();", ExpectedValue::Null),
            VmTestCase(
                "let noReturn = fn() { };
           let noReturnTwo = fn() { noReturn(); };
           noReturn();
           noReturnTwo();",
                ExpectedValue::Null,
            ),
        ];

        run_vm_tests(&tests);
    }

    #[test]
    fn test_first_class_functions() {
        let tests = [
            VmTestCase(
                "let returnsOne = fn() { 1; };
            let returnsOneReturner = fn() { returnsOne; };
            returnsOneReturner()();",
                ExpectedValue::Integer(1),
            ),
            VmTestCase(
                "let returnsOneReturner = fn() {
                    let returnsOne = fn() { 1; };
                    returnsOne;
                };
                returnsOneReturner()();",
                ExpectedValue::Integer(1),
            ),
        ];

        run_vm_tests(&tests);
    }

    #[test]
    fn test_calling_function_with_bingdings() {
        let tests = [
            VmTestCase(
                "let one = fn() { let one = 1; one }; one();",
                ExpectedValue::Integer(1),
            ),
            VmTestCase(
                "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; }; oneAndTwo();",
                ExpectedValue::Integer(3),
            ),
            VmTestCase(
                "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
                let threeAndFour = fn() { let three = 3; let four = 4; three + four; };
                oneAndTwo() + threeAndFour();",
                ExpectedValue::Integer(10),
            ),
            VmTestCase(
                "let firstFoobar = fn() { let foobar = 50; foobar; };
                let secondFoobar = fn() { let foobar = 100; foobar; };
                firstFoobar() + secondFoobar();",
                ExpectedValue::Integer(150),
            ),
            VmTestCase(
                "let globalSeed = 50;
                let minusOne = fn() {
                    let num = 1;
                    globalSeed - num;
                }
                let minusTwo = fn() {
                    let num = 2;
                    globalSeed - num;
                }
                minusOne() + minusTwo();",
                ExpectedValue::Integer(97),
            ),
        ];

        run_vm_tests(&tests);
    }

    #[test]
    fn test_calling_functions_with_arguments_and_bindings() {
        let tests = [
            // VmTestCase(
            //     "let identity = fn(a) { a; }; identity(4);",
            //     ExpectedValue::Integer(4),
            // ),
            // VmTestCase(
            //     "let sum = fn(a, b) { a + b; }; sum(1, 2);",
            //     ExpectedValue::Integer(3),
            // ),
            // VmTestCase(
            //     "let sum = fn(a, b) {
            //         let c = a + b;
            //         c;
            //     };
            //     sum(1, 2);",
            //     ExpectedValue::Integer(3),
            // ),
            // VmTestCase(
            //     "let sum = fn(a, b) {
            //         let c = a + b;
            //         c;
            //     };
            //     sum(1, 2) + sum(3, 4);",
            //     ExpectedValue::Integer(10),
            // ),
            VmTestCase(
                "let sum = fn(a, b) {
                    let c = a + b;
                    c;
                };
                let outer = fn() {
                    sum(1, 2) + sum(3, 4);
                };
                outer();",
                ExpectedValue::Integer(10),
            ),
            // VmTestCase(
            //     "let globalNum = 10;
            //     let sum = fn(a, b) {
            //         let c = a + b;
            //         c + globalNum;
            //     };
            //     let outer = fn() {
            //         sum(1, 2) + sum(3, 4) + globalNum;
            //     };
            //     outer() + globalNum;",
            //     ExpectedValue::Integer(50),
            // ),
        ];

        run_vm_tests(&tests);
    }

    #[test]
    fn test_calling_functions_with_wrong_arguments() {
        let tests = [
            (
                "fn() { 1; }(1);",
                "wrong number of arguments: want=0, got=1",
            ),
            (
                "fn(a) { a; }();",
                "wrong number of arguments: want=1, got=0",
            ),
            (
                "fn(a, b) { a + b; }(1);",
                "wrong number of arguments: want=2, got=1",
            ),
        ];

        for (input, expected) in tests.iter() {
            let result = panic::catch_unwind(|| {
                let program = parse(input);
                let mut compiler = Compiler::new();
                compiler.compile(&program);
                let mut vm = Vm::new(compiler.bytecode());
                vm.run();
            });
            if let Err(error) = result {
                assert_eq!(*error.downcast::<String>().unwrap(), *expected);
            } else {
                panic!("should panic");
            }
        }
    }

    #[test]
    fn test_builtin_functions() {
        let tests = [
            VmTestCase(r#"len("")"#, ExpectedValue::Integer(0)),
            VmTestCase(r#"len("four")"#, ExpectedValue::Integer(4)),
            VmTestCase(r#"len("hello world")"#, ExpectedValue::Integer(11)),
            VmTestCase(
                r#"len(1)"#,
                ExpectedValue::Error("argument to `len` not supported, got INTEGER"),
            ),
            VmTestCase(
                r#"len("one", "two")"#,
                ExpectedValue::Error("wrong number of arguments. got=2, want=1"),
            ),
            VmTestCase(r#"len([1, 2, 3])"#, ExpectedValue::Integer(3)),
            VmTestCase(r#"len([])"#, ExpectedValue::Integer(0)),
            VmTestCase(r#"puts("hello", "world!")"#, ExpectedValue::Null),
            VmTestCase(r#"first([1, 2, 3])"#, ExpectedValue::Integer(1)),
            VmTestCase(r#"first([])"#, ExpectedValue::Null),
            VmTestCase(
                r#"first(1)"#,
                ExpectedValue::Error("argument to `first` must be ARRAY, got INTEGER"),
            ),
            VmTestCase(r#"last([1, 2, 3])"#, ExpectedValue::Integer(3)),
            VmTestCase(r#"last([])"#, ExpectedValue::Null),
            VmTestCase(
                r#"last(1)"#,
                ExpectedValue::Error("argument to `last` must be ARRAY, got INTEGER"),
            ),
            VmTestCase(r#"rest([1, 2, 3])"#, ExpectedValue::Array(vec![2, 3])),
            VmTestCase(r#"rest([])"#, ExpectedValue::Null),
            VmTestCase(r#"push([], 1)"#, ExpectedValue::Array(vec![1])),
            VmTestCase(
                r#"push(1, 1)"#,
                ExpectedValue::Error("first argument to `push` must be ARRAY, got INTEGER"),
            ),
        ];

        run_vm_tests(&tests);
    }

    #[test]
    fn test_closures() {
        let tests = [VmTestCase(
            "let newClosure = fn(a) {
                    fn() { a; };
                };
                let closure = newClosure(99);
                closure();
                ",
            ExpectedValue::Integer(99),
        )];

        run_vm_tests(&tests);
    }

    #[test]
    fn test_recursive_functions() {
        let tests = [
            VmTestCase(
                "let countDown = fn(x) {
               if (x == 0) {
                   return 0;
               } else {
                   countDown(x - 1);
               }
            };
            countDown(1);",
                ExpectedValue::Integer(0),
            ),
            VmTestCase(
                "let countDown = fn(x) {
                    if (x == 0) {
                        return 0;
                    } else {
                        countDown(x - 1);
                    }
                };
                let wrapper = fn() {
                    countDown(1);
                };
                wrapper();",
                ExpectedValue::Integer(0),
            ),
            VmTestCase(
                "let wrapper = fn() {
                    let countDown = fn(x) {
                        if (x == 0) {
                           return 0;
                        } else {
                           countDown(x - 1);
                        } 
                    };
                    countDown(1);
                };
                wrapper();",
                ExpectedValue::Integer(0),
            ),
        ];

        run_vm_tests(&tests);
    }
}
