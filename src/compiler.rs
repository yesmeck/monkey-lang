use crate::{
    ast::{
        Expression, InfixExpression, IntegerLiteral, LetStatement, Program, ReturnStatement,
        Statement,
    },
    code::{make, Instructions, OpCode},
    object::{Integer, Object},
};

#[derive(Debug)]
pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

#[derive(Debug, Default)]
pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
}

impl Compiler {
    pub fn compile(&mut self, node: &Program) {
        self.compile_program(node);
    }

    fn compile_program(&mut self, node: &Program) {
        for stmt in node.statements.iter() {
            self.compile_statment(stmt);
        }
    }

    fn compile_statment(&mut self, node: &Statement) {
        match node {
            Statement::Let(node) => self.compile_let_statement(&node),
            Statement::Return(node) => self.compile_return_statement(&node),
            Statement::Expression(node) => self.compile_expression(&node.expression),
        }
    }

    fn compile_let_statement(&mut self, node: &LetStatement) {}

    fn compile_return_statement(&mut self, node: &ReturnStatement) {}

    fn compile_expression(&mut self, node: &Expression) {
        match node {
            Expression::IntegerLiteral(node) => self.compile_integer_literal(&node),
            Expression::Infix(node) => self.compile_infix_expression(&node),
            _ => {}
        }
    }

    fn compile_infix_expression(&mut self, node: &InfixExpression) {
        self.compile_expression(&node.left);
        self.compile_expression(&node.right);

        match node.operator.as_str() {
            "+" => self.emit(&OpCode::OpAdd, vec![]),
            _ => panic!("unknown operator {}", node.operator),
        };
    }

    fn compile_integer_literal(&mut self, node: &IntegerLiteral) {
        let integer = Object::Integer(Integer::new(node.value.to_owned()));
        let const_pos = self.add_constant(integer);
        self.emit(&OpCode::OpConstant, vec![const_pos]);
    }

    fn add_constant(&mut self, object: Object) -> u16 {
        self.constants.push(object);
        (self.constants.len() - 1).try_into().unwrap()
    }

    fn add_instructions(&mut self, ins: &mut Instructions) -> usize {
        let new_ins_pos = self.instructions.0.len();
        self.instructions.append(ins);
        new_ins_pos
    }

    fn emit(&mut self, op: &OpCode, operands: Vec<u16>) -> usize {
        let mut ins = make(op, operands);
        self.add_instructions(&mut ins)
    }

    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.to_owned(),
            constants: self.constants.to_owned(),
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        code::{make, Instructions, OpCode},
        test_helper::{parse, test_integer_object},
    };

    use super::*;

    #[derive(Debug)]
    struct CompilerTestCase<'a> {
        input: &'a str,
        expected_constants: Vec<i64>,
        expected_instructions: Vec<Instructions>,
    }

    fn run_compiler_tests(tests: Vec<CompilerTestCase>) {
        for test in tests.iter() {
            let program = parse(test.input.into());
            let mut compiler = Compiler::default();
            compiler.compile(&program);
            test_contants(&compiler.constants, &test.expected_constants);
            test_instructions(&compiler.instructions, &test.expected_instructions);
        }
    }

    fn test_contants(actual: &Vec<Object>, expected: &Vec<i64>) {
        assert_eq!(expected.len(), actual.len());
        for (i, constant) in expected.iter().enumerate() {
            test_integer_object(&actual[i], *constant);
        }
    }

    fn test_instructions(actual: &Instructions, expected: &[Instructions]) {
        let concated = Instructions(expected.iter().flat_map(|i| i.0.to_owned()).collect());
        assert_eq!(&concated, actual);
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![CompilerTestCase {
            input: "1 + 2",
            expected_constants: vec![1, 2],
            expected_instructions: vec![
                make(&OpCode::OpConstant, vec![0]),
                make(&OpCode::OpConstant, vec![1]),
                make(&OpCode::OpAdd, vec![]),
            ],
        }];

        run_compiler_tests(tests);
    }
}
