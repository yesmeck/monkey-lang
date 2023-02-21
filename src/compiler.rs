use crate::{
    ast::{
        BooleanExpression, Expression, ExpressionStatement, InfixExpression, IntegerLiteral,
        LetStatement, Program, ReturnStatement, Statement, PrefixExpression,
    },
    code::{make, Instructions, OpCode},
    object::{Boolean, Integer, Object},
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
            Statement::Let(node) => self.compile_let_statement(node),
            Statement::Return(node) => self.compile_return_statement(node),
            Statement::Expression(node) => self.compile_expression_statement(node),
        }
    }

    fn compile_let_statement(&mut self, node: &LetStatement) {}

    fn compile_return_statement(&mut self, node: &ReturnStatement) {}

    fn compile_expression_statement(&mut self, node: &ExpressionStatement) {
        self.compile_expression(&node.expression);
        self.emit(OpCode::OpPop, vec![]);
    }

    fn compile_expression(&mut self, node: &Expression) {
        match node {
            Expression::IntegerLiteral(node) => self.compile_integer_literal(node),
            Expression::Boolean(node) => self.compiler_boolean_expression(node),
            Expression::Infix(node) => self.compile_infix_expression(node),
            Expression::Prefix(node) => self.compile_prefix_expression(node),
            _ => {}
        }
    }

    fn compile_infix_expression(&mut self, node: &InfixExpression) {
        if &node.operator == "<" {
            self.compile_expression(&node.right);
            self.compile_expression(&node.left);
        } else {
            self.compile_expression(&node.left);
            self.compile_expression(&node.right);
        }

        match node.operator.as_str() {
            "+" => self.emit(OpCode::OpAdd, vec![]),
            "-" => self.emit(OpCode::OpSub, vec![]),
            "*" => self.emit(OpCode::OpMul, vec![]),
            "/" => self.emit(OpCode::OpDiv, vec![]),
            ">" | "<" => self.emit(OpCode::OpGreaterThan, vec![]),
            "==" => self.emit(OpCode::OpEqual, vec![]),
            "!=" => self.emit(OpCode::OpNotEqual, vec![]),
            _ => panic!("unknown operator {}", node.operator),
        };
    }

    fn compile_prefix_expression(&mut self, node: &PrefixExpression) {
        self.compile_expression(&node.right);
        match node.operator.as_str() {
            "-" => self.emit(OpCode::OpMinus, vec![]),
            "!" => self.emit(OpCode::OpBang, vec![]),
            _ => panic!("unknown operator {}", node.operator),
        };
    }

    fn compile_integer_literal(&mut self, node: &IntegerLiteral) {
        let integer = Object::Integer(Integer::new(node.value.to_owned()));
        let const_pos = self.add_constant(integer);
        self.emit(OpCode::OpConstant, vec![const_pos]);
    }

    fn compiler_boolean_expression(&mut self, node: &BooleanExpression) {
        match node.value {
            true => self.emit(OpCode::OpTrue, vec![]),
            false => self.emit(OpCode::OpFalse, vec![]),
        };
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

    fn emit(&mut self, op: OpCode, operands: Vec<u16>) -> usize {
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
        test_helper::{parse, test_boolean_object, test_integer_object, ExpectedValue},
    };

    use super::*;

    #[derive(Debug)]
    struct CompilerTestCase<'a>(&'a str, Vec<ExpectedValue>, Vec<Instructions>);

    fn run_compiler_tests(tests: &[CompilerTestCase]) {
        for test in tests.iter() {
            let program = parse(test.0.into());
            let mut compiler = Compiler::default();
            compiler.compile(&program);
            test_contants(&compiler.constants, &test.1);
            test_instructions(&compiler.instructions, &test.2);
        }
    }

    fn test_contants(actual: &Vec<Object>, expected: &Vec<ExpectedValue>) {
        assert_eq!(actual.len(), expected.len());
        for (i, constant) in expected.iter().enumerate() {
            match constant {
                ExpectedValue::Integer(e) => test_integer_object(&actual[i], *e),
                ExpectedValue::Boolean(e) => test_boolean_object(&actual[i], *e),
            }
        }
    }

    fn test_instructions(actual: &Instructions, expected: &[Instructions]) {
        let concated = Instructions(expected.iter().flat_map(|i| i.0.to_owned()).collect());
        assert_eq!(&concated, actual);
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = [
            CompilerTestCase(
                "1 + 2",
                vec![ExpectedValue::Integer(1), ExpectedValue::Integer(2)],
                vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpAdd, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            ),
            CompilerTestCase(
                "1; 2",
                vec![ExpectedValue::Integer(1), ExpectedValue::Integer(2)],
                vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpPop, vec![]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpPop, vec![]),
                ],
            ),
            CompilerTestCase(
                "1 - 2",
                vec![ExpectedValue::Integer(1), ExpectedValue::Integer(2)],
                vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpSub, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            ),
            CompilerTestCase(
                "1 * 2",
                vec![ExpectedValue::Integer(1), ExpectedValue::Integer(2)],
                vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpMul, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            ),
            CompilerTestCase(
                "2 / 1",
                vec![ExpectedValue::Integer(2), ExpectedValue::Integer(1)],
                vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpDiv, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            ),
            CompilerTestCase(
                "-1",
                vec![ExpectedValue::Integer(1)],
                vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpMinus, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            ),
        ];

        run_compiler_tests(&tests);
    }

    #[test]
    fn test_boolean_expression() {
        let tests = [
            CompilerTestCase(
                "true",
                vec![],
                vec![make(OpCode::OpTrue, vec![]), make(OpCode::OpPop, vec![])],
            ),
            CompilerTestCase(
                "false",
                vec![],
                vec![make(OpCode::OpFalse, vec![]), make(OpCode::OpPop, vec![])],
            ),
            CompilerTestCase(
                "1 > 2",
                vec![ExpectedValue::Integer(1), ExpectedValue::Integer(2)],
                vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpGreaterThan, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            ),
            CompilerTestCase(
                "1 < 2",
                vec![ExpectedValue::Integer(2), ExpectedValue::Integer(1)],
                vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpGreaterThan, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            ),
            CompilerTestCase(
                "1 == 2",
                vec![ExpectedValue::Integer(1), ExpectedValue::Integer(2)],
                vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpEqual, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            ),
            CompilerTestCase(
                "1 != 2",
                vec![ExpectedValue::Integer(1), ExpectedValue::Integer(2)],
                vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpNotEqual, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            ),
            CompilerTestCase(
                "true == false",
                vec![],
                vec![
                    make(OpCode::OpTrue, vec![]),
                    make(OpCode::OpFalse, vec![]),
                    make(OpCode::OpEqual, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            ),
            CompilerTestCase(
                "true != false",
                vec![],
                vec![
                    make(OpCode::OpTrue, vec![]),
                    make(OpCode::OpFalse, vec![]),
                    make(OpCode::OpNotEqual, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            ),
            CompilerTestCase(
                "!true",
                vec![],
                vec![
                    make(OpCode::OpTrue, vec![]),
                    make(OpCode::OpBang, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            ),
        ];

        run_compiler_tests(&tests);
    }
}
