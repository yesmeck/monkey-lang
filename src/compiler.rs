use std::ops::Range;

use crate::{
    ast::{
        BlockStatement, BooleanExpression, Expression, ExpressionStatement, IfExpression,
        InfixExpression, IntegerLiteral, LetStatement, PrefixExpression, Program, ReturnStatement,
        Statement,
    },
    code::{make, Instructions, Opcode},
    object::{Integer, Object},
};

#[derive(Debug)]
pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

#[derive(Debug, Clone)]
struct EmittedInstruction {
    opcode: Opcode,
    position: usize,
}

#[derive(Debug, Default)]
pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,

    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>,
}

impl Compiler {
    pub fn compile(&mut self, node: &Program) {
        self.compile_program(node);
    }

    fn compile_program(&mut self, node: &Program) {
        for stmt in node.statements.iter() {
            self.compile_statement(stmt);
        }
    }

    fn compile_block_statsment(&mut self, node: &BlockStatement) {
        for stmt in node.statements.iter() {
            self.compile_statement(stmt);
        }
    }

    fn compile_statement(&mut self, node: &Statement) {
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
        self.emit(Opcode::OpPop, vec![]);
    }

    fn compile_expression(&mut self, node: &Expression) {
        match node {
            Expression::IntegerLiteral(node) => self.compile_integer_literal(node),
            Expression::StringLiteral(_) => todo!(),
            Expression::NullLiteral(_) => todo!(),
            Expression::ArrayLiteral(_) => todo!(),
            Expression::HashLiteral(_) => todo!(),
            Expression::FunctionLiteral(_) => todo!(),
            Expression::Boolean(node) => self.compiler_boolean_expression(node),
            Expression::Identifier(_) => todo!(),
            Expression::Infix(node) => self.compile_infix_expression(node),
            Expression::Prefix(node) => self.compile_prefix_expression(node),
            Expression::If(node) => self.compile_if_expression(node),
            Expression::Call(_) => todo!(),
            Expression::Index(_) => todo!(),
            Expression::MacroLiteral(_) => unreachable!(),
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
            "+" => self.emit(Opcode::OpAdd, vec![]),
            "-" => self.emit(Opcode::OpSub, vec![]),
            "*" => self.emit(Opcode::OpMul, vec![]),
            "/" => self.emit(Opcode::OpDiv, vec![]),
            ">" | "<" => self.emit(Opcode::OpGreaterThan, vec![]),
            "==" => self.emit(Opcode::OpEqual, vec![]),
            "!=" => self.emit(Opcode::OpNotEqual, vec![]),
            _ => panic!("unknown operator {}", node.operator),
        };
    }

    fn compile_prefix_expression(&mut self, node: &PrefixExpression) {
        self.compile_expression(&node.right);
        match node.operator.as_str() {
            "-" => self.emit(Opcode::OpMinus, vec![]),
            "!" => self.emit(Opcode::OpBang, vec![]),
            _ => panic!("unknown operator {}", node.operator),
        };
    }

    fn compile_if_expression(&mut self, node: &IfExpression) {
        self.compile_expression(&node.condition);

        let jump_not_truth_pos = self.emit(Opcode::OpJumpNotTruth, vec![9999]);

        self.compile_block_statsment(&node.consequence);

        if self.last_instruction_is_pop() {
            self.remove_last_pop();
        }

        let jump_pos = self.emit(Opcode::OpJump, vec![9999]);

        let after_consequence_pos = self.instructions.len();
        self.change_operand(jump_not_truth_pos, after_consequence_pos as u16);

        if let Some(ref alternative) = node.alternative {
            self.compile_block_statsment(alternative);

            if self.last_instruction_is_pop() {
                self.remove_last_pop();
            }
        } else {
            self.emit(Opcode::OpNull, vec![]);
        }

        let afte_alternative_pos = self.instructions.len();
        self.change_operand(jump_pos, afte_alternative_pos as u16);
    }

    fn compile_integer_literal(&mut self, node: &IntegerLiteral) {
        let integer = Object::Integer(Integer::new(node.value.to_owned()));
        let const_pos = self.add_constant(integer);
        self.emit(Opcode::OpConstant, vec![const_pos]);
    }

    fn compiler_boolean_expression(&mut self, node: &BooleanExpression) {
        match node.value {
            true => self.emit(Opcode::OpTrue, vec![]),
            false => self.emit(Opcode::OpFalse, vec![]),
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

    fn emit(&mut self, op: Opcode, operands: Vec<u16>) -> usize {
        let mut ins = make(op.to_owned(), operands);
        let pos = self.add_instructions(&mut ins);
        self.set_last_instruction(op, pos);
        pos
    }

    fn set_last_instruction(&mut self, opcode: Opcode, position: usize) {
        self.previous_instruction = self.last_instruction.to_owned();
        self.last_instruction = Some(EmittedInstruction { opcode, position });
    }

    fn last_instruction_is_pop(&mut self) -> bool {
        if let Some(ref last_instruction) = self.last_instruction {
            last_instruction.opcode == Opcode::OpPop
        } else {
            false
        }
    }

    fn remove_last_pop(&mut self) {
        if let Some(ref last_instruction) = self.last_instruction {
            self.instructions = self.instructions.take_range(Range {
                start: 0,
                end: last_instruction.position,
            });
            self.last_instruction = self.previous_instruction.to_owned();
        }
    }

    fn replace_instruction(&mut self, pos: usize, new_instruction: Instructions) {
        for (i, b) in new_instruction.0.iter().enumerate() {
            self.instructions.0[pos + i] = new_instruction.0[i];
        }
    }

    fn change_operand(&mut self, pos: usize, operand: u16) {
        let op = Opcode::from(self.instructions.0[pos]);
        let new_instruction = make(op, vec![operand]);
        self.replace_instruction(pos, new_instruction);
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
        code::{make, Instructions, Opcode},
        test_helper::{
            parse, test_boolean_object, test_integer_object, test_null_object, ExpectedValue,
        },
    };

    use super::*;

    #[derive(Debug)]
    struct CompilerTestCase<'a>(&'a str, Vec<ExpectedValue>, Vec<Instructions>);

    fn run_compiler_tests(tests: &[CompilerTestCase]) {
        for test in tests.iter() {
            let program = parse(test.0);
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
                ExpectedValue::Null => test_null_object(&actual[i]),
            }
        }
    }

    fn test_instructions(actual: &Instructions, expected: &[Instructions]) {
        let concated = Instructions(expected.iter().flat_map(|i| i.0.to_owned()).collect());
        assert_eq!(
            actual, &concated,
            "\nwrong instructions length.\nwant=\n{}got=\n{}",
            concated, actual
        );
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = [
            CompilerTestCase(
                "1 + 2",
                vec![ExpectedValue::Integer(1), ExpectedValue::Integer(2)],
                vec![
                    make(Opcode::OpConstant, vec![0]),
                    make(Opcode::OpConstant, vec![1]),
                    make(Opcode::OpAdd, vec![]),
                    make(Opcode::OpPop, vec![]),
                ],
            ),
            CompilerTestCase(
                "1; 2",
                vec![ExpectedValue::Integer(1), ExpectedValue::Integer(2)],
                vec![
                    make(Opcode::OpConstant, vec![0]),
                    make(Opcode::OpPop, vec![]),
                    make(Opcode::OpConstant, vec![1]),
                    make(Opcode::OpPop, vec![]),
                ],
            ),
            CompilerTestCase(
                "1 - 2",
                vec![ExpectedValue::Integer(1), ExpectedValue::Integer(2)],
                vec![
                    make(Opcode::OpConstant, vec![0]),
                    make(Opcode::OpConstant, vec![1]),
                    make(Opcode::OpSub, vec![]),
                    make(Opcode::OpPop, vec![]),
                ],
            ),
            CompilerTestCase(
                "1 * 2",
                vec![ExpectedValue::Integer(1), ExpectedValue::Integer(2)],
                vec![
                    make(Opcode::OpConstant, vec![0]),
                    make(Opcode::OpConstant, vec![1]),
                    make(Opcode::OpMul, vec![]),
                    make(Opcode::OpPop, vec![]),
                ],
            ),
            CompilerTestCase(
                "2 / 1",
                vec![ExpectedValue::Integer(2), ExpectedValue::Integer(1)],
                vec![
                    make(Opcode::OpConstant, vec![0]),
                    make(Opcode::OpConstant, vec![1]),
                    make(Opcode::OpDiv, vec![]),
                    make(Opcode::OpPop, vec![]),
                ],
            ),
            CompilerTestCase(
                "-1",
                vec![ExpectedValue::Integer(1)],
                vec![
                    make(Opcode::OpConstant, vec![0]),
                    make(Opcode::OpMinus, vec![]),
                    make(Opcode::OpPop, vec![]),
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
                vec![make(Opcode::OpTrue, vec![]), make(Opcode::OpPop, vec![])],
            ),
            CompilerTestCase(
                "false",
                vec![],
                vec![make(Opcode::OpFalse, vec![]), make(Opcode::OpPop, vec![])],
            ),
            CompilerTestCase(
                "1 > 2",
                vec![ExpectedValue::Integer(1), ExpectedValue::Integer(2)],
                vec![
                    make(Opcode::OpConstant, vec![0]),
                    make(Opcode::OpConstant, vec![1]),
                    make(Opcode::OpGreaterThan, vec![]),
                    make(Opcode::OpPop, vec![]),
                ],
            ),
            CompilerTestCase(
                "1 < 2",
                vec![ExpectedValue::Integer(2), ExpectedValue::Integer(1)],
                vec![
                    make(Opcode::OpConstant, vec![0]),
                    make(Opcode::OpConstant, vec![1]),
                    make(Opcode::OpGreaterThan, vec![]),
                    make(Opcode::OpPop, vec![]),
                ],
            ),
            CompilerTestCase(
                "1 == 2",
                vec![ExpectedValue::Integer(1), ExpectedValue::Integer(2)],
                vec![
                    make(Opcode::OpConstant, vec![0]),
                    make(Opcode::OpConstant, vec![1]),
                    make(Opcode::OpEqual, vec![]),
                    make(Opcode::OpPop, vec![]),
                ],
            ),
            CompilerTestCase(
                "1 != 2",
                vec![ExpectedValue::Integer(1), ExpectedValue::Integer(2)],
                vec![
                    make(Opcode::OpConstant, vec![0]),
                    make(Opcode::OpConstant, vec![1]),
                    make(Opcode::OpNotEqual, vec![]),
                    make(Opcode::OpPop, vec![]),
                ],
            ),
            CompilerTestCase(
                "true == false",
                vec![],
                vec![
                    make(Opcode::OpTrue, vec![]),
                    make(Opcode::OpFalse, vec![]),
                    make(Opcode::OpEqual, vec![]),
                    make(Opcode::OpPop, vec![]),
                ],
            ),
            CompilerTestCase(
                "true != false",
                vec![],
                vec![
                    make(Opcode::OpTrue, vec![]),
                    make(Opcode::OpFalse, vec![]),
                    make(Opcode::OpNotEqual, vec![]),
                    make(Opcode::OpPop, vec![]),
                ],
            ),
            CompilerTestCase(
                "!true",
                vec![],
                vec![
                    make(Opcode::OpTrue, vec![]),
                    make(Opcode::OpBang, vec![]),
                    make(Opcode::OpPop, vec![]),
                ],
            ),
        ];

        run_compiler_tests(&tests);
    }

    #[test]
    fn test_conditionals() {
        let tests = [
            CompilerTestCase(
                "if (true) { 10 }; 3333;",
                vec![ExpectedValue::Integer(10), ExpectedValue::Integer(3333)],
                vec![
                    // 0000
                    make(Opcode::OpTrue, vec![]),
                    // 0001
                    make(Opcode::OpJumpNotTruth, vec![10]),
                    // 0004
                    make(Opcode::OpConstant, vec![0]),
                    // 0007
                    make(Opcode::OpJump, vec![11]),
                    // 0010
                    make(Opcode::OpNull, vec![]),
                    // 0011
                    make(Opcode::OpPop, vec![]),
                    // 0012
                    make(Opcode::OpConstant, vec![1]),
                    // 0015
                    make(Opcode::OpPop, vec![]),
                ],
            ),
            CompilerTestCase(
                "if (true) { 10 } else { 20 }; 3333;",
                vec![
                    ExpectedValue::Integer(10),
                    ExpectedValue::Integer(20),
                    ExpectedValue::Integer(3333),
                ],
                vec![
                    // 0000
                    make(Opcode::OpTrue, vec![]),
                    // 0001
                    make(Opcode::OpJumpNotTruth, vec![10]),
                    // 0004
                    make(Opcode::OpConstant, vec![0]),
                    // 0007
                    make(Opcode::OpJump, vec![13]),
                    // 00010
                    make(Opcode::OpConstant, vec![1]),
                    // 00013
                    make(Opcode::OpPop, vec![]),
                    // 00014
                    make(Opcode::OpConstant, vec![2]),
                    // 00017
                    make(Opcode::OpPop, vec![]),
                ],
            ),
        ];

        run_compiler_tests(&tests);
    }
}
