use std::ops::Range;

use crate::{
    ast::{
        ArrayLiteral, BlockStatement, BooleanExpression, Expression, ExpressionStatement,
        FunctionLiteral, HashLiteral, IfExpression, IndexExpression, InfixExpression,
        IntegerLiteral, LetStatement, PrefixExpression, Program, ReturnStatement, Statement,
        StringLiteral, CallExpression,
    },
    code::{Instructions, Opcode},
    object::{CompiledFunction, Integer, Object, Str},
    symbol_table::SymbolTable,
};

#[derive(Debug)]
pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

#[derive(Debug, Clone)]
pub struct EmittedInstruction {
    opcode: Opcode,
    position: usize,
}

#[derive(Debug, Default)]
pub struct CompilationScope {
    pub instructions: Instructions,
    pub last_instruction: Option<EmittedInstruction>,
    pub previous_instruction: Option<EmittedInstruction>,
}

#[derive(Debug)]
pub struct Compiler {
    pub constants: Vec<Object>,

    pub scopes: Vec<CompilationScope>,
    pub scope_index: usize,

    pub symbol_table: SymbolTable,
}

impl Compiler {
    pub fn new() -> Self {
        let main_scope = CompilationScope::default();

        Self {
            constants: vec![],
            symbol_table: SymbolTable::default(),

            scopes: vec![main_scope],
            scope_index: 0,
        }
    }

    pub fn from(other: Self) -> Self {
        Self {
            symbol_table: other.symbol_table,
            constants: other.constants,

            ..Self::new()
        }
    }

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

    fn compile_let_statement(&mut self, node: &LetStatement) {
        self.compile_expression(&node.value);
        let symbol = self.symbol_table.define(node.name.value.as_str());
        let symbol_index = symbol.index;
        self.emit(Opcode::SetGlobal, vec![symbol_index]);
    }

    fn compile_return_statement(&mut self, node: &ReturnStatement) {
        self.compile_expression(&node.return_value);
        self.emit(Opcode::ReturnValue, vec![]);
    }

    fn compile_expression_statement(&mut self, node: &ExpressionStatement) {
        self.compile_expression(&node.expression);
        self.emit(Opcode::Pop, vec![]);
    }

    fn compile_expression(&mut self, node: &Expression) {
        match node {
            Expression::IntegerLiteral(node) => self.compile_integer_literal(node),
            Expression::StringLiteral(node) => self.compile_string_literal(node),
            Expression::NullLiteral(_) => todo!(),
            Expression::ArrayLiteral(node) => self.compile_array_literal(node),
            Expression::HashLiteral(node) => self.compile_hash_literal(node),
            Expression::FunctionLiteral(node) => self.compile_function_literal(node),
            Expression::Boolean(node) => self.compiler_boolean_expression(node),
            Expression::Identifier(node) => {
                match self.symbol_table.resolve(&node.value) {
                    Some(symbol) => self.emit(Opcode::GetGlobal, vec![symbol.index]),
                    None => panic!("undefined variable {}", node.value),
                };
            }
            Expression::Infix(node) => self.compile_infix_expression(node),
            Expression::Prefix(node) => self.compile_prefix_expression(node),
            Expression::If(node) => self.compile_if_expression(node),
            Expression::Call(node) => self.compile_call_expression(node),
            Expression::Index(node) => self.compile_index_expression(node),
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
            "+" => self.emit(Opcode::Add, vec![]),
            "-" => self.emit(Opcode::Sub, vec![]),
            "*" => self.emit(Opcode::Mul, vec![]),
            "/" => self.emit(Opcode::Div, vec![]),
            ">" | "<" => self.emit(Opcode::GreaterThan, vec![]),
            "==" => self.emit(Opcode::Equal, vec![]),
            "!=" => self.emit(Opcode::NotEqual, vec![]),
            _ => panic!("unknown operator {}", node.operator),
        };
    }

    fn compile_prefix_expression(&mut self, node: &PrefixExpression) {
        self.compile_expression(&node.right);
        match node.operator.as_str() {
            "-" => self.emit(Opcode::Minus, vec![]),
            "!" => self.emit(Opcode::Bang, vec![]),
            _ => panic!("unknown operator {}", node.operator),
        };
    }

    fn compile_if_expression(&mut self, node: &IfExpression) {
        self.compile_expression(&node.condition);

        let jump_not_truth_pos = self.emit(Opcode::JumpNotTruth, vec![9999]);

        self.compile_block_statsment(&node.consequence);

        if self.last_instruction_is(Opcode::Pop) {
            self.remove_last_pop();
        }

        let jump_pos = self.emit(Opcode::Jump, vec![9999]);

        let after_consequence_pos = self.current_scope().instructions.len();
        self.change_operand(jump_not_truth_pos, after_consequence_pos as u16);

        if let Some(ref alternative) = node.alternative {
            self.compile_block_statsment(alternative);

            if self.last_instruction_is(Opcode::Pop) {
                self.remove_last_pop();
            }
        } else {
            self.emit(Opcode::Null, vec![]);
        }

        let afte_alternative_pos = self.current_scope().instructions.len();
        self.change_operand(jump_pos, afte_alternative_pos as u16);
    }

    fn compile_call_expression(&mut self, node: &CallExpression) {
        self.compile_expression(&node.callee);
        self.emit(Opcode::Call, vec![]);
    }

    fn compile_index_expression(&mut self, node: &IndexExpression) {
        self.compile_expression(&node.left);
        self.compile_expression(&node.index);

        self.emit(Opcode::Index, vec![]);
    }

    fn compile_integer_literal(&mut self, node: &IntegerLiteral) {
        let integer = Object::Integer(Integer::new(node.value.to_owned()));
        let const_pos = self.add_constant(integer);
        self.emit(Opcode::Constant, vec![const_pos]);
    }

    fn compile_string_literal(&mut self, node: &StringLiteral) {
        let string = Object::Str(Str::new(node.value.to_owned()));
        let const_pos = self.add_constant(string);
        self.emit(Opcode::Constant, vec![const_pos]);
    }

    fn compile_array_literal(&mut self, node: &ArrayLiteral) {
        for el in node.elements.iter() {
            self.compile_expression(el);
        }
        self.emit(Opcode::Array, vec![node.elements.len() as u16]);
    }

    fn compile_hash_literal(&mut self, node: &HashLiteral) {
        let mut members = node.members.to_owned();
        members.sort_by(|a, b| format!("{}", a.key).cmp(&format!("{}", b.key)));
        for member in members.iter() {
            self.compile_expression(&member.key);
            self.compile_expression(&member.value);
        }
        self.emit(Opcode::Hash, vec![node.members.len() as u16 * 2]);
    }

    fn compile_function_literal(&mut self, node: &FunctionLiteral) {
        self.enter_scope();
        self.compile_block_statsment(&node.body);

        if self.last_instruction_is(Opcode::Pop) {
            self.replace_last_pop_with_return();
        }
        if !self.last_instruction_is(Opcode::ReturnValue) {
            self.emit(Opcode::Return, vec![]);
        }

        let instructions = self.leave_scope();
        let object = Object::CompiledFunction(CompiledFunction::new(instructions));
        let pos = self.add_constant(object);
        self.emit(Opcode::Constant, vec![pos]);
    }

    fn compiler_boolean_expression(&mut self, node: &BooleanExpression) {
        match node.value {
            true => self.emit(Opcode::True, vec![]),
            false => self.emit(Opcode::False, vec![]),
        };
    }

    fn add_constant(&mut self, object: Object) -> u16 {
        self.constants.push(object);
        (self.constants.len() - 1).try_into().unwrap()
    }

    fn add_instructions(&mut self, ins: &mut Instructions) -> usize {
        let scope = self.current_mut_scope();
        let new_ins_pos = scope.instructions.0.len();
        scope.instructions.append(ins);
        new_ins_pos
    }

    fn emit(&mut self, op: Opcode, operands: Vec<u16>) -> usize {
        let mut ins = op.make(operands);
        let pos = self.add_instructions(&mut ins);
        self.set_last_instruction(op, pos);
        pos
    }

    fn set_last_instruction(&mut self, opcode: Opcode, position: usize) {
        let scope = self.current_mut_scope();
        let previous = std::mem::replace(&mut scope.last_instruction, None);
        let last = Some(EmittedInstruction { opcode, position });

        scope.previous_instruction = previous;
        scope.last_instruction = last;
    }

    fn last_instruction_is(&mut self, op: Opcode) -> bool {
        if let Some(ref last_instruction) = self.current_scope().last_instruction {
            last_instruction.opcode == op
        } else {
            false
        }
    }

    fn remove_last_pop(&mut self) {
        let scope = self.current_mut_scope();
        if let Some(ref last_instruction) = scope.last_instruction {
            scope.instructions = scope.instructions.take_range(Range {
                start: 0,
                end: last_instruction.position,
            });

            scope.last_instruction = scope.previous_instruction.to_owned();
        }
    }

    fn replace_instruction(&mut self, pos: usize, new_instruction: Instructions) {
        for (i, _) in new_instruction.0.iter().enumerate() {
            self.current_mut_scope().instructions.0[pos + i] = new_instruction.0[i];
        }
    }

    fn replace_last_pop_with_return(&mut self) {
        let scope = self.current_mut_scope();
        let last_instruction = scope.last_instruction.as_mut().unwrap();
        last_instruction.opcode = Opcode::ReturnValue;
        let las_pos = last_instruction.position;
        self.replace_instruction(las_pos, Opcode::ReturnValue.make(vec![]));
    }

    fn change_operand(&mut self, pos: usize, operand: u16) {
        let op = Opcode::from(self.current_scope().instructions.0[pos]);
        let new_instruction = op.make(vec![operand]);
        self.replace_instruction(pos, new_instruction);
    }

    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.current_scope().instructions.to_owned(),
            constants: self.constants.to_owned(),
        }
    }

    fn current_scope(&self) -> &CompilationScope {
        self.scopes.get(self.scope_index).unwrap()
    }

    fn current_mut_scope(&mut self) -> &mut CompilationScope {
        &mut self.scopes[self.scope_index]
    }

    pub fn enter_scope(&mut self) {
        let scope = CompilationScope::default();
        self.scopes.push(scope);
        self.scope_index += 1;
    }

    pub fn leave_scope(&mut self) -> Instructions {
        let instructions = self.current_scope().instructions.to_owned();

        self.scopes.pop();
        self.scope_index -= 1;

        instructions
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        code::{Instructions, Opcode},
        test_helper::{
            parse, test_array_object, test_boolean_object, test_compiled_function,
            test_hash_object, test_instructions, test_integer_object, test_null_object,
            test_string_object, ExpectedValue,
        },
    };

    use super::*;

    #[derive(Debug)]
    struct CompilerTestCase<'a>(&'a str, Vec<ExpectedValue<'a>>, Vec<Instructions>);

    fn run_compiler_tests(tests: &[CompilerTestCase]) {
        for test in tests.iter() {
            println!("{}", test.0);
            let program = parse(test.0);
            let mut compiler = Compiler::new();
            compiler.compile(&program);
            test_contants(&compiler.constants, &test.1);
            test_instructions(&compiler.current_scope().instructions, &test.2);
        }
    }

    fn test_contants(actual: &Vec<Object>, expected: &Vec<ExpectedValue>) {
        assert_eq!(actual.len(), expected.len());
        for (i, constant) in expected.iter().enumerate() {
            match constant {
                ExpectedValue::Integer(e) => test_integer_object(&actual[i], *e),
                ExpectedValue::Boolean(e) => test_boolean_object(&actual[i], *e),
                ExpectedValue::String(e) => test_string_object(&actual[i], e),
                ExpectedValue::Array(e) => test_array_object(&actual[i], e),
                ExpectedValue::Hash(e) => test_hash_object(&actual[i], e),
                ExpectedValue::Function(e) => test_compiled_function(&actual[i], e),
                ExpectedValue::Null => test_null_object(&actual[i]),
            }
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = [
            CompilerTestCase(
                "1 + 2",
                vec![ExpectedValue::Integer(1), ExpectedValue::Integer(2)],
                vec![
                    Opcode::Constant.make(vec![0]),
                    Opcode::Constant.make(vec![1]),
                    Opcode::Add.make(vec![]),
                    Opcode::Pop.make(vec![]),
                ],
            ),
            CompilerTestCase(
                "1; 2",
                vec![ExpectedValue::Integer(1), ExpectedValue::Integer(2)],
                vec![
                    Opcode::Constant.make(vec![0]),
                    Opcode::Pop.make(vec![]),
                    Opcode::Constant.make(vec![1]),
                    Opcode::Pop.make(vec![]),
                ],
            ),
            CompilerTestCase(
                "1 - 2",
                vec![ExpectedValue::Integer(1), ExpectedValue::Integer(2)],
                vec![
                    Opcode::Constant.make(vec![0]),
                    Opcode::Constant.make(vec![1]),
                    Opcode::Sub.make(vec![]),
                    Opcode::Pop.make(vec![]),
                ],
            ),
            CompilerTestCase(
                "1 * 2",
                vec![ExpectedValue::Integer(1), ExpectedValue::Integer(2)],
                vec![
                    Opcode::Constant.make(vec![0]),
                    Opcode::Constant.make(vec![1]),
                    Opcode::Mul.make(vec![]),
                    Opcode::Pop.make(vec![]),
                ],
            ),
            CompilerTestCase(
                "2 / 1",
                vec![ExpectedValue::Integer(2), ExpectedValue::Integer(1)],
                vec![
                    Opcode::Constant.make(vec![0]),
                    Opcode::Constant.make(vec![1]),
                    Opcode::Div.make(vec![]),
                    Opcode::Pop.make(vec![]),
                ],
            ),
            CompilerTestCase(
                "-1",
                vec![ExpectedValue::Integer(1)],
                vec![
                    Opcode::Constant.make(vec![0]),
                    Opcode::Minus.make(vec![]),
                    Opcode::Pop.make(vec![]),
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
                vec![Opcode::True.make(vec![]), Opcode::Pop.make(vec![])],
            ),
            CompilerTestCase(
                "false",
                vec![],
                vec![Opcode::False.make(vec![]), Opcode::Pop.make(vec![])],
            ),
            CompilerTestCase(
                "1 > 2",
                vec![ExpectedValue::Integer(1), ExpectedValue::Integer(2)],
                vec![
                    Opcode::Constant.make(vec![0]),
                    Opcode::Constant.make(vec![1]),
                    Opcode::GreaterThan.make(vec![]),
                    Opcode::Pop.make(vec![]),
                ],
            ),
            CompilerTestCase(
                "1 < 2",
                vec![ExpectedValue::Integer(2), ExpectedValue::Integer(1)],
                vec![
                    Opcode::Constant.make(vec![0]),
                    Opcode::Constant.make(vec![1]),
                    Opcode::GreaterThan.make(vec![]),
                    Opcode::Pop.make(vec![]),
                ],
            ),
            CompilerTestCase(
                "1 == 2",
                vec![ExpectedValue::Integer(1), ExpectedValue::Integer(2)],
                vec![
                    Opcode::Constant.make(vec![0]),
                    Opcode::Constant.make(vec![1]),
                    Opcode::Equal.make(vec![]),
                    Opcode::Pop.make(vec![]),
                ],
            ),
            CompilerTestCase(
                "1 != 2",
                vec![ExpectedValue::Integer(1), ExpectedValue::Integer(2)],
                vec![
                    Opcode::Constant.make(vec![0]),
                    Opcode::Constant.make(vec![1]),
                    Opcode::NotEqual.make(vec![]),
                    Opcode::Pop.make(vec![]),
                ],
            ),
            CompilerTestCase(
                "true == false",
                vec![],
                vec![
                    Opcode::True.make(vec![]),
                    Opcode::False.make(vec![]),
                    Opcode::Equal.make(vec![]),
                    Opcode::Pop.make(vec![]),
                ],
            ),
            CompilerTestCase(
                "true != false",
                vec![],
                vec![
                    Opcode::True.make(vec![]),
                    Opcode::False.make(vec![]),
                    Opcode::NotEqual.make(vec![]),
                    Opcode::Pop.make(vec![]),
                ],
            ),
            CompilerTestCase(
                "!true",
                vec![],
                vec![
                    Opcode::True.make(vec![]),
                    Opcode::Bang.make(vec![]),
                    Opcode::Pop.make(vec![]),
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
                    Opcode::True.make(vec![]),
                    // 0001
                    Opcode::JumpNotTruth.make(vec![10]),
                    // 0004
                    Opcode::Constant.make(vec![0]),
                    // 0007
                    Opcode::Jump.make(vec![11]),
                    // 0010
                    Opcode::Null.make(vec![]),
                    // 0011
                    Opcode::Pop.make(vec![]),
                    // 0012
                    Opcode::Constant.make(vec![1]),
                    // 0015
                    Opcode::Pop.make(vec![]),
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
                    Opcode::True.make(vec![]),
                    // 0001
                    Opcode::JumpNotTruth.make(vec![10]),
                    // 0004
                    Opcode::Constant.make(vec![0]),
                    // 0007
                    Opcode::Jump.make(vec![13]),
                    // 00010
                    Opcode::Constant.make(vec![1]),
                    // 00013
                    Opcode::Pop.make(vec![]),
                    // 00014
                    Opcode::Constant.make(vec![2]),
                    // 00017
                    Opcode::Pop.make(vec![]),
                ],
            ),
        ];

        run_compiler_tests(&tests);
    }

    #[test]
    fn test_global_let_statements() {
        let tests = [
            CompilerTestCase(
                "let one = 1;
                let two = 2;",
                vec![ExpectedValue::Integer(1), ExpectedValue::Integer(2)],
                vec![
                    Opcode::Constant.make(vec![0]),
                    Opcode::SetGlobal.make(vec![0]),
                    Opcode::Constant.make(vec![1]),
                    Opcode::SetGlobal.make(vec![1]),
                ],
            ),
            CompilerTestCase(
                "let one = 1;
                one;",
                vec![ExpectedValue::Integer(1)],
                vec![
                    Opcode::Constant.make(vec![0]),
                    Opcode::SetGlobal.make(vec![0]),
                    Opcode::GetGlobal.make(vec![0]),
                    Opcode::Pop.make(vec![]),
                ],
            ),
            CompilerTestCase(
                "let one = 1;
                let two = one;
                two;",
                vec![ExpectedValue::Integer(1)],
                vec![
                    Opcode::Constant.make(vec![0]),
                    Opcode::SetGlobal.make(vec![0]),
                    Opcode::GetGlobal.make(vec![0]),
                    Opcode::SetGlobal.make(vec![1]),
                    Opcode::GetGlobal.make(vec![1]),
                    Opcode::Pop.make(vec![]),
                ],
            ),
        ];

        run_compiler_tests(&tests);
    }

    #[test]
    fn test_string_expressions() {
        let tests = [
            CompilerTestCase(
                r#""monkey""#,
                vec![ExpectedValue::String("monkey")],
                vec![Opcode::Constant.make(vec![0]), Opcode::Pop.make(vec![])],
            ),
            CompilerTestCase(
                r#""mon" + "key""#,
                vec![ExpectedValue::String("mon"), ExpectedValue::String("key")],
                vec![
                    Opcode::Constant.make(vec![0]),
                    Opcode::Constant.make(vec![1]),
                    Opcode::Add.make(vec![]),
                    Opcode::Pop.make(vec![]),
                ],
            ),
        ];

        run_compiler_tests(&tests);
    }

    #[test]
    fn test_array_literals() {
        let tests = [
            CompilerTestCase(
                "[]",
                vec![],
                vec![Opcode::Array.make(vec![0]), Opcode::Pop.make(vec![])],
            ),
            CompilerTestCase(
                "[1, 2, 3]",
                vec![
                    ExpectedValue::Integer(1),
                    ExpectedValue::Integer(2),
                    ExpectedValue::Integer(3),
                ],
                vec![
                    Opcode::Constant.make(vec![0]),
                    Opcode::Constant.make(vec![1]),
                    Opcode::Constant.make(vec![2]),
                    Opcode::Array.make(vec![3]),
                    Opcode::Pop.make(vec![]),
                ],
            ),
            CompilerTestCase(
                "[1 + 2, 3 - 4, 5 * 6]",
                vec![
                    ExpectedValue::Integer(1),
                    ExpectedValue::Integer(2),
                    ExpectedValue::Integer(3),
                    ExpectedValue::Integer(4),
                    ExpectedValue::Integer(5),
                    ExpectedValue::Integer(6),
                ],
                vec![
                    Opcode::Constant.make(vec![0]),
                    Opcode::Constant.make(vec![1]),
                    Opcode::Add.make(vec![]),
                    Opcode::Constant.make(vec![2]),
                    Opcode::Constant.make(vec![3]),
                    Opcode::Sub.make(vec![]),
                    Opcode::Constant.make(vec![4]),
                    Opcode::Constant.make(vec![5]),
                    Opcode::Mul.make(vec![]),
                    Opcode::Array.make(vec![3]),
                    Opcode::Pop.make(vec![]),
                ],
            ),
        ];

        run_compiler_tests(&tests);
    }

    #[test]
    fn test_hash_literals() {
        let tests = [
            CompilerTestCase(
                "{}",
                vec![],
                vec![Opcode::Hash.make(vec![0]), Opcode::Pop.make(vec![])],
            ),
            CompilerTestCase(
                "{1: 2, 3: 4, 5: 6}",
                vec![
                    ExpectedValue::Integer(1),
                    ExpectedValue::Integer(2),
                    ExpectedValue::Integer(3),
                    ExpectedValue::Integer(4),
                    ExpectedValue::Integer(5),
                    ExpectedValue::Integer(6),
                ],
                vec![
                    Opcode::Constant.make(vec![0]),
                    Opcode::Constant.make(vec![1]),
                    Opcode::Constant.make(vec![2]),
                    Opcode::Constant.make(vec![3]),
                    Opcode::Constant.make(vec![4]),
                    Opcode::Constant.make(vec![5]),
                    Opcode::Hash.make(vec![6]),
                    Opcode::Pop.make(vec![]),
                ],
            ),
            CompilerTestCase(
                "{1: 2 + 3, 4: 5 * 6}",
                vec![
                    ExpectedValue::Integer(1),
                    ExpectedValue::Integer(2),
                    ExpectedValue::Integer(3),
                    ExpectedValue::Integer(4),
                    ExpectedValue::Integer(5),
                    ExpectedValue::Integer(6),
                ],
                vec![
                    Opcode::Constant.make(vec![0]),
                    Opcode::Constant.make(vec![1]),
                    Opcode::Constant.make(vec![2]),
                    Opcode::Add.make(vec![]),
                    Opcode::Constant.make(vec![3]),
                    Opcode::Constant.make(vec![4]),
                    Opcode::Constant.make(vec![5]),
                    Opcode::Mul.make(vec![]),
                    Opcode::Hash.make(vec![4]),
                    Opcode::Pop.make(vec![]),
                ],
            ),
        ];

        run_compiler_tests(&tests);
    }

    #[test]
    fn test_index_expressions() {
        let tests = [
            CompilerTestCase(
                "[1, 2, 3][1 + 1]",
                vec![
                    ExpectedValue::Integer(1),
                    ExpectedValue::Integer(2),
                    ExpectedValue::Integer(3),
                    ExpectedValue::Integer(1),
                    ExpectedValue::Integer(1),
                ],
                vec![
                    Opcode::Constant.make(vec![0]),
                    Opcode::Constant.make(vec![1]),
                    Opcode::Constant.make(vec![2]),
                    Opcode::Array.make(vec![3]),
                    Opcode::Constant.make(vec![3]),
                    Opcode::Constant.make(vec![4]),
                    Opcode::Add.make(vec![]),
                    Opcode::Index.make(vec![]),
                    Opcode::Pop.make(vec![]),
                ],
            ),
            CompilerTestCase(
                "{1: 2}[2 - 1]",
                vec![
                    ExpectedValue::Integer(1),
                    ExpectedValue::Integer(2),
                    ExpectedValue::Integer(2),
                    ExpectedValue::Integer(1),
                ],
                vec![
                    Opcode::Constant.make(vec![0]),
                    Opcode::Constant.make(vec![1]),
                    Opcode::Hash.make(vec![2]),
                    Opcode::Constant.make(vec![2]),
                    Opcode::Constant.make(vec![3]),
                    Opcode::Sub.make(vec![]),
                    Opcode::Index.make(vec![]),
                    Opcode::Pop.make(vec![]),
                ],
            ),
        ];

        run_compiler_tests(&tests);
    }

    #[test]
    fn test_functions() {
        let tests = [
            CompilerTestCase(
                "fn() { return 5 + 10 }",
                vec![
                    ExpectedValue::Integer(5),
                    ExpectedValue::Integer(10),
                    ExpectedValue::Function(vec![
                        Opcode::Constant.make(vec![0]),
                        Opcode::Constant.make(vec![1]),
                        Opcode::Add.make(vec![]),
                        Opcode::ReturnValue.make(vec![]),
                    ]),
                ],
                vec![Opcode::Constant.make(vec![2]), Opcode::Pop.make(vec![])],
            ),
            CompilerTestCase(
                "fn() { 5 + 10 }",
                vec![
                    ExpectedValue::Integer(5),
                    ExpectedValue::Integer(10),
                    ExpectedValue::Function(vec![
                        Opcode::Constant.make(vec![0]),
                        Opcode::Constant.make(vec![1]),
                        Opcode::Add.make(vec![]),
                        Opcode::ReturnValue.make(vec![]),
                    ]),
                ],
                vec![Opcode::Constant.make(vec![2]), Opcode::Pop.make(vec![])],
            ),
            CompilerTestCase(
                "fn() { 1; 2 }",
                vec![
                    ExpectedValue::Integer(1),
                    ExpectedValue::Integer(2),
                    ExpectedValue::Function(vec![
                        Opcode::Constant.make(vec![0]),
                        Opcode::Pop.make(vec![]),
                        Opcode::Constant.make(vec![1]),
                        Opcode::ReturnValue.make(vec![]),
                    ]),
                ],
                vec![Opcode::Constant.make(vec![2]), Opcode::Pop.make(vec![])],
            ),
            CompilerTestCase(
                "fn() {}",
                vec![ExpectedValue::Function(vec![
                    Opcode::Return.make(vec![])
                ])],
                vec![Opcode::Constant.make(vec![0]), Opcode::Pop.make(vec![])],
            ),
        ];

        run_compiler_tests(&tests);
    }

    #[test]
    fn test_compiler_scopes() {
        let mut compiler = Compiler::new();
        assert_eq!(compiler.scope_index, 0);

        compiler.emit(Opcode::Mul, vec![]);

        compiler.enter_scope();
        assert_eq!(compiler.scope_index, 1);

        compiler.emit(Opcode::Sub, vec![]);

        assert_eq!(compiler.current_scope().instructions.len(), 1);
        assert_eq!(
            compiler
                .current_scope()
                .last_instruction
                .as_ref()
                .unwrap()
                .opcode,
            Opcode::Sub
        );

        compiler.leave_scope();
        assert_eq!(compiler.scope_index, 0);

        compiler.emit(Opcode::Add, vec![]);
        assert_eq!(compiler.current_scope().instructions.len(), 2);
        assert_eq!(
            compiler
                .current_scope()
                .last_instruction
                .as_ref()
                .unwrap()
                .opcode,
            Opcode::Add
        );
        assert_eq!(
            compiler
                .current_scope()
                .previous_instruction
                .as_ref()
                .unwrap()
                .opcode,
            Opcode::Mul
        );
    }

    #[test]
    fn test_function_calls() {
        let tests = [
            CompilerTestCase(
                "fn() { 24 }();",
                vec![
                    ExpectedValue::Integer(24),
                    ExpectedValue::Function(vec![
                        Opcode::Constant.make(vec![0]),
                        Opcode::ReturnValue.make(vec![]),
                    ]),
                ],
                vec![
                    Opcode::Constant.make(vec![1]),
                    Opcode::Call.make(vec![]),
                    Opcode::Pop.make(vec![]),
                ],
            ),
            CompilerTestCase(
                "let noArg = fn() { 24 };noArg();",
                vec![
                    ExpectedValue::Integer(24),
                    ExpectedValue::Function(vec![
                        Opcode::Constant.make(vec![0]),
                        Opcode::ReturnValue.make(vec![]),
                    ]),
                ],
                vec![
                    Opcode::Constant.make(vec![1]),
                    Opcode::SetGlobal.make(vec![0]),
                    Opcode::GetGlobal.make(vec![0]),
                    Opcode::Call.make(vec![]),
                    Opcode::Pop.make(vec![]),
                ],
            ),
        ];

        run_compiler_tests(&tests);
    }
}
