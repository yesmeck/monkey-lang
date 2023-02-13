use crate::ast::{BlockStatement, Expression, Identifier, IfExpression, Node, Program, Statement};
use crate::enviroment::Enviroment;
use crate::object::{Boolean, Inspector, Integer, Null, Object, ReturnValue, RuntimeError};

pub struct Evaluator<'a> {
    env: &'a mut Enviroment,
}

impl<'a> Evaluator<'a> {
    pub fn new(env: &'a mut Enviroment) -> Self {
        Self { env }
    }

    pub fn eval(&mut self, ast: Node) -> Object {
        match ast {
            Node::Program(node) => self.eval_program(node),
            // Node::Statement(node) => eval(ast)
            Node::Statement(Statement::Expression(node)) => {
                self.eval(Node::Expression(&node.expression))
            }

            Node::Statement(Statement::Return(node)) => {
                let value = self.eval(Node::Expression(&node.return_value));
                if self.is_error(&value) {
                    value
                } else {
                    Object::ReturnValue(ReturnValue::new(value))
                }
            }

            Node::Statement(Statement::Let(node)) => {
                let value = self.eval(Node::Expression(&node.value));
                if self.is_error(&value) {
                    value
                } else {
                    self.env.set(node.name.value.to_owned(), value.clone());
                    value
                }
            }

            Node::BlockStatement(node) => self.eval_block_statement(node),

            Node::Expression(Expression::Identifier(node)) => self.eval_indentifier(node),

            Node::Expression(Expression::IntegerLiteral(node)) => {
                Object::Integer(Integer::new(node.value))
            }

            Node::Expression(Expression::Boolean(node)) => {
                self.native_bool_to_boolean_object(node.value)
            }

            Node::Expression(Expression::Prefix(node)) => {
                let right = self.eval(Node::Expression(node.right.as_ref()));
                if self.is_error(&right) {
                    right
                } else {
                    self.eval_prefix_expression(node.operator.to_owned(), right)
                }
            }

            Node::Expression(Expression::Infix(node)) => {
                let left = self.eval(Node::Expression(node.left.as_ref()));

                if self.is_error(&left) {
                    return left;
                }

                let right = self.eval(Node::Expression(node.right.as_ref()));

                if self.is_error(&right) {
                    return right;
                }

                self.eval_infix_expression(node.operator.to_owned(), left, right)
            }

            Node::Expression(Expression::If(node)) => self.eval_if_expression(node),

            _ => Object::Null(Null {}),
        }
    }

    fn eval_program(&mut self, program: &Program) -> Object {
        let mut result = Object::Null(Null {});

        for stmt in program.statements.iter() {
            result = self.eval(Node::Statement(stmt));

            match result {
                Object::ReturnValue(return_object) => return *return_object.value,
                Object::RuntimeError(_) => return result,
                _ => {}
            }
        }

        result
    }

    fn eval_block_statement(&mut self, block: &BlockStatement) -> Object {
        let mut result = Object::Null(Null {});

        for stmt in block.statements.iter() {
            result = self.eval(Node::Statement(stmt));

            match result {
                Object::ReturnValue(_) => return result,
                Object::RuntimeError(_) => return result,
                _ => {}
            }
        }

        result
    }

    fn eval_prefix_expression(&mut self, operator: String, right: Object) -> Object {
        match operator.as_str() {
            "!" => self.eval_bang_operator_expression(right),
            "-" => self.eval_minux_operator_expression(right),
            _ => Object::RuntimeError(RuntimeError::new(format!(
                "unknown operator: {}{}",
                operator,
                right.kind()
            ))),
        }
    }

    fn eval_infix_expression(&self, operator: String, left: Object, right: Object) -> Object {
        if left.kind() != right.kind() {
            return Object::RuntimeError(RuntimeError::new(format!(
                "type mismatch: {} {} {}",
                left.kind(),
                operator,
                right.kind()
            )));
        }

        if let Object::Integer(ref left) = left {
            if let Object::Integer(ref right) = right {
                return self.eval_integer_infix_expression(operator, left, right);
            }
        }
        if operator == "==" {
            let result = left == right;
            return self.native_bool_to_boolean_object(result);
        }
        if operator == "!=" {
            let result = left != right;
            return self.native_bool_to_boolean_object(result);
        }
        Object::RuntimeError(RuntimeError::new(format!(
            "unknown operator: {} {} {}",
            left.kind(),
            operator,
            right.kind()
        )))
    }

    fn eval_integer_infix_expression(
        &self,
        operator: String,
        left: &Integer,
        right: &Integer,
    ) -> Object {
        let left_value = left.value;
        let right_value = right.value;

        match operator.as_str() {
            "+" => Object::Integer(Integer::new(left_value + right_value)),
            "-" => Object::Integer(Integer::new(left_value - right_value)),
            "*" => Object::Integer(Integer::new(left_value * right_value)),
            "/" => Object::Integer(Integer::new(left_value / right_value)),
            "<" => self.native_bool_to_boolean_object(left_value < right_value),
            ">" => self.native_bool_to_boolean_object(left_value > right_value),
            "==" => self.native_bool_to_boolean_object(left_value == right_value),
            "!=" => self.native_bool_to_boolean_object(left_value != right_value),
            _ => Object::RuntimeError(RuntimeError::new(format!(
                "unknown operator: {} {} {}",
                left.kind(),
                operator,
                right.kind()
            ))),
        }
    }

    fn eval_bang_operator_expression(&self, right: Object) -> Object {
        match right {
            Object::Boolean(object) => {
                if object.value {
                    Object::Boolean(Boolean::new(false))
                } else {
                    Object::Boolean(Boolean::new(true))
                }
            }
            Object::Null(_) => Object::Null(Null {}),
            _ => Object::Boolean(Boolean::new(false)),
        }
    }

    fn eval_minux_operator_expression(&self, right: Object) -> Object {
        match right {
            Object::Integer(object) => Object::Integer(Integer::new(-object.value)),
            _ => Object::RuntimeError(RuntimeError::new(format!(
                "unknown operator: -{}",
                right.kind()
            ))),
        }
    }

    fn eval_if_expression(&mut self, ie: &IfExpression) -> Object {
        let condition = self.eval(Node::Expression(&ie.condition));

        if self.is_error(&condition) {
            return condition;
        }

        if self.is_truthy(condition) {
            return self.eval(Node::BlockStatement(&ie.consequence));
        } else if let Some(ref alternative) = ie.alternative {
            return self.eval(Node::BlockStatement(alternative));
        }

        Object::Null(Null {})
    }

    fn eval_indentifier(&self, node: &Identifier) -> Object {
        if let Some(value) = self.env.get(node.value.to_owned()) {
            value.clone()
        } else {
            Object::RuntimeError(RuntimeError::new(format!(
                "identifier not found: {}",
                node.value
            )))
        }
    }

    fn native_bool_to_boolean_object(&self, input: bool) -> Object {
        if input {
            Object::Boolean(Boolean::new(true))
        } else {
            Object::Boolean(Boolean::new(false))
        }
    }

    fn is_truthy(&self, object: Object) -> bool {
        match object {
            Object::Null(_) => false,
            Object::Boolean(obj) => obj.value,
            _ => true,
        }
    }

    fn is_error(&self, object: &Object) -> bool {
        matches!(object, Object::RuntimeError(_))
    }
}

#[cfg(test)]
mod tests {
    use super::Evaluator;
    use crate::{
        ast::Node,
        enviroment::Enviroment,
        lexer::Lexer,
        object::{Inspector, Object},
        parser::Parser,
    };

    fn test_eval(input: &str) -> Object {
        let mut lexer = Lexer::new(input.into());
        let mut parser = Parser::new(&mut lexer);
        let mut env = Enviroment::new();
        let program = parser.parse_program();
        let mut evaluator = Evaluator::new(&mut env);
        evaluator.eval(Node::Program(&program))
    }

    fn test_integer_object(object: Object, expected: i64) {
        if let Object::Integer(integer) = object {
            assert_eq!(integer.value, expected);
        } else {
            panic!("not a integer");
        }
    }

    fn test_boolean_object(object: Object, expected: bool) {
        if let Object::Boolean(bool) = object {
            assert_eq!(bool.value, expected);
        } else {
            panic!("not a boolean");
        }
    }

    fn test_null_object(object: Object) {
        assert!(matches!(object, Object::Null(_)))
    }

    #[test]
    fn test_eval_integer_expressions() {
        let tests = [
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for (input, output) in tests.iter() {
            test_integer_object(test_eval(input), *output);
        }
    }

    #[test]
    fn test_eval_boolean_expresions() {
        let tests = [
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for (input, output) in tests.iter() {
            test_boolean_object(test_eval(input), *output);
        }
    }

    #[test]
    fn test_eval_bang_operators() {
        let tests = [
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for (input, output) in tests.iter() {
            test_boolean_object(test_eval(input), *output);
        }
    }

    #[test]
    fn test_if_else_expressions() {
        let tests = [
            ("if (true) { 10 )", Some(10)),
            ("if (false) { 10 )", None),
            ("if (1) { 10 )", Some(10)),
            ("if (1 < 2) { 10 }", Some(10)),
            ("if (1 > 2) { 10 }", None),
            ("if (1 > 2) { 10 } else { 20 }", Some(20)),
            ("if (1 < 2) { 10 } else { 20 }", Some(10)),
        ];

        for (input, output) in tests.iter() {
            let evaluated = test_eval(input);
            if let Some(integer) = output {
                test_integer_object(evaluated, *integer as i64);
            } else {
                test_null_object(evaluated);
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = [
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            (
                "if (10 > 1) {
                    if (10 > 1) {
                        return 10; 
                    }
                    return 1;
                }",
                10,
            ),
        ];

        for (input, output) in tests.iter() {
            println!("{} = {}", input, output);
            test_integer_object(test_eval(input), *output);
        }
    }

    #[test]
    fn test_error_handling() {
        let tests = [
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                "if (10 > 1) {
                  if (10 > 1) {
                    return true + false;
                  }
                  return 1; 
                }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            ("foobar", "identifier not found: foobar"),
        ];

        for (input, output) in tests.iter() {
            println!("{} => {}", input, output);
            let evaluated = test_eval(input);

            if let Object::RuntimeError(error) = evaluated {
                assert_eq!(error.inspect(), format!("Error: {}", output));
            } else {
                panic!("not a error")
            }
        }
    }

    #[test]
    fn test_let_statements() {
        let tests = [
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for (input, output) in tests.iter() {
            test_integer_object(test_eval(input), *output);
        }
    }
}
