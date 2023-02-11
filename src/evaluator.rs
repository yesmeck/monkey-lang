use crate::ast::{BlockStatement, Expression, IfExpression, Node, Statement};
use crate::object::{Boolean, Integer, Null, Object};

pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Self {
        Self {}
    }

    pub fn eval(&self, ast: Node) -> Object {
        match ast {
            Node::Program(node) => self.eval_statements(&node.statements),
            // Node::Statement(node) => eval(ast)
            Node::Statement(Statement::Expression(node)) => {
                self.eval(Node::Expression(&node.expression))
            }

            Node::BlockStatement(node) => self.eval_statements(&node.statements),

            Node::Expression(Expression::IntegerLiteral(node)) => {
                Object::Integer(Integer::new(node.value))
            }

            Node::Expression(Expression::Boolean(node)) => {
                self.native_bool_to_boolean_object(node.value)
            }

            Node::Expression(Expression::Prefix(node)) => {
                let right = self.eval(Node::Expression(node.right.as_ref()));
                self.eval_prefix_expression(node.operator.to_owned(), right)
            }

            Node::Expression(Expression::Infix(node)) => {
                let left = self.eval(Node::Expression(node.left.as_ref()));
                let right = self.eval(Node::Expression(node.right.as_ref()));
                self.eval_infix_expression(node.operator.to_owned(), left, right)
            }

            Node::Expression(Expression::If(node)) => self.eval_if_expression(node),

            _ => Object::Null(Null {}),
        }
    }

    fn eval_statements(&self, stmts: &Vec<Statement>) -> Object {
        let mut result = Object::Null(Null {});

        for stmt in stmts.iter() {
            result = self.eval(Node::Statement(stmt));
        }

        result
    }

    fn eval_prefix_expression(&self, operator: String, right: Object) -> Object {
        match operator.as_str() {
            "!" => self.eval_bang_operator_expression(right),
            "-" => self.eval_minux_operator_expression(right),
            _ => Object::Null(Null {}),
        }
    }

    fn eval_infix_expression(&self, operator: String, left: Object, right: Object) -> Object {
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
        Object::Null(Null {})
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
            _ => Object::Null(Null {}),
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
            _ => Object::Null(Null {}),
        }
    }

    fn eval_if_expression(&self, ie: &IfExpression) -> Object {
        let condition = self.eval(Node::Expression(&ie.condition));

        if self.is_truthy(condition) {
            return self.eval(Node::BlockStatement(&ie.consequence));
        } else if let Some(ref alternative) = ie.alternative {
            return self.eval(Node::BlockStatement(alternative))
        }

        Object::Null(Null {})
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
}

#[cfg(test)]
mod tests {
    use super::Evaluator;
    use crate::{
        ast::Node,
        lexer::Lexer,
        object::{Boolean, Inspector, Null, Object},
        parser::Parser,
    };

    fn test_eval(input: &str) -> Object {
        static evaluator: Evaluator = Evaluator {};
        let mut lexer = Lexer::new(input.into());
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();
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
        assert!(matches!(object, Object::Null(Null)))
    }

    #[test]
    fn test_eval_integer_expression() {
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
    fn test_eval_boolean_expresion() {
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
    fn test_eval_bang_operator() {
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
    fn test_if_else_expression() {
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
}
