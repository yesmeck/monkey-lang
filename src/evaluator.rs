use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{
    ArrayLiteral, BlockStatement, FunctionLiteral, HashLiteral, Identifier, IfExpression,
    IndexExpression, InfixExpression, LetStatement, PrefixExpression,
    Program, ReturnStatement, Statement, CallExpression,
};
use crate::ast::Expression;
use crate::builtin::Builtin;
use crate::enviroment::Enviroment;
use crate::makro::{EvalUnqupteCalls, MacroExpension};
use crate::object::{
    Array, BuiltinFunction, Function, HashKeyable, Inspector, Integer, Object,
    Quote, ReturnValue, RuntimeError, Str,
};
use crate::object::Hash;
use crate::traverser::Traverable;

#[derive(Debug)]
pub struct Evaluator<'a> {
    env: Rc<RefCell<Enviroment>>,
    builtin: Builtin<'a>,
}

impl<'a> Evaluator<'a> {
    pub fn new(env: Rc<RefCell<Enviroment>>) -> Self {
        Self {
            env: Rc::clone(&env),
            builtin: Builtin::new(Rc::clone(&env)),
        }
    }

    pub fn eval(&mut self, program: &mut Program) -> Rc<Object> {
        let makro = MacroExpension::new(Rc::clone(&self.env));
        makro.define_macros(program);
        makro.expand_macros(program);

        self.eval_program(program)
    }

    fn eval_statement(&mut self, ast: &Statement) -> Rc<Object> {
        match ast {
            Statement::Expression(node) => self.eval_expression(&node.expression),
            Statement::Return(node) => self.eval_return_statement(node),
            Statement::Let(node) => self.eval_let_statement(node),
        }
    }

    fn eval_expression(&mut self, ast: &Expression) -> Rc<Object> {
        match ast {
            Expression::Identifier(ref node) => self.eval_indentifier(node),
            Expression::NullLiteral(_) => Rc::clone(&self.env.borrow().null_object),
            Expression::IntegerLiteral(node) => Object::Integer(Integer::new(node.value)).into(),
            Expression::StringLiteral(node) => Object::Str(Str::new(node.value.to_owned())).into(),
            Expression::ArrayLiteral(node) => self.eval_array_literal(node),
            Expression::HashLiteral(node) => self.eval_hash_literal(node),
            Expression::Index(node) => self.eval_index_expression(node),
            Expression::Boolean(node) => self.native_bool_to_boolean_object(node.value),
            Expression::FunctionLiteral(node) => self.eval_function_literal(node),
            Expression::MacroLiteral(_) => Rc::clone(&self.env.borrow().null_object),
            Expression::Prefix(node) => self.eval_prefix_expression(node),
            Expression::Infix(node) => self.eval_infix_expression(node),
            Expression::If(node) => self.eval_if_expression(node),
            Expression::Call(node) => self.eval_call_expression(ast, node),
        }
    }

    fn eval_let_statement(&mut self, node: &LetStatement) -> Rc<Object> {
        let value = self.eval_expression(&node.value);
        if self.is_error(&value) {
            value
        } else {
            self.env
                .borrow_mut()
                .set(node.name.value.to_owned(), value.clone());
            value
        }
    }

    fn eval_return_statement(&mut self, node: &ReturnStatement) -> Rc<Object> {
        let value = self.eval_expression(&node.return_value);
        if self.is_error(&value) {
            value
        } else {
            Rc::new(Object::ReturnValue(ReturnValue::new(value)))
        }
    }

    fn quote(&mut self, node: &mut Expression) -> Rc<Object> {
        self.eval_unquote_calls(node);
        match *node {
            Expression::Call(ref call) => {
                let argument = call.arguments.get(0).unwrap();
                Object::Quote(Quote::new(argument.to_owned())).into()
            }
            _ => Rc::clone(&self.env.borrow().null_object),
        }
    }

    fn eval_unquote_calls(&mut self, node: &mut Expression) {
        let visitor = EvalUnqupteCalls::new(Rc::clone(&self.env));
        node.visit_mut(&visitor);
    }

    fn eval_call_expression(&mut self, parent: &Expression, node: &CallExpression) -> Rc<Object> {
        if let Expression::Identifier(ref callee) = *node.callee {
            if callee.value == "quote" {
                if node.arguments.len() == 1 {
                    return self.quote(&mut parent.to_owned());
                } else {
                    return Object::RuntimeError(RuntimeError::new(format!(
                        "wrong number of arguments. got={}, want=1",
                        node.arguments.len()
                    )))
                    .into();
                }
            }
        }

        let func = self.eval_expression(&node.callee);

        if self.is_error(&func) {
            return func;
        }

        match *func {
            Object::Function(ref func) => {
                let args = self.eval_expressions(&node.arguments);
                if args.len() == 1 && self.is_error(&args[0]) {
                    return args[0].clone();
                }
                self.apply_function(func, args)
            }
            Object::BuiltinFunction(ref func) => {
                let args = self.eval_expressions(&node.arguments);
                if args.len() == 1 && self.is_error(&args[0]) {
                    return args[0].clone();
                }
                if let Some(result) = self.builtin.try_function(&func.name, args) {
                    result
                } else {
                    Object::RuntimeError(RuntimeError::new(format!(
                        "Not a function: {}",
                        func.name,
                    )))
                    .into()
                }
            }
            _ => Object::RuntimeError(RuntimeError::new(format!(
                "Not a function: {}",
                func.kind(),
            )))
            .into(),
        }
    }

    fn apply_function(&mut self, func: &Function, args: Vec<Rc<Object>>) -> Rc<Object> {
        let old_env = Rc::clone(&self.env);
        self.extend_function_env(func, args);
        let return_value = self.eval_block_statement(&func.body);
        self.env = old_env;
        self.unwrap_return_value(return_value)
    }

    fn extend_function_env(&mut self, func: &Function, args: Vec<Rc<Object>>) {
        let mut env = Enviroment::new(Rc::clone(&func.env));
        for (i, param) in func.parameters.iter().enumerate() {
            env.set(param.value.to_owned(), args[i].to_owned());
        }
        self.env = Rc::new(RefCell::new(env));
    }

    fn unwrap_return_value(&self, obj: Rc<Object>) -> Rc<Object> {
        if let Object::ReturnValue(ref return_value) = *obj {
            Rc::clone(&return_value.value)
        } else {
            obj
        }
    }

    fn eval_program(&mut self, program: &mut Program) -> Rc<Object> {
        let mut result = Rc::clone(&self.env.borrow().null_object);

        for stmt in program.statements.iter_mut() {
            result = self.eval_statement(stmt);

            match *result {
                Object::ReturnValue(ref return_object) => return Rc::clone(&return_object.value),
                Object::RuntimeError(_) => return result,
                _ => {}
            }
        }

        result
    }

    fn eval_expressions(&mut self, exps: &[Expression]) -> Vec<Rc<Object>> {
        let mut result: Vec<_> = vec![];

        for exp in exps.iter() {
            let evaluated = self.eval_expression(exp);
            if self.is_error(&evaluated) {
                return vec![evaluated];
            }
            result.push(evaluated);
        }

        result
    }

    fn eval_block_statement(&mut self, block: &BlockStatement) -> Rc<Object> {
        let mut result = Rc::clone(&self.env.borrow().null_object);

        for stmt in block.statements.iter() {
            result = self.eval_statement(stmt);

            match *result {
                Object::ReturnValue(_) => return result,
                Object::RuntimeError(_) => return result,
                _ => {}
            }
        }

        result
    }

    fn eval_index_expression(&mut self, node: &IndexExpression) -> Rc<Object> {
        let left = self.eval_expression(&node.left);
        if self.is_error(&left) {
            return left;
        }
        let index = self.eval_expression(&node.index);
        if self.is_error(&index) {
            return index;
        }

        match *left {
            Object::Array(ref array) => self.eval_array_index_expression(array, &index),
            Object::Hash(ref hash) => self.eval_hash_index_expression(hash, &index),
            _ => Object::RuntimeError(RuntimeError::new(format!(
                "index operator not supported: {}",
                left.kind()
            )))
            .into(),
        }
    }

    fn eval_array_index_expression(&self, array: &Array, index: &Rc<Object>) -> Rc<Object> {
        match **index {
            Object::Integer(ref index) => {
                if index.value < 0 || array.elements.len() < (index.value + 1).try_into().unwrap() {
                    Rc::clone(&self.env.borrow().null_object)
                } else {
                    array.elements[index.value as usize].clone()
                }
            }
            _ => Object::RuntimeError(RuntimeError::new(format!(
                "index is not a integer: {}",
                index.kind()
            )))
            .into(),
        }
    }

    fn eval_hash_index_expression(&self, hash: &Hash, index: &Rc<Object>) -> Rc<Object> {
        let key = match **index {
            Object::Integer(ref o) => o.hash_key(),
            Object::Str(ref o) => o.hash_key(),
            Object::Boolean(ref o) => o.hash_key(),
            _ => {
                return Rc::new(Object::RuntimeError(RuntimeError::new(format!(
                    "unusable as hash key: {}",
                    index.kind()
                ))))
            }
        };

        Rc::clone(
            hash.value
                .get(&key)
                .unwrap_or(&Rc::clone(&self.env.borrow().null_object)),
        )
    }

    fn eval_prefix_expression(&mut self, node: &PrefixExpression) -> Rc<Object> {
        let right = self.eval_expression(&node.right);
        if self.is_error(&right) {
            return right;
        }

        match node.operator.as_str() {
            "!" => self.eval_bang_operator_expression(right),
            "-" => self.eval_minux_operator_expression(right),
            _ => Object::RuntimeError(RuntimeError::new(format!(
                "unknown operator: {}{}",
                node.operator,
                right.kind()
            )))
            .into(),
        }
    }

    fn eval_infix_expression(&mut self, node: &InfixExpression) -> Rc<Object> {
        let left = self.eval_expression(&node.left);

        if self.is_error(&left) {
            return left;
        }

        let right = self.eval_expression(&node.right);

        if self.is_error(&right) {
            return right;
        }

        if left.kind() != right.kind() {
            return Object::RuntimeError(RuntimeError::new(format!(
                "type mismatch: {} {} {}",
                left.kind(),
                node.operator,
                right.kind()
            )))
            .into();
        }

        if let Object::Integer(ref left) = *left {
            if let Object::Integer(ref right) = *right {
                return self.eval_integer_infix_expression(&node.operator, left, right);
            }
        }

        if let Object::Str(ref left) = *left {
            if let Object::Str(ref right) = *right {
                return self.eval_string_infix_expression(&node.operator, left, right);
            }
        }

        match node.operator.as_str() {
            "==" => {
                let result = left == right;
                self.native_bool_to_boolean_object(result)
            }
            "!=" => {
                let result = left != right;
                self.native_bool_to_boolean_object(result)
            }
            _ => Object::RuntimeError(RuntimeError::new(format!(
                "unknown operator: {} {} {}",
                left.kind(),
                node.operator,
                right.kind()
            )))
            .into(),
        }
    }

    fn eval_string_infix_expression(
        &self,
        operator: &String,
        left: &Str,
        right: &Str,
    ) -> Rc<Object> {
        match operator.as_str() {
            "+" => Object::Str(Str::new(format!("{}{}", left.value, right.value))).into(),
            _ => Object::RuntimeError(RuntimeError::new(format!(
                "unknown operator: {} {} {}",
                left.kind(),
                operator,
                right.kind(),
            )))
            .into(),
        }
    }

    fn eval_integer_infix_expression(
        &self,
        operator: &String,
        left: &Integer,
        right: &Integer,
    ) -> Rc<Object> {
        let left_value = left.value;
        let right_value = right.value;

        match operator.as_str() {
            "+" => Object::Integer(Integer::new(left_value + right_value)).into(),
            "-" => Object::Integer(Integer::new(left_value - right_value)).into(),
            "*" => Object::Integer(Integer::new(left_value * right_value)).into(),
            "/" => Object::Integer(Integer::new(left_value / right_value)).into(),
            "<" => self.native_bool_to_boolean_object(left_value < right_value),
            ">" => self.native_bool_to_boolean_object(left_value > right_value),
            "==" => self.native_bool_to_boolean_object(left_value == right_value),
            "!=" => self.native_bool_to_boolean_object(left_value != right_value),
            _ => Object::RuntimeError(RuntimeError::new(format!(
                "unknown operator: {} {} {}",
                left.kind(),
                operator,
                right.kind()
            )))
            .into(),
        }
    }

    fn eval_function_literal(&mut self, node: &FunctionLiteral) -> Rc<Object> {
        Object::Function(Function::new(
            node.parameters.to_owned(),
            node.body.to_owned(),
            Rc::clone(&self.env),
        ))
        .into()
    }

    fn eval_array_literal(&mut self, node: &ArrayLiteral) -> Rc<Object> {
        let elements = self.eval_expressions(&node.elements);
        if elements.len() == 1 && self.is_error(&elements[0]) {
            return elements[0].clone();
        }
        Object::Array(Array::new(elements)).into()
    }

    fn eval_hash_literal(&mut self, node: &HashLiteral) -> Rc<Object> {
        let mut hash_value = HashMap::new();

        for member in node.members.iter() {
            let key = self.eval_expression(&member.key);

            if self.is_error(&key) {
                return key;
            }

            let hash_key = match *key {
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

            let value = self.eval_expression(&member.value);

            if self.is_error(&value) {
                return value;
            }
            hash_value.insert(hash_key, value);
        }

        Object::Hash(Hash::new(hash_value)).into()
    }

    fn eval_bang_operator_expression(&self, right: Rc<Object>) -> Rc<Object> {
        match *right {
            Object::Boolean(ref object) => {
                if object.value {
                    Rc::clone(&self.env.borrow().false_object)
                } else {
                    Rc::clone(&self.env.borrow().true_object)
                }
            }
            Object::Null(_) => Rc::clone(&self.env.borrow().null_object),
            _ => Rc::clone(&self.env.borrow().false_object),
        }
    }

    fn eval_minux_operator_expression(&self, right: Rc<Object>) -> Rc<Object> {
        match *right {
            Object::Integer(ref object) => Object::Integer(Integer::new(-object.value)).into(),
            _ => Object::RuntimeError(RuntimeError::new(format!(
                "unknown operator: -{}",
                right.kind()
            )))
            .into(),
        }
    }

    fn eval_if_expression(&mut self, ie: &IfExpression) -> Rc<Object> {
        let condition = self.eval_expression(&ie.condition);

        if self.is_error(&condition) {
            return condition;
        }

        if self.is_truthy(condition) {
            return self.eval_block_statement(&ie.consequence);
        } else if let Some(ref alternative) = ie.alternative {
            return self.eval_block_statement(alternative);
        }

        Rc::clone(&self.env.borrow().null_object)
    }

    fn eval_indentifier(&self, node: &Identifier) -> Rc<Object> {
        if let Some(value) = self.env.borrow().get(node.value.to_owned()) {
            value
        } else if self.builtin.function_exists(&node.value) {
            Rc::new(Object::BuiltinFunction(BuiltinFunction::new(
                node.value.to_owned(),
            )))
        } else {
            Rc::new(Object::RuntimeError(RuntimeError::new(format!(
                "identifier not found: {}",
                node.value
            ))))
        }
    }

    fn native_bool_to_boolean_object(&self, input: bool) -> Rc<Object> {
        if input {
            Rc::clone(&self.env.borrow().true_object)
        } else {
            Rc::clone(&self.env.borrow().false_object)
        }
    }

    fn is_truthy(&self, object: Rc<Object>) -> bool {
        match *object {
            Object::Null(_) => false,
            Object::Boolean(ref obj) => obj.value,
            _ => true,
        }
    }

    fn is_error(&self, object: &Rc<Object>) -> bool {
        matches!(**object, Object::RuntimeError(_))
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, collections::HashMap, rc::Rc};

    use super::Evaluator;
    use crate::{
        enviroment::Enviroment,
        lexer::Lexer,
        object::{Boolean, HashKey, HashKeyable, Inspector, Integer, Object, Str},
        parser::Parser, test_helper::{test_integer_object, test_boolean_object},
    };

    fn test_eval(input: &str) -> Rc<Object> {
        let mut lexer = Lexer::new(input.into());
        let mut parser = Parser::new(&mut lexer);
        let env = Rc::new(RefCell::new(Enviroment::default()));
        let mut program = parser.parse_program();
        let mut evaluator = Evaluator::new(env);
        evaluator.eval(&mut program)
    }

    fn test_string_object(object: &Rc<Object>, expected: String) {
        if let Object::Str(ref string) = **object {
            assert_eq!(string.value, expected);
        } else {
            panic!("not a string");
        }
    }

    fn test_null_object(object: &Rc<Object>) {
        assert!(matches!(**object, Object::Null(_)))
    }

    fn test_error_object(object: &Rc<Object>, expected: String) {
        if let Object::RuntimeError(ref error) = **object {
            assert_eq!(error.inspect(), format!("Error: {}", expected));
        } else {
            panic!("not a error")
        }
    }

    fn test_quote_object(object: &Rc<Object>, expected: &str) {
        if let Object::Quote(ref quote) = **object {
            assert_eq!(format!("{}", quote.node), expected);
        } else {
            panic!("not a quote");
        }
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
            test_integer_object(&test_eval(input), *output);
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
            test_boolean_object(&test_eval(input), *output);
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
            test_boolean_object(&test_eval(input), *output);
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
                test_integer_object(&evaluated, *integer as i64);
            } else {
                test_null_object(&evaluated);
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
            test_integer_object(&test_eval(input), *output);
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
            ("\"Hello\" - \"World\"", "unknown operator: STRING - STRING"),
            (
                r#"{"name": "Monkey"}[fn(x) { x }];"#,
                "unusable as hash key: FUNCTION",
            ),
        ];

        for (input, output) in tests.iter() {
            println!("{} => {}", input, output);
            let evaluated = test_eval(input);
            test_error_object(&evaluated, output.to_string());
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
            test_integer_object(&test_eval(input), *output);
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2 };";
        let evaluated = test_eval(input);

        if let Object::Function(ref object) = *evaluated {
            assert_eq!(object.parameters.len(), 1);
            assert_eq!(object.parameters[0].value, "x".to_string());
            assert_eq!(format!("{}", object.body), "(x + 2)");
        } else {
            panic!("not a function")
        }
    }

    #[test]
    fn test_function_application() {
        let tests = [
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        for (input, output) in tests.iter() {
            test_integer_object(&test_eval(input), *output);
        }
    }

    #[test]
    fn test_closures() {
        let input = "
            let newAdder = fn(x) {
                fn(y) { x + y };
            };
            let addTwo = newAdder(2);
            addTwo(2);
        ";

        test_integer_object(&test_eval(input), 4);
    }

    #[test]
    fn test_string_literal() {
        let input = "\"Hello world!\"";

        test_string_object(&test_eval(input), "Hello world!".into());
    }

    #[test]
    fn test_string_concatenation() {
        let input = "\"Hello\" + \" \" + \"world!\"";

        test_string_object(&test_eval(input), "Hello world!".into());
    }

    #[test]
    fn test_builtin_functions() {
        let number_tests = [
            (r#"len("")"#, 0),
            (r#"len("four")"#, 4),
            (r#"len("hello world")"#, 11),
            (r#"len([1, 2, 3])"#, 3),
            (r#"first([1, 2, 3])"#, 1),
            (r#"last([1, 2, 3])"#, 3),
        ];
        let error_tests = [
            (r#"len(1)"#, "argument to `len` not supported, got INTEGER"),
            (
                r#"len("one", "two")"#,
                "wrong number of arguments. got=2, want=1",
            ),
        ];

        for (input, output) in number_tests.iter() {
            test_integer_object(&test_eval(input), *output);
        }

        for (input, output) in error_tests.iter() {
            test_error_object(&test_eval(input), output.to_string());
        }
    }

    #[test]
    fn test_rest_function() {
        let tests = [(r#"rest([1, 2, 3])"#, "[2, 3]"), (r#"rest([2, 3])"#, "[3]")];

        for (input, output) in tests.iter() {
            println!("{} => {}", input, output);
            assert_eq!(test_eval(input).inspect(), *output);
        }
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";

        let evaluated = test_eval(input);

        if let Object::Array(ref array) = *evaluated {
            test_integer_object(&array.elements[0], 1);
            test_integer_object(&array.elements[1], 4);
            test_integer_object(&array.elements[2], 6)
        } else {
            panic!("not a array")
        }
    }

    #[test]
    fn test_array_index_expressions() {
        let tests = [
            ("[1, 2, 3][0]", 1),
            ("[1, 2, 3][1]", 2),
            ("[1, 2, 3][2]", 3),
            ("let i = 0; [1][i];", 1),
            ("[1, 2, 3][1 + 1];", 3),
            ("let myArray = [1, 2, 3]; myArray[2];", 3),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                6,
            ),
            ("let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]", 2),
        ];

        let null_tests = ["[1, 2, 3][3]", "[1, 2, 3][-1]"];

        for (input, output) in tests.iter() {
            test_integer_object(&test_eval(input), *output);
        }

        for input in null_tests {
            test_null_object(&test_eval(input));
        }
    }

    #[test]
    fn test_hash_literals() {
        let input = r#"
            let two = "two";
            {
              "one": 10 - 9,
              two: 1 + 1,
              "thr" + "ee": 6 / 2,
              4: 4,
              true: 5,
              false: 6
            }
"#;
        let evaluated = test_eval(input);

        if let Object::Hash(ref hash) = *evaluated {
            let expected: HashMap<HashKey, Rc<Object>> = HashMap::from([
                (
                    Str::new("one".into()).hash_key(),
                    Object::Integer(Integer::new(1)).into(),
                ),
                (
                    Str::new("two".into()).hash_key(),
                    Object::Integer(Integer::new(2)).into(),
                ),
                (
                    Str::new("three".into()).hash_key(),
                    Object::Integer(Integer::new(3)).into(),
                ),
                (
                    Integer::new(4).hash_key(),
                    Object::Integer(Integer::new(4)).into(),
                ),
                (
                    Boolean::new(true).hash_key(),
                    Object::Integer(Integer::new(5)).into(),
                ),
                (
                    Boolean::new(false).hash_key(),
                    Object::Integer(Integer::new(6)).into(),
                ),
            ]);
            assert_eq!(hash.value, expected);
        } else {
            panic!("not a hash")
        }
    }

    #[test]
    fn test_hash_index_expressions() {
        let tests = [
            (r#"{"foo": 5}["foo"]"#, 5),
            (r#"let key = "foo"; {"foo": 5}[key]"#, 5),
            (r#"{5: 5}[5]"#, 5),
            (r#"{true: 5}[true]"#, 5),
            (r#"{false: 5}[false]"#, 5),
        ];

        let null_tests = [r#"{"foo": 5}["bar"]"#, r#"{}["foo"]"#];

        for (input, output) in tests.iter() {
            test_integer_object(&test_eval(input), *output);
        }

        for input in null_tests.iter() {
            test_null_object(&test_eval(input));
        }
    }

    #[test]
    fn test_quote() {
        let tests = [
            ("quote(5)", "5"),
            ("quote(5 + 8)", "(5 + 8)"),
            ("quote(foobar)", "foobar"),
            ("quote(foobar + barfoo)", "(foobar + barfoo)"),
            ("let foobar = 8;quote(foobar)", "foobar"),
            ("let foobar = 8;quote(unquote(foobar))", "8"),
        ];

        for (input, output) in tests.iter() {
            test_quote_object(&test_eval(input), output);
        }
    }

    #[test]
    fn test_unquote() {
        let tests = [
            ("quote(unquote(4))", "4"),
            ("quote(unquote(4 + 4))", "8"),
            ("quote(8 + unquote(4 + 4))", "(8 + 8)"),
            ("quote(unquote(4 + 4) + 8)", "(8 + 8)"),
            ("quote(unquote(true))", "true"),
            ("quote(unquote(true == false))", "false"),
            ("quote(unquote(quote(4 + 4)))", "(4 + 4)"),
            (
                "let quotedInfixExpression = quote(4 + 4);
                quote(unquote(4 + 4) + unquote(quotedInfixExpression))",
                "(8 + (4 + 4))",
            ),
        ];

        for (input, output) in tests.iter() {
            test_quote_object(&test_eval(input), output);
        }
    }
}
