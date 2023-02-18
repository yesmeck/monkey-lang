use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{
        ArrayLiteral, BooleanExpression, CallExpression, Expression, ExpressionStatement,
        HashLiteral, HashMember, IntegerLiteral, NullLiteral, Program, Statement, StringLiteral,
    },
    enviroment::Enviroment,
    evaluator::Evaluator,
    object::{self, Macro, Object, ObjectKind, Quote},
    traverser::{Traverable, Visitor},
};

pub struct EvalUnqupteCalls {
    env: Rc<RefCell<Enviroment>>,
}

impl EvalUnqupteCalls {
    pub fn new(env: Rc<RefCell<Enviroment>>) -> Self {
        Self { env }
    }
}

impl EvalUnqupteCalls {
    fn convert_object_to_ast_node(object: &Object) -> Expression {
        match *object {
            Object::Integer(ref integer) => {
                Expression::IntegerLiteral(IntegerLiteral::new(integer.value.to_owned()))
            }
            Object::Str(ref str) => {
                Expression::StringLiteral(StringLiteral::new(str.value.to_owned()))
            }
            Object::Boolean(ref bool) => {
                Expression::Boolean(BooleanExpression::new(bool.value.to_owned()))
            }
            Object::Null(_) => Expression::NullLiteral(NullLiteral::default()),
            Object::Array(ref array) => Expression::ArrayLiteral(ArrayLiteral::new(
                array
                    .elements
                    .iter()
                    .map(|e| EvalUnqupteCalls::convert_object_to_ast_node(e))
                    .collect(),
            )),
            Object::Hash(ref hash) => Expression::HashLiteral(HashLiteral::new(
                hash.value
                    .iter()
                    .map(|(k, v)| {
                        let key = match k.kind {
                            ObjectKind::Boolean => Expression::Boolean(BooleanExpression::new(
                                k.name.parse::<bool>().unwrap(),
                            )),
                            ObjectKind::Integer => Expression::IntegerLiteral(IntegerLiteral::new(
                                k.name.parse::<i64>().unwrap(),
                            )),
                            ObjectKind::Str => {
                                Expression::StringLiteral(StringLiteral::new(k.name.to_owned()))
                            }
                            _ => Expression::NullLiteral(NullLiteral::default()),
                        };
                        let value = EvalUnqupteCalls::convert_object_to_ast_node(v);
                        HashMember::new(key, value)
                    })
                    .collect(),
            )),
            Object::Quote(ref quote) => quote.node.to_owned(),
            _ => Expression::NullLiteral(NullLiteral::default()),
        }
    }
}

impl Visitor for EvalUnqupteCalls {
    fn visit_mut_expression(&self, node: &mut Expression) {
        if let Expression::Call(ref call) = *node {
            if let Expression::Identifier(ref callee) = *call.callee {
                if callee.value == "unquote" && call.arguments.len() == 1 {
                    let mut program = Program {
                        statements: vec![Statement::Expression(ExpressionStatement::new(
                            call.arguments[0].to_owned(),
                        ))],
                    };
                    let mut evaluator = Evaluator::new(Rc::clone(&self.env));
                    let object = evaluator.eval(&mut program);
                    *node = EvalUnqupteCalls::convert_object_to_ast_node(&object);
                }
            }
        }
    }
}

#[derive(Debug)]
struct ExpandMacro {
    env: Rc<RefCell<Enviroment>>,
}

impl ExpandMacro {
    fn new(env: Rc<RefCell<Enviroment>>) -> Self {
        Self { env }
    }

    fn get_macro(&self, call: &CallExpression) -> Option<Rc<Object>> {
        if let Expression::Identifier(ref callee) = *call.callee {
            self.env.borrow().get(callee.value.to_owned())
        } else {
            None
        }
    }

    fn quote_args(&self, call: &CallExpression) -> Vec<Rc<Object>> {
        let mut args: Vec<_> = vec![];

        for arg in call.arguments.iter() {
            args.push(Rc::new(Object::Quote(Quote::new(arg.to_owned()))))
        }

        args
    }

    fn extend_macro_env(
        &self,
        macro_obj: &Macro,
        args: Vec<Rc<Object>>,
    ) -> Rc<RefCell<Enviroment>> {
        let env = Rc::new(RefCell::new(Enviroment::new(Rc::clone(&self.env))));

        for (i, parameter) in macro_obj.parameters.iter().enumerate() {
            env.borrow_mut()
                .set(parameter.value.to_owned(), Rc::clone(&args[i]));
        }

        env
    }
}

impl Visitor for ExpandMacro {
    fn visit_mut_expression(&self, node: &mut Expression) {
        if let Expression::Call(ref call) = *node {
            if let Some(object) = self.get_macro(call) {
                if let Object::Macro(ref macro_obj) = *object {
                    let args = self.quote_args(call);
                    let env = self.extend_macro_env(macro_obj, args);
                    let mut evaluator = Evaluator::new(env);
                    let mut program = Program {
                        statements: macro_obj.body.statements.to_owned(),
                    };
                    let object = evaluator.eval(&mut program);
                    if let Object::Quote(ref quote) = *object {
                        *node = quote.node.to_owned();
                    } else {
                        panic!("we only support returning AST-nodes from macros");
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct MacroExpension {
    env: Rc<RefCell<Enviroment>>,
}

impl MacroExpension {
    pub fn new(env: Rc<RefCell<Enviroment>>) -> Self {
        Self { env }
    }

    pub fn define_macros(&self, program: &mut Program) {
        let mut definitions: Vec<_> = vec![];
        let mut i = 0;

        program.statements.retain(|stmt| {
            let is_macro = self.is_macro_definition(stmt);
            if is_macro {
                self.add_macros(stmt);
                definitions.push((i, stmt.to_owned()));
            }
            i += 1;
            !is_macro
        });
    }

    pub fn expand_macros(&self, program: &mut Program) {
        let visitor = ExpandMacro::new(Rc::clone(&self.env));
        program.visit_mut(&visitor);
    }

    fn is_macro_definition(&self, stmt: &Statement) -> bool {
        if let Statement::Let(ref exp) = stmt {
            if let Expression::MacroLiteral(_) = exp.value {
                return true;
            }
        }
        false
    }

    fn add_macros(&self, stmt: &Statement) {
        if let Statement::Let(exp) = stmt {
            if let Expression::MacroLiteral(ref macro_liter) = exp.value {
                let macro_object = Object::Macro(object::Macro::new(
                    macro_liter.parameters.to_owned(),
                    macro_liter.body.to_owned(),
                    Rc::clone(&self.env),
                ));
                self.env
                    .borrow_mut()
                    .set(exp.name.value.to_owned(), macro_object.into());
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    use crate::{ast::Program, enviroment::Enviroment, lexer::Lexer, parser::Parser};

    use super::*;

    fn test_parse_program(input: &str) -> Program {
        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(&mut lexer);
        parser.parse_program()
    }

    #[test]
    fn test_define_macros() {
        let input = "
            let number = 1;
            let function = fn(x, y) { x + y };
            let mymacro = macro(x, y) { x + y; };
";

        let mut program = test_parse_program(input);
        let env = Rc::new(RefCell::new(Enviroment::default()));
        let makro = MacroExpension::new(Rc::clone(&env));
        makro.define_macros(&mut program);

        assert_eq!(program.statements.len(), 2);
        assert!(env.borrow().get("number".into()).is_none());
        assert!(env.borrow().get("function".into()).is_none());
        assert!(env.borrow().get("mymacro".into()).is_some());
    }

    #[test]
    fn test_expand_macros() {
        let tests = [
            (
                "let infixExpression = macro() { quote(1 + 2); };
             infixExpression();",
                "(1 + 2)",
            ),
            (
                "let reverse = macro(a, b) { quote(unquote(b) - unquote(a)); };
            reverse(2 + 2, 10 - 5);",
                "(10 - 5) - (2 + 2)",
            ),
        ];

        for (input, output) in tests.iter() {
            let mut program = test_parse_program(input);
            let expected = test_parse_program(output);

            let env = Rc::new(RefCell::new(Enviroment::default()));
            let makro = MacroExpension::new(Rc::clone(&env));
            makro.define_macros(&mut program);
            makro.expand_macros(&mut program);
            assert_eq!(format!("{}", program), format!("{}", expected));
        }
    }
}
