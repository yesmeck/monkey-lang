use crate::{ast::Program, lexer::Lexer, object::Object, parser::Parser};

#[derive(Debug)]
pub enum ExpectedValue {
    Integer(i64),
    Boolean(bool),
}

pub fn parse(input: &str) -> Program {
    let mut lexer = Lexer::new(input);
    let mut parser = Parser::new(&mut lexer);
    parser.parse_program()
}

pub fn test_integer_object(object: &Object, expected: i64) {
    if let Object::Integer(ref integer) = *object {
        assert_eq!(integer.value, expected);
    } else {
        panic!("not a integer");
    }
}

pub fn test_boolean_object(object: &Object, expected: bool) {
    if let Object::Boolean(ref bool) = *object {
        assert_eq!(bool.value, expected);
    } else {
        panic!("not a boolean");
    }
}
