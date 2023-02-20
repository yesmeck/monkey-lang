use crate::{object::Object, ast::Program, lexer::Lexer, parser::Parser};

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
