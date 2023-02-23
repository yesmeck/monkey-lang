use crate::{ast::Program, lexer::Lexer, object::Object, parser::Parser};

#[derive(Debug)]
pub enum ExpectedValue<'a> {
    Integer(i64),
    Boolean(bool),
    String(&'a str),
    Array(Vec<i64>),
    Hash(Vec<(i64, i64)>),
    Null,
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

pub fn test_null_object(object: &Object) {
    assert!(matches!(*object, Object::Null(_)))
}

pub fn test_string_object(object: &Object, expected: &str) {
    if let Object::Str(ref string) = *object {
        assert_eq!(string.value, expected);
    } else {
        panic!("not a string");
    }
}

pub fn test_array_object(object: &Object, expected: &[i64]) {
    if let Object::Array(ref array) = *object {
        assert_eq!(array.elements.len(), expected.len());
        for (i, e) in expected.iter().enumerate()  {
            test_integer_object(&array.elements[i], *e);
        }
    } else {
        panic!("not a array");
    }
}

pub fn test_hash_object(object: &Object, expected: &[(i64, i64)]) {
    if let Object::Hash(ref hash) = *object {
        assert_eq!(hash.value.len(), expected.len());
        let mut expected_iter = expected.iter();
        for (key, value) in hash.value.iter() {
            if let Some((ek, ev)) = expected_iter.next() {
                assert_eq!(key.name, ek.to_string());
                test_integer_object(value, *ev);
            }
        }
    } else {
        panic!("not a hash")
    }
}
