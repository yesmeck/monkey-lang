use crate::object::{Array, Integer, Null, Object, RuntimeError};

#[derive(Debug)]
pub struct Builtin<'a> {
    functions: [&'a str; 4],
}

impl<'a> Builtin<'a> {
    pub fn new() -> Self {
        Self {
            functions: ["len", "first", "last", "rest"],
        }
    }

    pub fn function_exists(&self, func: &str) -> bool {
        self.functions.contains(&func)
    }

    pub fn try_function(&self, func: &str, args: Vec<Object>) -> Option<Object> {
        match func {
            "len" => Some(self.len(args)),
            "first" => Some(self.first(args)),
            "last" => Some(self.last(args)),
            "rest" => Some(self.rest(args)),
            _ => None,
        }
    }

    pub fn len(&self, args: Vec<Object>) -> Object {
        if args.len() != 1 {
            return Object::RuntimeError(RuntimeError::new(format!(
                "wrong number of arguments. got={}, want=1",
                args.len()
            )));
        }

        match args[0] {
            Object::Str(ref string) => Object::Integer(Integer::new(string.value.len() as i64)),
            Object::Array(ref array) => Object::Integer(Integer::new(array.elements.len() as i64)),
            _ => Object::RuntimeError(RuntimeError::new(format!(
                "argument to `len` not supported, got {}",
                args[0].kind()
            ))),
        }
    }

    pub fn first(&self, args: Vec<Object>) -> Object {
        if args.len() != 1 {
            return Object::RuntimeError(RuntimeError::new(format!(
                "wrong number of arguments. got={}, want=1",
                args.len()
            )));
        }

        match args[0] {
            Object::Array(ref array) => array
                .elements
                .first()
                .unwrap_or(&Object::Null(Null::default()))
                .clone(),
            _ => Object::RuntimeError(RuntimeError::new(format!(
                "argument to `first` must be ARRAY, got {}",
                args[0].kind()
            ))),
        }
    }

    pub fn last(&self, args: Vec<Object>) -> Object {
        if args.len() != 1 {
            return Object::RuntimeError(RuntimeError::new(format!(
                "wrong number of arguments. got={}, want=1",
                args.len()
            )));
        }

        match args[0] {
            Object::Array(ref array) => array
                .elements
                .last()
                .unwrap_or(&Object::Null(Null::default()))
                .clone(),
            _ => Object::RuntimeError(RuntimeError::new(format!(
                "argument to `last` must be ARRAY, got {}",
                args[0].kind()
            ))),
        }
    }

    pub fn rest(&self, args: Vec<Object>) -> Object {
        if args.len() != 1 {
            return Object::RuntimeError(RuntimeError::new(format!(
                "wrong number of arguments. got={}, want=1",
                args.len()
            )));
        }

        match args[0] {
            Object::Array(ref array) => Object::Array(Array::new(
                array.elements[1..].into())
            ),
            _ => Object::RuntimeError(RuntimeError::new(format!(
                "argument to `last` must be ARRAY, got {}",
                args[0].kind()
            ))),
        }
    }
}
