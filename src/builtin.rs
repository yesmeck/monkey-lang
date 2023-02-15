use crate::object::{Integer, Null, Object, RuntimeError};

#[derive(Debug)]
pub struct Builtin<'a> {
    functions: [&'a str; 2],
}

impl<'a> Builtin<'a> {
    pub fn new() -> Self {
        Self { functions: ["len", "first"] }
    }

    pub fn function_exists(&self, func: &str) -> bool {
        self.functions.contains(&func)
    }

    pub fn try_function(&self, func: &str, args: Vec<Object>) -> Option<Object> {
        match func {
            "len" => Some(self.len(args)),
            "first" => Some(self.first(args)),
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
}
