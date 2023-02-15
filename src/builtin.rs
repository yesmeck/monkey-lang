use crate::object::{Integer, Object, RuntimeError};

#[derive(Debug)]
pub struct Builtin<'a> {
    functions: [&'a str; 1],
}

impl<'a> Builtin<'a> {
    pub fn new() -> Self {
        Self { functions: ["len"] }
    }

    pub fn function_exists(&self, func: &str) -> bool {
        self.functions.contains(&func)
    }

    pub fn try_function(&self, func: &str, args: Vec<Object>) -> Option<Object> {
        match func {
            "len" => Some(self.len(args)),
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
            _ => Object::RuntimeError(RuntimeError::new(format!(
                "argument to `len` not supported, got {}",
                args[0].kind()
            ))),
        }
    }
}
