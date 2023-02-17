use std::{cell::RefCell, rc::Rc};

use crate::{
    enviroment::Enviroment,
    object::{Array, Integer, Object, RuntimeError},
};

#[derive(Debug)]
pub struct Builtin<'a> {
    functions: [&'a str; 6],
    env: Rc<RefCell<Enviroment>>,
}

impl<'a> Builtin<'a> {
    pub fn new(env: Rc<RefCell<Enviroment>>) -> Self {
        Self {
            functions: ["len", "first", "last", "rest", "push", "puts"],
            env,
        }
    }

    pub fn function_exists(&self, func: &str) -> bool {
        self.functions.contains(&func)
    }

    pub fn try_function(&self, func: &str, args: Vec<Rc<Object>>) -> Option<Rc<Object>> {
        match func {
            "len" => Some(self.len(args)),
            "first" => Some(self.first(args)),
            "last" => Some(self.last(args)),
            "rest" => Some(self.rest(args)),
            "puts" => Some(self.puts(args)),
            "push" => Some(self.push(args)),
            _ => None,
        }
    }

    pub fn len(&self, args: Vec<Rc<Object>>) -> Rc<Object> {
        if args.len() != 1 {
            return Object::RuntimeError(RuntimeError::new(format!(
                "wrong number of arguments. got={}, want=1",
                args.len()
            )))
            .into();
        }

        match *args[0] {
            Object::Str(ref string) => {
                Object::Integer(Integer::new(string.value.len() as i64)).into()
            }
            Object::Array(ref array) => {
                Object::Integer(Integer::new(array.elements.len() as i64)).into()
            }
            _ => Object::RuntimeError(RuntimeError::new(format!(
                "argument to `len` not supported, got {}",
                args[0].kind()
            )))
            .into(),
        }
    }

    pub fn first(&self, args: Vec<Rc<Object>>) -> Rc<Object> {
        if args.len() != 1 {
            return Object::RuntimeError(RuntimeError::new(format!(
                "wrong number of arguments. got={}, want=1",
                args.len()
            )))
            .into();
        }

        match *args[0] {
            Object::Array(ref array) => Rc::clone(
                array
                    .elements
                    .first()
                    .unwrap_or(&self.env.borrow().null_object),
            ),
            _ => Object::RuntimeError(RuntimeError::new(format!(
                "argument to `first` must be ARRAY, got {}",
                args[0].kind()
            )))
            .into(),
        }
    }

    pub fn last(&self, args: Vec<Rc<Object>>) -> Rc<Object> {
        if args.len() != 1 {
            return Object::RuntimeError(RuntimeError::new(format!(
                "wrong number of arguments. got={}, want=1",
                args.len()
            )))
            .into();
        }

        match *args[0] {
            Object::Array(ref array) => Rc::clone(
                array
                    .elements
                    .last()
                    .unwrap_or(&self.env.borrow().null_object),
            ),
            _ => Object::RuntimeError(RuntimeError::new(format!(
                "argument to `last` must be ARRAY, got {}",
                args[0].kind()
            )))
            .into(),
        }
    }

    pub fn rest(&self, args: Vec<Rc<Object>>) -> Rc<Object> {
        if args.len() != 1 {
            return Object::RuntimeError(RuntimeError::new(format!(
                "wrong number of arguments. got={}, want=1",
                args.len()
            )))
            .into();
        }

        match *args[0] {
            Object::Array(ref array) => {
                Object::Array(Array::new(array.elements[1..].into())).into()
            }
            _ => Object::RuntimeError(RuntimeError::new(format!(
                "argument to `rest` must be ARRAY, got {}",
                args[0].kind()
            )))
            .into(),
        }
    }

    pub fn push(&self, args: Vec<Rc<Object>>) -> Rc<Object> {
        if args.len() != 2 {
            return Object::RuntimeError(RuntimeError::new(format!(
                "wrong number of arguments. got={}, want=2",
                args.len()
            )))
            .into();
        }

        match *args[0] {
            Object::Array(ref array) => {
                let mut elements = array.elements.clone();
                elements.push(Rc::clone(&args[1]));
                Object::Array(Array::new(elements)).into()
            }
            _ => Object::RuntimeError(RuntimeError::new(format!(
                "first argument to `push` must be ARRAY, got {}",
                args[0].kind()
            )))
            .into(),
        }
    }

    pub fn puts(&self, args: Vec<Rc<Object>>) -> Rc<Object> {
        for arg in args.iter() {
            println!("{}", arg.inspect());
        }

        Rc::clone(&self.env.borrow().null_object)
    }
}
