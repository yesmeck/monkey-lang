use std::{cell::RefCell, fmt::Display, rc::Rc};

use crate::{
    ast::{BlockStatement, Identifier},
    enviroment::Enviroment,
};

#[derive(Debug, PartialEq)]
pub enum ObjectKind {
    Integer,
    Str,
    Boolean,
    Null,
    ReturnValue,
    RuntimeError,
    Function,
    BuiltinFunction,
}

impl Display for ObjectKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer => write!(f, "INTEGER"),
            Self::Str => write!(f, "STRING"),
            Self::Boolean => write!(f, "BOOLEAN"),
            Self::Null => write!(f, "NULL"),
            Self::ReturnValue => write!(f, "RETURN VALUE"),
            Self::RuntimeError => write!(f, "RUNTIME ERROR"),
            Self::Function => write!(f, "FUNCTION"),
            Self::BuiltinFunction => write!(f, "BUILTIN"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(Integer),
    Str(Str),
    Boolean(Boolean),
    Null(Null),
    ReturnValue(ReturnValue),
    RuntimeError(RuntimeError),
    Function(Function),
    BuiltinFunction(BuiltinFunction),
}

impl Object {
    pub fn kind(&self) -> ObjectKind {
        match self {
            Self::Integer(o) => o.kind(),
            Self::Str(o) => o.kind(),
            Self::Boolean(o) => o.kind(),
            Self::Null(o) => o.kind(),
            Self::ReturnValue(o) => o.kind(),
            Self::RuntimeError(o) => o.kind(),
            Self::Function(o) => o.kind(),
            Self::BuiltinFunction(o) => o.kind(),
        }
    }

    pub fn inspect(&self) -> String {
        match self {
            Self::Integer(o) => o.inspect(),
            Self::Str(o) => o.inspect(),
            Self::Boolean(o) => o.inspect(),
            Self::Null(o) => o.inspect(),
            Self::ReturnValue(o) => o.inspect(),
            Self::RuntimeError(o) => o.inspect(),
            Self::Function(o) => o.inspect(),
            Self::BuiltinFunction(o) => o.inspect(),
        }
    }
}

pub trait Inspector {
    fn kind(&self) -> ObjectKind;
    fn inspect(&self) -> String;
}

#[derive(Debug, PartialEq, Clone)]
pub struct Integer {
    pub value: i64,
}

impl Integer {
    pub fn new(value: i64) -> Self {
        Self { value }
    }
}

impl Inspector for Integer {
    fn kind(&self) -> ObjectKind {
        ObjectKind::Integer
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Str {
    pub value: String,
}

impl Str {
    pub fn new(value: String) -> Self {
        Self { value }
    }
}

impl Inspector for Str {
    fn kind(&self) -> ObjectKind {
        ObjectKind::Str
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Boolean {
    pub value: bool,
}

impl Boolean {
    pub fn new(value: bool) -> Self {
        Self { value }
    }
}

impl Inspector for Boolean {
    fn kind(&self) -> ObjectKind {
        ObjectKind::Boolean
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Null {}

impl Inspector for Null {
    fn kind(&self) -> ObjectKind {
        ObjectKind::Null
    }

    fn inspect(&self) -> String {
        "Null".into()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnValue {
    pub value: Box<Object>,
}

impl ReturnValue {
    pub fn new(value: Object) -> Self {
        Self {
            value: Box::new(value),
        }
    }
}

impl Inspector for ReturnValue {
    fn kind(&self) -> ObjectKind {
        ObjectKind::ReturnValue
    }

    fn inspect(&self) -> String {
        self.value.inspect()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct RuntimeError {
    pub message: String,
}

impl RuntimeError {
    pub fn new(message: String) -> Self {
        Self { message }
    }
}

impl Inspector for RuntimeError {
    fn kind(&self) -> ObjectKind {
        ObjectKind::RuntimeError
    }

    fn inspect(&self) -> String {
        format!("Error: {}", self.message)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Rc<RefCell<Enviroment>>,
}

impl Function {
    pub fn new(
        parameters: Vec<Identifier>,
        body: BlockStatement,
        env: Rc<RefCell<Enviroment>>,
    ) -> Self {
        Self {
            parameters,
            body,
            env,
        }
    }
}

impl Inspector for Function {
    fn kind(&self) -> ObjectKind {
        ObjectKind::Function
    }

    fn inspect(&self) -> String {
        format!(
            "fn({}) {{\n{}\n}}",
            self.parameters
                .iter()
                .map(|i| format!("{i}"))
                .collect::<Vec<String>>()
                .join(", "),
            self.body
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BuiltinFunction {
    pub name: String,
}

impl BuiltinFunction {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}

impl Inspector for BuiltinFunction {
    fn kind(&self) -> ObjectKind {
        ObjectKind::BuiltinFunction
    }

    fn inspect(&self) -> String {
        "builtin function".into()
    }
}
