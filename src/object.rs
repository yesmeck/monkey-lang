use std::{cell::RefCell, collections::HashMap, fmt::Display, hash::Hasher, rc::Rc};

use fxhash::FxHasher64;

use crate::{
    ast::{BlockStatement, Identifier},
    enviroment::Enviroment,
};

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
pub enum ObjectKind {
    Integer,
    Str,
    Boolean,
    Null,
    ReturnValue,
    RuntimeError,
    Function,
    BuiltinFunction,
    Array,
    Hash,
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
            Self::Array => write!(f, "ARRAY"),
            Self::Hash => write!(f, "HASH"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(Integer),
    Str(Str),
    Boolean(Boolean),
    Array(Array),
    Hash(Hash),
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
            Self::Array(o) => o.kind(),
            Self::Hash(o) => o.kind(),
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
            Self::Array(o) => o.inspect(),
            Self::Hash(o) => o.inspect(),
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

pub trait HashKeyable {
    fn hash_key(&self) -> HashKey;
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

impl HashKeyable for Integer {
    fn hash_key(&self) -> HashKey {
        HashKey::new(ObjectKind::Integer, self.value.to_string(), self.value)
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

impl HashKeyable for Str {
    fn hash_key(&self) -> HashKey {
        let mut hasher = FxHasher64::default();
        hasher.write(self.value.as_bytes());
        HashKey::new(ObjectKind::Str, self.value.to_owned(), hasher.finish() as i64)
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

impl HashKeyable for Boolean {
    fn hash_key(&self) -> HashKey {
        let value = match self.value {
            true => 1,
            false => 0,
        };
        HashKey::new(ObjectKind::Boolean, self.value.to_string(), value)
    }
}

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Null {}

impl Inspector for Null {
    fn kind(&self) -> ObjectKind {
        ObjectKind::Null
    }

    fn inspect(&self) -> String {
        "null".into()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnValue {
    pub value: Rc<Object>,
}

impl ReturnValue {
    pub fn new(value: Rc<Object>) -> Self {
        Self {
            value
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

#[derive(Debug, PartialEq, Clone)]
pub struct Array {
    pub elements: Vec<Rc<Object>>,
}

impl Array {
    pub fn new(elements: Vec<Rc<Object>>) -> Self {
        Self { elements }
    }
}

impl Inspector for Array {
    fn kind(&self) -> ObjectKind {
        ObjectKind::Array
    }

    fn inspect(&self) -> String {
        format!(
            "[{}]",
            self.elements
                .iter()
                .map(|i| i.inspect())
                .collect::<Vec<String>>()
                .join(", "),
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Hash {
    pub value: HashMap<HashKey, Rc<Object>>,
}

impl Hash {
    pub fn new(value: HashMap<HashKey, Rc<Object>>) -> Self {
        Self { value }
    }
}

impl Inspector for Hash {
    fn kind(&self) -> ObjectKind {
        ObjectKind::Hash
    }

    fn inspect(&self) -> String {
        format!(
            "{{{}}}",
            self.value
                .iter()
                .map(|(k, v)| format!("{}: {}", k.name, v.inspect()))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
pub struct HashKey {
    pub kind: ObjectKind,
    pub name: String,
    pub value: i64,
}

impl HashKey {
    pub fn new(kind: ObjectKind, name: String, value: i64) -> Self {
        Self { kind, name, value }
    }
}
