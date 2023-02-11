#[derive(Debug)]
pub enum ObjectKind {
    Integer,
    Boolean,
    Null,
}

#[derive(Debug, PartialEq)]
pub enum Object {
    Integer(Integer),
    Boolean(Boolean),
    Null(Null),
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Self::Integer(o) => o.inspect(),
            Self::Boolean(o) => o.inspect(),
            Self::Null(o) => o.inspect(),
        }
    }
}

pub trait Inspector {
    fn kind(&self) -> ObjectKind;
    fn inspect(&self) -> String;
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub struct Null {}

impl Inspector for Null {
    fn kind(&self) -> ObjectKind {
        ObjectKind::Null
    }

    fn inspect(&self) -> String {
        "Null".into()
    }
}
