use std::fmt::{Display, Formatter, Result};

pub enum ObjectType {
    Integer,
    Boolean,
    Null,
}

impl Display for ObjectType {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            ObjectType::Integer => write!(f, "INTEGER"),
            ObjectType::Boolean => write!(f, "BOOLEAN"),
            ObjectType::Null => write!(f, "NULL"),
        }
    }
}

pub trait Object {
    fn object_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

pub struct Integer {
    value: i64,
}

impl Object for Integer {
    fn object_type(&self) -> ObjectType {
        ObjectType::Integer
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

impl Integer {
    pub fn new(value: i64) -> Integer {
        Integer { value }
    }
}

pub struct Boolean {
    value: bool,
}

impl Object for Boolean {
    fn object_type(&self) -> ObjectType {
        ObjectType::Boolean
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

impl Boolean {
    pub fn new(value: bool) -> Boolean {
        Boolean { value }
    }
}

pub struct Null {}

impl Object for Null {
    fn object_type(&self) -> ObjectType {
        ObjectType::Null
    }

    fn inspect(&self) -> String {
        String::from("null")
    }
}

impl Null {
    pub fn new() -> Null {
        Null {}
    }
}
