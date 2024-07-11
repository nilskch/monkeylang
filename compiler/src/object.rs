use std::fmt::{Display, Formatter, Result};

pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Object::Integer(_) => write!(f, "TODO"),
            Object::Boolean(_) => write!(f, "TODO"),
            Object::Null => write!(f, "TODO"),
        }
    }
}

impl Object {
    pub fn object_type(&self) -> String {
        match self {
            Object::Boolean(_) => String::from("BOOLEAN"),
            Object::Integer(_) => String::from("INTEGER"),
            Object::Null => String::from("NULL"),
        }
    }

    pub fn inspect(&self) -> String {
        match self {
            Object::Boolean(value) => format!("{}", value),
            Object::Integer(value) => format!("{}", value),
            Object::Null => String::from("null"),
        }
    }
}
