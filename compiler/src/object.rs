use lazy_static::lazy_static;
use std::fmt::{Display, Formatter, Result};

//pub const BOOLEAN_OBJ: &str = "BOOLEAN";
//pub const INTEGER_OBJ: &str = "INTEGER";
//pub const NULL_OBJ: &str = "NULL";

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
    //pub fn object_type(&self) -> &str {
    //    match self {
    //        Object::Boolean(_) => BOOLEAN_OBJ,
    //        Object::Integer(_) => INTEGER_OBJ,
    //        Object::Null => NULL_OBJ,
    //    }
    //}

    pub fn inspect(&self) -> String {
        match self {
            Object::Boolean(value) => format!("{}", value),
            Object::Integer(value) => format!("{}", value),
            Object::Null => String::from("null"),
        }
    }
}

// TODO: use those somehow instead of creating so many new enums
lazy_static! {
    pub static ref NULL: Object = Object::Null;
    pub static ref TRUE: Object = Object::Boolean(true);
    pub static ref FALSE: Object = Object::Boolean(false);
}
