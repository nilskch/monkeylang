use std::fmt::{Display, Formatter, Result};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParserError {
    pub msg: String,
}

impl ParserError {
    pub fn new(msg: String) -> ParserError {
        ParserError { msg }
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.msg)
    }
}
