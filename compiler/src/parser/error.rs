use std::fmt::{Display, Formatter, Result};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParserError {
    pub msg: String,
    pub line: i64,
}

impl ParserError {
    pub fn new(msg: String, line: i64) -> ParserError {
        ParserError { msg, line }
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "ERROR in line {}: {}", self.line, self.msg)
    }
}
