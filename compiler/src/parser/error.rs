use std::fmt::{Display, Formatter, Result};

use crate::token::TokenPosition;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParserError {
    pub msg: String,
    pub pos: TokenPosition,
}

impl ParserError {
    pub fn new(msg: String, pos: TokenPosition) -> ParserError {
        ParserError { msg, pos }
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(
            f,
            "ERROR in line {}, col {}: {}",
            self.pos.0 .0, self.pos.0 .1, self.msg
        )
    }
}
