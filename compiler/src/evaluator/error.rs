use std::fmt::{Display, Formatter, Result};

use crate::token::TokenPosition;

#[derive(Debug)]
pub struct EvaluationError {
    pub msg: String,
    pub token_position: TokenPosition,
}

impl EvaluationError {
    // TODO: parse token position into EvaluationError
    pub fn new(msg: String) -> EvaluationError {
        EvaluationError {
            msg,
            token_position: (0, 0),
        }
    }
}

impl Display for EvaluationError {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(
            f,
            "ERROR in line {}, col {}: {}",
            self.token_position.0, self.token_position.1, self.msg
        )
    }
}
