use std::fmt::{Display, Formatter, Result};

#[derive(Debug)]
pub struct EvaluationError {
    pub msg: String,
}

impl EvaluationError {
    pub fn new(msg: String) -> EvaluationError {
        EvaluationError { msg }
    }
}

impl Display for EvaluationError {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "ERROR: {}", self.msg)
    }
}
