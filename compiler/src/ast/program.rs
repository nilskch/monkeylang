use super::statement::Statement;

use std::fmt::{Display, Formatter, Result};

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    #[allow(dead_code)]
    fn token_literal(&self) -> &str {
        if self.statements.is_empty() {
            ""
        } else {
            self.statements[0].token_literal()
        }
    }
}

impl Program {
    pub fn new() -> Program {
        Program { statements: vec![] }
    }

    pub fn format(&self) -> String {
        let mut output_buffer = String::new();
        for stmt in &self.statements {
            stmt.format(&mut output_buffer, 0);
        }
        output_buffer
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}
