struct Program {
    statements: Vec<Box<dyn Statement>>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.is_empty() {
            return "".into();
        }
        self.statements[0].token_literal()
    }
}
