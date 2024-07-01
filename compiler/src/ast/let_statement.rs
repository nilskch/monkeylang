struct let_statement {
    token: Token,      // token.Let
    name: Identifier,  // five
    value: Expression, // 5
}

impl Statement for let_statement {}

impl Node for let_statement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
