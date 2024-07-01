use crate::ast::expression::Identifier;
use crate::ast::program::Program;
use crate::ast::statement::{LetStatement, Statement};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

pub struct Parser {
    l: Lexer,
    cur_token: Token,
    peek_token: Token,
}

impl Parser {
    pub fn new(mut l: Lexer) -> Parser {
        Parser {
            cur_token: l.next_token(),
            peek_token: l.next_token(),
            l,
        }
    }

    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while self.cur_token.token_type != TokenType::Eof {
            if let Ok(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            }
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Result<Statement, ()> {
        // TODO: implement
        match self.cur_token.token_type {
            TokenType::Let => Ok(self.parse_let_statement()),
            _ => Err(()),
        }
    }

    fn parse_let_statement(&mut self) -> Statement {
        let token = self.cur_token.clone();

        if !self.expect_peek(TokenType::Ident) {
            return Statement::Nil;
        }
        let name = Identifier::new(self.cur_token.clone(), self.cur_token.literal.clone());

        if !self.expect_peek(TokenType::Assign) {
            return Statement::Nil;
        }

        // skip expression until we encouter semicolon

        while !self.cur_token_is(TokenType::Semicolon) {
            self.next_token()
        }

        Statement::Let(LetStatement::new(token, name))
    }

    fn cur_token_is(&self, token_type: TokenType) -> bool {
        self.cur_token.token_type == token_type
    }

    fn peek_token_is(&self, token_type: TokenType) -> bool {
        self.peek_token.token_type == token_type
    }

    fn expect_peek(&mut self, token_type: TokenType) -> bool {
        if self.peek_token_is(token_type) {
            self.next_token();
            true
        } else {
            false
        }
    }
}
