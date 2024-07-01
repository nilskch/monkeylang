pub mod expression;
pub mod program;
pub mod statement;

trait Node {
    fn token_literal(&self) -> String;
}
