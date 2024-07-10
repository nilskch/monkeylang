use crate::ast::Node;
use crate::object::Object;

pub fn eval(node: impl Node) -> Object {
    Object::Null
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer::Lexer, object::Object, parser::Parser};

    #[test]
    fn test_eval_integer_expression() {
        let tests = [("5", 5), ("10", 10)];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_integer_object(evaluated, expected);
        }
    }

    fn test_eval(input: &str) -> Object {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        eval(program)
    }

    fn test_integer_object(object: Object, expected: i64) {
        // let integer_obj = match
    }
}
