pub mod builtin;

use builtin::Builtin;

use crate::ast::expression::{Expression, Identifier, IfExpression};
use crate::ast::program::Program;
use crate::ast::statement::{BlockStatement, Statement};
use crate::object::environment::{Env, Environment};
use crate::object::{
    Function, Object, BOOLEAN_OBJ, ERROR_OBJ, INTEGER_OBJ, RETURN_VALUE_OBJ, STRING_OBJ,
};
use std::cell::RefCell;
use std::rc::Rc;

pub fn eval_program(program: Program, env: &Env) -> Object {
    let mut result = Object::Null;

    for stmt in program.statements {
        result = eval_statement(stmt, env);

        match result {
            Object::ReturnValue(value) => return *value,
            Object::Error(_) => return result,
            _ => continue,
        }
    }

    result
}

fn eval_block_statement(block_stmt: BlockStatement, env: &Env) -> Object {
    let mut result = Object::Null;

    for stmt in block_stmt.statements {
        result = eval_statement(stmt, env);

        let object_type = result.object_type();
        if object_type == RETURN_VALUE_OBJ || object_type == ERROR_OBJ {
            return result;
        }
    }

    result
}

fn eval_statement(stmt: Statement, env: &Env) -> Object {
    match stmt {
        Statement::Expr(expr_stmt) => eval_expression(expr_stmt.expression, env),
        Statement::Return(return_stmt) => {
            let value = eval_expression(return_stmt.return_value, env);
            if is_error(&value) {
                return value;
            }
            Object::ReturnValue(Box::new(value))
        }
        Statement::Let(let_stmt) => {
            let value = eval_expression(let_stmt.value, env);
            if is_error(&value) {
                return value;
            }

            env.borrow_mut().set(let_stmt.name.value.to_string(), value);
            Object::Null
        }
        _ => Object::Null,
    }
}

fn eval_expression(expr: Expression, env: &Env) -> Object {
    match expr {
        Expression::Integer(integer_literal) => Object::Integer(integer_literal.value),
        Expression::Boolean(boolean_litereal) => Object::Boolean(boolean_litereal.value),
        Expression::Prefix(prefix_expr) => {
            let right = eval_expression(*prefix_expr.right, env);
            if is_error(&right) {
                return right;
            }
            eval_prefix_expression(&prefix_expr.operator, right)
        }
        Expression::Infix(infix_expr) => {
            let left = eval_expression(*infix_expr.left, env);
            if is_error(&left) {
                return left;
            }
            let right = eval_expression(*infix_expr.right, env);
            if is_error(&right) {
                return right;
            }
            eval_infix_expression(&infix_expr.operator, left, right)
        }
        Expression::IfElse(if_expr) => eval_if_expression(if_expr, env),
        Expression::Ident(ident_expr) => eval_identifier(ident_expr, env),
        Expression::Function(func_expr) => Object::Function(Function::new(
            func_expr.parameters,
            func_expr.body,
            Rc::clone(env),
        )),
        Expression::Call(call_expr) => {
            let function = eval_expression(*call_expr.function, env);
            if is_error(&function) {
                return function;
            }
            let args = eval_expressions(call_expr.arguments, env);
            if args.len() == 1 && is_error(&args[0]) {
                return args[0].clone();
            }
            apply_function(function, args)
        }
        Expression::String(string_literal) => Object::String(string_literal.value),
        _ => Object::Null,
    }
}

fn apply_function(function: Object, args: Vec<Object>) -> Object {
    match function {
        Object::Function(function) => {
            let extended_env = extended_function_env(&function, args);
            let evaluated = eval_block_statement(function.body, &extended_env);
            unwrap_return_value(evaluated)
        }
        Object::Builtin(builtin) => builtin.apply_func(args),
        _ => Object::Error(format!("not a function: {}", function.object_type())),
    }
}

fn unwrap_return_value(obj: Object) -> Object {
    if let Object::ReturnValue(val) = obj {
        *val
    } else {
        obj
    }
}

fn extended_function_env(function: &Function, args: Vec<Object>) -> Env {
    let mut env = Environment::new_enclosed_environment(&function.env);

    for (param_idx, param) in function.parameters.iter().enumerate() {
        env.set(param.value.clone(), args[param_idx].clone());
    }

    Rc::from(RefCell::new(env))
}

fn eval_expressions(exprs: Vec<Expression>, env: &Env) -> Vec<Object> {
    let mut result = vec![];

    for expr in exprs {
        let evaluated = eval_expression(expr, env);
        if is_error(&evaluated) {
            return vec![evaluated];
        }
        result.push(evaluated);
    }
    result
}

fn eval_identifier(ident: Identifier, env: &Env) -> Object {
    if let Some(value) = env.borrow().get(&ident.value) {
        return value;
    }
    match Builtin::lookup(&ident.value) {
        Some(builtin) => builtin,
        None => Object::Error(format!("identifier not found: {}", ident.value)),
    }
}

fn eval_prefix_expression(operator: &str, right: Object) -> Object {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Object::Error(format!(
            "unknown operator: {}{}",
            operator,
            right.object_type()
        )),
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(value) => Object::Integer(-value),
        _ => Object::Error(format!("unknown operator: -{}", right.object_type())),
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::Boolean(value) => Object::Boolean(!value),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    }
}

fn eval_infix_expression(operator: &str, left: Object, right: Object) -> Object {
    if left.object_type() != right.object_type() {
        return Object::Error(format!(
            "type mismatch: {} {} {}",
            left.object_type(),
            operator,
            right.object_type()
        ));
    }

    match left.object_type() {
        STRING_OBJ => eval_string_infix_expression(operator, left, right),
        INTEGER_OBJ => eval_integer_infix_expression(operator, left, right),
        BOOLEAN_OBJ => eval_boolean_infix_expression(operator, left, right),
        _ => Object::Error(format!(
            "unknown operator: {} {} {}",
            left.object_type(),
            operator,
            right.object_type()
        )),
    }
}

fn eval_string_infix_expression(operator: &str, left: Object, right: Object) -> Object {
    let left_value = match &left {
        Object::String(value) => value,
        _ => return Object::Null,
    };
    let right_value = match &right {
        Object::String(value) => value,
        _ => return Object::Null,
    };

    match operator {
        "+" => Object::String(format!("{}{}", left_value, right_value)),
        _ => Object::Error(format!(
            "unknown operator: {} {} {}",
            left.object_type(),
            operator,
            right.object_type()
        )),
    }
}

fn eval_boolean_infix_expression(operator: &str, left: Object, right: Object) -> Object {
    let left_value = match left {
        Object::Boolean(value) => value,
        _ => return Object::Null,
    };
    let right_value = match right {
        Object::Boolean(value) => value,
        _ => return Object::Null,
    };

    match operator {
        "==" => Object::Boolean(left_value == right_value),
        "!=" => Object::Boolean(left_value != right_value),
        _ => Object::Error(format!(
            "unknown operator: {} {} {}",
            left.object_type(),
            operator,
            right.object_type()
        )),
    }
}

fn eval_integer_infix_expression(operator: &str, left: Object, right: Object) -> Object {
    let left_value = match left {
        Object::Integer(value) => value,
        _ => return Object::Null,
    };
    let right_value = match right {
        Object::Integer(value) => value,
        _ => return Object::Null,
    };

    match operator {
        "+" => Object::Integer(left_value + right_value),
        "-" => Object::Integer(left_value - right_value),
        "*" => Object::Integer(left_value * right_value),
        "/" => Object::Integer(left_value / right_value),
        "<" => Object::Boolean(left_value < right_value),
        ">" => Object::Boolean(left_value > right_value),
        "<=" => Object::Boolean(left_value <= right_value),
        ">=" => Object::Boolean(left_value >= right_value),
        "==" => Object::Boolean(left_value == right_value),
        "!=" => Object::Boolean(left_value != right_value),
        _ => Object::Error(format!(
            "unknown operator: {} {} {}",
            left.object_type(),
            operator,
            right.object_type()
        )),
    }
}

fn eval_if_expression(if_expr: IfExpression, env: &Env) -> Object {
    let condition = eval_expression(*if_expr.condition, env);
    if is_error(&condition) {
        return condition;
    }

    if is_truthy(condition) {
        return eval_block_statement(if_expr.consequence, env);
    }
    if let Some(alternative) = if_expr.alternative {
        return eval_block_statement(alternative, env);
    }

    Object::Null
}

fn is_truthy(object: Object) -> bool {
    match object {
        Object::Null => false,
        Object::Boolean(val) => val,
        _ => true,
    }
}

fn is_error(object: &Object) -> bool {
    match object {
        Object::Error(_) => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::object::environment::Environment;
    use crate::{
        lexer::Lexer,
        object::{Object, NULL_OBJ},
        parser::Parser,
    };
    use std::cell::RefCell;

    enum ExpectedValue {
        Integer(i64),
        String(String),
    }

    #[test]
    fn test_eval_integer_expression() {
        let tests = [
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_integer_object(evaluated, expected);
        }
    }

    fn test_eval(input: &str) -> Object {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let env = &Rc::from(RefCell::from(Environment::new()));

        eval_program(program, env)
    }

    fn test_integer_object(object: Object, expected: i64) {
        let integer_value = match object {
            Object::Integer(integer_value) => integer_value,
            _ => unreachable!(),
        };

        assert_eq!(
            integer_value, expected,
            "object has wrong value. got='{}'. wanted='{}'",
            integer_value, expected
        );
    }

    #[test]
    fn test_boolean_expression() {
        let tests = [
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("1 <= 2", true),
            ("1 >= 2", false),
            ("2 <= 2", true),
            ("2 >= 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_boolean_object(evaluated, expected);
        }
    }

    fn test_boolean_object(object: Object, expected: bool) {
        let boolean_value = match object {
            Object::Boolean(boolean_value) => boolean_value,
            _ => unreachable!(),
        };

        assert_eq!(
            boolean_value, expected,
            "object has wrong value. got='{}'. wanted='{}'",
            boolean_value, expected
        );
    }

    #[test]
    fn test_bang_operator() {
        let tests = [
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_boolean_object(evaluated, expected);
        }
    }

    #[test]
    fn test_if_else_expressions() {
        let tests = [
            ("if (true) { 10 }", Object::Integer(10)),
            ("if (false) { 10 }", Object::Null),
            ("if (1) { 10 }", Object::Integer(10)),
            ("if (1 < 2) { 10 }", Object::Integer(10)),
            ("if (1 > 2) { 10 }", Object::Null),
            ("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_object(evaluated, expected);
        }
    }

    fn test_object(object: Object, expected: Object) {
        let object_type = object.object_type();
        let expected_type = expected.object_type();

        assert_eq!(
            object_type, expected_type,
            "object.object_type() not '{}'. got='{}'",
            expected_type, object_type
        );

        match expected {
            Object::Integer(value) => test_integer_object(object, value),
            Object::Null => test_null_object(object),
            _ => unreachable!(),
        }
    }

    fn test_null_object(object: Object) {
        let object_type = object.object_type();
        assert_eq!(
            object_type, NULL_OBJ,
            "object.object_type() not '{}'. got='{}'",
            NULL_OBJ, object_type
        );
    }

    #[test]
    fn test_return_statements() {
        let tests = [
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            ("if (10 > 1) { return 10; }", 10),
            (
                "if (10 > 1) {
                    if (10 > 1) {
                        return 10;
                    }

                    return 1;
                }",
                10,
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_integer_object(evaluated, expected);
        }
    }

    #[test]
    fn test_error_handling() {
        let tests = [
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true;", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5;", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            ("foobar", "identifier not found: foobar"),
            ("\"Hello\" - \"World\"", "unknown operator: STRING - STRING"),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);

            let error_msg = match evaluated {
                Object::Error(msg) => msg,
                _ => unreachable!(),
            };

            assert_eq!(
                error_msg, expected,
                "wrong error message. got='{}'. expected='{}'",
                error_msg, expected
            );
        }
    }

    #[test]
    fn test_let_statements() {
        let tests = [
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_integer_object(evaluated, expected);
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 1;}";
        let evaluated = test_eval(input);

        let function = match evaluated {
            Object::Function(function) => function,
            _ => unreachable!(),
        };

        assert_eq!(
            function.parameters.len(),
            1,
            "expected 1 parameter. got='{}'",
            function.parameters.len()
        );

        let param_name = format!("{}", function.parameters[0]);
        assert_eq!(
            param_name,
            String::from("x"),
            "wrong parameter. wanted='x'. got='{}'",
            param_name
        );

        let body = format!("{}", function.body);
        let expected_body = String::from("(x + 1)");
        assert_eq!(
            body, expected_body,
            "wrong function body. wanted='{}'. got='{}'",
            expected_body, body
        );
    }

    #[test]
    fn test_function_application() {
        let tests = [
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { 2 * x; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        for (input, expected) in tests {
            test_integer_object(test_eval(input), expected);
        }
    }

    #[test]
    fn test_string_literal() {
        let input = "\"Hello World!\"";
        let evaluated = test_eval(input);

        let string_literal = match evaluated {
            Object::String(string_literal) => string_literal,
            _ => unreachable!(),
        };

        let expected = "Hello World!";
        assert_eq!(
            string_literal, expected,
            "String has the wrong value. expected='{}'. got='{}'",
            expected, string_literal
        );
    }

    #[test]
    fn test_string_concatenation() {
        let input = "\"Hello\" + \" \" + \"World\" + \"!\"";
        let evaluated = test_eval(input);

        let string_literal = match evaluated {
            Object::String(string_literal) => string_literal,
            _ => unreachable!(),
        };

        let expected = "Hello World!";
        assert_eq!(
            string_literal, expected,
            "String has the wrong value. expected='{}'. got='{}'",
            expected, string_literal
        );
    }

    #[test]
    fn test_builtin_functions() {
        let tests = [
            ("len(\"\")", ExpectedValue::Integer(0)),
            ("len(\"four\")", ExpectedValue::Integer(4)),
            ("len(\"hello world\")", ExpectedValue::Integer(11)),
            (
                "len(1)",
                ExpectedValue::String(String::from("argument to `len` not supported, got INTEGER")),
            ),
            (
                "len(\"one\", \"two\")",
                ExpectedValue::String(String::from("wrong number of arguments. got=2. want=1")),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match expected {
                ExpectedValue::Integer(val) => test_integer_object(evaluated, val),
                ExpectedValue::String(val) => {
                    let err_msg = match evaluated {
                        Object::Error(err_msg) => err_msg,
                        _ => unreachable!(),
                    };
                    assert_eq!(
                        err_msg, val,
                        "wrong error message. want='{}'. got='{}'",
                        val, err_msg
                    );
                }
            }
        }
    }
}
