pub mod builtin;
mod error;

use builtin::Builtin;
use error::EvaluationError;

use crate::ast::expression::{Expression, HashLiteral, Identifier, IfExpression};
use crate::ast::program::Program;
use crate::ast::statement::{BlockStatement, Statement};
use crate::object::environment::{Env, Environment};
use crate::object::{Function, Object};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

type EvaluationResult = Result<Object, EvaluationError>;

pub fn eval_program(program: Program, env: &Env) -> EvaluationResult {
    let mut result = Ok(Object::Null);

    for stmt in program.statements {
        let value = eval_statement(stmt, env)?;

        match value {
            Object::ReturnValue(value) => return Ok(*value),
            _ => result = Ok(value),
        }
    }

    result
}

fn eval_block_statement(block_stmt: BlockStatement, env: &Env) -> EvaluationResult {
    let mut result = Ok(Object::Null);

    for stmt in block_stmt.statements {
        let value = eval_statement(stmt, env)?;

        match value {
            Object::ReturnValue(_) => return Ok(value),
            _ => result = Ok(value),
        }
    }

    result
}

fn eval_statement(stmt: Statement, env: &Env) -> EvaluationResult {
    match stmt {
        Statement::Expr(expr_stmt) => eval_expression(expr_stmt.expression, env),
        Statement::Return(return_stmt) => {
            let value = eval_expression(return_stmt.return_value, env)?;
            Ok(Object::ReturnValue(Box::new(value)))
        }
        Statement::Let(let_stmt) => {
            let value = eval_expression(let_stmt.value, env)?;

            env.borrow_mut().set(let_stmt.name.value.to_string(), value);
            Ok(Object::Null)
        }
        _ => Ok(Object::Null),
    }
}

fn eval_expression(expr: Expression, env: &Env) -> EvaluationResult {
    match expr {
        Expression::Integer(integer_literal) => Ok(Object::Integer(integer_literal.value)),
        Expression::Boolean(boolean_litereal) => Ok(Object::Boolean(boolean_litereal.value)),
        Expression::Prefix(prefix_expr) => {
            let right = eval_expression(*prefix_expr.right, env)?;
            eval_prefix_expression(&prefix_expr.operator, right)
        }
        Expression::Infix(infix_expr) => {
            let left = eval_expression(*infix_expr.left, env)?;
            let right = eval_expression(*infix_expr.right, env)?;
            eval_infix_expression(&infix_expr.operator, left, right)
        }
        Expression::IfElse(if_expr) => eval_if_expression(if_expr, env),
        Expression::Ident(ident_expr) => eval_identifier(ident_expr, env),
        Expression::Function(func_expr) => Ok(Object::Function(Function::new(
            func_expr.parameters,
            func_expr.body,
            Rc::clone(env),
        ))),
        Expression::Call(call_expr) => {
            let function = eval_expression(*call_expr.function, env)?;
            let args = eval_expressions(call_expr.arguments, env)?;
            apply_function(function, args)
        }
        Expression::String(string_literal) => Ok(Object::String(string_literal.value)),
        Expression::Array(arr) => {
            let elements = eval_expressions(arr.elements, env)?;
            Ok(Object::Array(elements))
        }
        Expression::Index(index) => {
            let left = eval_expression(*index.left, env)?;
            let index = eval_expression(*index.index, env)?;
            eval_index_expression(left, index)
        }
        Expression::Hash(hash_literal) => eval_hash_literal(hash_literal, env),
        _ => Ok(Object::Null),
    }
}

fn eval_hash_literal(hash_literal: HashLiteral, env: &Env) -> EvaluationResult {
    let mut pairs = HashMap::new();

    for (key, value) in hash_literal.pairs.into_iter() {
        let key = eval_expression(key, env)?;
        let value = eval_expression(value, env)?;
        pairs.insert(key, value);
    }

    Ok(Object::Hash(pairs))
}

fn eval_index_expression(left: Object, index: Object) -> EvaluationResult {
    match left {
        Object::Array(arr) => match index {
            Object::Integer(idx) => eval_array_index_expression(arr, idx),
            _ => Err(EvaluationError::new(format!(
                "unusable as hash key: {}",
                index.object_type()
            ))),
        },
        Object::Hash(hash) => eval_hash_index_expression(hash, index),
        _ => Err(EvaluationError::new(format!(
            "index operator not supported: {}",
            left.object_type()
        ))),
    }
}

fn eval_hash_index_expression(hash: HashMap<Object, Object>, index: Object) -> EvaluationResult {
    if !index.is_hashable() {
        return Err(EvaluationError::new(format!(
            "unusable as hash key: {}",
            index.object_type()
        )));
    }

    match hash.get(&index) {
        Some(value) => Ok(value.clone()),
        None => Ok(Object::Null),
    }
}

fn eval_array_index_expression(array: Vec<Object>, index: i64) -> EvaluationResult {
    let max = (array.len() - 1) as i64;
    if index < 0 || index > max {
        // this is a language design decision: we don't want to
        // throw an out of bounce error, but return null
        Ok(Object::Null)
    } else {
        Ok(array[index as usize].clone())
    }
}

fn apply_function(function: Object, args: Vec<Object>) -> EvaluationResult {
    match function {
        Object::Function(function) => {
            let extended_env = extended_function_env(&function, args);
            let evaluated = eval_block_statement(function.body, &extended_env)?;
            unwrap_return_value(evaluated)
        }
        Object::Builtin(builtin) => builtin.apply_func(args),
        _ => Err(EvaluationError::new(format!(
            "not a function: {}",
            function.object_type()
        ))),
    }
}

fn unwrap_return_value(obj: Object) -> EvaluationResult {
    if let Object::ReturnValue(val) = obj {
        Ok(*val)
    } else {
        Ok(obj)
    }
}

fn extended_function_env(function: &Function, args: Vec<Object>) -> Env {
    let mut env = Environment::new_enclosed_environment(&function.env);

    for (param_idx, param) in function.parameters.iter().enumerate() {
        env.set(param.value.clone(), args[param_idx].clone());
    }

    Rc::from(RefCell::new(env))
}

fn eval_expressions(exprs: Vec<Expression>, env: &Env) -> Result<Vec<Object>, EvaluationError> {
    let mut result = vec![];

    for expr in exprs {
        let evaluated = eval_expression(expr, env)?;
        result.push(evaluated);
    }
    Ok(result)
}

fn eval_identifier(ident: Identifier, env: &Env) -> EvaluationResult {
    if let Some(value) = env.borrow().get(&ident.value) {
        return Ok(value);
    }
    match Builtin::lookup(&ident.value) {
        Some(builtin) => Ok(builtin),
        None => Err(EvaluationError::new(format!(
            "identifier not found: {}",
            ident.value
        ))),
    }
}

fn eval_prefix_expression(operator: &str, right: Object) -> EvaluationResult {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Err(EvaluationError::new(format!(
            "unknown operator: {}{}",
            operator,
            right.object_type()
        ))),
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> EvaluationResult {
    match right {
        Object::Integer(value) => Ok(Object::Integer(-value)),
        _ => Err(EvaluationError::new(format!(
            "unknown operator: -{}",
            right.object_type()
        ))),
    }
}

fn eval_bang_operator_expression(right: Object) -> EvaluationResult {
    match right {
        Object::Boolean(value) => Ok(Object::Boolean(!value)),
        Object::Null => Ok(Object::Boolean(true)),
        _ => Ok(Object::Boolean(false)),
    }
}

fn eval_infix_expression(operator: &str, left: Object, right: Object) -> EvaluationResult {
    if left.object_type() != right.object_type() {
        return Err(EvaluationError::new(format!(
            "type mismatch: {} {} {}",
            left.object_type(),
            operator,
            right.object_type()
        )));
    }

    match left {
        Object::String(_) => eval_string_infix_expression(operator, left, right),
        Object::Integer(_) => eval_integer_infix_expression(operator, left, right),
        Object::Boolean(_) => eval_boolean_infix_expression(operator, left, right),
        _ => Err(EvaluationError::new(format!(
            "unknown operator: {} {} {}",
            left.object_type(),
            operator,
            right.object_type()
        ))),
    }
}

fn eval_string_infix_expression(operator: &str, left: Object, right: Object) -> EvaluationResult {
    let left_value = match &left {
        Object::String(value) => value,
        _ => return Ok(Object::Null),
    };
    let right_value = match &right {
        Object::String(value) => value,
        _ => return Ok(Object::Null),
    };

    match operator {
        "+" => Ok(Object::String(format!("{}{}", left_value, right_value))),
        _ => Err(EvaluationError::new(format!(
            "unknown operator: {} {} {}",
            left.object_type(),
            operator,
            right.object_type()
        ))),
    }
}

fn eval_boolean_infix_expression(operator: &str, left: Object, right: Object) -> EvaluationResult {
    let left_value = match left {
        Object::Boolean(value) => value,
        _ => return Ok(Object::Null),
    };
    let right_value = match right {
        Object::Boolean(value) => value,
        _ => return Ok(Object::Null),
    };

    match operator {
        "==" => Ok(Object::Boolean(left_value == right_value)),
        "!=" => Ok(Object::Boolean(left_value != right_value)),
        _ => Err(EvaluationError::new(format!(
            "unknown operator: {} {} {}",
            left.object_type(),
            operator,
            right.object_type()
        ))),
    }
}

fn eval_integer_infix_expression(operator: &str, left: Object, right: Object) -> EvaluationResult {
    let left_value = match left {
        Object::Integer(value) => value,
        _ => return Ok(Object::Null),
    };
    let right_value = match right {
        Object::Integer(value) => value,
        _ => return Ok(Object::Null),
    };

    match operator {
        "+" => Ok(Object::Integer(left_value + right_value)),
        "-" => Ok(Object::Integer(left_value - right_value)),
        "*" => Ok(Object::Integer(left_value * right_value)),
        "/" => Ok(Object::Integer(left_value / right_value)),
        "<" => Ok(Object::Boolean(left_value < right_value)),
        ">" => Ok(Object::Boolean(left_value > right_value)),
        "<=" => Ok(Object::Boolean(left_value <= right_value)),
        ">=" => Ok(Object::Boolean(left_value >= right_value)),
        "==" => Ok(Object::Boolean(left_value == right_value)),
        "!=" => Ok(Object::Boolean(left_value != right_value)),
        _ => Err(EvaluationError::new(format!(
            "unknown operator: {} {} {}",
            left.object_type(),
            operator,
            right.object_type()
        ))),
    }
}

fn eval_if_expression(if_expr: IfExpression, env: &Env) -> EvaluationResult {
    let condition = eval_expression(*if_expr.condition, env)?;

    if is_truthy(condition) {
        return eval_block_statement(if_expr.consequence, env);
    }
    if let Some(alternative) = if_expr.alternative {
        return eval_block_statement(alternative, env);
    }

    Ok(Object::Null)
}

fn is_truthy(object: Object) -> bool {
    match object {
        Object::Null => false,
        Object::Boolean(val) => val,
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::object::environment::Environment;
    use crate::{lexer::Lexer, object::Object, parser::Parser};
    use std::cell::RefCell;

    enum ExpectedValue {
        Integer(i64),
        String(String),
        Null,
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
            let evaluated = test_eval(input).unwrap();
            test_integer_object(&evaluated, expected);
        }
    }

    fn test_eval(input: &str) -> EvaluationResult {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let env = &Rc::from(RefCell::from(Environment::new()));

        eval_program(program, env)
    }

    fn test_integer_object(object: &Object, expected: i64) {
        let integer_value = match *object {
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
            let evaluated = test_eval(input).unwrap();
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
            let evaluated = test_eval(input).unwrap();
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
            let evaluated = test_eval(input).unwrap();
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
            Object::Integer(value) => test_integer_object(&object, value),
            Object::Null => test_null_object(&object),
            _ => unreachable!(),
        }
    }

    fn test_null_object(object: &Object) {
        if let Object::Null = object {
            return;
        }
        let object_type = object.object_type();
        panic!("object is not NULL. got='{}'", object_type)
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
            let evaluated = test_eval(input).unwrap();
            test_integer_object(&evaluated, expected);
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
            (
                "{\"name\": \"Monkey\"}[fn(x) { x }];",
                "unusable as hash key: FUNCTION",
            ),
        ];

        for (input, expected) in tests {
            let err = match test_eval(input) {
                Ok(_) => unreachable!(),
                Err(err) => err,
            };

            assert_eq!(
                err.msg, expected,
                "wrong error message. got='{}'. expected='{}'",
                err.msg, expected
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
            let evaluated = test_eval(input).unwrap();
            test_integer_object(&evaluated, expected);
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 1;}";
        let evaluated = test_eval(input).unwrap();

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
            test_integer_object(&test_eval(input).unwrap(), expected);
        }
    }

    #[test]
    fn test_string_literal() {
        let input = "\"Hello World!\"";
        let evaluated = test_eval(input).unwrap();

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
        let evaluated = test_eval(input).unwrap();

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
                ExpectedValue::String(String::from("wrong number of arguments. want=1. got=2")),
            ),
        ];

        for (input, expected) in tests {
            match test_eval(input) {
                Ok(result) => match expected {
                    ExpectedValue::Integer(val) => test_integer_object(&result, val),
                    _ => unreachable!(),
                },
                Err(err) => match expected {
                    ExpectedValue::String(val) => assert_eq!(
                        err.msg, val,
                        "wrong error message. want='{}'. got='{}'",
                        val, err.msg
                    ),
                    _ => unreachable!(),
                },
            }
        }
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        let evaluated = test_eval(input).unwrap();

        let arr = match evaluated {
            Object::Array(arr) => arr,
            _ => unreachable!(),
        };

        assert_eq!(
            arr.len(),
            3,
            "array has wrong number of elements. want=3. got={}",
            arr.len()
        );
        test_integer_object(&arr[0], 1);
        test_integer_object(&arr[1], 4);
        test_integer_object(&arr[2], 6);
    }

    #[test]
    fn test_array_index_expressions() {
        let tests = [
            ("[1, 2, 3][0]", ExpectedValue::Integer(1)),
            ("[1, 2, 3][1]", ExpectedValue::Integer(2)),
            ("[1, 2, 3][2]", ExpectedValue::Integer(3)),
            ("let i = 0; [1][i];", ExpectedValue::Integer(1)),
            ("[1, 2, 3][1 + 1]", ExpectedValue::Integer(3)),
            (
                "let myArray = [1, 2, 3]; myArray[2];",
                ExpectedValue::Integer(3),
            ),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                ExpectedValue::Integer(6),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                ExpectedValue::Integer(2),
            ),
            ("[1, 2, 3][3]", ExpectedValue::Null),
            ("[1, 2, 3][-1]", ExpectedValue::Null),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();

            match expected {
                ExpectedValue::Integer(val) => test_integer_object(&evaluated, val),
                ExpectedValue::Null => match evaluated {
                    Object::Null => continue,
                    _ => panic!(
                        "Wrong evaluation. expected=NULL. got={}",
                        evaluated.object_type()
                    ),
                },
                _ => continue,
            }
        }
    }

    #[test]
    fn test_hash_literals() {
        let input = "
        let two = \"two\";
        {
            \"one\": 10 - 9,
            two: 1 + 1,
            \"thr\" + \"ee\": 6 / 2,
            4: 4,
            true: 5,
            false: 6
        }
        ";

        let evaluated = test_eval(input).unwrap();
        let hash_obj = match evaluated {
            Object::Hash(hash_obj) => hash_obj,
            _ => unreachable!(),
        };

        let mut expected = HashMap::new();
        expected.insert(Object::String(String::from("one")), 1);
        expected.insert(Object::String(String::from("two")), 2);
        expected.insert(Object::String(String::from("three")), 3);
        expected.insert(Object::Integer(4), 4);
        expected.insert(Object::Boolean(true), 5);
        expected.insert(Object::Boolean(false), 6);

        assert_eq!(
            hash_obj.len(),
            expected.len(),
            "hash has wrong number of pairs. want={}. got={}",
            expected.len(),
            hash_obj.len()
        );

        for (expected_key, expected_value) in expected.into_iter() {
            let object = match hash_obj.get(&expected_key) {
                Some(value) => value,
                None => unreachable!(),
            };

            test_integer_object(object, expected_value)
        }
    }

    #[test]
    fn test_hash_index_expression() {
        let tests = [
            ("{\"foo\": 5}[\"foo\"]", ExpectedValue::Integer(5)),
            (
                "let key = \"foo\"; {\"foo\": 5}[key]",
                ExpectedValue::Integer(5),
            ),
            ("{}[\"foo\"]", ExpectedValue::Null),
            ("{5: 5}[5]", ExpectedValue::Integer(5)),
            ("{true: 5}[true]", ExpectedValue::Integer(5)),
            ("{false: 5}[false]", ExpectedValue::Integer(5)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input).unwrap();
            match expected {
                ExpectedValue::Integer(val) => test_integer_object(&evaluated, val),
                ExpectedValue::Null => test_null_object(&evaluated),
                _ => unreachable!(),
            }
        }
    }
}
