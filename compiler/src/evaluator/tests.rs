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
fn test_eval_integer_expression() -> Result<(), EvaluationError> {
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
        let evaluated = test_eval(input)?;
        test_integer_object(&evaluated, expected);
    }
    Ok(())
}

fn test_eval(input: &str) -> EvaluationResult {
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let program = match parser.parse_program() {
        Ok(program) => program,
        Err(err) => panic!("{}", err),
    };
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
fn test_boolean_expression() -> Result<(), EvaluationError> {
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
        ("1 > 2 && 2 < 1 == false", false),
        ("1 > 2 || 2 < 1 == false", true),
        ("(1 > 2 && 2 > 1) == false", true),
        ("(1 > 2 || 2 > 1) == false", false),
    ];
    for (input, expected) in tests {
        let evaluated = test_eval(input)?;
        test_boolean_object(evaluated, expected);
    }
    Ok(())
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
fn test_bang_operator() -> Result<(), EvaluationError> {
    let tests = [
        ("!true", false),
        ("!false", true),
        ("!5", false),
        ("!!true", true),
        ("!!false", false),
        ("!!5", true),
    ];
    for (input, expected) in tests {
        let evaluated = test_eval(input)?;
        test_boolean_object(evaluated, expected);
    }
    Ok(())
}

#[test]
fn test_if_else_expressions() -> Result<(), EvaluationError> {
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
        let evaluated = test_eval(input)?;
        test_object(evaluated, expected);
    }
    Ok(())
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
fn test_return_statements() -> Result<(), EvaluationError> {
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
        let evaluated = test_eval(input)?;
        test_integer_object(&evaluated, expected);
    }
    Ok(())
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
fn test_let_statements() -> Result<(), EvaluationError> {
    let tests = [
        ("let a = 5; a;", 5),
        ("let a = 5 * 5; a;", 25),
        ("let a = 5; let b = a; b;", 5),
        ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input)?;
        test_integer_object(&evaluated, expected);
    }
    Ok(())
}

#[test]
fn test_function_object() -> Result<(), EvaluationError> {
    let input = "fn(x) { x + 1;}";
    let evaluated = test_eval(input)?;

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
    Ok(())
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
fn test_string_literal() -> Result<(), EvaluationError> {
    let input = "\"Hello World!\"";
    let evaluated = test_eval(input)?;

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
    Ok(())
}

#[test]
fn test_string_concatenation() -> Result<(), EvaluationError> {
    let input = "\"Hello\" + \" \" + \"World\" + \"!\"";
    let evaluated = test_eval(input)?;

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
    Ok(())
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
fn test_array_literals() -> Result<(), EvaluationError> {
    let input = "[1, 2 * 2, 3 + 3]";
    let evaluated = test_eval(input)?;

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
    Ok(())
}

#[test]
fn test_array_index_expressions() -> Result<(), EvaluationError> {
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
        let evaluated = test_eval(input)?;

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
    Ok(())
}

#[test]
fn test_hash_literals() -> Result<(), EvaluationError> {
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

    let evaluated = test_eval(input)?;
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
    Ok(())
}

#[test]
fn test_hash_index_expression() -> Result<(), EvaluationError> {
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
        let evaluated = test_eval(input)?;
        match expected {
            ExpectedValue::Integer(val) => test_integer_object(&evaluated, val),
            ExpectedValue::Null => test_null_object(&evaluated),
            _ => unreachable!(),
        }
    }
    Ok(())
}
