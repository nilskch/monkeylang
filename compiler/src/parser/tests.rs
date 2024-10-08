use super::*;

enum ExpectedValue {
    Integer(i64),
    String(String),
    Boolean(bool),
}

#[test]
fn test_let_statements() -> Result<(), ParserError> {
    let tests = [
        ("let x = 5;", "x", ExpectedValue::Integer(5)),
        ("let y = true;", "y", ExpectedValue::Boolean(true)),
        (
            "let foobar = y;",
            "foobar",
            ExpectedValue::String(String::from("y")),
        ),
    ];
    for (input, identifier, expected) in tests {
        let mut parser = Parser::new(input.to_string());
        let program = parser.parse_program()?;

        let num_stmt = program.statements.len();
        assert_eq!(
            num_stmt, 1,
            "program.statements.len() wrong. expected={}, got={}",
            1, num_stmt
        );

        let stmt = &program.statements[0];
        test_let_statement(stmt, identifier);

        let let_stmt = match stmt {
            Statement::Let(let_stmt) => let_stmt,
            _ => panic!("Expected Statement::Let"),
        };

        test_literal_expression(&let_stmt.value, expected)
    }
    Ok(())
}

fn test_let_statement(stmt: &Statement, name: &str) {
    let token_literal = stmt.token_literal();
    assert_eq!(
        token_literal, "let",
        "stmt.token_literal() not 'let'. got={}",
        token_literal
    );

    let let_stmt = match stmt {
        Statement::Let(stmt) => stmt,
        _ => panic!("Expected Statement::Let"),
    };

    let stmt_name = &let_stmt.name.value;
    assert_eq!(
        stmt_name, name,
        "let_stmt.name.value not '{}'. got='{}'",
        name, stmt_name
    );
}

#[test]
fn test_return_statements() -> Result<(), ParserError> {
    let tests = [
        ("return 5;", ExpectedValue::Integer(5)),
        ("return true;", ExpectedValue::Boolean(true)),
        (
            "return foobar;",
            ExpectedValue::String(String::from("foobar")),
        ),
    ];

    for (input, expected) in tests {
        let mut parser = Parser::new(input.to_string());
        let program = parser.parse_program()?;

        let num_stmt = program.statements.len();
        assert_eq!(
            num_stmt, 1,
            "program.statements.len() wrong. expected={}, got={}",
            1, num_stmt
        );

        let stmt = &program.statements[0];
        let token_literal = stmt.token_literal();
        assert_eq!(
            token_literal, "return",
            "stmt.token_literal() wrong. wanted='return', got='{}'",
            token_literal,
        );

        let return_stmt = match stmt {
            Statement::Return(stmt) => stmt,
            _ => panic!("Expected Statement::Return"),
        };

        test_literal_expression(&return_stmt.return_value, expected)
    }
    Ok(())
}

#[test]
fn test_indentifier_expression() -> Result<(), ParserError> {
    let input = "foobar";
    let mut parser = Parser::new(input.to_string());
    let program = parser.parse_program()?;

    let num_stmts = program.statements.len();
    assert_eq!(
        num_stmts, 1,
        "program.statements.len() wrong. expected={}, got={}",
        1, num_stmts
    );

    let stmt = &program.statements[0];
    let token_literal = stmt.token_literal();
    assert_eq!(
        token_literal, "foobar",
        "token_literal() not 'foobar'. got='{}'",
        token_literal
    );

    let expr_stmt = match stmt {
        Statement::Expr(stmt) => stmt,
        _ => panic!("Expected Statement::Expr"),
    };

    let ident = match &expr_stmt.expression {
        Expression::Ident(ident) => ident,
        _ => panic!("Expected Expression::Ident"),
    };

    assert_eq!(
        ident.value, "foobar",
        "ident.value not 'foobar'. got='{}'",
        ident.value
    );
    Ok(())
}

#[test]
fn test_integer_literal_expression() -> Result<(), ParserError> {
    let input = "5";
    let mut parser = Parser::new(input.to_string());
    let program = parser.parse_program()?;

    let num_stmts = program.statements.len();
    assert_eq!(
        num_stmts, 1,
        "program.statements.len() wrong. expected={}, got={}",
        1, num_stmts
    );

    let stmt = &program.statements[0];
    let token_literal = stmt.token_literal();
    assert_eq!(
        token_literal, "5",
        "stmt.token_literal() not 'foobar'. got='{}'",
        token_literal
    );

    let expr_stmt = match stmt {
        Statement::Expr(stmt) => stmt,
        _ => panic!("Expected Statement::Expr"),
    };

    let integer = match &expr_stmt.expression {
        Expression::Integer(integer) => integer,
        _ => panic!("Expected Expression::Integer"),
    };

    assert_eq!(
        integer.value, 5,
        "integer.value not 'foobar'. got='{}'",
        integer.value
    );
    Ok(())
}

#[test]
fn test_parsing_prefix_expressions() -> Result<(), ParserError> {
    let prefix_tests = [
        ("!5;", "!", ExpectedValue::Integer(5)),
        ("-15;", "-", ExpectedValue::Integer(15)),
        ("!true;", "!", ExpectedValue::Boolean(true)),
        ("!false;", "!", ExpectedValue::Boolean(false)),
    ];

    for (input, operator, value) in prefix_tests {
        let mut parser = Parser::new(input.to_string());
        let program = parser.parse_program()?;

        let num_stmts = program.statements.len();
        assert_eq!(
            num_stmts, 1,
            "program.statements.len() wrong. expected={}, got={}",
            1, num_stmts
        );

        let stmt = &program.statements[0];
        let expr_stmt = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => panic!("Expected Statement::Expr"),
        };

        let prefix_expr = match &expr_stmt.expression {
            Expression::Prefix(prefix_expr) => prefix_expr,
            _ => panic!("Expected Expression::Prefix"),
        };

        assert_eq!(
            prefix_expr.operator, operator,
            "prefix_expr.operator is not '{}'. got='{}'",
            operator, prefix_expr.operator
        );

        test_literal_expression(&*prefix_expr.right, value);
    }
    Ok(())
}

fn test_integer_literal(expr: &Expression, value: i64) {
    let token_literal = expr.token_literal();
    let expected_token_literal = value.to_string();
    assert_eq!(
        token_literal, expected_token_literal,
        "integer.token_literal() not '{}'. got=''{}",
        expected_token_literal, token_literal
    );

    let integer = match expr {
        Expression::Integer(integer) => integer,
        _ => panic!("Expected Expression::Integer"),
    };

    assert_eq!(
        integer.value, value,
        "integer.value not {}. got={}",
        value, integer.value
    );
}

#[test]
fn test_parsing_infix_expressions() -> Result<(), ParserError> {
    let infix_tests = [
        (
            "6 + 5;",
            ExpectedValue::Integer(6),
            "+",
            ExpectedValue::Integer(5),
        ),
        (
            "6 - 5;",
            ExpectedValue::Integer(6),
            "-",
            ExpectedValue::Integer(5),
        ),
        (
            "6 * 5;",
            ExpectedValue::Integer(6),
            "*",
            ExpectedValue::Integer(5),
        ),
        (
            "6 / 5;",
            ExpectedValue::Integer(6),
            "/",
            ExpectedValue::Integer(5),
        ),
        (
            "6 > 5;",
            ExpectedValue::Integer(6),
            ">",
            ExpectedValue::Integer(5),
        ),
        (
            "6 < 5;",
            ExpectedValue::Integer(6),
            "<",
            ExpectedValue::Integer(5),
        ),
        (
            "6 == 5;",
            ExpectedValue::Integer(6),
            "==",
            ExpectedValue::Integer(5),
        ),
        (
            "6 != 5;",
            ExpectedValue::Integer(6),
            "!=",
            ExpectedValue::Integer(5),
        ),
        (
            "6 <= 5;",
            ExpectedValue::Integer(6),
            "<=",
            ExpectedValue::Integer(5),
        ),
        (
            "6 >= 5;",
            ExpectedValue::Integer(6),
            ">=",
            ExpectedValue::Integer(5),
        ),
        (
            "true == true",
            ExpectedValue::Boolean(true),
            "==",
            ExpectedValue::Boolean(true),
        ),
        (
            "true != false",
            ExpectedValue::Boolean(true),
            "!=",
            ExpectedValue::Boolean(false),
        ),
        (
            "false == false",
            ExpectedValue::Boolean(false),
            "==",
            ExpectedValue::Boolean(false),
        ),
        (
            "false && false",
            ExpectedValue::Boolean(false),
            "&&",
            ExpectedValue::Boolean(false),
        ),
        (
            "false || 0",
            ExpectedValue::Boolean(false),
            "||",
            ExpectedValue::Integer(0),
        ),
    ];

    for (input, left_value, operator, right_value) in infix_tests {
        let mut parser = Parser::new(input.to_string());
        let program = parser.parse_program()?;

        let num_stmts = program.statements.len();
        assert_eq!(
            num_stmts, 1,
            "program.statements.len() wrong. expected={}, got={}",
            1, num_stmts
        );

        let stmt = &program.statements[0];
        let expr_stmt = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => panic!("Expected Statement::Exp"),
        };

        test_infix_expression(&expr_stmt.expression, left_value, operator, right_value)
    }
    Ok(())
}

#[test]
fn test_operator_precedence_parsing() -> Result<(), ParserError> {
    let tests = [
        ("-a * b", "((-a) * b)"),
        ("!-a", "(!(-a))"),
        ("a + b + c", "((a + b) + c)"),
        ("a + b - c", "((a + b) - c)"),
        ("a * b * c", "((a * b) * c)"),
        ("a * b / c", "((a * b) / c)"),
        ("a + b / c", "(a + (b / c))"),
        ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
        ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
        ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
        ("5 > 4 != 3 < 4", "((5 > 4) != (3 < 4))"),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        ),
        ("true;", "true"),
        ("false;", "false"),
        ("3 > 5 == false", "((3 > 5) == false)"),
        ("3 < 5 == true", "((3 < 5) == true)"),
        ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
        ("(5 + 5) * 2", "((5 + 5) * 2)"),
        ("2 / (5 + 5)", "(2 / (5 + 5))"),
        ("-(5 + 5)", "(-(5 + 5))"),
        ("!(true == true)", "(!(true == true))"),
        ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
        (
            "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        ),
        (
            "add(a + b + c * d / f + g)",
            "add((((a + b) + ((c * d) / f)) + g))",
        ),
        (
            "a * [1, 2, 3, 4][b * c] * d",
            "((a * ([1, 2, 3, 4][(b * c)])) * d)",
        ),
        (
            "add(a * b[2], b[1], 2 * [1, 2][1])",
            "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
        ),
    ];

    for (input, expected) in tests {
        let mut parser = Parser::new(input.to_string());
        let program = parser.parse_program()?;

        let actual = program.to_string();
        assert_eq!(
            actual, expected,
            "expected='{}'. got='{}'",
            expected, actual
        );
    }
    Ok(())
}

fn test_identifier(expr: &Expression, value: String) {
    let token_literal = expr.token_literal();
    assert_eq!(
        token_literal, value,
        "token_literal() not '{}', got='{}'",
        token_literal, value
    );

    let ident = match expr {
        Expression::Ident(ident) => ident,
        _ => panic!("Expected Expression::Ident"),
    };

    assert_eq!(
        ident.value, value,
        "ident.value not '{}', got='{}'",
        value, ident.value
    );
}

fn test_literal_expression(expr: &Expression, expected: ExpectedValue) {
    match expected {
        ExpectedValue::Integer(value) => test_integer_literal(expr, value),
        ExpectedValue::String(value) => test_identifier(expr, value),
        ExpectedValue::Boolean(value) => test_boolean_literal(expr, value),
    }
}

fn test_boolean_literal(expr: &Expression, value: bool) {
    let token_literal = expr.token_literal();
    let expected = format!("{}", value);
    assert_eq!(
        token_literal, expected,
        "token_literal() not '{}', got='{}'",
        expected, token_literal
    );

    let boolean_expr = match expr {
        Expression::Boolean(boolean_expr) => boolean_expr,
        _ => panic!("Expected Expression::Boolean"),
    };

    assert_eq!(
        boolean_expr.value, value,
        "boolean_expr.value not '{}'. got='{}'",
        value, boolean_expr.value
    );
}

fn test_infix_expression(
    expr: &Expression,
    left: ExpectedValue,
    operator: &str,
    right: ExpectedValue,
) {
    let infix_expr = match expr {
        Expression::Infix(infix_expr) => infix_expr,
        _ => panic!("Expected Expression::Infix"),
    };

    test_literal_expression(&*infix_expr.left, left);
    assert_eq!(
        infix_expr.operator, operator,
        "infix_expr.operator not '{}'. got='{}'",
        operator, infix_expr.operator
    );

    test_literal_expression(&*infix_expr.right, right);
}

#[test]
fn test_if_expression() -> Result<(), ParserError> {
    let input = "if (x < y) { x }";
    let mut parser = Parser::new(input.to_string());
    let program = parser.parse_program()?;

    let num_stmts = program.statements.len();
    assert_eq!(
        num_stmts, 1,
        "program.statements.len() wrong. expected={}, got={}",
        1, num_stmts
    );

    let stmt = &program.statements[0];
    let expr_stmt = match stmt {
        Statement::Expr(stmt) => stmt,
        _ => panic!("Expected Statement::Expr"),
    };

    let if_expr = match &expr_stmt.expression {
        Expression::IfElse(if_expr) => if_expr,
        _ => panic!("Expected Expression::IfElse"),
    };

    test_infix_expression(
        &*if_expr.condition,
        ExpectedValue::String("x".to_string()),
        "<",
        ExpectedValue::String("y".to_string()),
    );

    let num_stmts = if_expr.consequence.statements.len();
    assert_eq!(
        num_stmts, 1,
        "if_expr.consequence.statements.len() wrong. expected={}, got={}",
        1, num_stmts
    );

    let stmt = &if_expr.consequence.statements[0];
    let consequence = match stmt {
        Statement::Expr(stmt) => stmt,
        _ => panic!("Expected Statement::Expr"),
    };

    test_identifier(&consequence.expression, "x".to_string());

    if let Some(_) = if_expr.alternative {
        panic!("if_expr.alternative supposed to be empty");
    }

    Ok(())
}

#[test]
fn test_if_else_expression() -> Result<(), ParserError> {
    let input = "if (x < y) { x } else { y }";
    let mut parser = Parser::new(input.to_string());
    let program = parser.parse_program()?;

    let num_stmts = program.statements.len();
    assert_eq!(
        num_stmts, 1,
        "program.statements.len() wrong. expected={}, got={}",
        1, num_stmts
    );

    let stmt = &program.statements[0];
    let expr_stmt = match stmt {
        Statement::Expr(stmt) => stmt,
        _ => panic!("Expected Statement::Expr"),
    };

    let if_expr = match &expr_stmt.expression {
        Expression::IfElse(if_expr) => if_expr,
        _ => panic!("Expected Expression::IfElse"),
    };

    test_infix_expression(
        &*if_expr.condition,
        ExpectedValue::String(String::from("x")),
        "<",
        ExpectedValue::String(String::from("y")),
    );

    let num_stmts = if_expr.consequence.statements.len();
    assert_eq!(
        num_stmts, 1,
        "if_expr.consequence.statements.len() wrong. expected={}, got={}",
        1, num_stmts
    );

    let stmt = &if_expr.consequence.statements[0];
    let consequence = match stmt {
        Statement::Expr(stmt) => stmt,
        _ => panic!("Expected Statement::Expr"),
    };

    test_identifier(&consequence.expression, "x".to_string());

    let alternative = match &if_expr.alternative {
        Some(alternative) => alternative,
        _ => panic!("Expected a block statement"),
    };

    let num_stmts = alternative.statements.len();
    assert_eq!(
        num_stmts, 1,
        "alternative.statements.len() wrong. expected={}, got={}",
        1, num_stmts
    );

    let stmt = &alternative.statements[0];
    let alternative = match stmt {
        Statement::Expr(stmt) => stmt,
        _ => panic!("Expected Statement::Expr"),
    };

    test_identifier(&alternative.expression, String::from("y"));
    Ok(())
}

#[test]
fn test_parsing_function_literal() -> Result<(), ParserError> {
    let input = "fn(x, y) {x + y;}";
    let mut parser = Parser::new(input.to_string());
    let program = parser.parse_program()?;

    let num_stmts = program.statements.len();
    assert_eq!(
        num_stmts, 1,
        "program.statements.len() wrong. expected={}, got={}",
        1, num_stmts
    );

    let stmt = &program.statements[0];
    let expr_stmt = match stmt {
        Statement::Expr(stmt) => stmt,
        _ => panic!("Expected Statement::Expr"),
    };

    let func_literal = match &expr_stmt.expression {
        Expression::Function(func_literal) => func_literal,
        _ => panic!("Expected Expression::Function"),
    };

    assert_eq!(
        func_literal.parameters.len(),
        2,
        "func_literal.parameters.len() not '2'. got='{}'",
        func_literal.parameters.len()
    );

    test_identifier(
        &Expression::Ident(func_literal.parameters[0].clone()),
        String::from("x"),
    );
    test_identifier(
        &Expression::Ident(func_literal.parameters[1].clone()),
        String::from("y"),
    );

    assert_eq!(
        func_literal.body.statements.len(),
        1,
        "func_literal.body.statements.len() not '1'. got='{}'",
        func_literal.body.statements.len()
    );

    let stmt = &func_literal.body.statements[0];
    let expr_stmt = match stmt {
        Statement::Expr(stmt) => stmt,
        _ => panic!("Expected Statement::Expr"),
    };

    test_infix_expression(
        &expr_stmt.expression,
        ExpectedValue::String(String::from("x")),
        "+",
        ExpectedValue::String(String::from("y")),
    );
    Ok(())
}

#[test]
fn test_object_expression_parsing() -> Result<(), ParserError> {
    let input = "add(1, 2 * 3, 4 + 5)";
    let mut parser = Parser::new(input.to_string());
    let program = parser.parse_program()?;

    let num_stmts = program.statements.len();
    assert_eq!(
        num_stmts, 1,
        "program.statements.len() wrong. expected={}, got={}",
        1, num_stmts
    );

    let stmt = &program.statements[0];
    let expr_stmt = match stmt {
        Statement::Expr(stmt) => stmt,
        _ => panic!("Expected Statement::Expr"),
    };

    let object_expr = match &expr_stmt.expression {
        Expression::Call(object_expr) => object_expr,
        _ => panic!("Expected Expression::Call"),
    };

    test_identifier(&*object_expr.function, String::from("add"));
    assert_eq!(
        object_expr.arguments.len(),
        3,
        "Object_expr.arguments.len() not '3'. got='{}'",
        object_expr.arguments.len()
    );

    test_literal_expression(&object_expr.arguments[0], ExpectedValue::Integer(1));
    test_infix_expression(
        &object_expr.arguments[1],
        ExpectedValue::Integer(2),
        "*",
        ExpectedValue::Integer(3),
    );
    test_infix_expression(
        &object_expr.arguments[2],
        ExpectedValue::Integer(4),
        "+",
        ExpectedValue::Integer(5),
    );
    Ok(())
}

#[test]
fn test_parsing_string_literal() -> Result<(), ParserError> {
    let input = "\"hello world\"";
    let mut parser = Parser::new(input.to_string());
    let program = parser.parse_program()?;

    let stmt = &program.statements[0];
    let expr_stmt = match stmt {
        Statement::Expr(stmt) => stmt,
        _ => panic!("Expected Statement::Expr"),
    };

    let string_literal = match &expr_stmt.expression {
        Expression::String(string_literal) => string_literal,
        _ => panic!("Expected Expression::String"),
    };

    let expected = "hello world";
    assert_eq!(
        string_literal.value, expected,
        "invalid string literal. expected='{}'. got='{}'",
        expected, string_literal.value
    );
    Ok(())
}

#[test]
fn test_parsing_array_literals() -> Result<(), ParserError> {
    let input = "[1, 2 * 2, 3 + 3]";
    let mut parser = Parser::new(input.to_string());
    let program = parser.parse_program()?;

    let stmt = &program.statements[0];
    let expr_stmt = match stmt {
        Statement::Expr(stmt) => stmt,
        _ => panic!("Expected Statement::Expr"),
    };

    let array_literal = match &expr_stmt.expression {
        Expression::Array(array_literal) => array_literal,
        _ => panic!("Expected Expression::Array"),
    };

    assert_eq!(
        array_literal.elements.len(),
        3,
        "len(arr.elements) not 3. got={}",
        array_literal.elements.len()
    );

    test_integer_literal(&array_literal.elements[0], 1);
    test_infix_expression(
        &array_literal.elements[1],
        ExpectedValue::Integer(2),
        "*",
        ExpectedValue::Integer(2),
    );
    test_infix_expression(
        &array_literal.elements[2],
        ExpectedValue::Integer(3),
        "+",
        ExpectedValue::Integer(3),
    );
    Ok(())
}

#[test]
fn test_parsing_index_expressions() -> Result<(), ParserError> {
    let input = "myArray[1 + 1]";
    let mut parser = Parser::new(input.to_string());
    let program = parser.parse_program()?;

    let stmt = &program.statements[0];
    let expr_stmt = match stmt {
        Statement::Expr(stmt) => stmt,
        _ => panic!("Expected Statement::Expr"),
    };

    let index_expr = match &expr_stmt.expression {
        Expression::Index(index) => index,
        _ => panic!("Expected Expression::Index"),
    };

    test_identifier(&index_expr.left, String::from("myArray"));
    test_infix_expression(
        &index_expr.index,
        ExpectedValue::Integer(1),
        "+",
        ExpectedValue::Integer(1),
    );
    Ok(())
}

#[test]
fn test_parsing_hash_literal_string_keys() -> Result<(), ParserError> {
    let input = "{\"one\": 1, \"two\": 2, \"three\": 3}";
    let mut parser = Parser::new(input.to_string());
    let program = parser.parse_program()?;

    let stmt = &program.statements[0];
    let expr_stmt = match stmt {
        Statement::Expr(stmt) => stmt,
        _ => panic!("Expected Statement::Expr, got={}", stmt),
    };

    let hash_expr = match &expr_stmt.expression {
        Expression::Hash(hash_expr) => hash_expr,
        _ => panic!("Expected Expression::Hash"),
    };

    assert_eq!(
        hash_expr.pairs.len(),
        3,
        "hash.pairs has wrong length. want=3. got={}",
        hash_expr.pairs.len()
    );

    let mut expected = HashMap::new();
    expected.insert(String::from("one"), 1 as i64);
    expected.insert(String::from("two"), 2 as i64);
    expected.insert(String::from("three"), 3 as i64);

    for (key, value) in hash_expr.pairs.iter() {
        let string_literal = match key {
            Expression::String(string_literal) => string_literal,
            _ => panic!("Expected Expression::String"),
        };

        let expected_value = match expected.get(&string_literal.value) {
            Some(val) => *val,
            None => panic!("Expected to find a value"),
        };

        test_integer_literal(value, expected_value)
    }
    Ok(())
}

#[test]
fn test_parsing_empty_hash_literal() -> Result<(), ParserError> {
    let input = "{}";
    let mut parser = Parser::new(input.to_string());
    let program = parser.parse_program()?;

    let stmt = &program.statements[0];
    let expr_stmt = match stmt {
        Statement::Expr(stmt) => stmt,
        _ => panic!("Expected Statement::Expr"),
    };

    let hash_expr = match &expr_stmt.expression {
        Expression::Hash(hash_expr) => hash_expr,
        _ => panic!("Expected Expression::Hash"),
    };

    assert_eq!(
        hash_expr.pairs.len(),
        0,
        "hash.pairs has wrong length. want=0. got={}",
        hash_expr.pairs.len()
    );
    Ok(())
}

#[test]
fn test_parsing_hash_literal_with_expression() -> Result<(), ParserError> {
    let input = "{\"one\": 0 + 1, \"two\": 10 - 8, \"three\": 15 / 5}";
    let mut parser = Parser::new(input.to_string());

    let program = parser.parse_program()?;

    let stmt = &program.statements[0];
    let expr_stmt = match stmt {
        Statement::Expr(stmt) => stmt,
        _ => panic!("Expected Statement::Expr"),
    };

    let hash_expr = match &expr_stmt.expression {
        Expression::Hash(hash_expr) => hash_expr,
        _ => panic!("Expected Expression::Hash"),
    };

    assert_eq!(
        hash_expr.pairs.len(),
        3,
        "hash.pairs has wrong length. want=3. got={}",
        hash_expr.pairs.len()
    );

    for (key, value) in hash_expr.pairs.iter() {
        let string_literal = match key {
            Expression::String(string_literal) => string_literal,
            _ => panic!("Expected Expression::String"),
        };

        match string_literal.value.as_str() {
            "one" => test_infix_expression(
                value,
                ExpectedValue::Integer(0),
                "+",
                ExpectedValue::Integer(1),
            ),
            "two" => test_infix_expression(
                value,
                ExpectedValue::Integer(10),
                "-",
                ExpectedValue::Integer(8),
            ),
            "three" => test_infix_expression(
                value,
                ExpectedValue::Integer(15),
                "/",
                ExpectedValue::Integer(5),
            ),
            _ => panic!("Expected, 'one', 'two', or 'three'"),
        };
    }
    Ok(())
}
