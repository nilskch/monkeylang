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
    let mut result = Object::Null;

    for stmt in program.statements {
        let value = eval_statement(stmt, env)?;

        match value {
            Object::ReturnValue(value) => return Ok(*value),
            _ => result = value,
        }
    }

    Ok(result)
}

fn eval_block_statement(block_stmt: BlockStatement, env: &Env) -> EvaluationResult {
    let mut result = Object::Null;

    for stmt in block_stmt.statements {
        let value = eval_statement(stmt, env)?;

        match value {
            Object::ReturnValue(_) => return Ok(value),
            _ => result = value,
        }
    }

    Ok(result)
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
mod tests;
