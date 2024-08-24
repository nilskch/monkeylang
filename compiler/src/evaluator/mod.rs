use std::collections::HashMap;
use std::rc::Rc;

use builtin::Builtin;
use error::EvaluationError;

use crate::ast::expression::{Expression, HashLiteral, Identifier, IfExpression};
use crate::ast::program::Program;
use crate::ast::statement::{BlockStatement, Statement};
use crate::object::environment::{Env, Environment};
use crate::object::{Function, Object};

pub mod builtin;
mod error;

type EvaluationResult = Result<Object, EvaluationError>;

#[derive(Default)]
pub struct Evaluator {
    pub output_buffer: String,
    env: Env,
}

impl Evaluator {
    pub fn reset_output_buffer(&mut self) {
        self.output_buffer = String::new()
    }

    pub fn eval_program(&mut self, program: Program) -> EvaluationResult {
        let mut result = Object::Null;

        for stmt in program.statements {
            let value = self.eval_statement(stmt, &Rc::clone(&self.env))?;

            match value {
                Object::ReturnValue(value) => return Ok(*value),
                _ => result = value,
            }
        }

        Ok(result)
    }

    fn eval_block_statement(&mut self, block_stmt: BlockStatement, env: &Env) -> EvaluationResult {
        let mut result = Object::Null;

        for stmt in block_stmt.statements {
            let value = self.eval_statement(stmt, env)?;

            match value {
                Object::ReturnValue(_) => return Ok(value),
                _ => result = value,
            }
        }

        Ok(result)
    }

    fn eval_statement(&mut self, stmt: Statement, env: &Env) -> EvaluationResult {
        match stmt {
            Statement::Expr(expr_stmt) => self.eval_expression(expr_stmt.expression, env),
            Statement::Return(return_stmt) => {
                let value = self.eval_expression(return_stmt.return_value, env)?;
                Ok(Object::ReturnValue(Box::new(value)))
            }
            Statement::Let(let_stmt) => {
                let value = self.eval_expression(let_stmt.value, env)?;
                env.borrow_mut().set(let_stmt.name.value.to_string(), value);
                Ok(Object::Null)
            }
            Statement::EmptyLine => Ok(Object::Null),
        }
    }

    fn eval_expression(&mut self, expr: Expression, env: &Env) -> EvaluationResult {
        match expr {
            Expression::Integer(integer_literal) => Ok(Object::Integer(integer_literal.value)),
            Expression::Boolean(boolean_literal) => Ok(Object::Boolean(boolean_literal.value)),
            Expression::Prefix(prefix_expr) => {
                let right = self.eval_expression(*prefix_expr.right, env)?;
                self.eval_prefix_expression(&prefix_expr.operator, right)
            }
            Expression::Infix(infix_expr) => {
                let left = self.eval_expression(*infix_expr.left, env)?;
                let right = self.eval_expression(*infix_expr.right, env)?;
                self.eval_infix_expression(&infix_expr.operator, left, right)
            }
            Expression::IfElse(if_expr) => self.eval_if_expression(if_expr, env),
            Expression::Ident(ident_expr) => self.eval_identifier(ident_expr, env),
            Expression::Function(func_expr) => Ok(Object::Function(Function::new(
                func_expr.parameters,
                func_expr.body,
                Rc::clone(env),
            ))),
            Expression::Call(call_expr) => {
                let function = self.eval_expression(*call_expr.function, env)?;
                let args = self.eval_expressions(call_expr.arguments, env)?;
                self.apply_function(function, args)
            }
            Expression::String(string_literal) => Ok(Object::String(string_literal.value)),
            Expression::Array(arr) => {
                let elements = self.eval_expressions(arr.elements, env)?;
                Ok(Object::Array(elements))
            }
            Expression::Index(index) => {
                let left = self.eval_expression(*index.left, env)?;
                let index = self.eval_expression(*index.index, env)?;
                self.eval_index_expression(left, index)
            }
            Expression::Hash(hash_literal) => self.eval_hash_literal(hash_literal, env),
        }
    }

    fn eval_hash_literal(&mut self, hash_literal: HashLiteral, env: &Env) -> EvaluationResult {
        #[allow(clippy::mutable_key_type)]
        let mut pairs = HashMap::new();

        for (key, value) in hash_literal.pairs.into_iter() {
            let key = self.eval_expression(key, env)?;
            let value = self.eval_expression(value, env)?;
            pairs.insert(key, value);
        }

        Ok(Object::Hash(pairs))
    }

    fn eval_index_expression(&mut self, left: Object, index: Object) -> EvaluationResult {
        match left {
            Object::Array(arr) => match index {
                Object::Integer(idx) => self.eval_array_index_expression(arr, idx),
                _ => Err(EvaluationError::new(format!(
                    "unusable as hash key: {}",
                    index.object_type()
                ))),
            },
            Object::Hash(hash) => self.eval_hash_index_expression(hash, index),
            _ => Err(EvaluationError::new(format!(
                "index operator not supported: {}",
                left.object_type()
            ))),
        }
    }

    #[allow(clippy::mutable_key_type)]
    fn eval_hash_index_expression(
        &mut self,
        hash: HashMap<Object, Object>,
        index: Object,
    ) -> EvaluationResult {
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

    fn eval_array_index_expression(&mut self, array: Vec<Object>, index: i64) -> EvaluationResult {
        let max = (array.len() - 1) as i64;
        if index < 0 || index > max {
            // this is a language design decision: we don't want to
            // throw an out of bounce error, but return null
            Ok(Object::Null)
        } else {
            Ok(array[index as usize].clone())
        }
    }

    fn apply_function(&mut self, function: Object, args: Vec<Object>) -> EvaluationResult {
        match function {
            Object::Function(function) => {
                let extended_env = self.extended_function_env(&function, args);
                let evaluated = self.eval_block_statement(function.body, &extended_env)?;
                self.unwrap_return_value(evaluated)
            }
            Object::Builtin(builtin) => builtin.apply_func(args, &mut self.output_buffer),
            _ => Err(EvaluationError::new(format!(
                "not a function: {}",
                function.object_type()
            ))),
        }
    }

    fn unwrap_return_value(&mut self, obj: Object) -> EvaluationResult {
        if let Object::ReturnValue(val) = obj {
            Ok(*val)
        } else {
            Ok(obj)
        }
    }

    fn extended_function_env(&mut self, function: &Function, args: Vec<Object>) -> Env {
        let env = Environment::new_enclosed_environment(&function.env);

        for (param_idx, param) in function.parameters.iter().enumerate() {
            env.borrow_mut()
                .set(param.value.clone(), args[param_idx].clone());
        }

        env
    }

    fn eval_expressions(
        &mut self,
        expressions: Vec<Expression>,
        env: &Env,
    ) -> Result<Vec<Object>, EvaluationError> {
        let mut result = vec![];

        for expr in expressions {
            let evaluated = self.eval_expression(expr, env)?;
            result.push(evaluated);
        }
        Ok(result)
    }

    fn eval_identifier(&mut self, ident: Identifier, env: &Env) -> EvaluationResult {
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

    fn eval_prefix_expression(&mut self, operator: &str, right: Object) -> EvaluationResult {
        match operator {
            "!" => self.eval_bang_operator_expression(right),
            "-" => self.eval_minus_prefix_operator_expression(right),
            _ => Err(EvaluationError::new(format!(
                "unknown operator: {}{}",
                operator,
                right.object_type()
            ))),
        }
    }

    fn eval_minus_prefix_operator_expression(&mut self, right: Object) -> EvaluationResult {
        match right {
            Object::Integer(value) => Ok(Object::Integer(-value)),
            _ => Err(EvaluationError::new(format!(
                "unknown operator: -{}",
                right.object_type()
            ))),
        }
    }

    fn eval_bang_operator_expression(&mut self, right: Object) -> EvaluationResult {
        match right {
            Object::Boolean(value) => Ok(Object::Boolean(!value)),
            Object::Null => Ok(Object::Boolean(true)),
            _ => Ok(Object::Boolean(false)),
        }
    }

    fn eval_infix_expression(
        &mut self,
        operator: &str,
        left: Object,
        right: Object,
    ) -> EvaluationResult {
        if left.object_type() != right.object_type() {
            return Err(EvaluationError::new(format!(
                "type mismatch: {} {} {}",
                left.object_type(),
                operator,
                right.object_type()
            )));
        }

        match left {
            Object::String(_) => self.eval_string_infix_expression(operator, left, right),
            Object::Integer(_) => self.eval_integer_infix_expression(operator, left, right),
            Object::Boolean(_) => self.eval_boolean_infix_expression(operator, left, right),
            _ => Err(EvaluationError::new(format!(
                "unknown operator: {} {} {}",
                left.object_type(),
                operator,
                right.object_type()
            ))),
        }
    }

    fn eval_string_infix_expression(
        &mut self,
        operator: &str,
        left: Object,
        right: Object,
    ) -> EvaluationResult {
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

    fn eval_boolean_infix_expression(
        &mut self,
        operator: &str,
        left: Object,
        right: Object,
    ) -> EvaluationResult {
        let left_value = match left {
            Object::Boolean(value) => value,
            _ => unreachable!(),
        };
        let right_value = match right {
            Object::Boolean(value) => value,
            _ => unreachable!(),
        };

        match operator {
            "==" => Ok(Object::Boolean(left_value == right_value)),
            "!=" => Ok(Object::Boolean(left_value != right_value)),
            "&&" => Ok(Object::Boolean(left_value && right_value)),
            "||" => Ok(Object::Boolean(left_value || right_value)),
            _ => Err(EvaluationError::new(format!(
                "unknown operator: {} {} {}",
                left.object_type(),
                operator,
                right.object_type()
            ))),
        }
    }

    fn eval_integer_infix_expression(
        &mut self,
        operator: &str,
        left: Object,
        right: Object,
    ) -> EvaluationResult {
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

    fn eval_if_expression(&mut self, if_expr: IfExpression, env: &Env) -> EvaluationResult {
        let condition = self.eval_expression(*if_expr.condition, env)?;

        if self.is_truthy(condition) {
            return self.eval_block_statement(if_expr.consequence, env);
        }
        if let Some(alternative) = if_expr.alternative {
            return self.eval_block_statement(alternative, env);
        }

        Ok(Object::Null)
    }

    fn is_truthy(&self, object: Object) -> bool {
        match object {
            Object::Null => false,
            Object::Boolean(val) => val,
            _ => true,
        }
    }
}

#[cfg(test)]
mod tests;
