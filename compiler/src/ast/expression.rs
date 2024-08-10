use crate::ast::statement::BlockStatement;
use crate::token::Token;
use std::hash::{Hash, Hasher};
use std::{
    collections::HashMap,
    fmt::{Display, Formatter, Result, Write},
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Expression {
    Ident(Identifier),
    Integer(IntegerLiteral),
    String(StringLiteral),
    Boolean(BooleanLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    IfElse(IfExpression),
    Function(FunctionLiteral),
    Call(CallExpression),
    Array(ArrayLiteral),
    Index(Index),
    Hash(HashLiteral),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Expression::Ident(ident) => write!(f, "{}", ident),
            Expression::Integer(integer) => write!(f, "{}", integer),
            Expression::Prefix(prefix_expr) => write!(f, "{}", prefix_expr),
            Expression::Infix(infix_expr) => write!(f, "{}", infix_expr),
            Expression::Boolean(bool_expr) => write!(f, "{}", bool_expr),
            Expression::IfElse(if_expr) => write!(f, "{}", if_expr),
            Expression::Function(func) => write!(f, "{}", func),
            Expression::Call(call_expr) => write!(f, "{}", call_expr),
            Expression::String(string) => write!(f, "{}", string),
            Expression::Array(arr) => write!(f, "{}", arr),
            Expression::Index(index) => write!(f, "{}", index),
            Expression::Hash(hash_literal) => write!(f, "{}", hash_literal),
        }
    }
}

impl Expression {
    #[allow(dead_code)]
    pub fn token_literal(&self) -> &str {
        match self {
            Expression::Ident(ident) => &ident.token.literal,
            Expression::Integer(integer) => &integer.token.literal,
            Expression::Prefix(prefix_expr) => &prefix_expr.token.literal,
            Expression::Infix(infix_expr) => &infix_expr.token.literal,
            Expression::Boolean(bool_expr) => &bool_expr.token.literal,
            Expression::IfElse(if_expr) => &if_expr.token.literal,
            Expression::Function(func) => &func.token.literal,
            Expression::Call(call_expr) => &call_expr.token.literal,
            Expression::String(string) => &string.token.literal,
            Expression::Array(arr) => &arr.token.literal,
            Expression::Index(index) => &index.token.literal,
            Expression::Hash(hash_literal) => &hash_literal.token.literal,
        }
    }

    pub fn format(&self, output_buffer: &mut String, depth: usize) {
        let prefix = "\t".repeat(depth);
        match self {
            Expression::Ident(ident) => write!(output_buffer, "{}", ident.value).unwrap(),
            Expression::Integer(integer) => write!(output_buffer, "{}", integer.value).unwrap(),
            Expression::Prefix(prefix_expr) => {
                write!(output_buffer, "{}", prefix_expr.operator).unwrap();
                prefix_expr.right.format(output_buffer, depth);
            }
            Expression::Infix(infix_expr) => {
                infix_expr.left.format(output_buffer, depth);
                write!(output_buffer, " {} ", infix_expr.operator).unwrap();
                infix_expr.right.format(output_buffer, depth);
            }
            Expression::Boolean(bool_expr) => write!(output_buffer, "{}", bool_expr.value).unwrap(),
            Expression::IfElse(if_expr) => {
                write!(output_buffer, "if (").unwrap();
                if_expr.condition.format(output_buffer, depth);
                if if_expr.consequence.statements.is_empty() {
                    write!(output_buffer, ") {{}}").unwrap();
                } else {
                    writeln!(output_buffer, ") {{").unwrap();
                    if_expr.consequence.format(output_buffer, depth);
                    write!(output_buffer, "{}}}", prefix).unwrap();
                }

                if let Some(alternative) = &if_expr.alternative {
                    if alternative.statements.is_empty() {
                        write!(output_buffer, " else {{}}").unwrap();
                    } else {
                        writeln!(output_buffer, " else {{").unwrap();
                        alternative.format(output_buffer, depth);
                        write!(output_buffer, "{}}}", prefix).unwrap();
                    }
                }
            }
            Expression::String(string) => write!(output_buffer, "\"{}\"", string.value).unwrap(),
            Expression::Array(arr) => {
                write!(output_buffer, "[").unwrap();
                for (idx, element) in arr.elements.iter().enumerate() {
                    if idx != 0 {
                        write!(output_buffer, ", ").unwrap();
                    }
                    element.format(output_buffer, depth);
                }
                write!(output_buffer, "]").unwrap();
            }
            Expression::Index(index) => {
                index.left.format(output_buffer, depth);
                write!(output_buffer, "[").unwrap();
                index.index.format(output_buffer, depth);
                write!(output_buffer, "]").unwrap();
            }
            Expression::Function(func) => {
                write!(output_buffer, "fn(").unwrap();
                for (idx, param) in func.parameters.iter().enumerate() {
                    if idx != 0 {
                        write!(output_buffer, ", ").unwrap();
                    }
                    write!(output_buffer, "{}", param.value).unwrap();
                }
                if func.body.statements.is_empty() {
                    write!(output_buffer, ") {{}}").unwrap();
                } else {
                    writeln!(output_buffer, ") {{").unwrap();
                    func.body.format(output_buffer, depth);
                    write!(output_buffer, "{}}}", prefix).unwrap();
                }
            }
            Expression::Call(call_expr) => {
                call_expr.function.format(output_buffer, depth);
                write!(output_buffer, "(").unwrap();
                for (idx, arg) in call_expr.arguments.iter().enumerate() {
                    if idx != 0 {
                        write!(output_buffer, ", ").unwrap();
                    }
                    arg.format(output_buffer, depth);
                }
                write!(output_buffer, ")").unwrap();
            }
            Expression::Hash(hash_literal) => {
                if hash_literal.pairs.is_empty() {
                    write!(output_buffer, "{{}}").unwrap();
                } else {
                    let prefix = "\t".repeat(depth + 1);
                    writeln!(output_buffer, "{{").unwrap();
                    for (idx, (key, value)) in hash_literal.clone().pairs.into_iter().enumerate() {
                        write!(output_buffer, "{}", prefix).unwrap();
                        key.format(output_buffer, depth + 1);
                        write!(output_buffer, ":").unwrap();
                        value.format(output_buffer, depth + 1);
                        if idx != hash_literal.pairs.len() - 1 {
                            writeln!(output_buffer, ",").unwrap();
                        }
                    }
                    write!(output_buffer, "\n{}}}", "\t".repeat(depth)).unwrap();
                }
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.value)
    }
}

impl Identifier {
    pub fn new(token: Token, value: String) -> Identifier {
        Identifier { token, value }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.token.literal)
    }
}

impl IntegerLiteral {
    pub fn new(token: Token, value: i64) -> IntegerLiteral {
        IntegerLiteral { token, value }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

impl Display for StringLiteral {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.value)
    }
}

impl StringLiteral {
    pub fn new(token: Token, value: String) -> StringLiteral {
        StringLiteral { token, value }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

impl PrefixExpression {
    pub fn new(token: Token, operator: String, right: Expression) -> PrefixExpression {
        PrefixExpression {
            token,
            operator,
            right: Box::new(right),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

impl InfixExpression {
    pub fn new(
        token: Token,
        left: Expression,
        operator: String,
        right: Expression,
    ) -> InfixExpression {
        InfixExpression {
            token,
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

impl Display for BooleanLiteral {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.token.literal)
    }
}

impl BooleanLiteral {
    pub fn new(token: Token, value: bool) -> BooleanLiteral {
        BooleanLiteral { token, value }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "if {} {}", self.condition, self.consequence)?;
        if let Some(alternative) = &self.alternative {
            write!(f, " else {}", alternative)?;
        }
        Ok(())
    }
}

impl IfExpression {
    pub fn new(
        token: Token,
        condition: Expression,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    ) -> IfExpression {
        IfExpression {
            token,
            condition: Box::new(condition),
            consequence,
            alternative,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let params: Vec<String> = self
            .parameters
            .clone()
            .into_iter()
            .map(|param| format!("{}", param))
            .collect();
        let params = params.join(", ");
        write!(f, "{}({}) {}", self.token.literal, params, self.body)
    }
}

impl FunctionLiteral {
    pub fn new(token: Token, parameters: Vec<Identifier>, body: BlockStatement) -> FunctionLiteral {
        FunctionLiteral {
            token,
            parameters,
            body,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<Expression>, // Identifier or FunctionLiteral
    pub arguments: Vec<Expression>,
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let arguments: Vec<String> = self
            .arguments
            .clone()
            .into_iter()
            .map(|param| format!("{}", param))
            .collect();
        let arguments = arguments.join(", ");
        write!(f, "{}({})", self.function, arguments)
    }
}

impl CallExpression {
    pub fn new(token: Token, function: Expression, arguments: Vec<Expression>) -> CallExpression {
        CallExpression {
            token,
            function: Box::new(function),
            arguments,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ArrayLiteral {
    pub token: Token,
    pub elements: Vec<Expression>,
}

impl Display for ArrayLiteral {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let elements: Vec<String> = self
            .elements
            .clone()
            .into_iter()
            .map(|elem| format!("{}", elem))
            .collect();
        let elements = elements.join(", ");
        write!(f, "[{}]", elements)
    }
}

impl ArrayLiteral {
    pub fn new(token: Token, elements: Vec<Expression>) -> ArrayLiteral {
        ArrayLiteral { token, elements }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Index {
    pub token: Token,
    pub left: Box<Expression>,
    pub index: Box<Expression>,
}

impl Display for Index {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "({}[{}])", self.left, self.index)
    }
}

impl Index {
    pub fn new(token: Token, left: Expression, index: Expression) -> Index {
        Index {
            token,
            left: Box::new(left),
            index: Box::new(index),
        }
    }
}

#[derive(PartialEq, Eq, Clone)]
pub struct HashLiteral {
    pub token: Token,
    pub pairs: HashMap<Expression, Expression>,
}

impl Display for HashLiteral {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let pairs: Vec<String> = self
            .pairs
            .clone()
            .into_iter()
            .map(|(key, value)| format!("{}:{}", key, value))
            .collect();
        let pairs = pairs.join(", ");
        write!(f, "{{{}}}", pairs)
    }
}

impl HashLiteral {
    pub fn new(token: Token, pairs: HashMap<Expression, Expression>) -> HashLiteral {
        HashLiteral { token, pairs }
    }
}

impl Hash for HashLiteral {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for (key, value) in self.pairs.iter() {
            key.hash(state);
            value.hash(state);
        }
    }
}
