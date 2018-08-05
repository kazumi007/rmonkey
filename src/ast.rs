use std::collections::HashMap;

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Statement {
    Let(Expression, Expression), // Expression::Identifier, Expression
    Return(Expression),
    Expression(Expression),
    BlockStatement(Vec<Statement>),
}

#[derive(PartialEq, Debug, Clone)]
pub enum PreOp {
    Neg, // -
    Not, // !
}

#[derive(PartialEq, Debug, Clone)]
pub enum InfixOp {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Eq,  // ==
    Neq, // !=
    Lt,  // <
    Gt,  // >
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Identifier(String),
    IntegerLiteral(i64),
    BooleanLiteral(bool),
    StringLiteral(String),
    Prefix(PreOp, Box<Expression>),
    ArrayLiteral(Vec<Expression>),
    Infix(Box<Expression>, InfixOp, Box<Expression>),
    HashLiteral(Vec<(Expression, Expression)>),
    FunctionLiteral(Vec<Expression>, Box<Statement>), //  Expression::Identifier, Expression::BlockStatement
    Index(Box<Expression>, Box<Expression>),                  // Left[Index]
    If(Box<Expression>, Box<Statement>, Option<Box<Statement>>),
    Call(Box<Expression>, Vec<Expression>), // Function, Arguments
}
