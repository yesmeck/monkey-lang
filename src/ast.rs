use std::fmt::Display;

use crate::token::Token;

trait Node  {
    fn token_literal() -> Token;
}


#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}


#[derive(Debug, PartialEq)]
pub struct LetStatement {
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    pub return_value: Expression
}

#[derive(Debug, PartialEq)]
pub struct ExpressionStatement {
    pub expression: Expression
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
}

#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub value: String
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>
}

