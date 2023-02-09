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
    IntegerLiteral(IntegerLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression)
}

#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub value: String
}

#[derive(Debug, PartialEq)]
pub struct IntegerLiteral {
    pub value: i32
}

#[derive(Debug, PartialEq)]
pub struct PrefixExpression {
    pub operator: String,
    pub right: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct InfixExpression {
    pub operator: String,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>
}

