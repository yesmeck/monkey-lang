use std::collections::HashMap;
use std::mem::swap;

use crate::{
    ast::{
        Expression, ExpressionStatement, Identifier, LetStatement, Program, ReturnStatement,
        Statement,
    },
    lexer::Lexer,
    token::{Token, TokenKind},
};

const LOWEST: i8 = 1;
const EQUALS: i8 = 2;
const LESSGREATER: i8 = 3;
const SUM: i8 = 4;
const PRODUCT: i8 = 5;
const PREFIX: i8 = 6;
const CALL: i8 = 7;

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: &'a mut Lexer,

    cur_token: Token,
    peek_token: Token,

    pub errors: Vec<String>,
}

impl<'a> Parser<'a> {
    fn new(lexer: &'a mut Lexer) -> Self {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();
        Self {
            lexer,
            cur_token,
            peek_token,
            errors: vec![],
        }
    }

    fn next_token(&mut self) {
        swap(&mut self.cur_token, &mut self.peek_token);
        self.peek_token = self.lexer.next_token();
    }

    fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        while self.cur_token.0 != TokenKind::EOF {
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            }
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token {
            Token(TokenKind::Let, _) => self.parse_let_statement(),
            Token(TokenKind::Return, _) => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        if let Some(expression) = self.parse_expression(LOWEST) {
            let stmt = Statement::Expression(ExpressionStatement { expression });

            if self.peek_token_is(&TokenKind::Semicolon) {
                self.next_token();
            }

            return Some(stmt);
        }
        None
    }

    fn parse_expression(&mut self, precendence: i8) -> Option<Expression> {
        self.prefix_parse()
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        Some(Expression::Identifier(Identifier {
            value: self.cur_token.1.to_owned(),
        }))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        while !self.cur_token_is(&TokenKind::Semicolon) {
            self.next_token();
        }

        Some(Statement::Return(ReturnStatement {
            return_value: Expression::Identifier(Identifier { value: "".into() }),
        }))
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        if !self.expect_peek(TokenKind::Ident) {
            return None;
        }

        let name = Identifier {
            value: self.cur_token.1.to_owned(),
        };

        if !self.expect_peek(TokenKind::Assign) {
            return None;
        }

        while !self.cur_token_is(&TokenKind::Semicolon) {
            self.next_token();
        }

        Some(Statement::Let(LetStatement {
            name,
            value: Expression::Identifier(Identifier { value: "".into() }),
        }))
    }

    fn cur_token_is(&self, kind: &TokenKind) -> bool {
        self.cur_token.0 == *kind
    }

    fn peek_token_is(&self, kind: &TokenKind) -> bool {
        self.peek_token.0 == *kind
    }

    fn expect_peek(&mut self, kind: TokenKind) -> bool {
        if self.peek_token_is(&kind) {
            self.next_token();
            true
        } else {
            self.peek_error(&kind);
            false
        }
    }

    fn peek_error(&mut self, kind: &TokenKind) {
        self.errors.push(format!(
            "expected next token to be {:?}, got {:?} instead",
            kind, self.peek_token.0
        ))
    }

    fn prefix_parse(&mut self) -> Option<Expression> {
        match self.cur_token.0 {
            TokenKind::Ident => self.parse_identifier(),
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Expression, ExpressionStatement, Identifier, LetStatement, Statement},
        lexer::Lexer,
    };

    use super::Parser;

    #[test]
    fn test_let_statements() {
        let input = "
let x = 5;
let y = 10;
let foobar = 838383;
";
        let mut lexer = Lexer::new(input.into());
        let mut parser = Parser::new(&mut lexer);

        let program = parser.parse_program();
        let statements = program.statements;

        assert_eq!(statements.len(), 3);
        assert_eq!(parser.errors.len(), 0);

        assert_eq!(
            statements[0],
            Statement::Let(LetStatement {
                name: Identifier { value: "x".into() },
                value: Expression::Identifier(Identifier { value: "".into() })
            })
        );

        assert_eq!(
            statements[1],
            Statement::Let(LetStatement {
                name: Identifier { value: "y".into() },
                value: Expression::Identifier(Identifier { value: "".into() })
            })
        );

        assert_eq!(
            statements[2],
            Statement::Let(LetStatement {
                name: Identifier {
                    value: "foobar".into()
                },
                value: Expression::Identifier(Identifier { value: "".into() })
            })
        );
    }

    #[test]
    fn test_return_statements() {
        let input = "
return 5;
return 10;
return 993322;
";

        let mut lexer = Lexer::new(input.into());
        let mut parser = Parser::new(&mut lexer);

        assert_eq!(parser.parse_program().statements.len(), 3);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let mut lexer = Lexer::new(input.into());
        let mut parser = Parser::new(&mut lexer);

        assert_eq!(
            parser.parse_program().statements,
            vec![Statement::Expression(ExpressionStatement {
                expression: Expression::Identifier(Identifier { value: "foobar".into() })
            })]
        );
    }
}
