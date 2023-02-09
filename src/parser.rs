use std::mem::swap;

use crate::{
    ast::{
        Expression, ExpressionStatement, Identifier, IntegerLiteral, LetStatement,
        PrefixExpression, Program, ReturnStatement, Statement, InfixExpression,
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
        let mut left_exp = self.prefix_parse();
        if left_exp.is_none() {
            self.errors.push(format!(
                "no prefix parse function for {:?} found",
                self.cur_token.0
            ));
        }

        while !self.peek_token_is(&TokenKind::Semicolon) && precendence < self.peek_precedence() {
            self.next_token();

            left_exp = self.infix_parse(left_exp.unwrap());
        }

        left_exp
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        Some(Expression::Identifier(Identifier {
            value: self.cur_token.1.to_owned(),
        }))
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        Some(Expression::IntegerLiteral(IntegerLiteral {
            value: self.cur_token.1.parse::<i32>().unwrap(),
        }))
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let operator = self.cur_token.1.to_owned();

        self.next_token();

        let right = self.parse_expression(PREFIX).unwrap();

        Some(Expression::Prefix(PrefixExpression {
            operator,
            right: Box::new(right),
        }))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let operator = self.cur_token.1.to_owned();
        let precendence = self.cur_precendence();

        self.next_token();

        let right = self.parse_expression(precendence).unwrap();

        Some(Expression::Infix(InfixExpression {
            operator,
            left: Box::new(left),
            right: Box::new(right)
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
            TokenKind::Int => self.parse_integer_literal(),
            TokenKind::Bang | TokenKind::Minus => self.parse_prefix_expression(),
            _ => None,
        }
    }

    fn infix_parse(&mut self, left: Expression) -> Option<Expression> {
        match self.cur_token.0 {
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Slash
            | TokenKind::Asterisk
            | TokenKind::Eq
            | TokenKind::NotEq
            | TokenKind::Lt
            | TokenKind::Gt => self.parse_infix_expression(left),
            _ => None,
        }
    }

    fn peek_precedence(&self) -> i8 {
        self.get_precedence(&self.peek_token)
    }

    fn cur_precendence(&self) -> i8 {
        self.get_precedence(&self.cur_token)
    }

    fn get_precedence(&self, token: &Token) -> i8 {
        match token.0 {
            TokenKind::Eq => EQUALS,
            TokenKind::NotEq => EQUALS,
            TokenKind::Lt => LESSGREATER,
            TokenKind::Gt => LESSGREATER,
            TokenKind::Plus => SUM,
            TokenKind::Minus => SUM,
            TokenKind::Slash => PRODUCT,
            TokenKind::Asterisk => PRODUCT,
            _ => LOWEST,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{
            Expression, ExpressionStatement, Identifier, InfixExpression, IntegerLiteral,
            LetStatement, PrefixExpression, Statement,
        },
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
                expression: Expression::Identifier(Identifier {
                    value: "foobar".into()
                })
            })]
        );
    }

    #[test]
    fn test_integer_literal() {
        let input = "5";

        let mut lexer = Lexer::new(input.into());
        let mut parser = Parser::new(&mut lexer);

        assert_eq!(parser.errors.len(), 0);
        assert_eq!(
            parser.parse_program().statements,
            vec![Statement::Expression(ExpressionStatement {
                expression: Expression::IntegerLiteral(IntegerLiteral { value: 5 })
            })]
        );
    }

    #[test]
    fn test_prefix_expression() {
        let tests = [("!5", "!", 5), ("-15", "-", 15)];
        for (input, operator, value) in tests.iter() {
            let mut lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(&mut lexer);
            let program = parser.parse_program();

            assert_eq!(parser.errors.len(), 0);
            assert_eq!(
                program.statements,
                vec![Statement::Expression(ExpressionStatement {
                    expression: Expression::Prefix(PrefixExpression {
                        operator: operator.to_string(),
                        right: Box::new(Expression::IntegerLiteral(IntegerLiteral {
                            value: *value
                        }))
                    })
                })]
            );
        }
    }

    #[test]
    fn test_infix_expression() {
        let tests = [
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 == 5;", 5, "==", 5),
            ("5 != 5;", 5, "!=", 5),
        ];

        for (input, left, operator, right) in tests.iter() {
            let mut lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(&mut lexer);
            let program = parser.parse_program();

            assert_eq!(parser.errors.len(), 0);
            assert_eq!(
                program.statements,
                vec![Statement::Expression(ExpressionStatement {
                    expression: Expression::Infix(InfixExpression {
                        operator: operator.to_string(),
                        left: Box::new(Expression::IntegerLiteral(IntegerLiteral { value: *left })),
                        right: Box::new(Expression::IntegerLiteral(IntegerLiteral {
                            value: *right
                        }))
                    })
                })]
            );
        }
    }
}
