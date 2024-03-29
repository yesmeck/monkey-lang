use std::mem::swap;

use crate::{
    ast::{
        ArrayLiteral, BlockStatement, BooleanExpression, CallExpression, Expression,
        ExpressionStatement, FunctionLiteral, HashLiteral, HashMember, Identifier, IfExpression,
        IndexExpression, InfixExpression, IntegerLiteral, LetStatement, MacroLiteral,
        PrefixExpression, Program, ReturnStatement, Statement, StringLiteral,
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
const INDEX: i8 = 8;

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: &'a mut Lexer,

    cur_token: Token,
    peek_token: Token,

    pub errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Self {
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

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        while self.cur_token.0 != TokenKind::Eof {
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
            let stmt = Statement::Expression(ExpressionStatement::new(expression));

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
            return None;
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
        Some(Expression::IntegerLiteral(IntegerLiteral::new(
            self.cur_token.1.parse::<i64>().unwrap(),
        )))
    }

    fn parse_string_literal(&mut self) -> Option<Expression> {
        Some(Expression::StringLiteral(StringLiteral::new(
            self.cur_token.1.to_owned(),
        )))
    }

    fn parse_array_literal(&mut self) -> Option<Expression> {
        Some(Expression::ArrayLiteral(ArrayLiteral::new(
            self.parse_expression_list(TokenKind::Rbracket),
        )))
    }

    fn parse_hash_literal(&mut self) -> Option<Expression> {
        let mut members: Vec<_> = vec![];

        while !self.peek_token_is(&TokenKind::Rbrace) {
            self.next_token();

            let key = self.parse_expression(LOWEST).unwrap();

            if !self.expect_peek(TokenKind::Colon) {
                continue;
            }

            self.next_token();

            let value = self.parse_expression(LOWEST).unwrap();

            members.push(HashMember::new(key, value));

            if !self.peek_token_is(&TokenKind::Rbrace) && !self.expect_peek(TokenKind::Comma) {
                return None;
            }
        }

        if !self.expect_peek(TokenKind::Rbrace) {
            None
        } else {
            Some(Expression::HashLiteral(HashLiteral::new(members)))
        }
    }

    fn parse_expression_list(&mut self, end_token: TokenKind) -> Vec<Expression> {
        let mut list: Vec<_> = vec![];

        if self.peek_token_is(&end_token) {
            self.next_token();
            return list;
        }

        self.next_token();
        list.push(self.parse_expression(LOWEST).unwrap());

        while self.peek_token_is(&TokenKind::Comma) {
            self.next_token();
            self.next_token();

            list.push(self.parse_expression(LOWEST).unwrap());
        }

        if !self.expect_peek(end_token) {
            return vec![];
        }

        list
    }

    fn parse_macro_literal(&mut self) -> Option<Expression> {
        if !self.expect_peek(TokenKind::Lparen) {
            return None;
        }

        let parameters = self.parse_function_parameters();

        if !self.expect_peek(TokenKind::Lbrace) {
            return None;
        }

        if let Some(body) = self.parse_block_statement() {
            return Some(Expression::MacroLiteral(MacroLiteral { parameters, body }));
        }

        None
    }

    fn parse_function_literal(&mut self) -> Option<Expression> {
        if !self.expect_peek(TokenKind::Lparen) {
            return None;
        }

        let parameters = self.parse_function_parameters();

        if !self.expect_peek(TokenKind::Lbrace) {
            return None;
        }

        if let Some(body) = self.parse_block_statement() {
            return Some(Expression::FunctionLiteral(FunctionLiteral::new(
                "".into(),
                parameters,
                body,
            )));
        }

        None
    }

    fn parse_function_parameters(&mut self) -> Vec<Identifier> {
        let mut identifiers: Vec<_> = vec![];

        if self.peek_token_is(&TokenKind::Rparen) {
            self.next_token();
            return identifiers;
        }

        self.next_token();

        let ident = Identifier {
            value: self.cur_token.1.to_owned(),
        };

        identifiers.push(ident);

        while self.peek_token_is(&TokenKind::Comma) {
            self.next_token();
            self.next_token();

            let ident = Identifier {
                value: self.cur_token.1.to_owned(),
            };

            identifiers.push(ident);
        }

        if !self.expect_peek(TokenKind::Rparen) {
            return vec![];
        }

        identifiers
    }

    fn parse_boolean(&mut self) -> Option<Expression> {
        Some(Expression::Boolean(BooleanExpression::new(
            self.cur_token.1.parse::<bool>().unwrap(),
        )))
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let operator = self.cur_token.1.to_owned();

        self.next_token();

        let right = self.parse_expression(PREFIX).unwrap();

        Some(Expression::Prefix(PrefixExpression::new(operator, right)))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let operator = self.cur_token.1.to_owned();
        let precendence = self.cur_precendence();

        self.next_token();

        if let Some(right) = self.parse_expression(precendence) {
            Some(Expression::Infix(InfixExpression::new(
                operator, left, right,
            )))
        } else {
            self.errors.push(format!("Parse error {}", operator));
            None
        }
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        Some(Expression::Call(CallExpression::new(
            function,
            self.parse_expression_list(TokenKind::Rparen),
        )))
    }

    fn parse_index_expression(&mut self, left: Expression) -> Option<Expression> {
        self.next_token();

        let index = self.parse_expression(LOWEST).unwrap();

        if !self.expect_peek(TokenKind::Rbracket) {
            return None;
        }

        Some(Expression::Index(IndexExpression::new(left, index)))
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();

        let exp = self.parse_expression(LOWEST);

        if !self.expect_peek(TokenKind::Rparen) {
            None
        } else {
            exp
        }
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        if !self.expect_peek(TokenKind::Lparen) {
            return None;
        }

        self.next_token();

        let condition = self.parse_expression(LOWEST).unwrap();

        if !self.expect_peek(TokenKind::Rparen) {
            return None;
        }

        if !self.expect_peek(TokenKind::Lbrace) {
            return None;
        }

        let consequence = self.parse_block_statement().unwrap();

        let mut exp = IfExpression {
            condition: Box::new(condition),
            consequence,
            alternative: None,
        };

        if self.peek_token_is(&TokenKind::Else) {
            self.next_token();

            if !self.expect_peek(TokenKind::Lbrace) {
                return None;
            }

            exp.alternative = self.parse_block_statement();
        }

        Some(Expression::If(exp))
    }

    fn parse_block_statement(&mut self) -> Option<BlockStatement> {
        let mut block = BlockStatement { statements: vec![] };

        self.next_token();

        while !self.cur_token_is(&TokenKind::Rbrace) && !self.cur_token_is(&TokenKind::Eof) {
            if let Some(stmt) = self.parse_statement() {
                block.statements.push(stmt);
            }
            self.next_token();
        }

        Some(block)
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        if let Some(return_value) = self.parse_expression(LOWEST) {
            if self.peek_token_is(&TokenKind::Semicolon) {
                self.next_token();
            }
            Some(Statement::Return(ReturnStatement::new(return_value)))
        } else {
            None
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        if !self.expect_peek(TokenKind::Ident) {
            return None;
        }

        let name = Identifier::new(self.cur_token.1.to_owned());

        if !self.expect_peek(TokenKind::Assign) {
            return None;
        }

        self.next_token();

        if let Some(ref mut value) = self.parse_expression(LOWEST) {
            if self.peek_token_is(&TokenKind::Semicolon) {
                self.next_token()
            }

            if let Expression::FunctionLiteral(ref mut func) = value {
                func.name = name.value.to_owned();
            }

            Some(Statement::Let(LetStatement::new(name, value.to_owned())))
        } else {
            None
        }
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
            TokenKind::String => self.parse_string_literal(),
            TokenKind::Function => self.parse_function_literal(),
            TokenKind::Macro => self.parse_macro_literal(),
            TokenKind::Lparen => self.parse_grouped_expression(),
            TokenKind::Lbracket => self.parse_array_literal(),
            TokenKind::Lbrace => self.parse_hash_literal(),
            TokenKind::If => self.parse_if_expression(),
            TokenKind::True | TokenKind::False => self.parse_boolean(),
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
            TokenKind::Lparen => self.parse_call_expression(left),
            TokenKind::Lbracket => self.parse_index_expression(left),
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
            TokenKind::Lparen => CALL,
            TokenKind::Lbracket => INDEX,
            _ => LOWEST,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{
            ArrayLiteral, BlockStatement, BooleanExpression, CallExpression, Expression,
            ExpressionStatement, FunctionLiteral, HashLiteral, HashMember, Identifier,
            IfExpression, IndexExpression, InfixExpression, IntegerLiteral, LetStatement,
            MacroLiteral, PrefixExpression, Program, ReturnStatement, Statement, StringLiteral,
        },
        lexer::Lexer,
    };

    use super::Parser;

    #[test]
    fn test_let_statements() {
        let input = "
let x = 5;
let y = true;
let foobar = y;
";
        let mut lexer = Lexer::new(input.into());
        let mut parser = Parser::new(&mut lexer);

        let program = parser.parse_program();
        let statements = program.statements;

        assert_eq!(statements.len(), 3);
        assert_eq!(parser.errors.len(), 0);

        assert_eq!(
            statements[0],
            Statement::Let(LetStatement::new(
                Identifier::new("x".into()),
                Expression::IntegerLiteral(IntegerLiteral::new(5))
            ))
        );

        assert_eq!(
            statements[1],
            Statement::Let(LetStatement::new(
                Identifier::new("y".into()),
                Expression::Boolean(BooleanExpression::new(true))
            ))
        );

        assert_eq!(
            statements[2],
            Statement::Let(LetStatement::new(
                Identifier::new("foobar".into()),
                Expression::Identifier(Identifier::new("y".into()))
            ))
        );
    }

    #[test]
    fn test_return_statements() {
        let input = "
return 5;
return true;
return y;
";

        let mut lexer = Lexer::new(input.into());
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(program.statements.len(), 3);
        assert_eq!(parser.errors.len(), 0);

        assert_eq!(program, {
            Program {
                statements: vec![
                    Statement::Return(ReturnStatement::new(Expression::IntegerLiteral(
                        IntegerLiteral::new(5),
                    ))),
                    Statement::Return(ReturnStatement::new(Expression::Boolean(
                        BooleanExpression::new(true),
                    ))),
                    Statement::Return(ReturnStatement::new(Expression::Identifier(
                        Identifier::new("y".into()),
                    ))),
                ],
            }
        })
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let mut lexer = Lexer::new(input.into());
        let mut parser = Parser::new(&mut lexer);

        assert_eq!(
            parser.parse_program().statements,
            vec![Statement::Expression(ExpressionStatement::new(
                Expression::Identifier(Identifier {
                    value: "foobar".into()
                })
            ))]
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
            vec![Statement::Expression(ExpressionStatement::new(
                Expression::IntegerLiteral(IntegerLiteral::new(5))
            ))]
        );
    }

    #[test]
    fn test_prefix_expression() {
        let tests = [("!5", "!", 5), ("-15", "-", 15)];
        for (input, operator, value) in tests.iter() {
            let mut lexer = Lexer::new(input);
            let mut parser = Parser::new(&mut lexer);
            let program = parser.parse_program();

            assert_eq!(parser.errors.len(), 0);
            assert_eq!(
                program.statements,
                vec![Statement::Expression(ExpressionStatement::new(
                    Expression::Prefix(PrefixExpression::new(
                        operator.to_string(),
                        Expression::IntegerLiteral(IntegerLiteral::new(*value))
                    ))
                ))]
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
            let mut lexer = Lexer::new(input);
            let mut parser = Parser::new(&mut lexer);
            let program = parser.parse_program();

            assert_eq!(parser.errors.len(), 0);
            assert_eq!(
                program.statements,
                vec![Statement::Expression(ExpressionStatement::new(
                    Expression::Infix(InfixExpression {
                        operator: operator.to_string(),
                        left: Box::new(Expression::IntegerLiteral(IntegerLiteral::new(*left))),
                        right: Box::new(Expression::IntegerLiteral(IntegerLiteral::new(*right)))
                    })
                ))]
            );
        }
    }

    #[test]
    fn test_operator_precedence() {
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
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("(5 + 5) * 2 * (5 + 5)", "(((5 + 5) * 2) * (5 + 5))"),
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

        for (input, output) in tests.iter() {
            let mut lexer = Lexer::new(input);
            let mut parser = Parser::new(&mut lexer);
            let program = parser.parse_program();

            assert_eq!(format!("{}", program), output.to_string());
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x } else { y }";

        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(
            program,
            Program {
                statements: vec![Statement::Expression(ExpressionStatement::new(
                    Expression::If(IfExpression {
                        condition: Box::new(Expression::Infix(InfixExpression {
                            operator: "<".into(),
                            left: Box::new(Expression::Identifier(Identifier {
                                value: "x".into()
                            })),
                            right: Box::new(Expression::Identifier(Identifier {
                                value: "y".into()
                            }))
                        })),
                        consequence: BlockStatement {
                            statements: vec![Statement::Expression(ExpressionStatement {
                                expression: Expression::Identifier(Identifier {
                                    value: "x".into()
                                })
                            })]
                        },
                        alternative: Some(BlockStatement {
                            statements: vec![Statement::Expression(ExpressionStatement {
                                expression: Expression::Identifier(Identifier {
                                    value: "y".into()
                                })
                            })]
                        })
                    })
                ))]
            }
        );
    }

    #[test]
    fn test_function_literal() {
        let input = "fn(x, y) { x + y };";

        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(
            program,
            Program {
                statements: vec![Statement::Expression(ExpressionStatement {
                    expression: Expression::FunctionLiteral(FunctionLiteral {
                        name: "".into(),
                        parameters: vec![
                            Identifier { value: "x".into() },
                            Identifier { value: "y".into() },
                        ],
                        body: BlockStatement {
                            statements: vec![Statement::Expression(ExpressionStatement {
                                expression: Expression::Infix(InfixExpression {
                                    operator: "+".into(),
                                    left: Box::new(Expression::Identifier(Identifier {
                                        value: "x".into()
                                    })),
                                    right: Box::new(Expression::Identifier(Identifier {
                                        value: "y".into()
                                    })),
                                })
                            })]
                        }
                    })
                })]
            }
        )
    }

    #[test]
    fn test_call_expression() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(
            program,
            Program {
                statements: vec![Statement::Expression(ExpressionStatement {
                    expression: Expression::Call(CallExpression::new(
                        Expression::Identifier(Identifier::new("add".into())),
                        vec![
                            Expression::IntegerLiteral(IntegerLiteral::new(1)),
                            Expression::Infix(InfixExpression::new(
                                "*".into(),
                                Expression::IntegerLiteral(IntegerLiteral::new(2)),
                                Expression::IntegerLiteral(IntegerLiteral::new(3)),
                            )),
                            Expression::Infix(InfixExpression::new(
                                "+".into(),
                                Expression::IntegerLiteral(IntegerLiteral::new(4)),
                                Expression::IntegerLiteral(IntegerLiteral::new(5)),
                            ))
                        ],
                    ))
                })]
            }
        );
    }

    #[test]
    fn test_string_literal_expression() {
        let input = "\"hello world!\"";

        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(
            program,
            Program {
                statements: vec![Statement::Expression(ExpressionStatement::new(
                    Expression::StringLiteral(StringLiteral::new("hello world!".into()))
                ))]
            }
        );
    }

    #[test]
    fn test_array_literals() {
        let input = "[1,  2 * 2, 3 + 3]";

        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(
            program,
            Program {
                statements: vec![Statement::Expression(ExpressionStatement::new(
                    Expression::ArrayLiteral(ArrayLiteral::new(vec![
                        Expression::IntegerLiteral(IntegerLiteral::new(1),),
                        Expression::Infix(InfixExpression::new(
                            "*".into(),
                            Expression::IntegerLiteral(IntegerLiteral::new(2)),
                            Expression::IntegerLiteral(IntegerLiteral::new(2)),
                        )),
                        Expression::Infix(InfixExpression::new(
                            "+".into(),
                            Expression::IntegerLiteral(IntegerLiteral::new(3)),
                            Expression::IntegerLiteral(IntegerLiteral::new(3)),
                        ))
                    ]))
                ))]
            }
        );
    }

    #[test]
    fn test_parsing_index_expressions() {
        let input = "myArray[1 + 1]";

        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(
            program,
            Program {
                statements: vec![Statement::Expression(ExpressionStatement::new(
                    Expression::Index(IndexExpression::new(
                        Expression::Identifier(Identifier::new("myArray".into())),
                        Expression::Infix(InfixExpression::new(
                            "+".into(),
                            Expression::IntegerLiteral(IntegerLiteral::new(1)),
                            Expression::IntegerLiteral(IntegerLiteral::new(1)),
                        ))
                    ))
                ))]
            }
        );
    }

    #[test]
    fn test_parsing_hash_literals_string_keys() {
        let input = r#"{"one": 1, "two": 2, "three": 3}"#;

        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(
            program,
            Program {
                statements: vec![Statement::Expression(ExpressionStatement::new(
                    Expression::HashLiteral(HashLiteral::new(vec![
                        HashMember::new(
                            Expression::StringLiteral(StringLiteral::new("one".into())),
                            Expression::IntegerLiteral(IntegerLiteral::new(1)),
                        ),
                        HashMember::new(
                            Expression::StringLiteral(StringLiteral::new("two".into())),
                            Expression::IntegerLiteral(IntegerLiteral::new(2)),
                        ),
                        HashMember::new(
                            Expression::StringLiteral(StringLiteral::new("three".into())),
                            Expression::IntegerLiteral(IntegerLiteral::new(3)),
                        )
                    ]))
                ))]
            }
        );
    }

    #[test]
    fn test_parsing_empty_hash_literal() {
        let input = "{}";

        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(
            program,
            Program {
                statements: vec![Statement::Expression(ExpressionStatement::new(
                    Expression::HashLiteral(HashLiteral::new(vec![]))
                ))]
            }
        );
    }

    #[test]
    fn test_parsing_hash_literals_with_expressions() {
        let input = r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#;

        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(
            program,
            Program {
                statements: vec![Statement::Expression(ExpressionStatement::new(
                    Expression::HashLiteral(HashLiteral::new(vec![
                        HashMember::new(
                            Expression::StringLiteral(StringLiteral::new("one".into())),
                            Expression::Infix(InfixExpression::new(
                                "+".into(),
                                Expression::IntegerLiteral(IntegerLiteral::new(0)),
                                Expression::IntegerLiteral(IntegerLiteral::new(1)),
                            ))
                        ),
                        HashMember::new(
                            Expression::StringLiteral(StringLiteral::new("two".into())),
                            Expression::Infix(InfixExpression::new(
                                "-".into(),
                                Expression::IntegerLiteral(IntegerLiteral::new(10)),
                                Expression::IntegerLiteral(IntegerLiteral::new(8)),
                            ))
                        ),
                        HashMember::new(
                            Expression::StringLiteral(StringLiteral::new("three".into())),
                            Expression::Infix(InfixExpression::new(
                                "/".into(),
                                Expression::IntegerLiteral(IntegerLiteral::new(15)),
                                Expression::IntegerLiteral(IntegerLiteral::new(5)),
                            ))
                        )
                    ]))
                ))]
            }
        );
    }

    #[test]
    fn test_macro_literal_parsing() {
        let input = "macro(x, y) { x + y; }";

        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(
            program,
            Program {
                statements: vec![Statement::Expression(ExpressionStatement::new(
                    Expression::MacroLiteral(MacroLiteral::new(
                        vec![Identifier::new("x".into()), Identifier::new("y".into()),],
                        BlockStatement {
                            statements: vec![Statement::Expression(ExpressionStatement::new(
                                Expression::Infix(InfixExpression {
                                    operator: "+".into(),
                                    left: Box::new(Expression::Identifier(Identifier::new(
                                        "x".into()
                                    ))),
                                    right: Box::new(Expression::Identifier(Identifier::new(
                                        "y".into()
                                    ))),
                                })
                            ))]
                        }
                    ))
                ))]
            }
        );
    }

    #[test]
    fn test_function_literal_with_name() {
        let input = "let myFunction = fn() { };";

        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        assert_eq!(
            program,
            Program {
                statements: vec![Statement::Let(LetStatement::new(
                    Identifier::new("myFunction".to_string()),
                    Expression::FunctionLiteral(FunctionLiteral::new(
                        "myFunction".to_string(),
                        vec![],
                        BlockStatement::new(vec![])
                    ))
                ))]
            }
        );
    }
}
