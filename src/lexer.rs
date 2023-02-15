use crate::token::{Token, TokenKind};

#[derive(Debug)]
pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: Option<char>,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut l = Self {
            input,
            position: 0,
            read_position: 0,
            ch: None,
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = None;
        } else {
            self.ch = self.input.chars().nth(self.read_position);
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.ch {
            Some('=') => match self.peek_char() {
                Some('=') => {
                    self.read_char();
                    Token(TokenKind::Eq, "==".into())
                }
                _ => Token(TokenKind::Assign, "=".into()),
            },
            Some('+') => Token(TokenKind::Plus, "+".into()),
            Some('-') => Token(TokenKind::Minus, "-".into()),
            Some('!') => match self.peek_char() {
                Some('=') => {
                    self.read_char();
                    Token(TokenKind::NotEq, "!=".into())
                }
                _ => Token(TokenKind::Bang, "!".into()),
            },
            Some('/') => Token(TokenKind::Slash, "/".into()),
            Some('*') => Token(TokenKind::Asterisk, "*".into()),
            Some('<') => Token(TokenKind::Lt, "<".into()),
            Some('>') => Token(TokenKind::Gt, ">".into()),
            Some(';') => Token(TokenKind::Semicolon, ";".into()),
            Some('(') => Token(TokenKind::Lparen, "(".into()),
            Some(')') => Token(TokenKind::Rparen, ")".into()),
            Some(',') => Token(TokenKind::Comma, ",".into()),
            Some('{') => Token(TokenKind::Lbrace, "{".into()),
            Some('}') => Token(TokenKind::Rbrace, "}".into()),
            Some('[') => Token(TokenKind::Lbracket, "[".into()),
            Some(']') => Token(TokenKind::Rbracket, "]".into()),
            Some('"') => Token(TokenKind::String, self.read_string().into()),
            None => Token(TokenKind::Eof, "\n".into()),
            Some(ch) => {
                if self.is_letter(Some(ch)) {
                    return Token::from_word(self.read_identifier());
                } else if self.is_digital(Some(ch)) {
                    return Token(TokenKind::Int, self.read_number().into());
                } else {
                    Token(TokenKind::Illegal, "".into())
                }
            }
        };

        self.read_char();

        token
    }

    fn skip_whitespace(&mut self) {
        if let Some(mut ch) = self.ch {
            while ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' {
                self.read_char();
                if self.ch.is_none() {
                    break;
                } else {
                    ch = self.ch.unwrap();
                }
            }
        }
    }

    fn read_identifier(&mut self) -> &str {
        let position = self.position;
        while self.ch.is_some() && self.is_letter(self.ch) {
            self.read_char();
        }
        self.input[position..self.position].into()
    }

    fn read_number(&mut self) -> &str {
        let position = self.position;
        while self.is_digital(self.ch) {
            self.read_char();
        }
        self.input[position..self.position].into()
    }

    fn read_string(&mut self) -> &str {
        let position = self.position + 1;
        loop {
            self.read_char();
            if let Some(ch) = self.ch {
                if ch == '"'  {
                    break;
                }
            } else {
                break;
            }
        }
        self.input[position..self.position].into()
    }

    fn is_letter(&self, ch: Option<char>) -> bool {
        if let Some(ch) = ch {
            return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_';
        }
        false
    }

    fn is_digital(&self, ch: Option<char>) -> bool {
        if let Some(ch) = ch {
            return '0' <= ch && ch <= '9';
        }
        false
    }

    fn peek_char(&self) -> Option<char> {
        if self.read_position >= self.input.len() {
            None
        } else {
            self.input.chars().nth(self.read_position)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Lexer;
    use crate::token::{Token, TokenKind};

    #[test]
    fn test_simple_tokens() {
        let input = "=+(){},;";
        let mut lexer = Lexer::new(input.into());

        let expected: Vec<_> = vec![
            TokenKind::Assign,
            TokenKind::Plus,
            TokenKind::Lparen,
            TokenKind::Rparen,
            TokenKind::Lbrace,
            TokenKind::Rbrace,
        ];

        for token in expected.iter() {
            assert_eq!(lexer.next_token().0, *token);
        }
    }

    #[test]
    fn test_complex_tokens() {
        let input = "let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};
let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10; 
10 != 9;
\"foobar\"
\"foo bar\"
[1, 2];
";
        let mut lexer = Lexer::new(input.into());

        let expected = vec![
            Token(TokenKind::Let, "let".into()),
            Token(TokenKind::Ident, "five".into()),
            Token(TokenKind::Assign, "=".into()),
            Token(TokenKind::Int, "5".into()),
            Token(TokenKind::Semicolon, ";".into()),
            Token(TokenKind::Let, "let".into()),
            Token(TokenKind::Ident, "ten".into()),
            Token(TokenKind::Assign, "=".into()),
            Token(TokenKind::Int, "10".into()),
            Token(TokenKind::Semicolon, ";".into()),
            Token(TokenKind::Let, "let".into()),
            Token(TokenKind::Ident, "add".into()),
            Token(TokenKind::Assign, "=".into()),
            Token(TokenKind::Function, "fn".into()),
            Token(TokenKind::Lparen, "(".into()),
            Token(TokenKind::Ident, "x".into()),
            Token(TokenKind::Comma, ",".into()),
            Token(TokenKind::Ident, "y".into()),
            Token(TokenKind::Rparen, ")".into()),
            Token(TokenKind::Lbrace, "{".into()),
            Token(TokenKind::Ident, "x".into()),
            Token(TokenKind::Plus, "+".into()),
            Token(TokenKind::Ident, "y".into()),
            Token(TokenKind::Semicolon, ";".into()),
            Token(TokenKind::Rbrace, "}".into()),
            Token(TokenKind::Semicolon, ";".into()),
            Token(TokenKind::Let, "let".into()),
            Token(TokenKind::Ident, "result".into()),
            Token(TokenKind::Assign, "=".into()),
            Token(TokenKind::Ident, "add".into()),
            Token(TokenKind::Lparen, "(".into()),
            Token(TokenKind::Ident, "five".into()),
            Token(TokenKind::Comma, ",".into()),
            Token(TokenKind::Ident, "ten".into()),
            Token(TokenKind::Rparen, ")".into()),
            Token(TokenKind::Semicolon, ";".into()),
            Token(TokenKind::Bang, "!".into()),
            Token(TokenKind::Minus, "-".into()),
            Token(TokenKind::Slash, "/".into()),
            Token(TokenKind::Asterisk, "*".into()),
            Token(TokenKind::Int, "5".into()),
            Token(TokenKind::Semicolon, ";".into()),
            Token(TokenKind::Int, "5".into()),
            Token(TokenKind::Lt, "<".into()),
            Token(TokenKind::Int, "10".into()),
            Token(TokenKind::Gt, ">".into()),
            Token(TokenKind::Int, "5".into()),
            Token(TokenKind::Semicolon, ";".into()),
            Token(TokenKind::If, "if".into()),
            Token(TokenKind::Lparen, "(".into()),
            Token(TokenKind::Int, "5".into()),
            Token(TokenKind::Lt, "<".into()),
            Token(TokenKind::Int, "10".into()),
            Token(TokenKind::Rparen, ")".into()),
            Token(TokenKind::Lbrace, "{".into()),
            Token(TokenKind::Return, "return".into()),
            Token(TokenKind::True, "true".into()),
            Token(TokenKind::Semicolon, ";".into()),
            Token(TokenKind::Rbrace, "}".into()),
            Token(TokenKind::Else, "else".into()),
            Token(TokenKind::Lbrace, "{".into()),
            Token(TokenKind::Return, "return".into()),
            Token(TokenKind::False, "false".into()),
            Token(TokenKind::Semicolon, ";".into()),
            Token(TokenKind::Rbrace, "}".into()),
            Token(TokenKind::Int, "10".into()),
            Token(TokenKind::Eq, "==".into()),
            Token(TokenKind::Int, "10".into()),
            Token(TokenKind::Semicolon, ";".into()),
            Token(TokenKind::Int, "10".into()),
            Token(TokenKind::NotEq, "!=".into()),
            Token(TokenKind::Int, "9".into()),
            Token(TokenKind::Semicolon, ";".into()),
            Token(TokenKind::String, "foobar".into()),
            Token(TokenKind::String, "foo bar".into()),
            Token(TokenKind::Lbracket, "[".into()),
            Token(TokenKind::Int, "1".into()),
            Token(TokenKind::Comma, ",".into()),
            Token(TokenKind::Int, "2".into()),
            Token(TokenKind::Rbracket, "]".into()),
            Token(TokenKind::Semicolon, ";".into()),
            Token(TokenKind::Eof, "\n".into()),
        ];

        for token in expected.iter() {
            let t = lexer.next_token();
            assert_eq!(t, *token);
        }
    }
}
