use crate::token::{Token, Word};

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
            Some('=') => {
                match self.peek_char() {
                    Some('=') => {
                        self.read_char();
                        Token::Eq
                    }
                    _ => Token::Assign
                }
            }
            Some('+') => Token::Plus,
            Some('-') => Token::Minus,
            Some('!') => {
                match self.peek_char() {
                    Some('=') => {
                        self.read_char();
                        Token::NotEq
                    },
                    _ => Token::Bang
                }
            }
            Some('/') => Token::Slash,
            Some('*') => Token::Asterisk,
            Some('<') => Token::Lt,
            Some('>') => Token::Gt,
            Some(';') => Token::Semicolon,
            Some('(') => Token::Lparen,
            Some(')') => Token::Rparen,
            Some(',') => Token::Comma,
            Some('{') => Token::Lbrace,
            Some('}') => Token::Rbrace,
            None => Token::EOF,
            Some(ch) => {
                if self.is_letter(&ch) {
                    return Token::Word(Word::from(self.read_identifier()));
                } else if self.is_digital(&ch) {
                    return Token::Int(self.read_number().into());
                } else {
                    Token::Illegal
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
        while self.ch.is_some() && self.is_letter(&self.ch.unwrap()) {
            self.read_char();
        }
        self.input[position..self.position].into()
    }

    fn read_number(&mut self) -> &str {
        let position = self.position;
        while self.is_digital(&self.ch.unwrap()) {
            self.read_char();
        }
        self.input[position..self.position].into()
    }

    fn is_letter(&self, ch: &char) -> bool {
        'a' <= *ch && *ch <= 'z' || 'A' <= *ch && *ch <= 'Z' || *ch == '_'
    }

    fn is_digital(&self, ch: &char) -> bool {
        '0' <= *ch && *ch <= '9'
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
    use crate::token::{Token, Word};

    #[test]
    fn test_simple_tokens() {
        let input = "=+(){},;";
        let mut lexer = Lexer::new(input.into());

        let expected: Vec<_> = vec![
            Token::Assign,
            Token::Plus,
            Token::Lparen,
            Token::Rparen,
            Token::Lbrace,
            Token::Rbrace,
        ];

        for token in expected.iter() {
            assert_eq!(lexer.next_token(), *token);
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
";
        let mut lexer = Lexer::new(input.into());

        let expected = vec![
            Token::Word(Word::from("let")),
            Token::Word(Word::from("five")),
            Token::Assign,
            Token::Int("5".into()),
            Token::Semicolon,
            Token::Word(Word::from("let")),
            Token::Word(Word::from("ten")),
            Token::Assign,
            Token::Int("10".into()),
            Token::Semicolon,
            Token::Word(Word::from("let")),
            Token::Word(Word::from("add")),
            Token::Assign,
            Token::Word(Word::from("fn")),
            Token::Lparen,
            Token::Word(Word::from("x")),
            Token::Comma,
            Token::Word(Word::from("y")),
            Token::Rparen,
            Token::Lbrace,
            Token::Word(Word::from("x")),
            Token::Plus,
            Token::Word(Word::from("y")),
            Token::Semicolon,
            Token::Rbrace,
            Token::Semicolon,
            Token::Word(Word::from("let")),
            Token::Word(Word::from("result")),
            Token::Assign,
            Token::Word(Word::from("add")),
            Token::Lparen,
            Token::Word(Word::from("five")),
            Token::Comma,
            Token::Word(Word::from("ten")),
            Token::Rparen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int("5".into()),
            Token::Semicolon,
            Token::Int("5".into()),
            Token::Lt,
            Token::Int("10".into()),
            Token::Gt,
            Token::Int("5".into()),
            Token::Semicolon,
            Token::Word(Word::from("if")),
            Token::Lparen,
            Token::Int("5".into()),
            Token::Lt,
            Token::Int("10".into()),
            Token::Rparen,
            Token::Lbrace,
            Token::Word(Word::from("return")),
            Token::Word(Word::from("true")),
            Token::Semicolon,
            Token::Rbrace,
            Token::Word(Word::from("else")),
            Token::Lbrace,
            Token::Word(Word::from("return")),
            Token::Word(Word::from("false")),
            Token::Semicolon,
            Token::Rbrace,
            Token::Int("10".into()),
            Token::Eq,
            Token::Int("10".into()),
            Token::Semicolon,
            Token::Int("10".into()),
            Token::NotEq,
            Token::Int("9".into()),
            Token::Semicolon,
            Token::EOF,
        ];

        for token in expected.iter() {
            let t = lexer.next_token();
            assert_eq!(t, *token);
        }
    }
}
