#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Illegal,
    Eof,

    Int,
    Ident,
    String,

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Eq,
    NotEq,

    Comma,
    Semicolon,
    Colon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lbracket,
    Rbracket,

    // Keyword
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
    Macro,
}

#[derive(Debug, PartialEq)]
pub struct Token(pub TokenKind, pub String);

impl Token {
    pub fn from_word(value: &str) -> Self {
        match value {
            "fn" => Token(TokenKind::Function, value.into()),
            "let" => Token(TokenKind::Let, value.into()),
            "true" => Token(TokenKind::True, value.into()),
            "false" => Token(TokenKind::False, value.into()),
            "if" => Token(TokenKind::If, value.into()),
            "else" => Token(TokenKind::Else, value.into()),
            "return" => Token(TokenKind::Return, value.into()),
            "macro" => Token(TokenKind::Macro, value.into()),
            _ => Token(TokenKind::Ident, value.into()),
        }
    }
}

