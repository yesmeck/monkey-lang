#[derive(Debug, PartialEq)]
pub enum Token {
    Illegal,
    EOF,

    Word(Word),
    Int(String),

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

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
}


#[derive(Debug, PartialEq)]
pub enum Word {
    Keyword(Keyword),

    Ident(String,)
}


impl From<&str> for Word {
    fn from(value: &str) -> Self {
        match value {
            "fn" => Word::Keyword(Keyword::Funfction),
            "let" => Word::Keyword(Keyword::Let),
            "true" => Word::Keyword(Keyword::True),
            "false" => Word::Keyword(Keyword::False),
            "if" => Word::Keyword(Keyword::If),
            "else" => Word::Keyword(Keyword::Else),
            "return" => Word::Keyword(Keyword::Return),
            _ => Word::Ident(value.into()),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Keyword {
    Funfction,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

