use std::io::{self, BufRead, Write};

use crate::{lexer::Lexer, token::TokenKind};

fn prompt() {
    print!(">> ");
    std::io::stdout().flush().unwrap();
}

pub fn start() {
    prompt();
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let mut lexer = Lexer::new(line.unwrap());
        loop {
            let token = lexer.next_token();
            println!("{:?}", token);
            if token.0 == TokenKind::EOF {
                break;
            }
        }

        prompt();
    }
}
