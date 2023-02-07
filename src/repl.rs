use std::io::{self, BufRead, Write};

use crate::{lexer::Lexer, token::Token};

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
            if token == Token::EOF {
                break;
            }
        }

        prompt();
    }
}
