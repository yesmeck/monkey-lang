use std::io::{self, BufRead, Write};

use crate::{lexer::Lexer, parser::Parser, token::TokenKind};

const MONKEY_FACE: &str = r#"            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
"#;

pub struct Repl {}

impl Repl {
    pub fn new() -> Self { Self { } }

    fn prompt(&self) {
        print!(">> ");
        std::io::stdout().flush().unwrap();
    }

    fn print_errors(&self, errors: Vec<String>) {
        println!("{}", MONKEY_FACE);
        println!("Woops! We ran into some monkey business here!");
        println!("  parser errors:");
        for error in errors.iter() {
            println!("\t{}", error);
        }
    }

    pub fn start(&self) {
        self.prompt();
        let stdin = io::stdin();
        for line in stdin.lock().lines() {
            let mut lexer = Lexer::new(line.unwrap());
            let mut parser = Parser::new(&mut lexer);
            let program = parser.parse_program();

            if !parser.errors.is_empty() {
                self.print_errors(parser.errors);
                self.prompt();
                continue;
            }

            println!("{}", program);
            self.prompt();
        }
    }
}
