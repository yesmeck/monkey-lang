use crate::repl::Repl;

mod token;
mod lexer;
mod repl;
mod ast;
mod parser;

fn main() {
    println!("Hello {}! This is the Monkey programming language!", whoami::username());
    println!("Feel free to type in commands");
    let repl = Repl::new();
    repl.start();
}
