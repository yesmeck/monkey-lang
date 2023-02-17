use std::{env::args, fs, process};

use enviroment::Enviroment;
use evaluator::Evaluator;
use lexer::Lexer;
use parser::Parser;

use crate::{ast::Node, repl::Repl};

mod ast;
mod builtin;
mod enviroment;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;

fn main() {
    let mut args = args();

    println!("{:?}", args);

    if args.len() == 2 {
        run(&args.nth(1).unwrap())
    } else {
        repl()
    }
}

fn run(file: &str) {
    let script = fs::read_to_string(file).expect("Unable to read file");
    let env = Enviroment::default();
    let mut evaluator = Evaluator::new(env);
    let mut lexer = Lexer::new(script);
    let mut parser = Parser::new(&mut lexer);
    let program = parser.parse_program();
    if !parser.errors.is_empty() {
        for error in parser.errors.iter() {
            println!("\t{}", error);
        }
        process::exit(1);
    }
    let evaluated = evaluator.eval(Node::Program(&program));
    println!("{}", evaluated.inspect());
}

fn repl() {
    println!(
        "Hello {}! This is the Monkey programming language!",
        whoami::username()
    );
    println!("Feel free to type in commands");
    let repl = Repl::new();
    repl.start();
}
