#![feature(test)]
use std::{cell::RefCell, env::args, fs, path::PathBuf, process, rc::Rc};

use clap::Parser;
use monkey_lang::{
    compiler::Compiler, enviroment::Enviroment, evaluator::Evaluator, lexer::Lexer,
    makro::MacroExpension, parser, repl::Repl, vm::Vm, Engine,
};

#[derive(Parser)]
#[command(author, version, about, long_about= None)]
struct Cli {
    file: Option<PathBuf>,

    #[arg(short, long, value_name="ENGINE", value_enum, default_value_t=Engine::Vm)]
    engine: Engine,
}

fn main() {
    let cli = Cli::parse_from(args());

    if let Some(filename) = cli.file {
        run(&filename, &cli.engine);
    } else {
        repl(&cli.engine);
    }
}

fn run(file: &PathBuf, engine: &Engine) {
    let script = fs::read_to_string(file).expect("Unable to read file");
    let mut lexer = Lexer::new(&script);
    let mut parser = parser::Parser::new(&mut lexer);
    let mut program = parser.parse_program();
    if !parser.errors.is_empty() {
        for error in parser.errors.iter() {
            println!("\t{error}");
        }
        process::exit(1);
    }
    match engine {
        Engine::Vm => {
            let mut compiler = Compiler::new();
            compiler.compile(&program);
            let mut vm = Vm::new(compiler.bytecode());
            vm.run();
            let result = vm.last_popped_stack_elem();
            println!("{}", result.inspect());
        }
        Engine::Eval => {
            let env = Rc::new(RefCell::new(Enviroment::default()));
            let makro = MacroExpension::new(Rc::clone(&env));
            makro.define_macros(&mut program);
            makro.expand_macros(&mut program);
            let mut evaluator = Evaluator::new(env);
            let result = evaluator.eval(&mut program);
            println!("{}", result.inspect());
        }
    };
}

fn repl(engine: &Engine) {
    println!(
        "Hello {}! This is the Monkey programming language!",
        whoami::username()
    );
    println!("Feel free to type in commands");
    let repl = Repl::new(engine);
    repl.start();
}
