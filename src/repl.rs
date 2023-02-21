use std::{
    cell::RefCell,
    io::{self, BufRead, Write},
    rc::Rc,
};

use crate::{enviroment::Enviroment, evaluator::Evaluator, lexer::Lexer, parser::Parser, Engine, compiler::Compiler, vm::Vm};

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

pub struct Repl<'a> {
    engine: &'a Engine,
}

impl<'a> Repl<'a> {
    pub fn new(engine: &'a Engine) -> Self {
        Self { engine }
    }

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
        let env = Rc::new(RefCell::new(Enviroment::default()));
        let stdin = io::stdin();
        for line in stdin.lock().lines() {
            let mut lexer = Lexer::new(&line.unwrap());
            let mut parser = Parser::new(&mut lexer);
            let mut program = parser.parse_program();

            if !parser.errors.is_empty() {
                self.print_errors(parser.errors);
                self.prompt();
                continue;
            }

            match self.engine {
                Engine::Vm => {
                    let mut compiler = Compiler::default();
                    compiler.compile(&program);
                    let mut vm = Vm::new(compiler.bytecode());
                    vm.run();
                    let result = vm.last_popped_stack_elem();
                    println!("{}", result.inspect());
                }
                Engine::Eval => {
                    let mut evaluator = Evaluator::new(Rc::clone(&env));
                    let result = evaluator.eval(&mut program);
                    println!("{}", result.inspect());
                }
            };

            self.prompt();
        }
    }
}
