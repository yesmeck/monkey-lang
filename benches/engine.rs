use std::{cell::RefCell, rc::Rc};

use monkey_lang::{
    ast::Program, compiler::Compiler, enviroment::Enviroment, evaluator::Evaluator, lexer::Lexer,
    parser::Parser, vm::Vm,
};

use criterion::{criterion_group, criterion_main, Criterion};

static INPUT: &str = "
let fibonacci = fn(x) {
    if (x == 0) {
        return 0;
    } else {
        if (x == 1) {
            return 1;
        } else {
            fibonacci(x - 1) + fibonacci(x - 2);
        }
    }
};
fibonacci(15);
";

fn eval(program: &Program) {
    let env = Rc::new(RefCell::new(Enviroment::default()));
    let mut evaluator = Evaluator::new(env);
    evaluator.eval(program);
}

fn vm(program: &Program) {
    let mut compiler = Compiler::new();
    compiler.compile(program);
    let mut vm = Vm::new(compiler.bytecode());
    vm.run();
}

fn criterion_benchmark(c: &mut Criterion) {
    let mut lexer = Lexer::new(INPUT);
    let mut parser = Parser::new(&mut lexer);
    let program = parser.parse_program();

    let mut group = c.benchmark_group("Engine");
    group.bench_function("eval", |b| b.iter(|| eval(&program)));
    group.bench_function("vm", |b| b.iter(|| vm(&program)));
    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
