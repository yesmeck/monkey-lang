use clap::ValueEnum;

pub mod ast;
mod builtin;
pub mod enviroment;
pub mod evaluator;
pub mod lexer;
mod object;
pub mod parser;
pub mod repl;
mod token;
mod traverser;
pub mod makro;
mod code;
pub mod compiler;
mod symbol_table;
pub mod vm;
mod frame;

#[cfg(test)]
pub mod test_helper;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum Engine {
    Vm,
    Eval
}


