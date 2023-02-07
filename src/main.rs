
mod token;
mod lexer;
mod repl;

fn main() {
    println!("Hello {}! This is the Monkey programming language!", whoami::username());
    println!("Feel free to type in commands");
    repl::start();
}
