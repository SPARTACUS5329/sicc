use core::panic;
use std::env;
mod lexer;
mod parser;
mod utils;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    if let Ok(contents) = utils::read_file(&filename) {
        let Ok(tokens) = lexer::lexer(&contents) else {
            panic!("Error in lexing the grammar");
        };

        for (_, token) in tokens.iter().enumerate() {
            println!("{}", token);
        }

        let _program = match parser::recursive_descent(tokens) {
            Ok(program) => program,
            Err(_) => panic!("Error in parsing program"),
        };
    } else {
        panic!("Error in reading file");
    }
}
