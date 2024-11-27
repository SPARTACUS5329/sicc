use core::panic;
use std::env;

use lexer::construct_lexer_trie;
mod lexer;
mod utils;

fn lexer(_contents: &String) -> Vec<String> {
    let tokens: Vec<String> = vec![];
    let _ = construct_lexer_trie();

    tokens
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    if let Ok(_contents) = utils::read_file(&filename) {
        lexer(&_contents);
        // recursive_descent(contents);
    } else {
        panic!("Error in reading file");
    }
}
