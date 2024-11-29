use core::panic;
use std::{env, rc::Rc};

use lexer::construct_lexer_trie;
mod lexer;
mod utils;

fn lexer(_contents: &String) -> Result<Vec<String>, String> {
    let tokens: Vec<String> = vec![];
    let root = match construct_lexer_trie() {
        Ok(root) => root,
        _ => return Err("Error in constructing lexer trie".to_string()),
    };

    let _node1 = match root.borrow().next.get(&"%".to_string()) {
        Some(node1) => Rc::clone(&node1),
        _ => return Err("Node1 not found".to_string()),
    };

    Ok(tokens)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    if let Ok(_contents) = utils::read_file(&filename) {
        let _ = lexer(&_contents);
        // recursive_descent(contents);
    } else {
        panic!("Error in reading file");
    }
}
