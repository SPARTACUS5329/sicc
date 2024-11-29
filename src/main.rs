use core::panic;
use lexer::{construct_lexer_trie, LexerNodeE};
use std::{env, rc::Rc};
use utils::NiceError;
mod lexer;
mod utils;

fn lexer(contents: &String) -> Result<Vec<String>, NiceError> {
    let mut curr_token = String::from("");
    let mut tokens: Vec<String> = vec![];
    let chars: Vec<char> = contents.chars().collect();
    let root = match construct_lexer_trie() {
        Ok(root) => root,
        _ => {
            return Err(NiceError::new(
                "Error in constructing lexer trie".to_string(),
            ))
        }
    };

    let mut node = Rc::clone(&root);

    let mut i = 0;
    while i < chars.len() {
        let c = chars[i];
        if c == ' ' || c == '\n' || c == '\t' {
            if curr_token.len() > 0 {
                tokens.push(curr_token);
                curr_token = String::from("");
            }
            i += 1;
            continue;
        }

        let neighbour = match node.borrow().next.get(&c.to_string()) {
            Some(node) => Rc::clone(&node),
            _ => {
                if c.is_alphabetic() && node.borrow().next.contains_key(&"%s".to_string()) {
                    let neighbour = match node.borrow().next.get(&"%s".to_string()) {
                        Some(node) => Rc::clone(&node),
                        _ => {
                            return Err(NiceError::new(format!(
                                "Unexpected token: {} at node {}",
                                c,
                                node.borrow().id
                            )))
                        }
                    };
                    neighbour
                } else if c.is_numeric() && node.borrow().next.contains_key(&"%d".to_string()) {
                    let neighbour = match node.borrow().next.get(&"%d".to_string()) {
                        Some(node) => Rc::clone(&node),
                        _ => return Err(NiceError::new("Unexpected token: digit".to_string())),
                    };
                    neighbour
                } else {
                    let neighbour = match node.borrow().next.get(&"*".to_string()) {
                        Some(node) => Rc::clone(&node),
                        _ => return Err(NiceError::new("Unexpected token: digit".to_string())),
                    };

                    if node.borrow().kind != LexerNodeE::LexerNodeRoot {
                        i -= 1;
                    } else {
                        curr_token += &c.to_string();
                    }

                    neighbour
                }
            }
        };

        node = match neighbour.borrow().kind {
            LexerNodeE::LexerNodeTerminal => {
                if curr_token.len() > 0 {
                    tokens.push(curr_token);
                    curr_token = String::from("");
                }
                Rc::clone(&root)
            }
            _ => {
                curr_token += &c.to_string();
                Rc::clone(&neighbour)
            }
        };

        i += 1;
    }

    if curr_token.len() > 0 {
        tokens.push(curr_token);
    }

    Ok(tokens)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    if let Ok(contents) = utils::read_file(&filename) {
        let Ok(tokens) = lexer(&contents) else {
            panic!("Error in lexing the grammar");
        };

        for (_, token) in tokens.iter().enumerate() {
            println!("{}", token);
        }
        // recursive_descent(contents);
    } else {
        panic!("Error in reading file");
    }
}
