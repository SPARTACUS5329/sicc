use crate::utils;
use regex::Regex;
use std::collections::HashMap;

#[derive(Debug)]
pub struct LexerNode<'a> {
    id: i32,
    next: HashMap<String, &'a LexerNode<'a>>,
}

pub fn construct_lexer_trie() -> Result<LexerNode<'static>, String> {
    let lexer_grammar_filename = String::from("./assets/lexer_grammar.txt");
    let mut node_map = HashMap::<i32, LexerNode>::new();
    let root: LexerNode = LexerNode {
        id: 0,
        next: HashMap::<String, &LexerNode>::new(),
    };

    let Ok(lines) = utils::read_lines(&lexer_grammar_filename) else {
        return Err("Error in readling lexer grammar file".to_string());
    };

    for (i, _line) in lines.iter().enumerate() {
        let index = i as i32;
        let node: LexerNode = LexerNode {
            id: index,
            next: HashMap::<String, &LexerNode>::new(),
        };
        node_map.insert(index, node);
    }

    for line in lines {
        let Some((node_number, edges)) = line.split_once(": ") else {
            return Err("Invalid lexer grammar".to_string());
        };
        let node_id = match node_number.parse::<i32>() {
            Ok(num) => num,
            Err(_) => return Err("Error in parsing node_id".to_string()),
        };

        let Some(node) = node_map.get_mut(&node_id) else {
            return Err("Invalid node_id".to_string());
        };

        let edge_pairs: Vec<String> = edges.split(";").map(String::from).collect();
        for pair in edge_pairs {
            let trimmed_pair = pair.trim();
            let re = Regex::new(r"\((\w+),(\d+)\)").unwrap();
            let Some(captures) = re.captures(trimmed_pair) else {
                eprintln!("Invalid lexer grammar");
                return Err("Invalid lexer grammar".to_string());
            };

            let char = captures.get(1).map_or("", |m| m.as_str());
            let Ok(neighbour_id) = captures.get(2).map_or("", |m| m.as_str()).parse::<i32>() else {
                return Err("Error in parsing node_id".to_string());
            };

            let Some(neighbour) = node_map.get(&neighbour_id) else {
                return Err("Invalid node_id".to_string());
            };

            node.next.insert(char.to_string(), neighbour);
        }
    }

    Ok(root)
}
