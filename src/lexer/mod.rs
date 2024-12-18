use crate::utils::{self, NiceError};
use regex::Regex;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(PartialEq, Debug)]
pub enum LexerNodeE {
    LexerNodeRoot,
    LexerNodeTerminal,
    LexerNodeRegular,
}

impl Clone for LexerNodeE {
    fn clone(&self) -> Self {
        match self {
            LexerNodeE::LexerNodeRoot => LexerNodeE::LexerNodeRoot,
            LexerNodeE::LexerNodeTerminal => LexerNodeE::LexerNodeTerminal,
            LexerNodeE::LexerNodeRegular => LexerNodeE::LexerNodeRegular,
        }
    }
}

#[derive(Debug)]
pub struct LexerNode {
    pub kind: LexerNodeE,
    pub id: i32,
    pub next: HashMap<String, Rc<RefCell<LexerNode>>>,
}

impl Clone for LexerNode {
    fn clone(&self) -> Self {
        LexerNode {
            kind: self.kind.clone(),
            id: self.id,
            next: self.next.clone(),
        }
    }
}

pub fn construct_lexer_trie() -> Result<Rc<RefCell<LexerNode>>, NiceError> {
    let lexer_grammar_filename = String::from("./assets/lexer_grammar.txt");
    let mut node_map = HashMap::<i32, Rc<RefCell<LexerNode>>>::new();

    let Ok(lines) = utils::read_lines(&lexer_grammar_filename) else {
        return Err(NiceError::new("Error in readling lexer grammar file".to_string()));
    };

    let mut node_count = 0i32;
    for (i, _line) in lines.iter().enumerate() {
        node_count += 1;
        let index = i as i32;
        let node = Rc::new(RefCell::new(LexerNode {
            kind: if index == 0 {
                LexerNodeE::LexerNodeRoot
            } else {
                LexerNodeE::LexerNodeRegular
            },
            id: index,
            next: HashMap::<String, Rc<RefCell<LexerNode>>>::new(),
        }));

        node_map.insert(index, Rc::clone(&node));
    }

    let terminal_node = Rc::new(RefCell::new(LexerNode {
        kind: LexerNodeE::LexerNodeTerminal,
        id: node_count,
        next: HashMap::<String, Rc<RefCell<LexerNode>>>::new(),
    }));

    for line in lines {
        let Some((node_number, edges)) = line.split_once(":") else {
            return Err(NiceError::new("Invalid lexer grammar: Cannot read node id".to_string()));
        };
        let node_id = match node_number.parse::<i32>() {
            Ok(num) => num,
            Err(_) => return Err(NiceError::new("Error in parsing node_id".to_string())),
        };

        let Some(node) = node_map.get(&node_id) else {
            return Err(NiceError::new("Invalid node_id".to_string()));
        };

        let edge_pairs: Vec<String> = edges.split(";;").map(String::from).collect();

        for pair in edge_pairs {
            let trimmed_pair = pair.trim();
            let re = Regex::new(r"(?x) \(\s* (.+) \s*,\s* (.+) \s*\) ").unwrap();
            let Some(captures) = re.captures(trimmed_pair) else {
                return Err(NiceError::new(format!("Invalid lexer grammar: Cannot read graph pairs: {}, {}", node.borrow().id, pair)));
            };

            let char = captures.get(1).map_or("", |m| m.as_str());
            let neighbour_str = captures.get(2).map_or("", |m| m.as_str());

            let neighbour: Rc<RefCell<LexerNode>> = match neighbour_str {
                "#" => Rc::clone(&terminal_node),
                _ => {
                    let Ok(neighbour_id) = neighbour_str.parse::<i32>() else {
                        return Err(NiceError::new("Error parsing neighbour_id".to_string()));
                    };

                    let Some(neighbour) = node_map.get(&neighbour_id) else {
                        return Err(NiceError::new("Invalid neighbour_id".to_string()));
                    };

                    Rc::clone(&neighbour)
                }
            };

            node.borrow_mut()
                .next
                .insert(char.to_string(), Rc::clone(&neighbour));
        }
    }

    let root = match node_map.get(&0) {
        Some(root) => Rc::clone(&root),
        _ => return Err(NiceError::new("Root node not found".to_string())),
    };

    Ok(root)
}
