use crate::{
    parser::LexerRule,
    utils::{self, NiceError},
};
use regex::Regex;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt,
    rc::Rc,
};

#[derive(PartialEq, Debug, Clone)]
pub enum DFANodeE {
    DFANodeRoot,
    DFANodeTerminal,
    DFANodeRegular,
}

#[derive(Debug, Clone)]
pub struct DFANode {
    pub kind: DFANodeE,
    pub id: i32,
    pub next: HashMap<String, Rc<RefCell<DFANode>>>,
    pub lexeme: Option<Rc<LexerRule>>,
}

impl DFANode {
    pub fn new(kind: DFANodeE, id: i32) -> Self {
        DFANode {
            kind,
            id,
            next: HashMap::new(),
            lexeme: None,
        }
    }
}

impl fmt::Display for DFANode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Create a set to track visited nodes and prevent infinite recursion
        let mut visited_nodes = HashSet::new();

        // Write the GraphViz header
        writeln!(f, "digraph DFA {{")?;
        writeln!(f, "    rankdir=LR;")?;
        writeln!(f, "    node [shape=circle];")?;

        // Recursive helper function to generate graph
        fn generate_graph(
            node: &DFANode,
            visited: &mut HashSet<i32>,
            f: &mut fmt::Formatter<'_>,
        ) -> fmt::Result {
            // Prevent revisiting nodes
            if visited.contains(&node.id) {
                return Ok(());
            }
            visited.insert(node.id);

            // Node label with additional information
            let node_label = match node.kind {
                DFANodeE::DFANodeRoot => format!("Node {} (Start)", node.id),
                DFANodeE::DFANodeTerminal => format!("Node {} (Final)", node.id),
                DFANodeE::DFANodeRegular => format!("Node {}", node.id),
            };

            // Style the node based on its type
            let _node_style = match node.kind {
                DFANodeE::DFANodeRoot => "[shape=circle,style=bold]",
                DFANodeE::DFANodeTerminal => "[shape=doublecircle]",
                DFANodeE::DFANodeRegular => "[shape=circle]",
            };

            // Write node with its style
            writeln!(f, "    {} [label=\"{}\"];", node.id, node_label)?;

            // Recurse through and draw edges
            for (transition, next_node) in &node.next {
                let borrowed_next = next_node.borrow();

                // Draw edge with transition label
                writeln!(
                    f,
                    "    {} -> {} [label=\"{}\"];",
                    node.id, borrowed_next.id, transition
                )?;

                // Recursively process next node
                generate_graph(&borrowed_next, visited, f)?;
            }

            Ok(())
        }

        // Start graph generation from this node
        generate_graph(self, &mut visited_nodes, f)?;

        // Close the GraphViz graph
        writeln!(f, "}}")?;

        Ok(())
    }
}

impl PartialEq for DFANode {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

pub fn construct_lexer_trie() -> Result<Rc<RefCell<DFANode>>, NiceError> {
    let lexer_grammar_filename = String::from("./assets/lexer_grammar.txt");
    let mut node_map = HashMap::<i32, Rc<RefCell<DFANode>>>::new();

    let Ok(lines) = utils::read_lines(&lexer_grammar_filename) else {
        return Err(NiceError::new("Error in readling lexer grammar file".to_string()));
    };

    let mut node_count = 0i32;
    for (i, _line) in lines.iter().enumerate() {
        node_count += 1;
        let index = i as i32;
        let kind = if index == 0 {
            DFANodeE::DFANodeRoot
        } else {
            DFANodeE::DFANodeRegular
        };
        let node = Rc::new(RefCell::new(DFANode::new(kind, index)));

        node_map.insert(index, Rc::clone(&node));
    }

    let terminal_node = Rc::new(RefCell::new(DFANode::new(
        DFANodeE::DFANodeTerminal,
        node_count,
    )));

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

            let neighbour: Rc<RefCell<DFANode>> = match neighbour_str {
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

pub fn lexer(contents: &String) -> Result<Vec<String>, NiceError> {
    let mut curr_token = String::from("");
    let mut tokens: Vec<String> = vec![];
    let chars: Vec<char> = contents.chars().collect();
    let root = construct_lexer_trie().expect("Error in constructing lexer trie");

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

                    if node.borrow().kind != DFANodeE::DFANodeRoot {
                        i -= 1;
                    } else {
                        curr_token += &c.to_string();
                    }

                    neighbour
                }
            }
        };

        node = match neighbour.borrow().kind {
            DFANodeE::DFANodeTerminal => {
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
