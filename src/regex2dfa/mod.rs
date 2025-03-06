use crate::{
    lexer::{DFANode, DFANodeE},
    utils::NiceError,
};
use std::{cell::RefCell, rc::Rc};

fn handle_special_regex_element(
    node: Rc<RefCell<DFANode>>,
    c: char,
    node_count: i32,
    index: &mut i32,
    chars: Vec<char>,
    key: &mut Option<String>,
) -> Result<Rc<RefCell<DFANode>>, NiceError> {
    if c != '%' {
        return Err(NiceError::new("Invalid regex special element".to_string()));
    };

    let next_c = chars[(*index + 1) as usize];
    let next_node = Rc::new(RefCell::new(DFANode::new(
        DFANodeE::DFANodeRegular,
        node_count,
    )));

    *key = match next_c {
        '%' | '+' | '*' | '$' => Some(next_c.to_string()),
        's' | 'S' | 'd' | '.' => Some(format!("%{}", next_c)),
        _ => return Err(NiceError::new("Invalid regex grammar".to_string())),
    };

    node.borrow_mut()
        .next
        .insert(key.clone().expect("No key found"), Rc::clone(&next_node));

    *index += 1;

    Ok(next_node)
}

fn handle_general_regex_element(
    node: Rc<RefCell<DFANode>>,
    c: char,
    node_count: i32,
    key: &mut Option<String>,
) -> Result<Rc<RefCell<DFANode>>, NiceError> {
    let kind = if c == '$' {
        DFANodeE::DFANodeTerminal
    } else {
        DFANodeE::DFANodeRegular
    };
    let next_node = Rc::new(RefCell::new(DFANode::new(kind, node_count)));

    *key = Some(c.to_string());
    node.borrow_mut()
        .next
        .insert(key.clone().expect("No key found"), Rc::clone(&next_node));

    Ok(next_node)
}

fn handle_regex_element(
    node: &mut Rc<RefCell<DFANode>>,
    c: char,
    node_count: &mut i32,
    index: &mut i32,
    chars: Vec<char>,
    key: &mut Option<String>,
) -> Result<Rc<RefCell<DFANode>>, NiceError> {
    let next_node = match c {
        '%' => handle_special_regex_element(node.clone(), c, *node_count, index, chars, key)
            .expect("Invalid special regex element"),
        _ => handle_general_regex_element(node.clone(), c, *node_count, key)
            .expect("Invalid general regex element"),
    };

    Ok(next_node)
}

fn handle_kleene_star(
    node: &mut Rc<RefCell<DFANode>>,
    prev_node: &mut Option<Rc<RefCell<DFANode>>>,
    node_count: &mut i32,
    chars: Vec<char>,
    index: &mut i32,
    key: &mut Option<String>,
) {
    node.borrow_mut()
        .next
        .insert(key.clone().expect("No key found"), Rc::clone(&node));

    *index += 1;
    let next_c = chars[*index as usize];

    let next_node = handle_regex_element(node, next_c, node_count, index, chars, key)
        .expect("Invalid prev_element element");

    prev_node
        .as_mut()
        .expect("Invalid prev_node")
        .borrow_mut()
        .next
        .insert(key.clone().expect("No key found"), Rc::clone(&next_node));

    *prev_node = Some(Rc::clone(&node));
    *node = Rc::clone(&next_node);
    *node_count += 1;
}

fn handle_regex_operation(
    c: char,
    node: &mut Rc<RefCell<DFANode>>,
    prev_node: &mut Option<Rc<RefCell<DFANode>>>,
    chars: Vec<char>,
    node_count: &mut i32,
    key: &mut Option<String>,
    index: &mut i32,
) -> Result<(), NiceError> {
    match c {
        '+' => {
            node.borrow_mut()
                .next
                .insert(key.clone().expect("No key found"), Rc::clone(&node));
        }

        '*' => {
            handle_kleene_star(node, prev_node, node_count, chars, index, key);
        }

        _ => return Err(NiceError::new("Unsupported regex operation".to_string())),
    }

    Ok(())
}

pub fn regex2dfa(
    regex: String,
    node_count: &mut i32,
) -> Result<(Rc<RefCell<DFANode>>, Rc<RefCell<DFANode>>, i32), NiceError> {
    let chars: Vec<char> = regex.chars().collect();
    if chars[0] != '^' {
        return Err(NiceError::new(
            "Invalid regex, not starting with ^".to_string(),
        ));
    }

    let root = Rc::new(RefCell::new(DFANode::new(
        DFANodeE::DFANodeRoot,
        *node_count,
    )));

    *node_count += 1;

    let mut node = Rc::clone(&root);
    let mut index = 1i32;
    let mut key: Option<String> = None;
    let mut prev_node: Option<Rc<RefCell<DFANode>>> = None;

    while node.borrow().kind != DFANodeE::DFANodeTerminal {
        let c = chars[index as usize];
        match c {
            '+' | '*' => {
                handle_regex_operation(
                    c,
                    &mut node,
                    &mut prev_node,
                    chars.clone(),
                    node_count,
                    &mut key,
                    &mut index,
                )
                .expect("Invalid regex operation");
            }
            _ => {
                let next_node = handle_regex_element(
                    &mut node,
                    c,
                    node_count,
                    &mut index,
                    chars.clone(),
                    &mut key,
                )
                .expect("Invalid regex element");

                prev_node = Some(Rc::clone(&node));
                node = Rc::clone(&next_node);
                *node_count += 1;
            }
        };

        index += 1;
    }

    Ok((root, node, *node_count))
}
