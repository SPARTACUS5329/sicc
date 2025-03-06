use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::Rc;

use crate::lexer::{DFANode, DFANodeE};
use crate::parser::LexerRule;
use crate::regex2dfa;

fn augment_node(
    node: Rc<RefCell<DFANode>>,
    key: String,
    node_count: &mut i32,
) -> Rc<RefCell<DFANode>> {
    let augment = Rc::new(RefCell::new(DFANode::new(
        if node.borrow().kind == DFANodeE::DFANodeRoot {
            DFANodeE::DFANodeRoot
        } else {
            DFANodeE::DFANodeRegular
        },
        *node_count,
    )));
    *node_count += 1;

    augment.borrow_mut().next = node.borrow().next.clone();
    augment
        .borrow_mut()
        .next
        .insert(key.clone(), Rc::clone(&augment));

    node.borrow_mut().next.clear();
    node.borrow_mut().next.insert(key, Rc::clone(&augment));

    node
}

fn handle_merge(
    rule_node: Rc<RefCell<DFANode>>,
    rule_key: String,
    rule_next_node: Rc<RefCell<DFANode>>,
    trie_node: Rc<RefCell<DFANode>>,
    unmerged_nodes: &mut VecDeque<(Rc<RefCell<DFANode>>, Rc<RefCell<DFANode>>)>,
    node_count: &mut i32,
) {
    let trie_next = trie_node.borrow().next.clone();

    if let Some(trie_next_node) = trie_next.get(&rule_key) {
        let trie_node_same = *trie_next_node == trie_node;
        let rule_node_same = rule_next_node == rule_node;

        if trie_node_same == rule_node_same {
            if !trie_node_same {
                unmerged_nodes.push_back((Rc::clone(&rule_next_node), Rc::clone(trie_next_node)));
            }
        } else {
            let (next_rule_merge, next_trie_merge) = if rule_node_same {
                (
                    augment_node(Rc::clone(&rule_node), rule_key.to_string(), node_count),
                    Rc::clone(&trie_node),
                )
            } else {
                (
                    Rc::clone(&rule_node),
                    augment_node(Rc::clone(&trie_node), rule_key.to_string(), node_count),
                )
            };

            unmerged_nodes.push_back((Rc::clone(&next_rule_merge), Rc::clone(&next_trie_merge)));
        }
    } else {
        trie_node
            .borrow_mut()
            .next
            .insert(rule_key.to_string(), Rc::clone(&rule_next_node));
    }
}

fn submerge_egdes(
    rule_node: Rc<RefCell<DFANode>>,
    trie_node: Rc<RefCell<DFANode>>,
    unmerged_nodes: &mut VecDeque<(Rc<RefCell<DFANode>>, Rc<RefCell<DFANode>>)>,
    node_count: &mut i32,
    self_ref: bool,
) {
    let mut next_rules: VecDeque<(String, Rc<RefCell<DFANode>>)> = VecDeque::new();

    let rule_next = rule_node.borrow().next.clone();

    for (rule_key, rule_next_node) in rule_next.iter() {
        if (*rule_next_node == rule_node) == self_ref {
            next_rules.push_front((rule_key.to_string(), Rc::clone(&rule_next_node)));
        }
    }

    for (rule_key, rule_next_node) in next_rules {
        handle_merge(
            Rc::clone(&rule_node),
            rule_key.to_string(),
            rule_next_node,
            Rc::clone(&trie_node),
            unmerged_nodes,
            node_count,
        );
    }
}

fn add_rule_node_to_trie(
    rule_node: Rc<RefCell<DFANode>>,
    trie_node: Rc<RefCell<DFANode>>,
    node_count: &mut i32,
    unmerged_nodes: &mut VecDeque<(Rc<RefCell<DFANode>>, Rc<RefCell<DFANode>>)>,
) {
    let rule_next = rule_node.borrow().next.clone();
    let trie_next = trie_node.borrow().next.clone();

    for (trie_key, trie_next_node) in trie_next.iter() {
        if rule_next.get(trie_key).is_none() && *trie_next_node == trie_node {
            augment_node(Rc::clone(&trie_node), trie_key.to_string(), node_count);
        }
    }

    submerge_egdes(
        Rc::clone(&rule_node),
        Rc::clone(&trie_node),
        unmerged_nodes,
        node_count,
        true,
    );

    submerge_egdes(
        Rc::clone(&rule_node),
        Rc::clone(&trie_node),
        unmerged_nodes,
        node_count,
        false,
    );
}

fn merge_dfa_into_trie(trie_head: Rc<RefCell<DFANode>>, rule: &LexerRule, node_count: &mut i32) {
    let mut unmerged_nodes: VecDeque<(Rc<RefCell<DFANode>>, Rc<RefCell<DFANode>>)> =
        VecDeque::new();
    let rule_head = rule.dfa_root.as_ref().expect("rule_head not found");

    unmerged_nodes.push_back((Rc::clone(&rule_head), Rc::clone(&trie_head)));

    while !unmerged_nodes.is_empty() {
        let (rule_node, trie_node) = unmerged_nodes
            .pop_front()
            .expect("Unmerged rule_nodes not found");

        add_rule_node_to_trie(rule_node, trie_node, node_count, &mut unmerged_nodes);
    }
}

pub fn construct_kmp_dfa(lexer_rules: &mut Vec<LexerRule>) {
    let mut node_count = 0i32;
    let lexer_root = Rc::new(RefCell::new(DFANode::new(
        DFANodeE::DFANodeRoot,
        node_count,
    )));

    for rule in lexer_rules.iter_mut() {
        let (rule_head, rule_tail, dfa_end) =
            regex2dfa::regex2dfa(rule.regex.clone(), &mut node_count).expect("Invalid regex rule");

        node_count = dfa_end;
        rule.dfa_root = Some(rule_head);
        rule_tail.borrow_mut().lexeme = Some(Rc::new(rule.clone()));
    }

    for rule in lexer_rules {
        merge_dfa_into_trie(Rc::clone(&lexer_root), rule, &mut node_count);
    }

    println!("{}", lexer_root.borrow());
}
