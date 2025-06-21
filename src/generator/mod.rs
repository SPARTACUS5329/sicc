use std::cell::RefCell;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::rc::Rc;

use crate::lexer::{DFANode, DFANodeE};
use crate::parser::{
    Element, ElementE, Elements, LexerRule, Production, Productions, Rule, SharedElement,
};
use crate::regex2dfa;

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Derivative {
    pub non_terminal: String,
    pub rule: Rule,
}

impl fmt::Display for Derivative {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} -> {}\n", self.non_terminal, self.rule)
    }
}

#[derive(Clone)]
pub struct LRState {
    pub id: i32,
    pub derivatives: HashSet<Derivative>,
    pub next: HashMap<SharedElement, Rc<RefCell<LRState>>>,
    pub slr_rules: HashMap<SharedElement, Vec<SLRRuleE>>,
}

impl fmt::Display for LRState {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Graphviz header
        writeln!(f, "digraph DFA {{")?;
        writeln!(f, "    rankdir=LR;")?;
        writeln!(f, "    node [shape=circle];")?;

        let mut visited = HashSet::new();
        self.print_state(f, &mut visited)?;

        writeln!(f, "}}") // Close the graph
    }
}

impl PartialEq for LRState {
    fn eq(&self, other: &Self) -> bool {
        self.derivatives == other.derivatives
    }
}

impl Hash for LRState {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let mut hashes: Vec<u64> = self
            .derivatives
            .iter()
            .map(|d| {
                let mut hasher = DefaultHasher::new();
                d.hash(&mut hasher);
                hasher.finish()
            })
            .collect();

        hashes.sort_unstable();

        hashes.hash(state);
    }
}

impl LRState {
    fn new(id: i32, derivatives: Vec<Derivative>) -> LRState {
        LRState {
            id,
            derivatives: derivatives.into_iter().collect(),
            next: HashMap::new(),
            slr_rules: HashMap::new(),
        }
    }

    fn print_state(&self, f: &mut fmt::Formatter, visited: &mut HashSet<i32>) -> fmt::Result {
        if !visited.insert(self.id) {
            return Ok(()); // Skip already printed states
        }

        // Print current state node
        write!(
            f,
            "    {} [label=\"State {}\\nDerivatives:",
            self.id, self.id
        )?;
        for derivative in &self.derivatives {
            write!(f, " {} ", derivative)?;
        }
        writeln!(f, "\"];\n")?;

        // Print transitions
        for (element, next_state) in &self.next {
            let next_id = next_state.borrow().id;
            writeln!(
                f,
                "    {} -> {} [label=\"{}\"];",
                self.id,
                next_id,
                element.get_element().borrow()
            )?;

            // Recursively print connected states
            next_state.borrow().print_state(f, visited)?;
        }

        Ok(())
    }
}

#[derive(Clone)]
pub struct SLRShift {
    pub next_state: Rc<RefCell<LRState>>,
}

#[derive(Clone)]
pub struct SLRReduce {
    pub derivative: Derivative,
}

#[derive(Clone)]
pub enum SLRRuleE {
    SLRRuleShift(SLRShift),
    SLRRuleReduce(SLRReduce),
    SLRRuleAccept,
}

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

pub fn construct_kmp_dfa(lexer_rules: &mut Vec<LexerRule>) -> Rc<RefCell<DFANode>> {
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

    lexer_root
}

fn augment_production(production: &Production) -> Derivative {
    let borrowed_production_non_terminal_element = production.non_terminal_element.borrow();
    let non_terminal = match &borrowed_production_non_terminal_element.element {
        ElementE::ElementNonTerminal(nt) => nt,
        _ => unreachable!(),
    };

    Derivative {
        non_terminal: format!("{}'", non_terminal.value.clone()),
        rule: Rule {
            annotation: "primary_augmentation".to_string(),
            elements: Elements {
                element_set: vec![Rc::new(RefCell::new(Element {
                    element: ElementE::ElementNonTerminal(non_terminal.clone()),
                    pos: 0,
                }))],
                pos: 0,
            },
            pos: 0,
        },
    }
}

fn get_kernel_items(
    element: Rc<RefCell<Element>>,
    directive_map: &HashMap<String, Vec<Derivative>>,
) -> Vec<Derivative> {
    let non_terminal = match &element.borrow().element {
        ElementE::ElementTerminal(_) | ElementE::ElementLexeme(_) => return Vec::new(),
        ElementE::ElementNonTerminal(non_terminal) => non_terminal.value.clone(),
    };

    let derivatives: Vec<Derivative> = directive_map
        .get(&non_terminal)
        .expect(format!("No derivative found for non terminal {}", non_terminal).as_str())
        .to_vec();

    derivatives
}

fn fill_kernel(state: Rc<RefCell<LRState>>, directive_map: &HashMap<String, Vec<Derivative>>) {
    let mut old_len = state.borrow().derivatives.len();

    loop {
        let mut derivatives: Vec<Derivative> = Vec::new();

        for derivative in state.borrow().derivatives.iter() {
            let index = derivative.rule.pos;
            if let Some(element) = derivative.rule.elements.element_set.get(index as usize) {
                let new_derivatives = get_kernel_items(Rc::clone(&element), directive_map);
                derivatives.extend(new_derivatives);
            }
        }

        state.borrow_mut().derivatives.extend(derivatives);

        if state.borrow().derivatives.len() == old_len {
            break;
        }

        old_len = state.borrow().derivatives.len();
    }
}

fn construct_state(
    derivatives: Vec<Derivative>,
    derivative_map: &HashMap<String, Vec<Derivative>>,
    states: &mut Vec<Rc<RefCell<LRState>>>,
    node_count: &mut i32,
) -> Rc<RefCell<LRState>> {
    let state = Rc::new(RefCell::new(LRState::new(*node_count, derivatives)));
    fill_kernel(Rc::clone(&state), derivative_map);

    if let Some(other_state) = states.iter().find(|item| *item.borrow() == *state.borrow()) {
        return Rc::clone(&other_state);
    }

    let mut next_derivatives_map: HashMap<String, Vec<Derivative>> = HashMap::new();
    let mut element_map: HashMap<String, Rc<RefCell<Element>>> = HashMap::new();

    for derivative in state.borrow().derivatives.iter() {
        let element = match derivative
            .rule
            .elements
            .element_set
            .get(derivative.rule.pos as usize)
        {
            Some(element) => element,
            None => {
                continue;
            }
        };

        next_derivatives_map
            .entry(element.borrow().get_value())
            .or_insert_with(|| {
                element_map
                    .entry(element.borrow().get_value())
                    .or_insert(Rc::clone(&element));
                Vec::new()
            })
            .push(derivative.clone());
    }

    states.push(Rc::clone(&state));
    *node_count += 1;

    for (token, next) in next_derivatives_map {
        let mut next_derivatives: Vec<Derivative> = next.clone();
        let element = element_map.get(&token).expect("element not found");
        for next_derivative in next_derivatives.iter_mut() {
            next_derivative.rule.pos += 1;
        }
        let next_state = construct_state(next_derivatives, derivative_map, states, node_count);
        state
            .borrow_mut()
            .next
            .insert(SharedElement(Rc::clone(&element)), next_state);
    }

    state
}

pub fn construct_fsm(productions: &Productions) -> Rc<RefCell<LRState>> {
    let mut derivative_map: HashMap<String, Vec<Derivative>> = HashMap::new();

    for production in productions.production_set.iter() {
        let non_terminal = match &production.non_terminal_element.borrow().element {
            ElementE::ElementNonTerminal(nt) => nt.value.clone(),
            _ => unreachable!(),
        };

        for rule in production.rules.ruleset.iter() {
            let derivative = Derivative {
                non_terminal: non_terminal.clone(),
                rule: Rule {
                    annotation: rule.annotation.clone(),
                    elements: rule.elements.clone(),
                    pos: 0,
                },
            };

            derivative_map
                .entry(non_terminal.clone())
                .or_insert_with(Vec::new)
                .push(derivative.clone());
        }
    }

    let new_first_derivative = augment_production(&productions.production_set[0]);
    let mut states: Vec<Rc<RefCell<LRState>>> = Vec::new();
    let derivatives = vec![new_first_derivative];
    let mut node_count = 0i32;
    let i0 = construct_state(derivatives, &derivative_map, &mut states, &mut node_count);

    i0
}

pub fn construct_slr_table(state: Rc<RefCell<LRState>>, visited: &mut HashSet<i32>) {
    if !visited.insert(state.borrow().id) {
        return;
    }

    let next_entries = state.borrow_mut().next.clone();

    for (element, next_state) in next_entries {
        state
            .borrow_mut()
            .slr_rules
            .entry(element)
            .or_insert_with(Vec::new)
            .push(SLRRuleE::SLRRuleShift(SLRShift {
                next_state: Rc::clone(&next_state),
            }));
    }

    for (_element, next_state) in &state.borrow().next {
        construct_slr_table(Rc::clone(&next_state), visited);
    }
}
