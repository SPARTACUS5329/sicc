use std::cell::RefCell;
use std::rc::Rc;

use crate::lexer::DFANode;
use crate::parser::LexerRule;
use crate::regex2dfa::regex2dfa;

pub fn construct_kmp_dfa(lexer_rules: Vec<LexerRule>) {
    let mut node_count = 0i32;
    let mut rule_heads: Vec<Rc<RefCell<DFANode>>> = Vec::new();
    for rule in lexer_rules.iter() {
        let rule_head = regex2dfa(rule.regex.clone(), &mut node_count).expect("Invalid regex rule");
        rule_heads.push(rule_head);
    }
}
