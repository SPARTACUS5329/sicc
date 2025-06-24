use std::env;
use std::{collections::HashSet, rc::Rc};

use generator::{print_lr_fsm, print_slr_csv};
use parser::{find_nullables, get_first_sets, get_follow_sets};
use utils::NiceError;
mod generator;
mod lexer;
mod parser;
mod regex2dfa;
mod utils;

fn main() -> Result<(), NiceError> {
    let args: Vec<String> = env::args().collect();
    let parser_grammar_filename = &args[1];
    let lexer_grammar_filename = &args[2];

    let parse_grammar_contents = utils::read_file(parser_grammar_filename)?;
    let tokens = lexer::lexer(&parse_grammar_contents).expect("Error in lexing the grammar");

    let mut program = parser::recursive_descent(tokens).expect("Error in parsing program");

    let lexer_grammar_contents = utils::read_lines(lexer_grammar_filename)?;

    let mut lexer_rules = parser::read_lexer_grammar(lexer_grammar_contents)?;
    parser::replace_lexemes(&mut program, &lexer_rules);
    parser::unify_elements(&mut program.productions);

    find_nullables(&mut program.productions);
    get_first_sets(&mut program.productions);
    get_follow_sets(&mut program.productions);

    let _lexer_root = generator::construct_kmp_dfa(&mut lexer_rules);
    let parser_i0 = generator::construct_fsm(&program.productions);
    let mut visited: HashSet<i32> = HashSet::new();
    generator::construct_slr_table(Rc::clone(&parser_i0), &mut visited);
    // let borrow = program.productions.production_set[0]
    // .non_terminal_element
    // .borrow();
    //
    // if let parser::ElementE::ElementNonTerminal(nt) = &borrow.element {
    // for element in &nt.follow {
    // println!("{}", element.0.borrow());
    // }
    // }
    let _ = print_lr_fsm(&parser_i0);
    let _ = print_slr_csv(&parser_i0);

    Ok(())
}
