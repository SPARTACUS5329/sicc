use std::env;

use utils::NiceError;
mod lexer;
mod parser;
mod utils;

fn main() -> Result<(), NiceError> {
    let args: Vec<String> = env::args().collect();
    let parser_grammar_filename = &args[1];
    let lexer_grammar_filename = &args[2];

    let parse_grammar_contents = utils::read_file(parser_grammar_filename)?;
    let tokens = lexer::lexer(&parse_grammar_contents).expect("Error in lexing the grammar");

    let _program = parser::recursive_descent(tokens).expect("Error in parsing program");

    let lexer_grammar_contents = utils::read_lines(lexer_grammar_filename)?;

    let lexer_rules = parser::read_lexer_grammar(lexer_grammar_contents)?;
    println!("{}", lexer_rules[0].regex);

    Ok(())
}
