use crate::utils::NiceError;

#[derive(Clone)]
pub struct Terminal {
    pub value: String,
    pos: i32,
}

impl Terminal {
    fn parse(tokens: Vec<String>, pos: i32) -> Result<Terminal, NiceError> {
        let mut index = pos;
        if (index as usize) >= tokens.len() || tokens[index as usize] != "\"" {
            return Err(NiceError::new(format!(
                "Terminal not starting with \", received {}",
                tokens[index as usize]
            )));
        }

        index += 1;

        if (index as usize) >= tokens.len() {
            return Err(NiceError::new("Invalid terminal".to_string()));
        }

        let terminal_value = tokens[index as usize].clone();
        index += 1;

        if (index as usize) >= tokens.len() || tokens[index as usize] != "\"" {
            return Err(NiceError::new("Terminal not ending with \"".to_string()));
        }

        let terminal = Terminal {
            value: terminal_value,
            pos: index,
        };

        Ok(terminal)
    }
}

#[derive(Clone)]
pub struct NonTerminal {
    pub value: String,
    pos: i32,
}

impl NonTerminal {
    fn parse(tokens: Vec<String>, pos: i32) -> Result<NonTerminal, NiceError> {
        let index = pos;
        if (index as usize) >= tokens.len() {
            return Err(NiceError::new("Index out of bounds".to_string()));
        }

        let non_terminal = tokens[index as usize].clone();
        if !non_terminal.chars().all(char::is_alphabetic) {
            return Err(NiceError::new(format!(
                "Invalid non terminal, received {}",
                non_terminal
            )));
        }

        Ok(NonTerminal {
            value: non_terminal,
            pos: index,
        })
    }
}

#[derive(Clone)]
enum ElementE {
    ElementTerminal(Terminal),
    ElementNonTerminal(NonTerminal),
}

#[derive(Clone)]
pub struct Element {
    pub element: ElementE,
    pos: i32,
}

impl Element {
    fn parse(tokens: Vec<String>, pos: i32) -> Result<Element, NiceError> {
        Terminal::parse(tokens.clone(), pos)
            .and_then(|terminal| {
                Ok(Element {
                    element: ElementE::ElementTerminal(terminal.clone()),
                    pos: terminal.pos,
                })
            })
            .or_else(|_| {
                NonTerminal::parse(tokens.clone(), pos).and_then(|non_terminal| {
                    Ok(Element {
                        element: ElementE::ElementNonTerminal(non_terminal.clone()),
                        pos: non_terminal.pos,
                    })
                })
            })
            .map_err(|_| NiceError::new("Invalid element".to_string()))
    }
}

#[derive(Clone)]
pub struct Elements {
    pub element_set: Vec<Element>,
    pos: i32,
}

impl Elements {
    fn parse(tokens: Vec<String>, pos: i32) -> Result<Elements, NiceError> {
        let mut index = pos;
        let mut element_set: Vec<Element> = Vec::new();

        while let Ok(element) = Element::parse(tokens.clone(), index) {
            element_set.push(element.clone());
            index = element.pos + 1;
        }

        index = element_set[element_set.len() - 1].pos;

        Ok(Elements {
            element_set,
            pos: index,
        })
    }
}

#[derive(Clone)]
pub struct Rule {
    pub annotation: String,
    pub elements: Elements,
    pos: i32,
}

impl Rule {
    fn parse(tokens: Vec<String>, pos: i32) -> Result<Rule, NiceError> {
        let mut index = pos;
        if (index as usize) >= tokens.len() || tokens[index as usize] != "@" {
            return Err(NiceError::new("Rule not starting with @".to_string()));
        }

        index += 1;

        if (index as usize) >= tokens.len() {
            return Err(NiceError::new("Invalid rule".to_string()));
        }

        let identifier = tokens[index as usize].clone();

        index += 1;
        let elements = Elements::parse(tokens.clone(), index)?;
        Ok(Rule {
            annotation: identifier,
            elements: elements.clone(),
            pos: elements.pos,
        })
    }
}

#[derive(Clone)]
pub struct Rules {
    pub ruleset: Vec<Rule>,
    pos: i32,
}

impl Rules {
    fn parse(tokens: Vec<String>, pos: i32) -> Result<Rules, NiceError> {
        let mut index = pos;
        let mut ruleset: Vec<Rule> = Vec::new();
        let rule = Rule::parse(tokens.clone(), index)?;

        ruleset.push(rule.clone());

        let mut rules = Rules {
            ruleset,
            pos: rule.pos,
        };

        index = rule.pos + 1;

        // little hacky way to check for the last rule
        if (index as usize) >= tokens.len() || tokens[index as usize] == ";" {
            return Ok(rules);
        }

        if tokens[index as usize] != "|" {
            return Err(NiceError::new(format!(
                "Invalid delimiter in ruleset, received {}",
                tokens[index as usize]
            ))
            .show());
        }

        index += 1;
        let sub_rules = Rules::parse(tokens.clone(), index)?;
        for (_, sub_rule) in sub_rules.ruleset.iter().enumerate() {
            rules.ruleset.push(sub_rule.clone());
        }

        if let Some(last_rule) = sub_rules.ruleset.get(sub_rules.ruleset.len() - 1) {
            rules.pos = last_rule.pos;
        } else {
            return Err(NiceError::new("Invalid last rule".to_string()));
        }

        Ok(rules)
    }
}

#[derive(Clone)]
pub struct Production {
    pub non_terminal: NonTerminal,
    pub rules: Rules,
    pos: i32,
}

impl Production {
    fn parse(tokens: Vec<String>, pos: i32) -> Result<Production, NiceError> {
        let mut index = pos;
        let non_terminal = NonTerminal::parse(tokens.clone(), index)?;
        index = non_terminal.pos + 1;

        if (index as usize) >= tokens.len() || tokens[index as usize] != "->" {
            return Err(NiceError::new("Invalid production".to_string()));
        }

        index += 1;
        let rules = Rules::parse(tokens.clone(), index)?;
        index = rules.pos + 1;

        if (index as usize) >= tokens.len() || tokens[index as usize] != ";" {
            return Err(NiceError::new("Invalid production".to_string()));
        }

        Ok(Production {
            non_terminal,
            rules,
            pos: index,
        })
    }
}

#[derive(Clone)]
pub struct Productions {
    pub production_set: Vec<Production>,
    pos: i32,
}

impl Productions {
    fn parse(tokens: Vec<String>, pos: i32) -> Result<Productions, NiceError> {
        let mut index = pos;
        let mut production_set: Vec<Production> = Vec::new();

        let production = Production::parse(tokens.clone(), index)?;

        production_set.push(production.clone());

        let mut productions = Productions {
            production_set,
            pos: index,
        };

        index = production.pos + 1;
        if (index as usize) >= tokens.len() {
            return Ok(productions);
        }
        let sub_productions = Productions::parse(tokens.clone(), index)?;
        for (_, sub_production) in sub_productions.production_set.iter().enumerate() {
            productions.production_set.push(sub_production.clone());
        }

        if let Some(last_production) = sub_productions
            .production_set
            .get(sub_productions.production_set.len() - 1)
        {
            productions.pos = last_production.pos;
        } else {
            return Err(NiceError::new("Invalid last production".to_string()));
        }

        Ok(productions)
    }
}

#[derive(Clone)]
enum DirectiveE {
    DirectiveLeft,
    DirectiveRight,
}

#[derive(Clone)]
pub struct Directive {
    pub kind: DirectiveE,
    pub terminals: Vec<Terminal>,
    pos: i32,
}

impl Directive {
    fn parse(tokens: Vec<String>, pos: i32) -> Result<Directive, NiceError> {
        let mut index = pos;

        if (index as usize) >= tokens.len() || tokens[index as usize] != "%" {
            return Err(NiceError::new("Invalid directive".to_string()));
        }

        index += 1;

        let kind: DirectiveE;

        if tokens[index as usize] == "left" {
            kind = DirectiveE::DirectiveLeft;
        } else if tokens[index as usize] == "right" {
            kind = DirectiveE::DirectiveRight;
        } else {
            return Err(NiceError::new(format!(
                "Invalid directive kind expected left or right, received {}",
                tokens[index as usize]
            )));
        }

        index += 1;

        let mut terminals: Vec<Terminal> = Vec::new();
        while let Ok(terminal) = Terminal::parse(tokens.clone(), index) {
            terminals.push(terminal.clone());
            index = terminal.pos + 1;
        }

        let directive = Directive {
            kind,
            terminals,
            pos: index - 1,
        };

        Ok(directive)
    }
}

#[derive(Clone)]
pub struct Directives {
    pub eps: bool,
    pub directive_set: Vec<Directive>,
    pos: i32,
}

impl Directives {
    fn parse(tokens: Vec<String>, pos: i32) -> Result<Directives, NiceError> {
        let mut index = pos;
        let mut directive_set: Vec<Directive> = Vec::new();

        Directive::parse(tokens.clone(), index)
            .and_then(|directive| {
                directive_set.push(directive.clone());

                let mut directives = Directives {
                    eps: true,
                    directive_set: directive_set.clone(),
                    pos: directive.pos,
                };

                index = directive.pos + 1;
                let sub_directives = Directives::parse(tokens.clone(), index)?;
                for sub_directive in sub_directives.directive_set.iter() {
                    directive_set.push(sub_directive.clone());
                }
                if !sub_directives.eps {
                    let last_directive =
                        &sub_directives.directive_set[sub_directives.directive_set.len() - 1];
                    directives.pos = last_directive.pos;
                }
                Ok(directives)
            })
            .or_else(|_| {
                Ok(Directives {
                    eps: true,
                    directive_set: directive_set.clone(),
                    pos: index,
                })
            })
    }
}

#[derive(Clone)]
pub struct Program {
    pub directives: Directives,
    pub productions: Productions,
    pos: i32,
}

impl Program {
    fn parse(tokens: Vec<String>, pos: i32) -> Result<Program, NiceError> {
        let mut index = pos;
        let directives = Directives::parse(tokens.clone(), index)?;

        index = directives.pos + 1;
        if (index as usize) >= tokens.len() || tokens[index as usize] != "%%" {
            return Err(NiceError::new("Invalid program".to_string()));
        }

        index += 1;
        let productions = Productions::parse(tokens.clone(), index)?;

        Ok(Program {
            directives,
            productions: productions.clone(),
            pos: productions.pos,
        })
    }
}

pub fn recursive_descent(tokens: Vec<String>) -> Result<Program, NiceError> {
    Program::parse(tokens, 0)
}

pub struct LexerRule {
    pub identifier: String,
    pub regex: String,
}

pub fn read_lexer_grammar(rules: Vec<String>) -> Result<Vec<LexerRule>, NiceError> {
    let mut lexer_rules: Vec<LexerRule> = Vec::new();

    for rule in rules.iter() {
        let parts: Vec<&str> = rule.splitn(2, ": ").collect();

        if parts.len() == 2 {
            lexer_rules.push(LexerRule {
                identifier: parts[0].to_string(),
                regex: parts[1].to_string(),
            });
        } else {
            return Err(NiceError::new(format!(
                "Error in lexer grammar, received {}",
                rule
            )));
        }
    }

    Ok(lexer_rules)
}
