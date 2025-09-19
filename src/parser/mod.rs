use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt,
    hash::{Hash, Hasher},
    rc::Rc,
};

use crate::{
    codegen::{CEnum, CStruct, CStructField, CType, CTypedef},
    lexer::DFANode,
    utils::{pascal, snake, NiceError},
};

#[derive(Clone, PartialEq, Eq)]
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

#[derive(Clone, PartialEq, Eq)]
pub struct Lexeme {
    pub value: String,
    pub pos: i32,
}

#[derive(Clone)]
pub struct NonTerminal {
    pub value: String,
    pub first: HashSet<SharedElement>,
    pub follow: HashSet<SharedElement>,
    pub nullable: bool,
    pub pos: i32,
}

impl PartialEq for NonTerminal {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl Eq for NonTerminal {}

impl Hash for NonTerminal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

impl NonTerminal {
    pub fn new(value: String, pos: i32) -> Self {
        Self {
            value,
            first: HashSet::new(),
            follow: HashSet::new(),
            nullable: false,
            pos,
        }
    }

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

        Ok(NonTerminal::new(non_terminal, index))
    }

    fn get_typedef(self: &Self) -> CTypedef {
        return CTypedef {
            struct_name: pascal(vec![self.value.clone()]),
            type_name: snake(vec![self.value.clone(), "t".to_string()]),
        };
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum ElementE {
    ElementLexeme(Lexeme),
    ElementTerminal(Terminal),
    ElementNonTerminal(NonTerminal),
}

impl fmt::Display for ElementE {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ElementE::ElementLexeme(l) => write!(f, "{}", l.value),
            ElementE::ElementTerminal(t) => write!(f, "{}", t.value),
            ElementE::ElementNonTerminal(nt) => write!(f, "{}", nt.value),
        }
    }
}

impl Hash for ElementE {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            ElementE::ElementTerminal(terminal) => {
                0.hash(state);
                terminal.value.hash(state);
            }
            ElementE::ElementNonTerminal(non_terminal) => {
                1.hash(state);
                non_terminal.value.hash(state);
            }
            ElementE::ElementLexeme(lexeme) => {
                2.hash(state);
                lexeme.value.hash(state);
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Element {
    pub element: ElementE,
    pub pos: i32,
}

impl fmt::Display for Element {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.element)
    }
}

impl Element {
    fn parse(tokens: Vec<String>, pos: i32) -> Result<Rc<RefCell<Element>>, NiceError> {
        Terminal::parse(tokens.clone(), pos)
            .and_then(|terminal| {
                Ok(Rc::new(RefCell::new(Element {
                    element: ElementE::ElementTerminal(terminal.clone()),
                    pos: terminal.pos,
                })))
            })
            .or_else(|_| {
                NonTerminal::parse(tokens.clone(), pos).and_then(|non_terminal| {
                    Ok(Rc::new(RefCell::new(Element {
                        element: ElementE::ElementNonTerminal(non_terminal.clone()),
                        pos: non_terminal.pos,
                    })))
                })
            })
            .map_err(|_| NiceError::new("Invalid element".to_string()))
    }

    pub fn get_value(&self) -> String {
        match &self.element {
            ElementE::ElementLexeme(lexeme) => lexeme.value.clone(),
            ElementE::ElementNonTerminal(non_terminal) => non_terminal.value.clone(),
            ElementE::ElementTerminal(terminal) => terminal.value.clone(),
        }
    }
}

#[derive(Clone)]
pub struct SharedElement(pub Rc<RefCell<Element>>);

impl PartialEq for SharedElement {
    fn eq(&self, other: &Self) -> bool {
        self.0.borrow().eq(&other.0.borrow())
    }
}
impl Eq for SharedElement {}

impl Hash for SharedElement {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.borrow().hash(state)
    }
}

impl SharedElement {
    pub fn get_element(&self) -> &Rc<RefCell<Element>> {
        return &self.0;
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Elements {
    pub element_set: Vec<Rc<RefCell<Element>>>,
    pub pos: i32,
}

impl Elements {
    fn parse(tokens: Vec<String>, pos: i32) -> Result<Elements, NiceError> {
        let mut index = pos;
        let mut element_set: Vec<Rc<RefCell<Element>>> = Vec::new();

        while let Ok(element) = Element::parse(tokens.clone(), index) {
            element_set.push(Rc::clone(&element));
            index = element.borrow().pos + 1;
        }

        index = element_set[element_set.len() - 1].borrow().pos;

        Ok(Elements {
            element_set,
            pos: index,
        })
    }
}

impl Hash for Elements {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for elem in &self.element_set {
            elem.borrow().hash(state);
        }
        self.pos.hash(state);
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Rule {
    pub annotation: String,
    pub elements: Elements,
    pub pos: i32,
}

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: ", self.annotation)?;

        for (i, element) in self.elements.element_set.iter().enumerate() {
            if i as i32 == self.pos {
                write!(f, "• ")?;
            }
            write!(f, "{} ", element.borrow())?;
        }

        if self.pos as usize == self.elements.element_set.len() {
            write!(f, "•")?;
        }

        Ok(())
    }
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

    pub fn get_fields(self: &Self) -> Vec<CStructField> {
        return self
            .elements
            .element_set
            .iter()
            .filter_map(|e| match &e.borrow().element {
                ElementE::ElementLexeme(lexeme) => Some(CStructField::new(
                    CType::CCustomType("lexeme_t".to_string()),
                    lexeme.value.as_str(),
                    1,
                    None,
                    None,
                )),
                ElementE::ElementTerminal(_) => None,
                ElementE::ElementNonTerminal(non_terminal) => Some(CStructField::new(
                    CType::CCustomType(snake(vec![non_terminal.value.clone(), "t".to_string()])),
                    non_terminal.value.as_str(),
                    1,
                    None,
                    None,
                )),
            })
            .collect();
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
    pub non_terminal_element: Rc<RefCell<Element>>,
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

        let non_terminal_element = Rc::new(RefCell::new(Element {
            element: ElementE::ElementNonTerminal(non_terminal.clone()),
            pos: non_terminal.pos,
        }));

        Ok(Production {
            non_terminal_element,
            rules,
            pos: index,
        })
    }

    pub fn get_typedef(self: &Self) -> Vec<CTypedef> {
        let non_terminal = match &self.non_terminal_element.borrow().element {
            ElementE::ElementNonTerminal(non_terminal) => non_terminal.clone(),
            _ => {
                panic!("Unexpected error: expected non_terminal, received something else");
            }
        };

        let mut typedefs: Vec<CTypedef> = vec![non_terminal.get_typedef()];

        if self.rules.ruleset.len() == 1 {
            return typedefs;
        }

        for rule in self.rules.ruleset.iter() {
            let struct_name = pascal(vec![non_terminal.value.clone(), rule.annotation.clone()]);
            let type_name = snake(vec![
                non_terminal.value.clone(),
                rule.annotation.clone(),
                "t".to_string(),
            ]);

            typedefs.push(CTypedef {
                struct_name,
                type_name,
            });
        }

        return typedefs;
    }

    pub fn get_enum(self: &Self) -> Option<CEnum> {
        if self.rules.ruleset.len() == 1 {
            return None;
        }

        let non_terminal = match &self.non_terminal_element.borrow().element {
            ElementE::ElementNonTerminal(non_terminal) => non_terminal.clone(),
            _ => {
                panic!("Unexpected error: expected non_terminal, received something else");
            }
        };

        let enum_name = pascal(vec![non_terminal.value.clone(), "E".to_string()]);
        let type_name = snake(vec![non_terminal.value.clone(), "e".to_string()]);

        let mut enum_types: Vec<String> = vec![];
        for rule in self.rules.ruleset.iter() {
            let e: String = snake(vec![non_terminal.value.clone(), rule.annotation.clone()]);
            enum_types.push(e.to_uppercase());
        }

        return Some(CEnum {
            enum_name,
            type_name,
            enums: enum_types,
        });
    }

    pub fn get_fields(self: &Self) -> Vec<CStructField> {
        let non_terminal = match &self.non_terminal_element.borrow().element {
            ElementE::ElementNonTerminal(non_terminal) => non_terminal.clone(),
            _ => {
                panic!("Unexpected error: expected non_terminal, received something else");
            }
        };

        let mut fields: Vec<CStructField> = vec![];

        fields.push(CStructField::new(
            CType::CCustomType(snake(vec![non_terminal.value.clone(), "t".to_string()])),
            non_terminal.value.to_lowercase().as_str(),
            1,
            None,
            None,
        ));

        if self.rules.ruleset.len() == 1 {
            return fields;
        }

        for rule in self.rules.ruleset.iter() {
            fields.push(CStructField::new(
                CType::CCustomType(snake(vec![
                    non_terminal.value.clone(),
                    rule.annotation.clone(),
                    "t".to_string(),
                ])),
                snake(vec![
                    non_terminal.value.to_lowercase(),
                    rule.annotation.clone(),
                ])
                .as_str(),
                1,
                None,
                None,
            ));
        }

        return fields;
    }

    pub fn get_struct(self: &Self) -> Vec<CStruct> {
        let mut structs: Vec<CStruct> = vec![];

        let non_terminal = match &self.non_terminal_element.borrow().element {
            ElementE::ElementNonTerminal(non_terminal) => non_terminal.clone(),
            _ => {
                panic!("Unexpected error: expected non_terminal, received something else");
            }
        };

        if self.rules.ruleset.len() == 1 {
            let element_fields: Vec<CStructField> = self.rules.ruleset[0].get_fields();
            structs.push(CStruct::new(
                pascal(vec![non_terminal.value.clone()]).as_str(),
                snake(vec![non_terminal.value.clone(), "t".to_string()]).as_str(),
                element_fields,
            ));

            return structs;
        }

        for rule in self.rules.ruleset.iter() {
            let element_fields: Vec<CStructField> = rule.get_fields();

            structs.push(CStruct::new(
                pascal(vec![non_terminal.value.clone(), rule.annotation.clone()]).as_str(),
                snake(vec![
                    non_terminal.value.clone(),
                    rule.annotation.clone(),
                    "t".to_string(),
                ])
                .as_str(),
                element_fields,
            ));
        }

        structs.push(CStruct::new(
            pascal(vec![non_terminal.value.clone()]).as_str(),
            snake(vec![non_terminal.value.clone(), "t".to_string()]).as_str(),
            vec![
                CStructField::new(
                    CType::CCustomType(snake(vec![non_terminal.value.clone(), "e".to_string()])),
                    "type",
                    0,
                    None,
                    None,
                ),
                CStructField::new(
                    CType::CUnion,
                    non_terminal.value.clone().as_str(),
                    0,
                    None,
                    Some(
                        self.rules
                            .ruleset
                            .iter()
                            .map(|r| {
                                CStructField::new(
                                    CType::CCustomType(snake(vec![
                                        non_terminal.value.clone(),
                                        r.annotation.clone(),
                                        "t".to_string(),
                                    ])),
                                    r.annotation.clone().to_lowercase().as_str(),
                                    1,
                                    None,
                                    None,
                                )
                            })
                            .collect(),
                    ),
                ),
            ],
        ));

        return structs;
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

#[derive(Debug, Clone)]
pub struct LexerRule {
    pub identifier: String,
    pub regex: String,
    pub dfa_root: Option<Rc<RefCell<DFANode>>>,
}

pub fn read_lexer_grammar(rules: Vec<String>) -> Result<Vec<LexerRule>, NiceError> {
    let mut lexer_rules: Vec<LexerRule> = Vec::new();

    for rule in rules.iter() {
        let parts: Vec<&str> = rule.splitn(2, ": ").collect();

        if parts.len() == 2 {
            lexer_rules.push(LexerRule {
                dfa_root: None,
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

pub fn replace_lexemes(program: &mut Program, lexer_rules: &Vec<LexerRule>) {
    let lexemes: HashSet<String> = lexer_rules
        .iter()
        .map(|rule| rule.identifier.clone())
        .collect();

    for production in program.productions.production_set.iter_mut() {
        for rule in production.rules.ruleset.iter_mut() {
            for element in rule.elements.element_set.iter_mut() {
                let is_lexeme = {
                    let borrowed_element = element.borrow();
                    match &borrowed_element.element {
                        ElementE::ElementNonTerminal(non_terminal) => {
                            lexemes.contains(&non_terminal.value)
                        }
                        _ => false,
                    }
                };

                if !is_lexeme {
                    continue;
                }

                let new_lexeme = {
                    let borrowed_element = element.borrow();
                    if let ElementE::ElementNonTerminal(non_terminal) = &borrowed_element.element {
                        Lexeme {
                            value: non_terminal.value.clone(),
                            pos: 0,
                        }
                    } else {
                        continue;
                    }
                };

                element.borrow_mut().element = ElementE::ElementLexeme(new_lexeme);
            }
        }
    }
}

pub fn unify_elements(productions: &mut Productions) {
    let mut element_map: HashMap<NonTerminal, Rc<RefCell<Element>>> = HashMap::new();

    for production in &mut productions.production_set {
        let new_element = match &production.non_terminal_element.borrow().element {
            ElementE::ElementNonTerminal(nt) => {
                if let Some(existing_element) = element_map.get(&nt) {
                    Some(Rc::clone(&existing_element))
                } else {
                    element_map.insert(nt.clone(), Rc::clone(&production.non_terminal_element));
                    None
                }
            }
            _ => None,
        };

        if let Some(existing_element) = new_element {
            production.non_terminal_element = existing_element;
        }

        for rule in &mut production.rules.ruleset {
            for element in &mut rule.elements.element_set {
                let maybe_new = {
                    let borrowed = element.borrow();
                    match &borrowed.element {
                        ElementE::ElementNonTerminal(nt) => {
                            if let Some(existing_element) = element_map.get(&nt) {
                                Some(Rc::clone(existing_element))
                            } else {
                                element_map.insert(nt.clone(), Rc::clone(&element));
                                None
                            }
                        }
                        _ => None,
                    }
                };

                if let Some(new_element) = maybe_new {
                    *element = new_element;
                }
            }
        }
    }
}

pub fn find_nullables(productions: &mut Productions) {
    let mut update: bool;
    loop {
        update = false;
        for production in &mut productions.production_set {
            for rule in &production.rules.ruleset {
                if let ElementE::ElementNonTerminal(nt) =
                    &production.non_terminal_element.borrow().element
                {
                    if nt.nullable {
                        break;
                    }
                }
                let mut i = 0;
                let mut is_nullable = true;

                while is_nullable && i < rule.elements.element_set.len() {
                    let element = &rule.elements.element_set[i];

                    match &element.borrow().element {
                        ElementE::ElementTerminal(_) | ElementE::ElementLexeme(_) => {
                            is_nullable = false;
                        }
                        ElementE::ElementNonTerminal(nt) => {
                            is_nullable = nt.nullable || nt.value == "eps";
                        }
                    }

                    let mut borrowed = production.non_terminal_element.borrow_mut();
                    if let ElementE::ElementNonTerminal(nt) = &mut borrowed.element {
                        if nt.nullable != is_nullable {
                            update = true;
                            nt.nullable = is_nullable;
                        }
                        if !nt.nullable {
                            break;
                        }
                    }

                    i += 1;
                }
            }
        }

        if !update {
            break;
        }
    }
}

pub fn get_first_sets(productions: &mut Productions) {
    let mut update: bool;
    loop {
        update = false;
        for production in &mut productions.production_set {
            for rule in &production.rules.ruleset {
                let mut i = 0;
                let mut is_nullable = true;

                while is_nullable && i < rule.elements.element_set.len() {
                    let element = &rule.elements.element_set[i];

                    let mut extend_set: Option<HashSet<SharedElement>> = None;
                    let mut is_non_terminal = false;

                    match &element.borrow().element {
                        ElementE::ElementTerminal(_) | ElementE::ElementLexeme(_) => {
                            is_nullable = false;
                            let mut borrowed = production.non_terminal_element.borrow_mut();

                            if let ElementE::ElementNonTerminal(nt) = &mut borrowed.element {
                                let len_before = nt.first.len();
                                nt.first.insert(SharedElement(Rc::clone(element)));
                                let len_after = nt.first.len();
                                update = len_before != len_after;
                            }
                        }

                        ElementE::ElementNonTerminal(element_non_terminal) => {
                            extend_set = Some(element_non_terminal.first.clone());
                            is_non_terminal = true;
                        }
                    }

                    if is_non_terminal && extend_set.is_some() {
                        let mut borrowed = production.non_terminal_element.borrow_mut();

                        if let ElementE::ElementNonTerminal(nt) = &mut borrowed.element {
                            let len_before = nt.first.len();
                            nt.first.extend(extend_set.expect("Extend set not found"));
                            let len_after = nt.first.len();
                            update = len_before != len_after;
                            is_nullable = nt.nullable;
                        }
                    }

                    i += 1;
                }
            }
        }

        if !update {
            break;
        }
    }
}

pub fn get_follow_sets(productions: &mut Productions) {
    let mut update: bool;

    loop {
        update = false;

        for production in &mut productions.production_set {
            for rule in &mut production.rules.ruleset {
                let rule_length = rule.elements.element_set.len();

                for i in (0..rule_length).rev() {
                    let mut j = i + 1;

                    while j < rule_length {
                        let extend_set: HashSet<SharedElement>;

                        match &rule.elements.element_set[j].borrow().element {
                            ElementE::ElementTerminal(_) | ElementE::ElementLexeme(_) => {
                                let mutable_element =
                                    &mut rule.elements.element_set[i].borrow_mut();
                                let non_terminal = match &mut mutable_element.element {
                                    ElementE::ElementNonTerminal(nt) => nt,
                                    _ => {
                                        j += 1;
                                        continue;
                                    }
                                };

                                let len_before_extend = non_terminal.follow.len();
                                non_terminal.follow.insert(SharedElement(Rc::clone(
                                    &rule.elements.element_set[j],
                                )));

                                let len_after_extend = non_terminal.follow.len();
                                update = len_before_extend != len_after_extend;
                                break;
                            }
                            ElementE::ElementNonTerminal(nt) => {
                                extend_set = nt.first.clone();
                            }
                        }

                        let mutable_element = &mut rule.elements.element_set[i].borrow_mut();
                        let non_terminal = match &mut mutable_element.element {
                            ElementE::ElementNonTerminal(nt) => nt,
                            _ => {
                                j += 1;
                                continue;
                            }
                        };

                        let len_before_extend = non_terminal.follow.len();
                        non_terminal.follow.extend(extend_set);
                        let len_after_extend = non_terminal.follow.len();
                        update = len_before_extend != len_after_extend;

                        if !non_terminal.nullable {
                            break;
                        }

                        j += 1;
                    }

                    if j == rule_length
                        && !Rc::ptr_eq(
                            &production.non_terminal_element,
                            &rule.elements.element_set[i],
                        )
                    {
                        let mutable_element = &mut rule.elements.element_set[i].borrow_mut();
                        let non_terminal = match &mut mutable_element.element {
                            ElementE::ElementNonTerminal(nt) => nt,
                            _ => continue,
                        };

                        let borrowed_production_non_terminal_element =
                            production.non_terminal_element.borrow();
                        let production_non_terminal =
                            match &borrowed_production_non_terminal_element.element {
                                ElementE::ElementNonTerminal(nt) => nt,
                                _ => unreachable!(),
                            };

                        let len_before_extend = non_terminal.follow.len();
                        non_terminal
                            .follow
                            .extend(production_non_terminal.follow.clone());
                        let len_after_extend = non_terminal.follow.len();
                        update = len_before_extend != len_after_extend;
                    }
                }
            }
        }

        if !update {
            break;
        }
    }
}
