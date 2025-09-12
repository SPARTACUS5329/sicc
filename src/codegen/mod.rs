use crate::{
    parser::{ElementE, Productions},
    utils::{snake, write_lines_to_file},
};
use std::{fmt, fs::write};

pub struct CTypedef {
    pub struct_name: String,
    pub type_name: String,
}

impl fmt::Display for CTypedef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "typedef struct {} {};", self.struct_name, self.type_name)
    }
}

pub struct CEnum {
    pub enum_name: String,
    pub type_name: String,
    pub enums: Vec<String>,
}

impl fmt::Display for CEnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let enums_str = self.enums.join(", ");
        write!(
            f,
            "typedef enum {} {{ {} }} {};",
            self.enum_name, enums_str, self.type_name
        )
    }
}

#[derive(Clone)]
pub enum CType {
    CChar,
    CInt,
    CVoid,
    CUnion,
    CCustomType(String),
}

impl fmt::Display for CType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CType::CChar => write!(f, "char"),
            CType::CInt => write!(f, "int"),
            CType::CVoid => write!(f, "void"),
            CType::CUnion => write!(f, "union"),
            CType::CCustomType(t) => write!(f, "{}", t),
        }
    }
}

#[derive(Clone)]
pub struct CStructField {
    data_type: CType,
    name: String,
    ptr_depth: i32,
    unions: Option<Vec<CStructField>>,
    array_size: Option<String>,
}

impl CStructField {
    pub fn new(
        data_type: CType,
        name: &str,
        ptr_depth: i32,
        array_size: Option<&str>,
        unions: Option<Vec<CStructField>>,
    ) -> Self {
        Self {
            data_type,
            name: name.to_string(),
            ptr_depth,
            array_size: array_size.map(|s| s.to_string()),
            unions,
        }
    }
}

impl fmt::Display for CStructField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut field_str = format!("{} ", self.data_type);

        match self.data_type {
            CType::CUnion => {
                if let Some(unions) = &self.unions {
                    let union_str = unions
                        .iter()
                        .map(|u| u.to_string())
                        .collect::<Vec<String>>()
                        .join("\n");
                    field_str = format!("{} {{ {} }}", field_str, union_str);
                }
            }
            _ => {}
        };

        let ptrs = "*".repeat(self.ptr_depth as usize);
        field_str = format!("{} {}{}", field_str, ptrs, self.name);

        if let Some(array_size) = &self.array_size {
            field_str = format!("{} [{}]", field_str, array_size);
        };

        write!(f, "{};", field_str)
    }
}

#[derive(Clone)]
pub struct CStruct {
    struct_name: String,
    type_name: String,
    fields: Vec<CStructField>,
}

impl CStruct {
    pub fn new(struct_name: &str, type_name: &str, fields: Vec<CStructField>) -> Self {
        Self {
            struct_name: struct_name.to_string(),
            type_name: type_name.to_string(),
            fields,
        }
    }
}

impl fmt::Display for CStruct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let fields_str = self
            .fields
            .iter()
            .map(|f| f.to_string())
            .collect::<Vec<String>>()
            .join("\n");

        write!(
            f,
            "typedef struct {} {{ {} }} {};",
            self.struct_name, fields_str, self.type_name
        )
    }
}

pub struct CFunctionParameter {
    data_type: CType,
    name: String,
    ptr_depth: i32,
}

impl fmt::Display for CFunctionParameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ptrs = "*".repeat(self.ptr_depth as usize);
        write!(f, "{} {}{}", self.data_type, ptrs, self.name)
    }
}

impl CFunctionParameter {
    fn new(data_type: CType, name: &str, ptr_depth: i32) -> Self {
        Self {
            data_type,
            name: name.to_string(),
            ptr_depth,
        }
    }
}

pub struct CFunctionDeclaration {
    return_type: CType,
    return_ptr_depth: i32,
    name: String,
    parameters: Vec<CFunctionParameter>,
}

impl fmt::Display for CFunctionDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params_str = self
            .parameters
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        let ptrs = "*".repeat(self.return_ptr_depth as usize);

        write!(
            f,
            "{} {}{} ({});",
            self.return_type, ptrs, self.name, params_str
        )
    }
}

impl CFunctionDeclaration {
    fn new(
        return_type: CType,
        return_ptr_depth: i32,
        name: &str,
        parameters: Vec<CFunctionParameter>,
    ) -> Self {
        Self {
            return_type,
            return_ptr_depth,
            name: name.to_string(),
            parameters,
        }
    }
}

fn generate_header_file(productions: Productions) {
    let mut lines: Vec<String> = vec![];

    lines.push("#pragma once".to_string());
    lines.push("#include \"constants.h\"".to_string());
    lines.push("#define streq(str1, str2, n) (strncmp(str1, str2, n) == 0)".to_string());

    lines.push("typedef struct Lexeme lexeme_t;".to_string());
    lines.push("typedef struct Terminal terminal_t;".to_string());
    lines.push("typedef struct NonTerminal non_terminal_t;".to_string());
    lines.push("typedef struct Element element_t;".to_string());
    lines.push("typedef struct ElementSet element_set_t;".to_string());
    lines.push("typedef struct SLRShift slr_rule_shift_t;".to_string());
    lines.push("typedef struct SLRReduce slr_rule_reduce_t;".to_string());
    lines.push("typedef struct SLRRule slr_rule_t;".to_string());
    lines.push("typedef struct RuleTableItem rule_table_item_t;".to_string());
    lines.push("typedef struct LRState lr_state_t;".to_string());
    lines.push("typedef struct DFANodeMapItem dfa_map_item_t;".to_string());
    lines.push("typedef struct DFANode dfa_node_t;".to_string());
    lines.push("typedef struct SymbolTableItem symbol_table_item_t;".to_string());

    lines.push("".to_string());

    for production in productions.production_set.iter() {
        let typedefs = production.get_typedef();
        for typedef in typedefs.iter() {
            let typedef_str = format!("{}", typedef);
            lines.push(typedef_str);
        }
    }

    lines.push("".to_string());

    lines.push("typedef enum ElementE {".to_string());
    lines.push("  ELEMENT_LEXEME,".to_string());
    lines.push("  ELEMENT_TERMINAL,".to_string());
    lines.push("  ELEMENT_NON_TERMINAL".to_string());
    lines.push("} element_e;".to_string());

    lines.push("".to_string());

    lines.push("typedef enum NonTerminalE {".to_string());
    lines.push("  NON_TERMINAL_CONDITION_GOOD,".to_string());
    lines.push("  NON_TERMINAL_CONDITION_BAD,".to_string());
    lines.push("  NON_TERMINAL_CONDITION,".to_string());
    lines.push("  NON_TERMINAL_SENTENCE,".to_string());
    lines.push("} non_terminal_e;".to_string());

    lines.push("".to_string());

    lines.push("typedef enum SLRRuleE {".to_string());
    lines.push("  SLR_RULE_SHIFT,".to_string());
    lines.push("  SLR_RULE_REDUCE,".to_string());
    lines.push("  SLR_RULE_ACCEPT,".to_string());
    lines.push("} slr_rule_e;".to_string());

    lines.push("".to_string());

    lines.push("typedef enum DFANodeE {".to_string());
    lines.push("  DFA_NODE_ROOT,".to_string());
    lines.push("  DFA_NODE_TERMINAL,".to_string());
    lines.push("  DFA_NODE_REGULAR,".to_string());
    lines.push("} dfa_node_e;".to_string());

    lines.push("".to_string());

    for production in productions.production_set.iter() {
        let es = production.get_enum();
        for e in es.iter() {
            let e_str = format!("{}", e);
            lines.push(e_str);
        }
    }

    let mut c_structs: Vec<CStruct> = vec![];

    c_structs.push(CStruct::new(
        "Lexeme",
        "lexeme_t",
        vec![CStructField::new(
            CType::CChar,
            "value",
            0,
            Some("MAX_TERMINAL_SIZE"),
            None,
        )],
    ));

    c_structs.push(CStruct::new(
        "Terminal",
        "terminal_t",
        vec![CStructField::new(
            CType::CChar,
            "value",
            0,
            Some("MAX_TERMINAL_SIZE"),
            None,
        )],
    ));

    c_structs.push(CStruct::new(
        "NonTerminal",
        "non_terminal_t",
        vec![
            CStructField::new(
                CType::CCustomType("non_terminal_e".to_string()),
                "type",
                0,
                None,
                None,
            ),
            CStructField::new(CType::CInt, "numElements", 0, None, None),
            CStructField::new(
                CType::CUnion,
                "nonTerminal",
                0,
                None,
                Some(
                    productions
                        .production_set
                        .iter()
                        .map(|p| p.get_fields())
                        .into_iter()
                        .flatten()
                        .collect(),
                ),
            ),
        ],
    ));

    c_structs.push(CStruct::new(
        "Element",
        "element_t",
        vec![
            CStructField::new(
                CType::CCustomType("element_e".to_string()),
                "type",
                0,
                None,
                None,
            ),
            CStructField::new(
                CType::CUnion,
                "element",
                0,
                None,
                Some(vec![
                    CStructField::new(
                        CType::CCustomType("lexeme_t".to_string()),
                        "lexeme",
                        1,
                        None,
                        None,
                    ),
                    CStructField::new(
                        CType::CCustomType("terminal_t".to_string()),
                        "terminal",
                        1,
                        None,
                        None,
                    ),
                    CStructField::new(
                        CType::CCustomType("non_terminal_t".to_string()),
                        "nonTerminal",
                        1,
                        None,
                        None,
                    ),
                ]),
            ),
        ],
    ));

    c_structs.push(CStruct::new(
        "ElementSet",
        "element_set_t",
        vec![
            CStructField::new(CType::CInt, "numElements", 0, None, None),
            CStructField::new(
                CType::CCustomType("element_t".to_string()),
                "elements",
                2,
                None,
                None,
            ),
        ],
    ));

    c_structs.push(CStruct::new(
        "SLRShift",
        "slr_rule_shit_t",
        vec![CStructField::new(CType::CInt, "nextState", 0, None, None)],
    ));

    c_structs.push(CStruct::new(
        "SLRReduce",
        "slr_rule_reduce_t",
        vec![CStructField::new(
            CType::CCustomType("non_terminal_t".to_string()),
            "nonTerminal",
            1,
            None,
            None,
        )],
    ));

    c_structs.push(CStruct::new(
        "SLRRule",
        "slr_rule_t",
        vec![
            CStructField::new(
                CType::CCustomType("slr_rule_e".to_string()),
                "type",
                0,
                None,
                None,
            ),
            CStructField::new(
                CType::CUnion,
                "rule",
                0,
                None,
                Some(vec![
                    CStructField::new(
                        CType::CCustomType("slr_rule_shift_t".to_string()),
                        "shift",
                        1,
                        None,
                        None,
                    ),
                    CStructField::new(
                        CType::CCustomType("slr_rule_reduce_t".to_string()),
                        "reduce",
                        1,
                        None,
                        None,
                    ),
                ]),
            ),
        ],
    ));

    c_structs.push(CStruct::new(
        "RuleTableItem",
        "rule_table_item_t",
        vec![
            CStructField::new(CType::CInt, "key", 0, None, None),
            CStructField::new(
                CType::CCustomType("slr_rule_t".to_string()),
                "rule",
                1,
                None,
                None,
            ),
        ],
    ));

    c_structs.push(CStruct::new(
        "LRState",
        "lr_state_t",
        vec![
            CStructField::new(CType::CInt, "id", 0, None, None),
            CStructField::new(
                CType::CCustomType("rule_table_item_t".to_string()),
                "ruleTable",
                1,
                Some("3 * MAX_RULES_IN_STATE"),
                None,
            ),
        ],
    ));

    c_structs.push(CStruct::new(
        "DFANodeMapItem",
        "dfa_map_item_t",
        vec![
            CStructField::new(CType::CInt, "key", 0, None, None),
            CStructField::new(CType::CChar, "edge", 1, None, None),
            CStructField::new(
                CType::CCustomType("dfa_node_t".to_string()),
                "node",
                1,
                None,
                None,
            ),
        ],
    ));

    c_structs.push(CStruct::new(
        "DFANode",
        "dfa_node_t",
        vec![
            CStructField::new(
                CType::CCustomType("dfa_node_e".to_string()),
                "kind",
                0,
                None,
                None,
            ),
            CStructField::new(CType::CInt, "id", 0, None, None),
            CStructField::new(
                CType::CCustomType("dfa_map_item_t".to_string()),
                "next",
                2,
                None,
                None,
            ),
            CStructField::new(CType::CInt, "failurePrefixLength", 0, None, None),
            CStructField::new(
                CType::CCustomType("dfa_node_t".to_string()),
                "failure",
                1,
                None,
                None,
            ),
            CStructField::new(
                CType::CCustomType("lexeme_t".to_string()),
                "lexeme",
                1,
                None,
                None,
            ),
        ],
    ));

    c_structs.push(CStruct::new(
        "SymbolTableItem",
        "symbol_table_item_t",
        vec![
            CStructField::new(CType::CInt, "key", 0, None, None),
            CStructField::new(CType::CChar, "edge", 1, None, None),
            CStructField::new(CType::CVoid, "data", 1, None, None),
        ],
    ));

    for production in productions.production_set.iter() {
        let _ = production.get_struct().iter().map(|s| {
            c_structs.push(s.clone());
        });
    }

    for c_struct in c_structs.iter() {
        lines.push(format!("{}", c_struct));
    }

    let mut c_func_decs: Vec<CFunctionDeclaration> = vec![];

    c_func_decs.push(CFunctionDeclaration::new(
        CType::CCustomType("dfa_node_t".to_string()),
        1,
        "createLexerNode",
        vec![
            CFunctionParameter::new(CType::CCustomType("dfa_node_e".to_string()), "kind", 0),
            CFunctionParameter::new(CType::CInt, "id", 0),
            CFunctionParameter::new(CType::CInt, "failurePrefixLength", 0),
        ],
    ));

    c_func_decs.push(CFunctionDeclaration::new(
        CType::CVoid,
        0,
        "addFailureNodes",
        vec![],
    ));

    c_func_decs.push(CFunctionDeclaration::new(
        CType::CVoid,
        0,
        "addLexemesToLexerNodes",
        vec![],
    ));

    c_func_decs.push(CFunctionDeclaration::new(
        CType::CVoid,
        0,
        "addLexerTrieEdges",
        vec![],
    ));

    c_func_decs.push(CFunctionDeclaration::new(
        CType::CVoid,
        0,
        "initLexemes",
        vec![],
    ));

    c_func_decs.push(CFunctionDeclaration::new(
        CType::CVoid,
        0,
        "initTerminals",
        vec![],
    ));

    c_func_decs.push(CFunctionDeclaration::new(
        CType::CVoid,
        0,
        "initNonTerminals",
        vec![],
    ));

    c_func_decs.push(CFunctionDeclaration::new(
        CType::CVoid,
        0,
        "failureNode",
        vec![
            CFunctionParameter::new(CType::CCustomType("lexeme_t".to_string()), "currLexeme", 1),
            CFunctionParameter::new(CType::CChar, "currTerminal", 1),
            CFunctionParameter::new(CType::CInt, "numElements", 1),
            CFunctionParameter::new(CType::CCustomType("element_t".to_string()), "elements", 2),
            CFunctionParameter::new(CType::CCustomType("dfa_node_t".to_string()), "node", 1),
            CFunctionParameter::new(CType::CCustomType("dfa_node_t".to_string()), "root", 1),
            CFunctionParameter::new(CType::CChar, "ch", 0),
            CFunctionParameter::new(CType::CInt, "inputIndex", 1),
            CFunctionParameter::new(CType::CInt, "strIndex", 1),
            CFunctionParameter::new(CType::CChar, "contents", 1),
        ],
    ));

    c_func_decs.push(CFunctionDeclaration::new(
        CType::CCustomType("element_set_t".to_string()),
        1,
        "lex",
        vec![CFunctionParameter::new(CType::CChar, "contents", 1)],
    ));

    c_func_decs.push(CFunctionDeclaration::new(
        CType::CChar,
        1,
        "readFile",
        vec![CFunctionParameter::new(CType::CChar, "filename", 1)],
    ));

    c_func_decs.push(CFunctionDeclaration::new(
        CType::CCustomType("lr_state_t".to_string()),
        1,
        "createParserState",
        vec![CFunctionParameter::new(CType::CInt, "id", 0)],
    ));

    c_func_decs.push(CFunctionDeclaration::new(
        CType::CVoid,
        0,
        "createShiftRule",
        vec![
            CFunctionParameter::new(CType::CChar, "key[MAX_TERMINAL_SIZE]", 0),
            CFunctionParameter::new(
                CType::CCustomType("symbol_table_item_t".to_string()),
                "hashTable",
                2,
            ),
            CFunctionParameter::new(CType::CInt, "nextState", 0),
            CFunctionParameter::new(
                CType::CCustomType("rule_table_item_t".to_string()),
                "ruleTable",
                2,
            ),
        ],
    ));

    c_func_decs.push(CFunctionDeclaration::new(
        CType::CVoid,
        0,
        "createReduceRule",
        vec![
            CFunctionParameter::new(CType::CChar, "key[MAX_TERMINAL_SIZE]", 0),
            CFunctionParameter::new(CType::CChar, "nonTerminal[MAX_TERMINAL_SIZE]", 0),
            CFunctionParameter::new(
                CType::CCustomType("symbol_table_item_t".to_string()),
                "hashTable",
                2,
            ),
            CFunctionParameter::new(
                CType::CCustomType("rule_table_item_t".to_string()),
                "ruleTable",
                2,
            ),
        ],
    ));

    c_func_decs.push(CFunctionDeclaration::new(
        CType::CVoid,
        0,
        "initParserStates",
        vec![],
    ));

    c_func_decs.push(CFunctionDeclaration::new(
        CType::CVoid,
        0,
        "error",
        vec![CFunctionParameter::new(CType::CChar, "msg", 1)],
    ));

    c_func_decs.push(CFunctionDeclaration::new(
        CType::CCustomType("element_set_t".to_string()),
        1,
        "reverseElementSet",
        vec![CFunctionParameter::new(
            CType::CCustomType("element_set_t".to_string()),
            "elementSet",
            1,
        )],
    ));

    c_func_decs.push(CFunctionDeclaration::new(
        CType::CCustomType("unsigned long".to_string()),
        0,
        "hash",
        vec![
            CFunctionParameter::new(CType::CChar, "str", 1),
            CFunctionParameter::new(CType::CInt, "size", 0),
        ],
    ));

    c_func_decs.push(CFunctionDeclaration::new(
        CType::CCustomType("dfa_map_item_t".to_string()),
        1,
        "searchLexerNode",
        vec![
            CFunctionParameter::new(CType::CChar, "key", 1),
            CFunctionParameter::new(
                CType::CCustomType("dfa_map_item_t".to_string()),
                "hashTable[]",
                1,
            ),
            CFunctionParameter::new(CType::CInt, "size", 0),
        ],
    ));

    c_func_decs.push(CFunctionDeclaration::new(
        CType::CVoid,
        0,
        "insertLexerNode",
        vec![
            CFunctionParameter::new(CType::CChar, "key", 1),
            CFunctionParameter::new(CType::CCustomType("dfa_node_t".to_string()), "node", 1),
            CFunctionParameter::new(
                CType::CCustomType("dfa_map_item_t".to_string()),
                "hashTable[]",
                1,
            ),
            CFunctionParameter::new(CType::CInt, "size", 0),
        ],
    ));

    c_func_decs.push(CFunctionDeclaration::new(
        CType::CInt,
        0,
        "hashElement",
        vec![
            CFunctionParameter::new(CType::CCustomType("element_t".to_string()), "e", 1),
            CFunctionParameter::new(CType::CInt, "size", 0),
        ],
    ));

    c_func_decs.push(CFunctionDeclaration::new(
        CType::CCustomType("rule_table_item_t".to_string()),
        1,
        "searchSLRRule",
        vec![
            CFunctionParameter::new(CType::CCustomType("element_t".to_string()), "key", 1),
            CFunctionParameter::new(
                CType::CCustomType("rule_table_item_t".to_string()),
                "hashTable[]",
                1,
            ),
        ],
    ));

    c_func_decs.push(CFunctionDeclaration::new(
        CType::CVoid,
        0,
        "insertSLRRule",
        vec![
            CFunctionParameter::new(CType::CCustomType("element_t".to_string()), "key", 1),
            CFunctionParameter::new(CType::CCustomType("slr_rule_t".to_string()), "rule", 1),
            CFunctionParameter::new(
                CType::CCustomType("rule_table_item_t".to_string()),
                "hashTable[]",
                1,
            ),
        ],
    ));

    c_func_decs.push(CFunctionDeclaration::new(
        CType::CCustomType("symbol_table_item_t".to_string()),
        1,
        "searchSymbol",
        vec![
            CFunctionParameter::new(CType::CChar, "key", 1),
            CFunctionParameter::new(
                CType::CCustomType("symbol_table_item_t".to_string()),
                "hashTable[]",
                1,
            ),
            CFunctionParameter::new(CType::CInt, "size", 0),
        ],
    ));

    c_func_decs.push(CFunctionDeclaration::new(
        CType::CVoid,
        0,
        "insertSymbol",
        vec![
            CFunctionParameter::new(CType::CChar, "key", 1),
            CFunctionParameter::new(
                CType::CCustomType("symbol_table_item_t".to_string()),
                "hashTable[]",
                1,
            ),
            CFunctionParameter::new(CType::CInt, "size", 0),
        ],
    ));

    let root_production = &productions.production_set[1];
    let root_non_terminal = match &root_production.non_terminal_element.borrow().element {
        ElementE::ElementNonTerminal(non_terminal) => non_terminal.clone(),
        _ => {
            panic!("Received unexpected element in root_non_terminal");
        }
    };

    c_func_decs.push(CFunctionDeclaration::new(
        CType::CCustomType(snake(vec![
            root_non_terminal.value.clone(),
            "t".to_string(),
        ])),
        1,
        "parser",
        vec![CFunctionParameter::new(
            CType::CCustomType("element_set_t".to_string()),
            "elements",
            1,
        )],
    ));

    c_func_decs.push(CFunctionDeclaration::new(
        CType::CCustomType(snake(vec![
            root_non_terminal.value.clone(),
            "t".to_string(),
        ])),
        1,
        "getParseTree",
        vec![CFunctionParameter::new(CType::CChar, "filename", 1)],
    ));

    for c_func_dec in c_func_decs.iter() {
        lines.push(format!("{}", c_func_dec));
    }

    let _ = write_lines_to_file("lr.h", lines);
}

pub fn generate_constants_file() {
    let mut lines: Vec<String> = vec![];

    lines.push("#pragma once".to_string());
    lines.push("".to_string());
    lines.push("#define MAX_ANNOTATION_LENGTH 200".to_string());
    lines.push("#define MAX_ELEMENTS_IN_RULE 10".to_string());
    lines.push("#define MAX_TERMINAL_SIZE 100".to_string());
    lines.push("#define MAX_RULES_IN_STATE 500".to_string());
    lines.push("#define MAX_ELEMENTS 2000".to_string());
    lines.push("#define MAP_SIZE 5000".to_string());
    lines.push("".to_string());
    lines.push("#define MAX_PARSER_STATES 40".to_string());
    lines.push("#define MAX_LEXER_NODES 13".to_string());
    lines.push("#define MAX_LEXEMES 30".to_string());
    lines.push("#define MAX_TERMINALS 30".to_string());
    lines.push("#define MAX_NON_TERMINALS 30".to_string());

    let _ = write_lines_to_file("constants.h", lines);
}

pub fn codegen(productions: Productions) {
    generate_constants_file();
    generate_header_file(productions);
}
