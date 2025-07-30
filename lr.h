#pragma once

#define MAX_ANNOTATION_LENGTH 200
#define MAX_ELEMENTS_IN_RULE 10
#define MAX_TERMINAL_SIZE 20
#define MAX_RULES_IN_STATE 50

typedef enum ElementE {
  ELEMENT_LEXEME,
  ELEMENT_TERMINAL,
  ELEMENT_NON_TERMINAL
} element_e;

typedef struct Lexeme {
  char value[MAX_TERMINAL_SIZE];
} lexeme_t;

typedef struct Terminal {
  char value[MAX_TERMINAL_SIZE];
} terminal_t;

typedef struct NonTerminal {
  char value[MAX_TERMINAL_SIZE];
} non_terminal_t;

typedef struct Element {
  element_e type;
  union {
    lexeme_t lexeme;
    terminal_t terminal;
    non_terminal_t non_terminal;
  } element;
} element_t;

typedef struct Rule {
  char annotation[MAX_ANNOTATION_LENGTH];
  element_t elements[MAX_ELEMENTS_IN_RULE];
  int num_elements;
} rule_t;

typedef struct Derivative {
  element_t non_terminal_element;
  rule_t rule;
} derivative_t;

typedef struct SLRShift {
  int next_state;
} slr_rule_shift_t;

typedef struct SLRReduce {
  derivative_t derivative;
} slr_rule_reduce_t;

typedef enum SLRRuleE {
  SLR_RULE_SHIFT,
  SLR_RULE_REDUCE,
  SLR_RULE_ACCEPT,
} slr_rule_e;

typedef struct SLRRule {
  slr_rule_e type;
  union {
    slr_rule_shift_t shift;
    slr_rule_reduce_t reduce;
  } rule;
} slr_rule_t;

typedef struct RuleTableItem {
  slr_rule_t data;
  int key;
} rule_table_item_t;

typedef struct LRState {
  int id;
  rule_table_item_t rule_table[2 * MAX_RULES_IN_STATE];
} lr_state_t;
