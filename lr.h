#pragma once
#include "constants.h"
#define streq(str1, str2, n) (strncmp(str1, str2, n) == 0)
typedef struct Lexeme lexeme_t;
typedef struct Terminal terminal_t;
typedef struct NonTerminal non_terminal_t;
typedef struct Element element_t;
typedef struct ElementSet element_set_t;
typedef struct SLRShift slr_rule_shift_t;
typedef struct SLRReduce slr_rule_reduce_t;
typedef struct SLRRule slr_rule_t;
typedef struct RuleTableItem rule_table_item_t;
typedef struct LRState lr_state_t;
typedef struct DFANodeMapItem dfa_map_item_t;
typedef struct DFANode dfa_node_t;
typedef struct SymbolTableItem symbol_table_item_t;

typedef struct Exp exp_t;
typedef struct ExpPlus exp_plus_t;
typedef struct ExpMinus exp_minus_t;
typedef struct ExpValue exp_value_t;
typedef struct Sample sample_t;
typedef struct SampleEps sample_eps_t;
typedef struct SampleElse sample_else_t;

typedef enum ElementE {
  ELEMENT_LEXEME,
  ELEMENT_TERMINAL,
  ELEMENT_NON_TERMINAL
} element_e;

typedef enum NonTerminalE {
  NON_TERMINAL_CONDITION_GOOD,
  NON_TERMINAL_CONDITION_BAD,
  NON_TERMINAL_CONDITION,
  NON_TERMINAL_SENTENCE,
} non_terminal_e;

typedef enum SLRRuleE {
  SLR_RULE_SHIFT,
  SLR_RULE_REDUCE,
  SLR_RULE_ACCEPT,
} slr_rule_e;

typedef enum DFANodeE {
  DFA_NODE_ROOT,
  DFA_NODE_TERMINAL,
  DFA_NODE_REGULAR,
} dfa_node_e;

typedef enum ExpE { EXP_PLUS, EXP_MINUS, EXP_VALUE } exp_e;
typedef enum SampleE { SAMPLE_EPS, SAMPLE_ELSE } sample_e;
typedef struct Lexeme { char  value [MAX_TERMINAL_SIZE]; } lexeme_t;
typedef struct Terminal { char  value [MAX_TERMINAL_SIZE]; } terminal_t;
typedef struct NonTerminal { non_terminal_e  type;
int  numElements;
union  { exp_t  *exp;
exp_plus_t  *exp_plus;
exp_minus_t  *exp_minus;
exp_value_t  *exp_value;
sample_t  *sample;
sample_eps_t  *sample_eps;
sample_else_t  *sample_else; } nonTerminal; } non_terminal_t;
typedef struct Element { element_e  type;
union  { lexeme_t  *lexeme;
terminal_t  *terminal;
non_terminal_t  *nonTerminal; } element; } element_t;
typedef struct ElementSet { int  numElements;
element_t  **elements; } element_set_t;
typedef struct SLRShift { int  nextState; } slr_rule_shit_t;
typedef struct SLRReduce { non_terminal_t  *nonTerminal; } slr_rule_reduce_t;
typedef struct SLRRule { slr_rule_e  type;
union  { slr_rule_shift_t  *shift;
slr_rule_reduce_t  *reduce; } rule; } slr_rule_t;
typedef struct RuleTableItem { int  key;
slr_rule_t  *rule; } rule_table_item_t;
typedef struct LRState { int  id;
rule_table_item_t  *ruleTable [3 * MAX_RULES_IN_STATE]; } lr_state_t;
typedef struct DFANodeMapItem { int  key;
char  *edge;
dfa_node_t  *node; } dfa_map_item_t;
typedef struct DFANode { dfa_node_e  kind;
int  id;
dfa_map_item_t  **next;
int  failurePrefixLength;
dfa_node_t  *failure;
lexeme_t  *lexeme; } dfa_node_t;
typedef struct SymbolTableItem { int  key;
char  *edge;
void  *data; } symbol_table_item_t;
dfa_node_t *createLexerNode (dfa_node_e kind, int id, int failurePrefixLength);
void addFailureNodes ();
void addLexemesToLexerNodes ();
void addLexerTrieEdges ();
void initLexemes ();
void initTerminals ();
void initNonTerminals ();
void failureNode (lexeme_t *currLexeme, char *currTerminal, int *numElements, element_t **elements, dfa_node_t *node, dfa_node_t *root, char ch, int *inputIndex, int *strIndex, char *contents);
element_set_t *lex (char *contents);
char *readFile (char *filename);
lr_state_t *createParserState (int id);
void createShiftRule (char key[MAX_TERMINAL_SIZE], symbol_table_item_t **hashTable, int nextState, rule_table_item_t **ruleTable);
void createReduceRule (char key[MAX_TERMINAL_SIZE], char nonTerminal[MAX_TERMINAL_SIZE], symbol_table_item_t **hashTable, rule_table_item_t **ruleTable);
void initParserStates ();
void error (char *msg);
element_set_t *reverseElementSet (element_set_t *elementSet);
unsigned long hash (char *str, int size);
dfa_map_item_t *searchLexerNode (char *key, dfa_map_item_t *hashTable[], int size);
void insertLexerNode (char *key, dfa_node_t *node, dfa_map_item_t *hashTable[], int size);
int hashElement (element_t *e, int size);
rule_table_item_t *searchSLRRule (element_t *key, rule_table_item_t *hashTable[]);
void insertSLRRule (element_t *key, slr_rule_t *rule, rule_table_item_t *hashTable[]);
symbol_table_item_t *searchSymbol (char *key, symbol_table_item_t *hashTable[], int size);
void insertSymbol (char *key, symbol_table_item_t *hashTable[], int size);
sample_t *parser (element_set_t *elements);
sample_t *getParseTree (char *filename);
