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
typedef struct ConditionGood condition_good_t;
typedef struct ConditionBad condition_bad_t;
typedef struct Condition condition_t;
typedef struct Sentence sentence_t;

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

typedef enum ConditionE { CONDITION_GOOD, CONDITION_BAD } condition_e;

typedef struct Lexeme {
  char value[MAX_TERMINAL_SIZE];
} lexeme_t;

typedef struct Terminal {
  char value[MAX_TERMINAL_SIZE];
} terminal_t;

typedef struct NonTerminal {
  non_terminal_e type;
  int numElements;
  union {
    sentence_t *sentence;
    condition_t *condition;
    condition_good_t *condition_good;
    condition_bad_t *condition_bad;
  } nonTerminal;
  char value[MAX_TERMINAL_SIZE];
} non_terminal_t;

typedef struct Element {
  element_e type;
  union {
    lexeme_t *lexeme;
    terminal_t *terminal;
    non_terminal_t *nonTerminal;
  } element;
} element_t;

typedef struct ElementSet {
  int numElements;
  element_t **elements;
} element_set_t;

typedef struct SLRShift {
  int nextState;
} slr_rule_shift_t;

typedef struct SLRReduce {
  non_terminal_t *nonTerminal;
} slr_rule_reduce_t;

typedef struct SLRRule {
  slr_rule_e type;
  union {
    slr_rule_shift_t *shift;
    slr_rule_reduce_t *reduce;
  } rule;
} slr_rule_t;

typedef struct RuleTableItem {
  int key;
  slr_rule_t *rule;
} rule_table_item_t;

typedef struct LRState {
  int id;
  rule_table_item_t *ruleTable[3 * MAX_RULES_IN_STATE];
} lr_state_t;

typedef struct DFANodeMapItem {
  int key;
  char *edge;
  dfa_node_t *node;
} dfa_map_item_t;

typedef struct DFANode {
  dfa_node_e kind;
  int id;
  dfa_map_item_t **next;
  int failurePrefixLength;
  dfa_node_t *failure;
  lexeme_t *lexeme;
} dfa_node_t;

typedef struct SymbolTableItem {
  int key;
  char *edge;
  void *data;
} symbol_table_item_t;

typedef struct ConditionGood {
  lexeme_t *good;
} condition_good_t;

typedef struct ConditionBad {
  lexeme_t *bad;
} condition_bad_t;

typedef struct Condition {
  condition_e type;
  union {
    condition_good_t *good;
    condition_bad_t *bad;
  } condition;
} condition_t;

typedef struct Sentence {
  lexeme_t *she;
  lexeme_t *is;
  condition_t *condition;
} sentence_t;

dfa_node_t *createLexerNode(dfa_node_e kind, int id, int failurePrefixLength);
void initLexerNodes();
void addFailureNodes();
void addLexemesToLexerNodes();
void addLexerTrieEdges();
void initLexemes();
void initTerminals();
void initNonTerminals();
void failureNode(lexeme_t *currLexeme, char *currTerminal, int *numElements,
                 element_t **elements, dfa_node_t *node, dfa_node_t *root,
                 char ch, int *inputIndex, int *strIndex, char *contents);
element_set_t *lex(char *contents);
char *readFile(const char *filename);
lr_state_t *createParserState(int id);
void createShiftRule(char key[MAX_TERMINAL_SIZE],
                     symbol_table_item_t **hashTable, int nextState,
                     rule_table_item_t **ruleTable);
void createReduceRule(char key[MAX_TERMINAL_SIZE],
                      char nonTerminal[MAX_TERMINAL_SIZE],
                      symbol_table_item_t **hashTable,
                      rule_table_item_t **ruleTable);
void initParserStates();
void error(const char *msg);
element_set_t *reverseElementSet(element_set_t *elementSet);
void printElements(element_set_t *elementSet);
unsigned long hash(char *str, int size);
dfa_map_item_t *searchLexerNode(char *key, dfa_map_item_t *hashTable[],
                                int size);
void insertLexerNode(char *key, dfa_node_t *node, dfa_map_item_t *hashTable[],
                     int size);
int hashElement(element_t *e, int size);
rule_table_item_t *searchSLRRule(element_t *key,
                                 rule_table_item_t *hashTable[]);
void insertSLRRule(element_t *key, slr_rule_t *rule,
                   rule_table_item_t *hashTable[]);
symbol_table_item_t *searchSymbol(char *key, symbol_table_item_t *hashTable[],
                                  int size);
void insertSymbol(char *key, void *data, symbol_table_item_t *hashTable[],
                  int size);
sentence_t *parser(element_set_t *elements);
sentence_t *getParseTree(const char *filename);
