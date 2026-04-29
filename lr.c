#include "lr.h"
#include <complex.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
dfa_node_t *lexerNodeMap[MAX_LEXER_NODES];
lr_state_t *parserStateMap[MAX_PARSER_STATES];
symbol_table_item_t *lexemeMap[MAP_SIZE];
symbol_table_item_t *terminalMap[MAP_SIZE];
symbol_table_item_t *nonTerminalMap[MAP_SIZE];
void initLexerNodes() {
dfa_node_t *node;
int nodes[MAX_LEXER_NODES][4] = {
{DFA_NODE_ROOT, 0, 0},
{DFA_NODE_REGULAR, 1, 0},
{DFA_NODE_REGULAR, 2, 0},
{DFA_NODE_REGULAR, 3, 0},
{DFA_NODE_TERMINAL, 4, 0},
{DFA_NODE_REGULAR, 6, 0},
{DFA_NODE_REGULAR, 7, 0},
{DFA_NODE_TERMINAL, 8, 0},
{DFA_NODE_REGULAR, 10, 0},
{DFA_NODE_REGULAR, 11, 0},
{DFA_NODE_REGULAR, 12, 0},
{DFA_NODE_REGULAR, 13, 0},
{DFA_NODE_TERMINAL, 14, 0},
{DFA_NODE_REGULAR, 16, 0},
{DFA_NODE_REGULAR, 17, 0},
{DFA_NODE_REGULAR, 18, 0},
{DFA_NODE_TERMINAL, 19, 0},
{DFA_NODE_REGULAR, 21, 0},
{DFA_NODE_TERMINAL, 22, 0},
};
for (int i = 0; i < MAX_LEXER_NODES; i++) {
node = createLexerNode(nodes[i][0], nodes[i][1], nodes[i][2]);
lexerNodeMap[i] = node;
}
}
void addFailureNodes() {
int failures[MAX_LEXER_NODES] = {
};
for (int i = 0; i < MAX_LEXER_NODES; i++) {
lexerNodeMap[i]->failure = lexerNodeMap[failures[i]];
}
}
void addLexemesToLexerNodes() {
char lexemes[MAX_LEXER_NODES][MAX_TERMINAL_SIZE] = {
{
}
};

for (int i = 0; i < MAX_LEXER_NODES; i++) {
if (lexemes[i][0] == '\0')
continue;

symbol_table_item_t *item = searchSymbol(lexemes[i], lexemeMap, MAP_SIZE);

if (item == NULL) {
printf("[addLexemesToLexerNodes] Unexpected Error: Lexeme not found: %s",
lexemes[i]);
continue;
}

element_t *element = (element_t *)item->data;
lexeme_t *lexeme = element->element.lexeme;
dfa_node_t *node = lexerNodeMap[i];
node->lexeme = lexeme;
}
}
void addLexerTrieEdges() {
int adjacencyMatrixNodes[MAX_LEXER_NODES][MAX_LEXER_NODES] = {
{10,21,1,16,6,},{2,},{3,},{4,},{},{7,},{8,},{},{11,},{12,},{13,},{14,},{},{17,},{18,},{19,},{},{22,},{},
};
char adjacencyMatrixEdges[MAX_LEXER_NODES][MAX_LEXER_NODES][MAX_TERMINAL_SIZE] = {
{"g"," ","s","b","i",},{"h",},{"e",},{"$",},{},{"s",},{"$",},{},{"o",},{"o",},{"d",},{"$",},{},{"a",},{"d",},{"$",},{},{"$",},{},
};
for (int i = 0; i < MAX_LEXER_NODES; i++) {
dfa_node_t *node = lexerNodeMap[i];
for (int j = 0; j < MAX_LEXER_NODES; j++) {
if (adjacencyMatrixNodes[i][j] == 0 ||
adjacencyMatrixEdges[i][j][0] == '\0')
continue;

dfa_node_t *neighbour = lexerNodeMap[adjacencyMatrixNodes[i][j]];
insertLexerNode(adjacencyMatrixEdges[i][j], neighbour, node->next,
MAP_SIZE);
}
}
}
void initLexemes() {
char lexemes[MAX_LEXEMES][MAX_TERMINAL_SIZE] = {
};

for (int i = 0; i < MAX_LEXEMES; i++) {
lexeme_t *lexeme = (lexeme_t *)calloc(1, sizeof(lexeme_t));
strcpy(lexeme->value, lexemes[i]);
element_t *element = (element_t *)calloc(1, sizeof(element_t));
element->type = ELEMENT_LEXEME;
element->element.lexeme = lexeme;
insertSymbol(lexeme->value, element, lexemeMap, MAP_SIZE);
}
}
void initTerminals() {
char terminals[MAX_TERMINALS][MAX_TERMINAL_SIZE] = {

};

for (int i = 0; i < MAX_TERMINALS; i++) {
terminal_t *terminal = (terminal_t *)calloc(1, sizeof(terminal_t));
strcpy(terminal->value, terminals[i]);
element_t *element = (element_t *)calloc(1, sizeof(element_t));
element->type = ELEMENT_TERMINAL;
element->element.terminal = terminal;
insertSymbol(terminal->value, element, terminalMap, MAP_SIZE);
}
}
void initNonTerminals() {
char nonTerminals[MAX_NON_TERMINALS][MAX_TERMINAL_SIZE] = {
"condition","condition_good","sentence","condition_bad",
};
non_terminal_e nonTerminalTypes[MAX_NON_TERMINALS] = {
NON_TERMINAL_CONDITION,NON_TERMINAL_CONDITION_GOOD,NON_TERMINAL_SENTENCE,NON_TERMINAL_CONDITION_BAD,
};

for (int i = 0; i < MAX_NON_TERMINALS; i++) {
non_terminal_t *nonTerminal =
(non_terminal_t *)calloc(1, sizeof(non_terminal_t));
strcpy(nonTerminal->value, nonTerminals[i]);
nonTerminal->type = nonTerminalTypes[i];
element_t *element = (element_t *)calloc(1, sizeof(element_t));
element->type = ELEMENT_NON_TERMINAL;
element->element.nonTerminal = nonTerminal;
insertSymbol(nonTerminal->value, element, nonTerminalMap, MAP_SIZE);
}
}
void initParserStates() {
lr_state_t *state;
int states[MAX_PARSER_STATES] = {
0,2,1,3,4,5,6,
};
char lexemeShiftRulesKeys[MAX_PARSER_STATES][MAX_PARSER_STATES]
[MAX_TERMINAL_SIZE] = {
{"SHE",},{"SPACE",},{},{"BAD","GOOD",},{},{},{},
};
int lexemeShiftRulesStates[MAX_PARSER_STATES][MAX_PARSER_STATES] = {
{2,},{3,},{},{6,4,},{},{},{},
};

char terminalShiftRulesKeys[MAX_PARSER_STATES][MAX_PARSER_STATES]
[MAX_TERMINAL_SIZE] = {
{},{},{},{},{},{},{},
};

int terminalShiftRulesStates[MAX_PARSER_STATES][MAX_PARSER_STATES] = {
{},{},{},{},{},{},{},
};

char nonTerminalShiftRulesKeys[MAX_PARSER_STATES][MAX_PARSER_STATES]
[MAX_TERMINAL_SIZE] = {
{"sentence",},{},{},{"condition",},{},{},{},
};

int nonTerminalShiftRulesStates[MAX_PARSER_STATES][MAX_PARSER_STATES] = {
{1,},{},{},{5,},{},{},{},
};

char terminalReduceRulesKeys[MAX_PARSER_STATES][MAX_PARSER_STATES]
[MAX_TERMINAL_SIZE] = {
{},{},{},{},{},{},{},
};

char terminalReduceRulesNonTerminals[MAX_PARSER_STATES][MAX_PARSER_STATES]
[MAX_TERMINAL_SIZE] = {
{},{},{},{},{},{},{},
};

int terminalReduceNumElements[MAX_PARSER_STATES][MAX_PARSER_STATES] = {
{},{},{},{},{},{},{},
};

char *key = (char *)calloc(MAX_TERMINAL_SIZE, sizeof(char));
for (int i = 0; states[i] != -1; i++) {
state = createParserState(states[i]);

for (int j = 0; j < MAX_PARSER_STATES; j++) {
key = lexemeShiftRulesKeys[i][j];
if (key[0] != '\0') {
int nextState = lexemeShiftRulesStates[i][j];

createShiftRule(key, lexemeMap, nextState, state->ruleTable);
}
}

for (int j = 0; j < MAX_PARSER_STATES; j++) {
key = terminalShiftRulesKeys[i][j];
if (key[0] != '\0') {
int nextState = terminalShiftRulesStates[i][j];

createShiftRule(key, terminalMap, nextState, state->ruleTable);
}

key = terminalReduceRulesKeys[i][j];
if (key[0] == '\0')
break;

createReduceRule(key, terminalReduceRulesNonTerminals[i][j],
terminalReduceNumElements[i][j], terminalMap,
state->ruleTable);
}

for (int j = 0; j < MAX_PARSER_STATES; j++) {
key = nonTerminalShiftRulesKeys[i][j];
if (key[0] == '\0')
break;

int nextState = nonTerminalShiftRulesStates[i][j];

createShiftRule(key, nonTerminalMap, nextState, state->ruleTable);
}

parserStateMap[states[i]] = state;
}

lr_state_t *i0 = parserStateMap[0];
symbol_table_item_t *item =
searchSymbol("sentence", nonTerminalMap, MAP_SIZE);

element_t *acceptElement = (element_t *)item->data;
slr_rule_t *rule = (slr_rule_t *)calloc(1, sizeof(slr_rule_t));
rule->type = SLR_RULE_ACCEPT;
insertSLRRule(acceptElement, rule, i0->ruleTable);
}
sentence_t *getParseTree(char *filename) {
initLexemes();
initTerminals();
initNonTerminals();

initLexerNodes();
addFailureNodes();
addLexerTrieEdges();
addLexemesToLexerNodes();

initParserStates();

char *contents = readFile(filename);
if (!contents)
error("Error in reading input file");

element_set_t *elementSet = lex(contents);

terminal_t *terminal = (terminal_t *)calloc(1, sizeof(terminal_t));
strcpy(terminal->value, "$");
element_t *EOS = (element_t *)calloc(1, sizeof(element_t));
EOS->type = ELEMENT_TERMINAL;
EOS->element.terminal = terminal;
elementSet->elements[elementSet->numElements - 1] = EOS;

elementSet = reverseElementSet(elementSet);

sentence_t *sentence = parser(elementSet);

return sentence;
}
sentence_t *parser(element_set_t *elements) {
element_set_t *parsedElements =
(element_set_t *)calloc(1, sizeof(element_set_t *));
parsedElements->elements =
(element_t **)calloc(MAX_ELEMENTS, sizeof(element_t *));

lr_state_t **stateHistory =
(lr_state_t **)calloc(5 * MAX_PARSER_STATES, sizeof(lr_state_t));
int numStateHistory = 0;

lr_state_t *currState = parserStateMap[0];

while (elements->numElements >= 1) {
element_t *currElement = elements->elements[elements->numElements - 1];

rule_table_item_t *item = searchSLRRule(currElement, currState->ruleTable);

if (item == NULL) {
switch (currElement->type) {
case ELEMENT_LEXEME:
printf("[parser] Unexpected lexeme %s at state %d",
currElement->element.lexeme->value, currState->id);
error("");
break;
case ELEMENT_NON_TERMINAL:
printf("[parser] Unexpected nonTerminal %s at state %d",
currElement->element.nonTerminal->value, currState->id);
error("");
break;
case ELEMENT_TERMINAL:
printf("[parser] Unexpected terminal %s at state %d",
currElement->element.terminal->value, currState->id);
error("");
break;
}
}

slr_rule_t *rule = item->rule;
switch (rule->type) {
case SLR_RULE_SHIFT:
parsedElements->elements[parsedElements->numElements++] = currElement;
lr_state_t *nextState = parserStateMap[rule->rule.shift->nextState];
stateHistory[numStateHistory++] = currState;
currState = nextState;
elements->numElements--;
break;

case SLR_RULE_REDUCE:
slr_rule_reduce_t *reduceRule = rule->rule.reduce;
int j = parsedElements->numElements - reduceRule->numElements;

non_terminal_t *nonTerminal =
(non_terminal_t *)calloc(1, sizeof(non_terminal_t));
nonTerminal->type = reduceRule->nonTerminal->type;

handle_reduction(reduceRule, parsedElements, &j, nonTerminal);

parsedElements->numElements -= reduceRule->numElements;

element_t *reducedElement = (element_t *)calloc(1, sizeof(element_t));
reducedElement->type = ELEMENT_NON_TERMINAL;
reducedElement->element.nonTerminal = nonTerminal;

elements->elements[elements->numElements++] = reducedElement;

numStateHistory -= reduceRule->numElements;
currState = stateHistory[numStateHistory];

break;

case SLR_RULE_ACCEPT:
sentence_t *sentence = elements->elements[elements->numElements - 1]
->element.nonTerminal->nonTerminal.sentence;
return sentence;
break;
}
}

error("[parser] Error: Not enough tokens");
}

void handle_reduction(slr_rule_reduce_t *reduceRule,
element_set_t *parsedElements, int *j,
non_terminal_t *nonTerminal) {
switch (reduceRule->nonTerminal->type) {
condition_t *condition = (condition_t *)calloc(1, sizeof(condition_t));
case NON_TERMINAL_CONDITION:
printf("[parser] Unexpected error: Received %s for reduction", "condition");
break;
case NON_TERMINAL_CONDITION_GOOD:
condition_good_t *condition_good = (condition_good_t *)calloc(1, sizeof(condition_good_t));
condition_good->good = parsedElements->elements[*j]->element.lexeme;
*j = *j + 1;
condition->type = CONDITION_GOOD;
condition->condition.good = condition_good;
nonTerminal->type = NON_TERMINAL_CONDITION;
nonTerminal->nonTerminal.condition = condition;
strcpy(nonTerminal->value, "condition");
break;
case NON_TERMINAL_CONDITION_BAD:
condition_bad_t *condition_bad = (condition_bad_t *)calloc(1, sizeof(condition_bad_t));
condition_bad->bad = parsedElements->elements[*j]->element.lexeme;
*j = *j + 1;
condition->type = CONDITION_BAD;
condition->condition.bad = condition_bad;
nonTerminal->type = NON_TERMINAL_CONDITION;
nonTerminal->nonTerminal.condition = condition;
strcpy(nonTerminal->value, "condition");
break;
case NON_TERMINAL_SENTENCE:
sentence_t *sentence = (sentence_t *)calloc(1, sizeof(sentence_t));
sentence->she = parsedElements->elements[*j]->element.lexeme;
*j = *j + 1;
sentence->space = parsedElements->elements[*j]->element.lexeme;
*j = *j + 1;
sentence->condition = parsedElements->elements[*j]->element.nonTerminal->nonTerminal.condition;
*j = *j + 1;
nonTerminal->nonTerminal.sentence = sentence;
strcpy(nonTerminal->value, "sentence");
break;
}
}
