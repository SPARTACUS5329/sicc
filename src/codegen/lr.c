#include "lr.h"
#include "constants.h"
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

dfa_node_t *createLexerNode(dfa_node_e kind, int id, int failurePrefixLength) {
  dfa_node_t *node = (dfa_node_t *)calloc(1, sizeof(dfa_node_t));
  node->kind = kind;
  node->id = id;
  node->failurePrefixLength = failurePrefixLength;
  node->next = (dfa_map_item_t **)calloc(MAP_SIZE, sizeof(dfa_map_item_t *));

  return node;
}

void initLexerNodes() {
  dfa_node_t *node;
  int nodes[MAX_LEXER_NODES][4] = {
      {DFA_NODE_ROOT, 0, 0},     {DFA_NODE_REGULAR, 1, 0},
      {DFA_NODE_REGULAR, 2, 0},  {DFA_NODE_REGULAR, 3, 0},
      {DFA_NODE_REGULAR, 4, 0},  {DFA_NODE_REGULAR, 5, 0},
      {DFA_NODE_REGULAR, 6, 0},  {DFA_NODE_REGULAR, 7, 0},
      {DFA_NODE_REGULAR, 8, 0},  {DFA_NODE_REGULAR, 9, 0},
      {DFA_NODE_REGULAR, 10, 0}, {DFA_NODE_REGULAR, 11, 0},
      {DFA_NODE_REGULAR, 12, 0}};

  for (int i = 0; i < MAX_LEXER_NODES; i++) {
    node = createLexerNode(nodes[i][0], nodes[i][1], nodes[i][2]);
    lexerNodeMap[i] = node;
  }
}

void addFailureNodes() {
  int failures[MAX_LEXER_NODES] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
  for (int i = 0; i < MAX_LEXER_NODES; i++) {
    lexerNodeMap[i]->failure = lexerNodeMap[failures[i]];
  }
}

void addLexemesToLexerNodes() {
  char lexemes[MAX_LEXER_NODES][MAX_TERMINAL_SIZE] = {
      "", "", "", "SHE", "", "IS", "", "", "", "GOOD", "", "", "BAD"};

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
      {1, 4, 6, 10}, {2}, {3}, {}, {5}, {}, {7}, {8}, {9}, {}, {11}, {12}, {}};
  char adjacencyMatrixEdges[MAX_LEXER_NODES][MAX_LEXER_NODES]
                           [MAX_TERMINAL_SIZE] = {{"s", "i", "g", "b"},
                                                  {"h"},
                                                  {"e"},
                                                  {},
                                                  {"s"},
                                                  {},
                                                  {"o"},
                                                  {"o"},
                                                  {"d"},
                                                  {},
                                                  {"a"},
                                                  {"d"},
                                                  {}};

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
  char lexemes[MAX_LEXEMES][MAX_TERMINAL_SIZE] = {"SHE", "IS", "GOOD", "BAD"};

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
  char terminals[MAX_TERMINALS][MAX_TERMINAL_SIZE] = {" ", "$"};

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
      "sentence", "condition", "condition_good", "condition_bad"};
  non_terminal_e nonTerminalTypes[MAX_NON_TERMINALS] = {
      NON_TERMINAL_SENTENCE, NON_TERMINAL_CONDITION,
      NON_TERMINAL_CONDITION_GOOD, NON_TERMINAL_CONDITION_BAD};
  int nonTerminalsSize[MAX_NON_TERMINALS] = {5, 1, 1, 1};

  for (int i = 0; i < MAX_NON_TERMINALS; i++) {
    non_terminal_t *nonTerminal =
        (non_terminal_t *)calloc(1, sizeof(non_terminal_t));
    strcpy(nonTerminal->value, nonTerminals[i]);
    nonTerminal->type = nonTerminalTypes[i];
    nonTerminal->numElements = nonTerminalsSize[i];
    element_t *element = (element_t *)calloc(1, sizeof(element_t));
    element->type = ELEMENT_NON_TERMINAL;
    element->element.nonTerminal = nonTerminal;
    insertSymbol(nonTerminal->value, element, nonTerminalMap, MAP_SIZE);
  }
}

void failureNode(lexeme_t *currLexeme, char *currTerminal, int *numElements,
                 element_t **elements, dfa_node_t *node, dfa_node_t *root,
                 char ch, int *inputIndex, int *strIndex, char *contents) {
  if (currLexeme != NULL) {
    element_t *element;
    if (currTerminal[0] != '\0') {
      terminal_t *terminal = (terminal_t *)calloc(1, sizeof(terminal_t));
      strcpy(terminal->value, currTerminal);
      element = (element_t *)calloc(1, sizeof(element_t));
      element->type = ELEMENT_TERMINAL;
      element->element.terminal = terminal;
      elements[(*numElements)++] = element;
    }

    element = (element_t *)calloc(1, sizeof(element_t));
    element->type = ELEMENT_LEXEME;
    element->element.lexeme = currLexeme;
    elements[(*numElements)++] = element;

    memset(currTerminal, 0, strlen(currTerminal) * sizeof(char));
    currLexeme = NULL;

    node = root;
    *inputIndex = *strIndex;
  } else {
    if (node == root) {
      strcat(currTerminal, &ch);
      inputIndex++;
      return;
    }

    int rejectLength = inputIndex - strIndex - node->failurePrefixLength;
    char *reject = (char *)calloc(rejectLength + 1, sizeof(char));
    memcpy(reject, contents + *strIndex, rejectLength);
    reject[rejectLength] = '\0';
    strcat(currTerminal, reject);
    free(reject);

    node = node->failure;
    strIndex += rejectLength;
  }
}

element_set_t *lex(char *contents) {
  char ch;
  char currTerminal[MAX_TERMINAL_SIZE] = "\0";
  lexeme_t *currLexeme = NULL;
  int strIndex = 0;
  int inputIndex = 0;

  dfa_node_t *root = lexerNodeMap[0];
  dfa_node_t *node = root;
  element_t **elements =
      (element_t **)calloc(MAX_ELEMENTS, sizeof(element_t *));
  int numElements = 0;
  int contentSize = strlen(contents);
  char key[2];

  while (inputIndex < contentSize) {

    ch = contents[inputIndex];
    sprintf(key, "%c", ch);
    dfa_map_item_t *item = searchLexerNode(key, node->next, MAP_SIZE);

    // Test out regex
    if (item == NULL) {
      if (isalpha(ch)) {
        item = searchLexerNode("%s", node->next, MAP_SIZE);
      } else if (isdigit(ch)) {
        item = searchLexerNode("%d", node->next, MAP_SIZE);
      }
    }

    if (item == NULL) {
      if (currLexeme != NULL) {
        element_t *element;
        if (currTerminal[0] != '\0') {
          terminal_t *terminal = (terminal_t *)calloc(1, sizeof(terminal_t));
          strcpy(terminal->value, currTerminal);
          element = (element_t *)calloc(1, sizeof(element_t));
          element->type = ELEMENT_TERMINAL;
          element->element.terminal = terminal;
          elements[numElements++] = element;
        }

        element = (element_t *)calloc(1, sizeof(element_t));
        element->type = ELEMENT_LEXEME;
        element->element.lexeme = currLexeme;
        elements[numElements++] = element;

        memset(currTerminal, 0, sizeof(currTerminal));
        currLexeme = NULL;

        node = root;
        inputIndex = strIndex;
      } else {
        if (node == root) {
          currTerminal[strlen(currTerminal)] = ch;
          currTerminal[strlen(currTerminal) + 1] = '\0';
          inputIndex++;
          strIndex = inputIndex;
          continue;
        }

        int rejectLength = inputIndex - strIndex - node->failurePrefixLength;
        char *reject = (char *)calloc(rejectLength + 1, sizeof(char));
        memcpy(reject, contents + strIndex, rejectLength);
        reject[rejectLength] = '\0';
        strcat(currTerminal, reject);
        free(reject);

        node = node->failure;
        strIndex += rejectLength;
      }

    } else {
      node = item->node;
      inputIndex++;

      if (node->lexeme != NULL) {
        currLexeme = node->lexeme;
        strIndex = inputIndex;
      }
    }
  }

  if (currTerminal[0] != '\0') {
    terminal_t *terminal = (terminal_t *)calloc(1, sizeof(terminal_t));
    strcpy(terminal->value, currTerminal);
    element_t *element = (element_t *)calloc(1, sizeof(element_t));
    element->type = ELEMENT_TERMINAL;
    element->element.terminal = terminal;
    elements[numElements++] = element;
  }

  if (currLexeme != NULL) {
    element_t *element = (element_t *)calloc(1, sizeof(element_t));
    element->type = ELEMENT_LEXEME;
    element->element.lexeme = currLexeme;
    elements[numElements++] = element;
  }

  if (strIndex != inputIndex) {
    int rejectLength = inputIndex - strIndex;
    char *reject = (char *)calloc(rejectLength + 1, sizeof(char));
    memcpy(reject, contents + strIndex, rejectLength);
    reject[rejectLength] = '\0';

    terminal_t *terminal = (terminal_t *)calloc(1, sizeof(terminal_t));
    strcpy(terminal->value, reject);
    element_t *element = (element_t *)calloc(1, sizeof(element_t));
    element->type = ELEMENT_TERMINAL;
    element->element.terminal = terminal;
    elements[numElements++] = element;

    free(reject);
  }

  element_set_t *elementSet = (element_set_t *)calloc(1, sizeof(element_set_t));
  elementSet->numElements = numElements;
  elementSet->elements = elements;

  return elementSet;
}

char *readFile(const char *filename) {
  FILE *fp = fopen(filename, "rb");
  if (!fp) {
    perror("Error opening file");
    return NULL;
  }

  fseek(fp, 0, SEEK_END);
  long fileSize = ftell(fp);
  rewind(fp);

  char *buffer = malloc(fileSize + 1);
  if (!buffer) {
    perror("Memory allocation failed");
    fclose(fp);
    return NULL;
  }

  size_t bytesRead = fread(buffer, 1, fileSize, fp);
  buffer[bytesRead] = '\0';

  fclose(fp);
  return buffer;
}

lr_state_t *createParserState(int id) {
  lr_state_t *state = (lr_state_t *)calloc(1, sizeof(lr_state_t));
  state->id = id;
  return state;
}

void createShiftRule(char key[MAX_TERMINAL_SIZE],
                     symbol_table_item_t **hashTable, int nextState,
                     rule_table_item_t **ruleTable) {
  symbol_table_item_t *item = searchSymbol(key, hashTable, MAP_SIZE);

  if (item == NULL) {
    printf("[createShiftRule] Unexpected error: Element not found %s\n", key);
    error("");
  }

  element_t *element = (element_t *)item->data;
  slr_rule_shift_t *shiftRule =
      (slr_rule_shift_t *)calloc(1, sizeof(slr_rule_shift_t));
  shiftRule->nextState = nextState;

  slr_rule_t *rule = (slr_rule_t *)calloc(1, sizeof(slr_rule_t));
  rule->type = SLR_RULE_SHIFT;
  rule->rule.shift = shiftRule;

  insertSLRRule(element, rule, ruleTable);
}

void createReduceRule(char key[MAX_TERMINAL_SIZE],
                      char nonTerminal[MAX_TERMINAL_SIZE],
                      symbol_table_item_t **hashTable,
                      rule_table_item_t **ruleTable) {

  symbol_table_item_t *item = searchSymbol(key, hashTable, MAP_SIZE);

  if (item == NULL) {
    printf("[createReduceRule] Unexpected error: Edge element not found %s\n",
           key);
    error("");
  }

  element_t *edgeElement = (element_t *)item->data;

  item = searchSymbol(nonTerminal, nonTerminalMap, MAP_SIZE);

  if (item == NULL) {
    printf("[createReduceRule] Unexpected error: nonTerminalElement not found "
           "%s\n",
           key);
    error("");
  }

  element_t *element = (element_t *)item->data;
  slr_rule_reduce_t *reduceRule =
      (slr_rule_reduce_t *)calloc(1, sizeof(slr_rule_reduce_t));
  reduceRule->nonTerminal = element->element.nonTerminal;

  slr_rule_t *rule = (slr_rule_t *)calloc(1, sizeof(slr_rule_t));
  rule->type = SLR_RULE_REDUCE;
  rule->rule.reduce = reduceRule;

  insertSLRRule(edgeElement, rule, ruleTable);
}

void initParserStates() {
  lr_state_t *state;
  int states[MAX_PARSER_STATES] = {0, 1, 2, 3, 4, 5, 6, 7, 8, -1};

  char lexemeShiftRulesKeys[MAX_PARSER_STATES][MAX_PARSER_STATES]
                           [MAX_TERMINAL_SIZE] = {
                               {"SHE"},         {}, {"IS"}, {},
                               {"GOOD", "BAD"}, {}, {},     {}};
  int lexemeShiftRulesStates[MAX_PARSER_STATES][MAX_PARSER_STATES] = {
      {1}, {}, {3}, {}, {5, 6}, {}, {}, {}, {}};

  char terminalShiftRulesKeys[MAX_PARSER_STATES][MAX_PARSER_STATES]
                             [MAX_TERMINAL_SIZE] = {{}, {" "}, {}, {" "}, {},
                                                    {}, {},    {}, {}};
  int terminalShiftRulesStates[MAX_PARSER_STATES][MAX_PARSER_STATES] = {
      {}, {2}, {}, {4}, {}, {}, {}, {}, {}};

  char nonTerminalShiftRulesKeys[MAX_PARSER_STATES][MAX_PARSER_STATES]
                                [MAX_TERMINAL_SIZE] = {
                                    {}, {}, {}, {}, {"condition"},
                                    {}, {}, {}, {}};
  int nonTerminalShiftRulesStates[MAX_PARSER_STATES][MAX_PARSER_STATES] = {
      {}, {}, {}, {}, {7}, {}, {}, {}, {}};

  char lexemeReduceRulesKeys[MAX_PARSER_STATES][MAX_PARSER_STATES]
                            [MAX_TERMINAL_SIZE] = {{}, {}, {}, {},
                                                   {}, {}, {}, {}};
  char lexemeReduceRulesNonTerminals[MAX_PARSER_STATES][MAX_PARSER_STATES]
                                    [MAX_TERMINAL_SIZE] = {{}, {}, {}, {}, {},
                                                           {}, {}, {}, {}};

  char terminalReduceRulesKeys[MAX_PARSER_STATES][MAX_PARSER_STATES]
                              [MAX_TERMINAL_SIZE] = {
                                  {}, {}, {}, {}, {}, {"$"}, {"$"}, {"$"}, {}};
  char terminalReduceRulesNonTerminals[MAX_PARSER_STATES][MAX_PARSER_STATES]
                                      [MAX_TERMINAL_SIZE] = {{},
                                                             {},
                                                             {},
                                                             {},
                                                             {},
                                                             {"condition_good"},
                                                             {"condition_bad"},
                                                             {"sentence"},
                                                             {}};

  char *key = (char *)calloc(MAX_TERMINAL_SIZE, sizeof(char));
  for (int i = 0; states[i] != -1; i++) {
    state = createParserState(states[i]);

    for (int j = 0; j < MAX_PARSER_STATES; j++) {
      key = lexemeShiftRulesKeys[i][j];
      if (key[0] != '\0') {
        int nextState = lexemeShiftRulesStates[i][j];

        createShiftRule(key, lexemeMap, nextState, state->ruleTable);
      }

      key = lexemeReduceRulesKeys[i][j];
      if (key[0] == '\0')
        break;

      createReduceRule(key, lexemeReduceRulesNonTerminals[i][j], lexemeMap,
                       state->ruleTable);
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

      createReduceRule(key, terminalReduceRulesNonTerminals[i][j], terminalMap,
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
      int j =
          parsedElements->numElements - reduceRule->nonTerminal->numElements;

      non_terminal_t *nonTerminal =
          (non_terminal_t *)calloc(1, sizeof(non_terminal_t));
      nonTerminal->type = reduceRule->nonTerminal->type;
      nonTerminal->numElements = reduceRule->nonTerminal->numElements;

      condition_t *condition = (condition_t *)calloc(1, sizeof(condition_t));

      switch (reduceRule->nonTerminal->type) {
      case NON_TERMINAL_CONDITION_GOOD:
        condition_good_t *condition_good =
            (condition_good_t *)calloc(1, sizeof(condition_good_t));
        condition_good->good = parsedElements->elements[j]->element.lexeme;

        condition->type = CONDITION_GOOD;
        condition->condition.good =
            (condition_good_t *)calloc(1, sizeof(condition_good_t));
        condition->condition.good = condition_good;

        nonTerminal->type = NON_TERMINAL_CONDITION;
        nonTerminal->nonTerminal.condition = condition;
        strcpy(nonTerminal->value, "condition");

        j++;

        break;

      case NON_TERMINAL_CONDITION_BAD:
        condition_bad_t *condition_bad =
            (condition_bad_t *)calloc(1, sizeof(condition_bad_t));
        condition_bad->bad = parsedElements->elements[j]->element.lexeme;

        condition->type = CONDITION_BAD;
        condition->condition.bad =
            (condition_bad_t *)calloc(1, sizeof(condition_bad_t));
        condition->condition.bad = condition_bad;

        nonTerminal->type = NON_TERMINAL_CONDITION;
        nonTerminal->nonTerminal.condition = condition;
        strcpy(nonTerminal->value, "condition");

        j++;

        break;

      case NON_TERMINAL_CONDITION:
        printf("[parser] Unexpected error: Received %s for reduction",
               "condition");
        break;

      case NON_TERMINAL_SENTENCE:
        sentence_t *sentence = (sentence_t *)calloc(1, sizeof(sentence_t));
        sentence->she = parsedElements->elements[j]->element.lexeme;
        j++;

        j++;

        sentence->is = parsedElements->elements[j]->element.lexeme;
        j++;

        j++;

        sentence->condition = parsedElements->elements[j]
                                  ->element.nonTerminal->nonTerminal.condition;

        nonTerminal->nonTerminal.sentence = sentence;
        strcpy(nonTerminal->value, "sentence");

        j++;

        break;
      }

      parsedElements->numElements -= reduceRule->nonTerminal->numElements;

      element_t *reducedElement = (element_t *)calloc(1, sizeof(element_t));
      reducedElement->type = ELEMENT_NON_TERMINAL;
      reducedElement->element.nonTerminal = nonTerminal;

      elements->elements[elements->numElements++] = reducedElement;

      numStateHistory -= reduceRule->nonTerminal->numElements;
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

void error(const char *msg) {
  perror(msg);
  exit(1);
}

element_set_t *reverseElementSet(element_set_t *elementSet) {
  element_set_t *reversed = (element_set_t *)calloc(1, sizeof(element_set_t));
  reversed->elements = (element_t **)calloc(MAX_ELEMENTS, sizeof(element_t));

  for (int i = elementSet->numElements - 1; i >= 0; i--) {
    reversed->elements[reversed->numElements++] = elementSet->elements[i];
  }

  free(elementSet);
  return reversed;
}

void printElements(element_set_t *elementSet) {
  for (int i = 0; i < elementSet->numElements; i++) {
    element_t *element = elementSet->elements[i];
    switch (element->type) {
    case ELEMENT_LEXEME:
      printf("Lexeme: %s\n", element->element.lexeme->value);
      break;
    case ELEMENT_TERMINAL:
      printf("Terminal: %s\n", element->element.terminal->value);
      break;
    case ELEMENT_NON_TERMINAL:
      printf("NonTerminal: %s\n", element->element.nonTerminal->value);
      break;
    }
  }
  printf("\n\n");
}

int main() {
  initLexemes();
  initTerminals();
  initNonTerminals();

  initLexerNodes();
  addFailureNodes();
  addLexerTrieEdges();
  addLexemesToLexerNodes();

  initParserStates();

  char *contents = readFile("./sample.txt");
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
  printf("%s\n", sentence->she->value);
  printf("%s\n", sentence->is->value);
  printf("%s\n", sentence->condition->condition.good->good->value);

  return 0;
}

unsigned long hash(char *str, int size) {
  unsigned long hash = 51212421381;
  int c;
  while ((c = *str++))
    hash = ((hash << 5) + hash) + c;

  return hash % size;
}

dfa_map_item_t *searchLexerNode(char *key, dfa_map_item_t *hashTable[],
                                int size) {
  if (size == 0)
    return NULL;

  int hashIndex = hash(key, size);

  while (hashTable[hashIndex] != NULL) {
    if (hashTable[hashIndex]->key == hashIndex)
      return hashTable[hashIndex];
    ++hashIndex;
    hashIndex %= size;
  }

  return NULL;
}

void insertLexerNode(char *key, dfa_node_t *node, dfa_map_item_t *hashTable[],
                     int size) {
  dfa_map_item_t *item;
  item = searchLexerNode(key, hashTable, size);

  if (item != NULL && streq(key, item->edge, strlen(key)))
    return;

  int hashIndex = hash(key, size);
  item = (dfa_map_item_t *)calloc(1, sizeof(dfa_map_item_t));
  item->edge = (char *)calloc(1, sizeof(char));

  item->node = node;
  item->key = hashIndex;
  strcpy(item->edge, key);

  while (hashTable[hashIndex] != NULL) {
    ++hashIndex;
    hashIndex %= size;
  }

  hashTable[hashIndex] = item;
}

int hashElement(element_t *e, int size) {
  // element hashTable is partitioned into 3
  switch (e->type) {
  case ELEMENT_LEXEME:
    return hash(e->element.lexeme->value, size);
  case ELEMENT_TERMINAL:
    return size + hash(e->element.terminal->value, size);
  case ELEMENT_NON_TERMINAL:
    return 2 * size + hash(e->element.nonTerminal->value, size);
  }
}

rule_table_item_t *searchSLRRule(element_t *key,
                                 rule_table_item_t *hashTable[]) {
  int size = 3 * MAX_RULES_IN_STATE;
  if (size == 0)
    return NULL;

  int hashIndex = hashElement(key, MAX_RULES_IN_STATE);

  while (hashTable[hashIndex] != NULL) {
    if (hashTable[hashIndex]->key == hashIndex)
      return hashTable[hashIndex];
    ++hashIndex;
    hashIndex %= size;
  }

  return NULL;
}

void insertSLRRule(element_t *key, slr_rule_t *rule,
                   rule_table_item_t *hashTable[]) {
  rule_table_item_t *item;
  int size = 3 * MAX_RULES_IN_STATE;

  item = searchSLRRule(key, hashTable);

  if (item != NULL)
    return;

  int hashIndex = hashElement(key, MAX_RULES_IN_STATE);
  item = (rule_table_item_t *)calloc(1, sizeof(dfa_map_item_t));

  item->rule = rule;
  item->key = hashIndex;

  while (hashTable[hashIndex] != NULL) {
    ++hashIndex;
    hashIndex %= size;
  }

  hashTable[hashIndex] = item;
}

symbol_table_item_t *searchSymbol(char *key, symbol_table_item_t *hashTable[],
                                  int size) {
  int hashIndex = hash(key, size);

  while (hashTable[hashIndex] != NULL) {
    if (hashTable[hashIndex]->key == hashIndex)
      return hashTable[hashIndex];
    ++hashIndex;
    hashIndex %= size;
  }

  return NULL;
}

void insertSymbol(char *key, void *data, symbol_table_item_t *hashTable[],
                  int size) {
  symbol_table_item_t *item;
  item = searchSymbol(key, hashTable, size);

  if (item != NULL && streq(key, item->edge, strlen(key)))
    return;

  int hashIndex = hash(key, size);
  item = (symbol_table_item_t *)calloc(1, sizeof(symbol_table_item_t));
  item->edge = (char *)calloc(MAX_TERMINAL_SIZE, sizeof(char));

  item->data = data;
  item->key = hashIndex;
  strcpy(item->edge, key);

  while (hashTable[hashIndex] != NULL) {
    ++hashIndex;
    hashIndex %= size;
  }

  hashTable[hashIndex] = item;
}
