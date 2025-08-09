#include "lr.h"
#include "constants.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

dfa_node_t *lexerNodeMap[MAX_LEXER_NODES];
symbol_table_item_t *lexemeMap[MAP_SIZE];

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
  int nodes[MAX_LEXER_NODES][4] = {{DFA_NODE_ROOT, 0, 0},
                                   {DFA_NODE_REGULAR, 1, 0},
                                   {DFA_NODE_REGULAR, 2, 0},
                                   {DFA_NODE_REGULAR, 3, 0}};

  for (int i = 0; i < MAX_LEXER_NODES; i++) {
    node = createLexerNode(nodes[i][0], nodes[i][1], nodes[i][2]);
    lexerNodeMap[i] = node;
  }
}

void addFailureNodes() {
  int failures[MAX_LEXER_NODES] = {0, 0, 0};
  for (int i = 0; i < MAX_LEXER_NODES; i++) {
    lexerNodeMap[i]->failure = lexerNodeMap[failures[i]];
  }
}

void addLexemesToLexerNodes() {
  char lexemes[MAX_LEXER_NODES][MAX_TERMINAL_SIZE] = {"", "", "", "she"};

  for (int i = 0; i < MAX_LEXER_NODES; i++) {
    if (lexemes[i][0] == '\0')
      continue;

    symbol_table_item_t *item =
        searchSymbol(lexemes[i], lexemeMap, MAX_LEXEMES);

    if (item == NULL) {
      printf("[addLexemesToLexerNodes] Unexpected Error: Lexeme not found: %s",
             lexemes[i]);
      continue;
    }

    lexeme_t *lexeme = (lexeme_t *)item->data;
    dfa_node_t *node = lexerNodeMap[i];
    node->lexeme = lexeme;
  }
}

void addLexerTrieEdges() {
  int adjacencyMatrixNodes[MAX_LEXER_NODES][MAX_LEXER_NODES] = {
      {1}, {2}, {3}, {}};
  char adjacencyMatrixEdges[MAX_LEXER_NODES][MAX_LEXER_NODES] = {
      {'s'}, {'h'}, {'e'}, {}};

  for (int i = 0; i < MAX_LEXER_NODES; i++) {
    dfa_node_t *node = lexerNodeMap[i];
    for (int j = 0; j < MAX_LEXER_NODES; j++) {
      if (adjacencyMatrixNodes[i][j] == 0 || adjacencyMatrixEdges[i][j] == '\0')
        continue;

      dfa_node_t *neighbour = lexerNodeMap[adjacencyMatrixNodes[i][j]];
      insertLexerNode(&adjacencyMatrixEdges[i][j], neighbour, node->next,
                      MAP_SIZE);
    }
  }
}

void initLexemes() {
  char lexemes[MAX_LEXEMES][MAX_TERMINAL_SIZE] = {"she"};

  for (int i = 0; i < MAX_LEXEMES; i++) {
    lexeme_t *lexeme = (lexeme_t *)calloc(1, sizeof(lexeme_t));
    strcpy(lexeme->value, lexemes[i]);
    insertSymbol(lexeme->value, lexeme, lexemeMap, MAX_LEXEMES);
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

void error(const char *msg) {
  perror(msg);
  exit(1);
}

int main() {
  initLexemes();
  initLexerNodes();
  addFailureNodes();
  addLexerTrieEdges();
  addLexemesToLexerNodes();

  char *contents = readFile("./sample.txt");
  if (!contents)
    error("Error in reading input file");

  element_set_t *elementSet = lex(contents);

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
      printf("NonTerminal: %s\n", element->element.non_terminal->value);
      break;
    }
  }

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
