#include "lr.h"
#include <stdio.h>

int main() {
  char filename[50] = "./sample.txt";

  sentence_t *sentence = getParseTree(filename);
  printf("%s\n", sentence->she->value);
  printf("%s\n", sentence->is->value);
  printf("%s\n", sentence->condition->condition.good->good->value);

  return 0;
}
