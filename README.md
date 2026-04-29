# SICC — Simple Intermediate Compiler Compiler

SICC is a compiler-compiler that accepts an arbitrary context-free grammar (CFG) and emits self-contained C code implementing an SLR(1) parser along with typed AST structs for every production in the grammar.

---

## Overview

SICC reads two specification files — a **parser grammar** and a **lexer grammar** — and produces C source code containing:

1. Typed `struct` / `union` definitions for every non-terminal in the grammar.
2. An SLR(1) parse table encoded as static C arrays.
3. A shift-reduce driver loop that populates the AST.

The output is a single `.c` / `.h` pair that can be dropped into any C project with no additional runtime dependencies.

---

## Features

| Feature | Detail |
|---|---|
| Grammar input | Arbitrary CFG in a Yacc-inspired syntax |
| Parsing strategy | SLR(1) — single token of lookahead |
| Operator precedence | `%left`, `%right`, `%nonassoc` directives |
| Lexer spec | Per-token regex patterns in a separate file |
| AST generation | Tagged-union structs for every alternative production |
| Output | Plain C99, no external dependencies |

---

## Installation

```bash
git clone https://github.com/yourname/sicc.git
cd sicc
cargo run parser.txt lexer.txt
# Generates lr.c and lr.h
```

---

## Grammar File Format

### Parser Grammar

The parser grammar follows a Yacc-like syntax.

```
%left "+" "-"

%%

sentence ->
	@sentence SHE IS condition
;

condition ->
	@good GOOD
	| @bad BAD
;
```

**Precedence declarations** appear before `%%`. Later declarations bind more tightly.

**Productions** follow `%%`. Each rule has the form:

```
non_terminal -> @tag SYMBOL ... ;
```

- `@tag` is an identifier used to name the corresponding struct variant in the generated C output.
- `SYMBOL` is either a non-terminal (lowercase) or a quoted/unquoted terminal token name.
- Multiple alternatives for the same non-terminal are separated by `|` on the same line or written as separate rules.

### Lexer Grammar

One token definition per line:

```
TOKEN_NAME: <regex>
```

Token names must be uppercase. The regex is matched against the input stream in declaration order; the first match wins.

```
NUMBER:  ^[0-9]+$
IDENT:   ^[a-zA-Z_][a-zA-Z0-9_]*$
PLUS:    ^\+$
```

---

## Generated Output

### AST Types

For every non-terminal, SICC emits a C struct. When a non-terminal has multiple alternatives, a tagged union is produced.

Given the grammar rule:

```
condition -> @good GOOD | @bad BAD ;
```

SICC generates:

```c
/* Forward declarations */
typedef struct ConditionGood condition_good_t;
typedef struct ConditionBad condition_bad_t;
typedef struct Condition condition_t;

typedef struct ConditionGood {
    lexeme_t *good;
} condition_good_t;

typedef struct ConditionBad {
    lexeme_t *bad;
} condition_bad_t;

typedef struct Condition {
    enum {
        CONDITION_GOOD,
        CONDITION_BAD
    } kind;
    union {
        condition_good_t *good;
        condition_bad_t  *bad;
    } condition;
} condition_t;
```

For non-terminals with a single production, no union is emitted — just a flat struct whose fields correspond to each symbol in the rule, in order.
