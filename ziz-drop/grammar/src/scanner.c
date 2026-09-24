// SPDX-License-Identifier: MPL-2.0
// External scanner for tree-sitter-ziz: off-side (indentation) layout.
//
// Emits INDENT / DEDENT / NEWLINE from an indent stack. Layout is suppressed
// while bracket_depth > 0, i.e. inside ( ) [ ] { }, so explicit S-expressions
// are layout-insensitive as DESIGN.adoc requires.

#include "tree_sitter/parser.h"
#include <string.h>
#include <stdint.h>

enum TokenType { INDENT, DEDENT, NEWLINE, ERROR_SENTINEL };

#define MAX_INDENTS 128

typedef struct {
  uint16_t indents[MAX_INDENTS];
  uint8_t  depth;          // indent stack height (indents[0] is always 0)
  uint16_t bracket_depth;
  uint8_t  pending_dedents;
} Scanner;

static inline void push(Scanner *s, uint16_t col) {
  if (s->depth < MAX_INDENTS) s->indents[s->depth++] = col;
}

void *tree_sitter_ziz_external_scanner_create(void) {
  Scanner *s = (Scanner *)calloc(1, sizeof(Scanner));
  push(s, 0);
  return s;
}

void tree_sitter_ziz_external_scanner_destroy(void *p) { free(p); }

unsigned tree_sitter_ziz_external_scanner_serialize(void *p, char *buf) {
  Scanner *s = (Scanner *)p;
  unsigned n = 0;
  buf[n++] = (char)s->depth;
  buf[n++] = (char)(s->bracket_depth & 0xff);
  buf[n++] = (char)(s->bracket_depth >> 8);
  buf[n++] = (char)s->pending_dedents;
  for (unsigned i = 0; i < s->depth && n + 1 < TREE_SITTER_SERIALIZATION_BUFFER_SIZE; i++) {
    buf[n++] = (char)(s->indents[i] & 0xff);
    buf[n++] = (char)(s->indents[i] >> 8);
  }
  return n;
}

void tree_sitter_ziz_external_scanner_deserialize(void *p, const char *buf, unsigned len) {
  Scanner *s = (Scanner *)p;
  memset(s, 0, sizeof(*s));
  if (len == 0) { push(s, 0); return; }
  unsigned n = 0;
  uint8_t depth = (uint8_t)buf[n++];
  s->bracket_depth = (uint8_t)buf[n] | ((uint8_t)buf[n + 1] << 8); n += 2;
  s->pending_dedents = (uint8_t)buf[n++];
  for (unsigned i = 0; i < depth && n + 1 < len; i++) {
    s->indents[s->depth++] = (uint8_t)buf[n] | ((uint8_t)buf[n + 1] << 8);
    n += 2;
  }
  if (s->depth == 0) push(s, 0);
}

static void skip_comment(TSLexer *lx) {
  while (lx->lookahead && lx->lookahead != '\n') lx->advance(lx, true);
}

bool tree_sitter_ziz_external_scanner_scan(void *p, TSLexer *lx, const bool *valid) {
  Scanner *s = (Scanner *)p;

  if (valid[ERROR_SENTINEL]) return false;

  if (s->pending_dedents > 0 && valid[DEDENT]) {
    s->pending_dedents--;
    s->depth--;
    lx->result_symbol = DEDENT;
    return true;
  }

  // Observe brackets without consuming them (the internal lexer does that).
  if (lx->lookahead == '(' || lx->lookahead == '[' || lx->lookahead == '{') {
    s->bracket_depth++;
    return false;
  }
  if (lx->lookahead == ')' || lx->lookahead == ']' || lx->lookahead == '}') {
    if (s->bracket_depth > 0) s->bracket_depth--;
    return false;
  }

  if (s->bracket_depth > 0) return false;

  bool saw_newline = false;
  uint16_t col = 0;

  for (;;) {
    if (lx->lookahead == '\n') {
      saw_newline = true; col = 0;
      lx->advance(lx, true);
    } else if (lx->lookahead == '\r') {
      lx->advance(lx, true);
    } else if (lx->lookahead == ' ') {
      col++; lx->advance(lx, true);
    } else if (lx->lookahead == '\t') {
      col = (uint16_t)((col / 8 + 1) * 8); lx->advance(lx, true);
    } else if (lx->lookahead == ';') {
      skip_comment(lx);
    } else if (lx->lookahead == '\\') {
      lx->mark_end(lx);
      lx->advance(lx, true);
      if (lx->lookahead == '\r') lx->advance(lx, true);
      if (lx->lookahead == '\n') { lx->advance(lx, true); col = 0; continue; }
      return false;
    } else {
      break;
    }
  }

  if (!saw_newline && lx->lookahead != 0) return false;
  if (lx->lookahead == '\n') return false;

  uint16_t top = s->indents[s->depth - 1];

  if (lx->eof(lx)) {
    if (s->depth > 1 && valid[DEDENT]) {
      s->pending_dedents = (uint8_t)(s->depth - 2);
      s->depth--;
      lx->result_symbol = DEDENT;
      return true;
    }
    if (valid[NEWLINE]) { lx->result_symbol = NEWLINE; return true; }
    return false;
  }

  if (col > top) {
    if (valid[INDENT]) {
      push(s, col);
      lx->result_symbol = INDENT;
      return true;
    }
    if (valid[NEWLINE]) { lx->result_symbol = NEWLINE; return true; }
    return false;
  }

  if (col < top) {
    uint8_t levels = 0;
    for (int i = s->depth - 1; i > 0 && s->indents[i] > col; i--) levels++;
    if (levels > 0 && valid[DEDENT]) {
      s->pending_dedents = (uint8_t)(levels - 1);
      s->depth--;
      lx->result_symbol = DEDENT;
      return true;
    }
  }

  if (valid[NEWLINE]) { lx->result_symbol = NEWLINE; return true; }
  return false;
}
