// SPDX-License-Identifier: MPL-2.0
// tree-sitter grammar for Žiz — S-expression core + off-side layout.
//
// The layout (off-side) rule is NOT context-free. It is handled by the
// external scanner in src/scanner.c, which emits _indent / _dedent / _newline
// from an indent stack. Inside explicit brackets the scanner suppresses
// layout tokens, so parenthesised code is layout-insensitive.
//
// Surface syntax is ASCII-only. String *contents* may be any UTF-8.

const SYM_START = /[A-Za-z_+\-*\/<>=!?%&|^~$]/;
const SYM_REST = /[A-Za-z0-9_+\-*\/<>=!?%&|^~$.:]*/;

module.exports = grammar({
  name: 'ziz',

  externals: $ => [
    $._indent,
    $._dedent,
    $._newline,
    $.error_sentinel, // lets the scanner detect error recovery
  ],

  extras: $ => [
    /[ \t\r]/,
    $.comment,
    $.line_continuation,
  ],

  word: $ => $.symbol,

  rules: {
    source_file: $ => repeat(choice($._layout_form, $._newline)),

    // ---- layout (off-side) surface ------------------------------------
    // A logical line with >1 item, optionally followed by an indented block,
    // is an implicit list whose tail is the block's lines. A single bare
    // atom on a line is itself, not a one-element list.
    _layout_form: $ => choice(
      $.layout_list,
      seq($._form, $._newline),
    ),

    layout_list: $ => prec.right(seq(
      $._form,
      repeat1($._form),
      optional($.layout_block),
      $._newline,
    )),

    layout_block: $ => seq(
      $._indent,
      repeat1($._layout_form),
      $._dedent,
    ),

    // ---- S-expression core --------------------------------------------
    _form: $ => choice(
      $.list,
      $.vector,
      $.map,
      $.quote,
      $.quasiquote,
      $.unquote_splicing,
      $.unquote,
      $._atom,
    ),

    list:   $ => seq('(', repeat($._form), ')'),
    vector: $ => seq('[', repeat($._form), ']'),
    map:    $ => seq('{', repeat(seq(field('key', $._form), field('value', $._form))), '}'),

    quote:            $ => seq("'",  $._form),
    quasiquote:       $ => seq('`',  $._form),
    unquote_splicing: $ => seq(',@', $._form),
    unquote:          $ => seq(',',  $._form),

    _atom: $ => choice(
      $.nil,
      $.boolean,
      $.real,
      $.integer,
      $.string,
      $.keyword,
      $.symbol,
    ),

    nil:     $ => 'nil',
    boolean: $ => choice('true', 'false'),

    // real before integer so the longer match wins
    real:    $ => token(/-?[0-9]+\.[0-9]+([eE][-+]?[0-9]+)?/),
    integer: $ => token(/-?[0-9]+/),

    string: $ => seq(
      '"',
      repeat(choice(
        token.immediate(prec(1, /[^"\\\n]+/)),
        $.escape_sequence,
      )),
      '"',
    ),
    escape_sequence: $ => token.immediate(/\\[nrt"\\]|\\u[0-9a-fA-F]{4}/),

    keyword: $ => token(seq(':', SYM_START, SYM_REST)),
    symbol:  $ => token(seq(SYM_START, SYM_REST)),

    comment: $ => token(seq(';', /.*/)),
    line_continuation: $ => token(seq('\\', /\r?\n/)),
  },
});
