# Lexer Design

The lexer (lexical analyzer) is the first stage of the language processing pipeline, converting source code into a stream of tokens.

## Overview

```
Source Code (String)
        │
        ▼
┌───────────────────┐
│      Lexer        │
│  ┌─────────────┐  │
│  │  Scanner    │  │  Character-by-character processing
│  └──────┬──────┘  │
│         ▼         │
│  ┌─────────────┐  │
│  │  Tokenizer  │  │  Group characters into tokens
│  └──────┬──────┘  │
│         ▼         │
│  ┌─────────────┐  │
│  │  Filter     │  │  Remove whitespace/comments
│  └─────────────┘  │
└────────┬──────────┘
         ▼
    Token Stream
```

## Token Types

### Common Token Categories

```rust
enum TokenKind {
    // Literals
    IntLiteral,      // 42, 0xFF, 0b1010
    FloatLiteral,    // 3.14, 1e-10
    StringLiteral,   // "hello"
    CharLiteral,     // 'a'
    BoolLiteral,     // true, false

    // Identifiers
    Identifier,      // foo, myVar, _private
    TypeIdentifier,  // Int, String, MyType

    // Keywords (language-specific)
    Keyword,         // let, fn, if, while, ...

    // Operators
    Plus, Minus, Star, Slash,  // + - * /
    Eq, Ne, Lt, Le, Gt, Ge,    // == != < <= > >=
    And, Or, Not,              // && || !
    Assign,                     // =
    Arrow,                      // ->
    FatArrow,                   // =>

    // Delimiters
    LParen, RParen,    // ( )
    LBrace, RBrace,    // { }
    LBracket, RBracket, // [ ]
    Comma, Semicolon, Colon, Dot,

    // Special
    Newline,
    Indent, Dedent,    // For indentation-sensitive languages
    Eof,
    Error,
}
```

### Token Structure

```rust
struct Token {
    kind: TokenKind,
    lexeme: String,      // The actual text
    span: Span,          // Source location
    value: Option<Value>, // Parsed literal value
}

struct Span {
    file: FileId,
    start: Position,
    end: Position,
}

struct Position {
    offset: usize,  // Byte offset
    line: u32,
    column: u32,
}
```

## Per-Language Lexers

### Me (Ages 8-12)

Simple tokenizer with minimal special characters:

```
Tokens: words, numbers, quoted strings
Keywords: say, ask, remember, repeat, if, then, else, end
No operators - uses words: plus, minus, times, divided-by
```

### Solo (Ages 13-14)

Standard imperative tokenizer:

```
let count = 0
repeat 5 times
    count = count + 1
    say count
end
```

Tokens:
- Keywords: `let`, `repeat`, `times`, `say`, `end`
- Identifiers: `count`
- Literals: `0`, `5`, `1`
- Operators: `=`, `+`

### Duet (Ages 15)

Adds AI-related tokens:

```
@synth: function to sort list
@verify: sorted output
intent("find the maximum")
```

Special tokens:
- Annotations: `@synth`, `@verify`
- Intent strings: `intent("...")`

### Ensemble (Ages 16+)

Full-featured lexer with advanced constructs:

```rust
ai_model GPT4 {
    temperature: 0.7,
    max_tokens: 1000,
}

prompt analyze_code {
    system: "You are a code reviewer.",
    user: "{code}",
}
```

### betlang

Racket-based S-expression lexer:

```racket
(ternary-bet 'option-a 'option-b 'option-c)
(weighted-bet ((0.5 . heads) (0.5 . tails)))
```

Tokens: parentheses, symbols, numbers, quoted atoms

### julia-the-viper

Dual-channel lexer (Control + Data):

```
CONTROL: if input > 0 then output = process(input)
DATA:    [name: "John", age: 30]
```

Separate token streams for each channel.

### AffineScript

Advanced token set for type annotations:

```ocaml
let f : forall a. (Linear a -> a) -> Effect[IO] a =
  fun x -> consume x
```

Tokens for: `forall`, `Linear`, `Effect`, type brackets

### Ephapax

Linear/ephemeral markers:

```scheme
(once x (use x))  ; x can only be used once
(ephemeral (create-resource))
```

Special tokens: `once`, `ephemeral`, `consume`

## Implementation Approaches

### 1. Hand-Written Lexer

Best for: Simple languages, educational purposes

```rust
impl Lexer {
    fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        match self.current_char() {
            None => Token::eof(self.position()),
            Some(c) if c.is_alphabetic() => self.identifier(),
            Some(c) if c.is_digit(10) => self.number(),
            Some('"') => self.string(),
            Some('+') => self.single_char(TokenKind::Plus),
            // ... more patterns
        }
    }
}
```

Pros: Full control, excellent error messages
Cons: More code to maintain

### 2. Lexer Generator (Logos)

Best for: Production languages

```rust
use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
enum Token {
    #[token("let")]
    Let,

    #[token("fn")]
    Fn,

    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier,

    #[regex("[0-9]+")]
    Number,

    #[error]
    Error,
}
```

Pros: Fast, declarative, less bugs
Cons: Less control over error recovery

### 3. Parser Combinator Approach (nom)

Best for: Complex token patterns

```rust
fn identifier(input: &str) -> IResult<&str, Token> {
    map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_"))))
        )),
        |s| Token::Identifier(s.to_string())
    )(input)
}
```

## Features

### Source Location Tracking

Essential for error messages:

```rust
fn error_at(&self, span: Span, message: &str) -> Diagnostic {
    Diagnostic::error()
        .with_message(message)
        .with_label(Label::primary(span.file, span.start..span.end))
}

// Output:
// error: unexpected character '©'
//   --> src/main.solo:5:10
//   |
// 5 | let x = ©
//   |         ^ unexpected character
```

### Unicode Support

Handle international identifiers:

```rust
fn is_identifier_start(c: char) -> bool {
    c.is_alphabetic() || c == '_'
    // Or use Unicode categories:
    // unicode_xid::UnicodeXID::is_xid_start(c)
}
```

### Incremental Lexing

For IDE support, re-lex only changed portions:

```rust
struct IncrementalLexer {
    tokens: Vec<Token>,
    dirty_range: Option<Range<usize>>,
}

impl IncrementalLexer {
    fn update(&mut self, edit: TextEdit) {
        // Only re-lex affected region
        let start = self.find_token_at(edit.start);
        let end = self.find_token_at(edit.end);
        self.relex_range(start, end, &edit.new_text);
    }
}
```

### Error Recovery

Continue lexing after errors:

```rust
fn recover_from_error(&mut self) -> Token {
    let start = self.position();

    // Skip until we find something recognizable
    while let Some(c) = self.current_char() {
        if c.is_whitespace() || self.is_delimiter(c) {
            break;
        }
        self.advance();
    }

    Token::error("unrecognized token", Span::new(start, self.position()))
}
```

## Testing Lexers

### Unit Tests

```rust
#[test]
fn test_keywords() {
    let tokens: Vec<_> = Lexer::new("let fn if").collect();
    assert_eq!(tokens[0].kind, TokenKind::Let);
    assert_eq!(tokens[1].kind, TokenKind::Fn);
    assert_eq!(tokens[2].kind, TokenKind::If);
}

#[test]
fn test_numbers() {
    let tokens: Vec<_> = Lexer::new("42 3.14 0xFF").collect();
    assert_eq!(tokens[0].value, Some(Value::Int(42)));
    assert_eq!(tokens[1].value, Some(Value::Float(3.14)));
    assert_eq!(tokens[2].value, Some(Value::Int(255)));
}
```

### Fuzzing

```rust
#[test]
fn fuzz_lexer() {
    arbtest::arbtest(|u| {
        let input: String = u.arbitrary()?;
        // Lexer should never panic
        let _: Vec<_> = Lexer::new(&input).collect();
        Ok(())
    });
}
```

### Echidna Verification

```
(verify-property lexer-complete
  "Every valid program can be tokenized"
  (forall (prog : ValidProgram)
    (exists (tokens : TokenStream)
      (tokenize prog) = tokens)))
```

## Related Pages

- [[Parser Architecture]]
- [[Error Messages]]
- [[Unicode Handling]]
- [[IDE Support]]
