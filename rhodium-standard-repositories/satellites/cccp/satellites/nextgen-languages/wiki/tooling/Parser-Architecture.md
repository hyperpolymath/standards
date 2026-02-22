# Parser Architecture

The parser transforms a token stream into an Abstract Syntax Tree (AST), representing the grammatical structure of the program.

## Overview

```
Token Stream
      │
      ▼
┌─────────────────┐
│     Parser      │
│  ┌───────────┐  │
│  │  Grammar  │  │  Language rules
│  │   Rules   │  │
│  └─────┬─────┘  │
│        ▼        │
│  ┌───────────┐  │
│  │   Parse   │  │  Build tree structure
│  │   Tree    │  │
│  └─────┬─────┘  │
│        ▼        │
│  ┌───────────┐  │
│  │   Error   │  │  Recover and report
│  │  Handler  │  │
│  └───────────┘  │
└────────┬────────┘
         ▼
   Abstract Syntax Tree
```

## Grammar Specification

### EBNF Notation

```ebnf
(* Solo Language Grammar *)
program     = statement* ;
statement   = let_stmt | if_stmt | while_stmt | expr_stmt ;
let_stmt    = "let" IDENTIFIER "=" expression ;
if_stmt     = "if" expression "then" block ("else" block)? "end" ;
while_stmt  = "repeat" expression "times" block "end" ;
block       = statement* ;
expression  = equality ;
equality    = comparison ( ( "==" | "!=" ) comparison )* ;
comparison  = term ( ( "<" | ">" | "<=" | ">=" ) term )* ;
term        = factor ( ( "+" | "-" ) factor )* ;
factor      = unary ( ( "*" | "/" ) unary )* ;
unary       = ( "!" | "-" ) unary | primary ;
primary     = NUMBER | STRING | IDENTIFIER | "(" expression ")" ;
```

### PEG Notation

```peg
// Ensemble Language Grammar (PEG)
Program     <- Statement* EOF
Statement   <- AIModel / Prompt / Function / Let / Expression

AIModel     <- "ai_model" Identifier "{" Property* "}"
Property    <- Identifier ":" Expression ","?

Prompt      <- "prompt" Identifier "{" PromptField* "}"
PromptField <- ("system" / "user" / "assistant") ":" String ","?

Function    <- "fn" Identifier "(" Params? ")" "->" Type Block
Params      <- Param ("," Param)*
Param       <- Identifier ":" Type

Let         <- "let" Identifier (":" Type)? "=" Expression
Expression  <- Ternary
Ternary     <- Or ("?" Expression ":" Expression)?
Or          <- And ("||" And)*
// ... etc
```

## AST Design

### Node Types

```rust
#[derive(Debug, Clone)]
pub enum Expr {
    // Literals
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),

    // Variables
    Var(Identifier),

    // Operations
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        operand: Box<Expr>,
    },

    // Control flow
    If {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Option<Box<Expr>>,
    },

    // Functions
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    Lambda {
        params: Vec<Param>,
        body: Box<Expr>,
    },

    // Blocks
    Block(Vec<Stmt>),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let {
        name: Identifier,
        ty: Option<Type>,
        value: Expr,
    },
    Expr(Expr),
    Return(Option<Expr>),
    While {
        condition: Expr,
        body: Vec<Stmt>,
    },
}
```

### Span Preservation

```rust
#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

pub type SpannedExpr = Spanned<Expr>;
pub type SpannedStmt = Spanned<Stmt>;

// Usage
let expr = SpannedExpr {
    node: Expr::Int(42),
    span: Span { start: 10, end: 12, file: 0 },
};
```

## Parser Types

### Recursive Descent (Hand-Written)

Best for: Educational languages (Me, Solo), maximum control

```rust
impl Parser {
    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.comparison()?;

        while self.match_token(&[TokenKind::EqEq, TokenKind::BangEq]) {
            let op = self.previous().kind.into();
            let right = self.comparison()?;
            left = Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        // Similar pattern...
    }
}
```

### Pratt Parser (Operator Precedence)

Best for: Complex expression grammars (Duet, Ensemble)

```rust
impl Parser {
    fn expression(&mut self, min_bp: u8) -> Result<Expr, ParseError> {
        let mut lhs = self.prefix()?;

        while let Some((l_bp, r_bp)) = self.infix_binding_power() {
            if l_bp < min_bp {
                break;
            }

            let op = self.advance().kind.into();
            let rhs = self.expression(r_bp)?;
            lhs = Expr::Binary {
                left: Box::new(lhs),
                op,
                right: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    fn infix_binding_power(&self) -> Option<(u8, u8)> {
        match self.current().kind {
            TokenKind::Plus | TokenKind::Minus => Some((1, 2)),
            TokenKind::Star | TokenKind::Slash => Some((3, 4)),
            TokenKind::Caret => Some((6, 5)), // Right associative
            _ => None,
        }
    }
}
```

### Parser Combinators

Best for: Prototyping, languages with complex patterns

```rust
use nom::{
    branch::alt,
    combinator::map,
    sequence::{delimited, tuple},
};

fn expression(input: Tokens) -> IResult<Tokens, Expr> {
    alt((
        if_expr,
        lambda_expr,
        binary_expr,
    ))(input)
}

fn if_expr(input: Tokens) -> IResult<Tokens, Expr> {
    map(
        tuple((
            tag(Keyword::If),
            expression,
            tag(Keyword::Then),
            expression,
            tag(Keyword::Else),
            expression,
        )),
        |(_, cond, _, then, _, else_)| Expr::If {
            condition: Box::new(cond),
            then_branch: Box::new(then),
            else_branch: Some(Box::new(else_)),
        }
    )(input)
}
```

### Parser Generators

Best for: Languages with well-defined grammars

**LALRPOP (Rust):**
```rust
// grammar.lalrpop
Expr: Box<Expr> = {
    <l:Expr> "+" <r:Factor> => Box::new(Expr::Add(l, r)),
    Factor,
};

Factor: Box<Expr> = {
    <l:Factor> "*" <r:Term> => Box::new(Expr::Mul(l, r)),
    Term,
};

Term: Box<Expr> = {
    Num => Box::new(Expr::Num(<>)),
    "(" <Expr> ")",
};
```

**Tree-sitter:**
```javascript
// grammar.js
module.exports = grammar({
  name: 'ensemble',

  rules: {
    source_file: $ => repeat($._statement),

    _statement: $ => choice(
      $.let_declaration,
      $.function_declaration,
      $.ai_model_declaration,
    ),

    let_declaration: $ => seq(
      'let',
      field('name', $.identifier),
      optional(seq(':', field('type', $._type))),
      '=',
      field('value', $._expression),
    ),
  }
});
```

## Per-Language Parsers

### Me (Visual/Block-Based)

Not a traditional parser - uses block-based editor with direct AST manipulation.

### Solo

Simple recursive descent:
- No operator precedence complexity
- Word-based operators (`plus`, `minus`)
- Indentation for blocks

### Duet

Pratt parser with extensions:
- AI annotations (`@synth`, `@verify`)
- Intent expressions
- Type inference markers

### Ensemble

Full-featured parser:
- AI model declarations
- Prompt templates
- Generic types
- Effect annotations

### betlang

S-expression parser (trivial grammar):
```rust
fn parse_sexpr(tokens: &mut TokenStream) -> Result<SExpr, Error> {
    match tokens.next() {
        Token::LParen => {
            let mut list = vec![];
            while tokens.peek() != Some(&Token::RParen) {
                list.push(parse_sexpr(tokens)?);
            }
            tokens.expect(Token::RParen)?;
            Ok(SExpr::List(list))
        }
        Token::Symbol(s) => Ok(SExpr::Symbol(s)),
        Token::Number(n) => Ok(SExpr::Number(n)),
        // ...
    }
}
```

### julia-the-viper

Dual-grammar parser:
```rust
enum Channel {
    Control,
    Data,
}

fn parse_statement(tokens: &mut TokenStream) -> Result<Stmt, Error> {
    let channel = detect_channel(tokens)?;
    match channel {
        Channel::Control => parse_control_statement(tokens),
        Channel::Data => parse_data_expression(tokens),
    }
}
```

### AffineScript

Type-aware parser with dependent types:
```ocaml
(* Parse dependent function type *)
(* forall (n : Nat). Vector n Int -> Vector n Int *)
let rec parse_type () =
  match peek () with
  | FORALL ->
    advance ();
    let var = parse_identifier () in
    expect COLON;
    let kind = parse_type () in
    expect DOT;
    let body = parse_type () in
    TForall (var, kind, body)
  | _ -> parse_arrow_type ()
```

## Error Recovery

### Synchronization Points

```rust
fn synchronize(&mut self) {
    self.advance();

    while !self.is_at_end() {
        // Sync at statement boundaries
        if self.previous().kind == TokenKind::Semicolon {
            return;
        }

        // Sync at keywords
        match self.current().kind {
            TokenKind::Let
            | TokenKind::Fn
            | TokenKind::If
            | TokenKind::While
            | TokenKind::Return => return,
            _ => {}
        }

        self.advance();
    }
}
```

### Error Productions

```rust
fn let_statement(&mut self) -> Result<Stmt, ParseError> {
    self.expect(TokenKind::Let)?;

    let name = self.expect_identifier()
        .or_else(|_| {
            self.error("expected variable name");
            Ok(Identifier::error())
        })?;

    self.expect(TokenKind::Eq)
        .or_else(|_| {
            self.error("expected '=' after variable name");
            Ok(())
        })?;

    let value = self.expression()
        .or_else(|_| {
            self.error("expected expression");
            Ok(Expr::Error)
        })?;

    Ok(Stmt::Let { name, ty: None, value })
}
```

## CST vs AST

### Concrete Syntax Tree (CST)

Preserves all syntax details:
```rust
// CST for: let x = 1 + 2
LetDecl {
    let_keyword: Token("let"),
    name: Token("x"),
    eq: Token("="),
    value: BinaryExpr {
        left: Token("1"),
        op: Token("+"),
        right: Token("2"),
    },
}
```

### Abstract Syntax Tree (AST)

Normalized structure:
```rust
// AST for: let x = 1 + 2
Let {
    name: "x",
    value: Binary {
        op: Add,
        left: Int(1),
        right: Int(2),
    },
}
```

### When to Use Which

| Feature | CST | AST |
|---------|-----|-----|
| Formatting | ✓ | - |
| Refactoring | ✓ | - |
| Linting | ✓ | ✓ |
| Compilation | - | ✓ |
| Type checking | - | ✓ |

## Testing

### Golden Tests

```rust
#[test]
fn test_parse_let() {
    let input = "let x = 42";
    let ast = parse(input).unwrap();
    insta::assert_debug_snapshot!(ast);
}
```

### Fuzzing

```rust
#[test]
fn fuzz_parser() {
    arbtest::arbtest(|u| {
        let input: String = u.arbitrary()?;
        // Parser should not panic
        let _ = parse(&input);
        Ok(())
    });
}
```

### Echidna Verification

```agda
-- Prove parser correctness
parse-preserves-semantics : ∀ (src : String) →
  let ast = parse src in
  let cst = parse-cst src in
  eval ast ≡ eval (lower cst)
```

## Related Pages

- [[Lexer Design]]
- [[Compiler Pipeline]]
- [[AST Node Reference]]
- [[Error Messages]]
