# REPL Guide

The Read-Eval-Print Loop (REPL) provides interactive execution for rapid experimentation and learning.

## Overview

```
┌─────────────────────────────────────────────────────────┐
│                     REPL Session                         │
├─────────────────────────────────────────────────────────┤
│  >>> let x = 42                                         │
│  x = 42                                                 │
│                                                         │
│  >>> x + 8                                              │
│  50                                                     │
│                                                         │
│  >>> fn double(n) { n * 2 }                            │
│  <function double>                                      │
│                                                         │
│  >>> double(x)                                          │
│  84                                                     │
│                                                         │
│  >>> :help                                              │
│  Commands:                                              │
│    :help     Show this help                             │
│    :type <e> Show type of expression                    │
│    :quit     Exit REPL                                  │
│    :load <f> Load file                                  │
│    :reset    Reset environment                          │
└─────────────────────────────────────────────────────────┘
```

## Architecture

```
┌─────────────────────────────────────────┐
│              REPL Shell                 │
├─────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────────┐  │
│  │   Input     │  │    Output       │  │
│  │   Handler   │  │    Formatter    │  │
│  └──────┬──────┘  └────────▲────────┘  │
│         │                  │           │
│         ▼                  │           │
│  ┌─────────────────────────┴────────┐  │
│  │         Evaluation Engine        │  │
│  ├──────────────────────────────────┤  │
│  │  ┌──────┐ ┌──────┐ ┌──────────┐ │  │
│  │  │Lexer │→│Parser│→│Evaluator │ │  │
│  │  └──────┘ └──────┘ └──────────┘ │  │
│  └──────────────────────────────────┘  │
│                                         │
│  ┌─────────────────────────────────┐   │
│  │        Environment State        │   │
│  │  variables, functions, modules  │   │
│  └─────────────────────────────────┘   │
└─────────────────────────────────────────┘
```

## Per-Language REPLs

### Me (Ages 8-12)

Visual, supportive REPL:

```
🌟 Welcome to Me!

me> say "Hello!"
Hello!

me> let favorite = "pizza"
🎉 I'll remember that favorite = "pizza"

me> say "I like " plus favorite
I like pizza

me> oops forgot a quote
   ↑
💡 Looks like you might have forgotten a quote mark!
   Try: say "I like " plus favorite
```

Features:
- Emoji feedback
- Suggestion-based error messages
- No scary error traces
- Visual output

### Solo (Ages 13-14)

Standard REPL with educational features:

```
solo> let count = 0
count: Int = 0

solo> repeat 3 times
....>   count = count + 1
....>   say count
....> end
1
2
3

solo> :type count
Int

solo> :explain count + 1
  count + 1
  ↑
  'count' is an Int with value 3
        ↑
        '+' adds two numbers
          ↑
          '1' is an Int literal
  Result: Int = 4
```

### Duet (Ages 15)

AI-assisted REPL:

```
duet> @synth: function to reverse a string
🤖 Synthesizing...

fn reverse(s: String) -> String {
    s.chars().rev().collect()
}

Shall I add this to your session? [y/n] y
✓ Added function 'reverse'

duet> reverse("hello")
"olleh"

duet> @verify: reverse(reverse(s)) == s
✓ Verified for all String inputs

duet> intent("calculate fibonacci")
🤖 I can help with that. Do you want:
   1. Recursive implementation
   2. Iterative implementation
   3. Memoized implementation
>
```

### Ensemble (Ages 16+)

Full-featured REPL:

```
ensemble> ai_model GPT4 {
........>     temperature: 0.7
........> }
<ai_model GPT4>

ensemble> prompt analyze {
........>     system: "You are a code reviewer.",
........>     user: "{code}"
........> }
<prompt analyze>

ensemble> let review = GPT4.run(analyze, code: "fn add(a, b) { a + b }")
<AI<String>>

ensemble> await review
"The function 'add' is simple and correct. Consider adding type annotations
for better documentation."

ensemble> :effects
Active effects: [IO, AI, State]
```

### betlang

Racket-style REPL:

```racket
betlang> (ternary-bet 'heads 'tails 'edge)
#<ternary-bet: heads=0.333 tails=0.333 edge=0.333>

betlang> (sample (ternary-bet 'a 'b 'c) 5)
'(a c b a b)

betlang> (expected-value (weighted-bet '((0.6 . 10) (0.4 . 20))))
14.0

betlang> (define my-coin (ternary-bet 'heads 'tails 'edge))
betlang> (run-simulation my-coin 10000)
Results: heads=3342 (33.4%) tails=3318 (33.2%) edge=3340 (33.4%)
```

### julia-the-viper

Dual-channel REPL:

```
jtv> CONTROL: let x = 5
[CONTROL] x: Int = 5

jtv> DATA: [name: "Alice", age: 30]
[DATA] Record { name: String, age: Int }

jtv> CONTROL: if x > 0 then output = "positive"
[CONTROL] output: String = "positive"

jtv> // Try to mix channels - should fail
jtv> CONTROL: let y = [name: "Bob"]
ERROR: Cannot use DATA expression in CONTROL context
       DATA expressions cannot contain executable code

jtv> :channels
CONTROL: Turing-complete, imperative
DATA:    Halting-guaranteed, declarative
```

### AffineScript

Type-aware REPL with linearity tracking:

```ocaml
affine> let x : Linear Int = 42
x : Linear Int = 42

affine> let y = x + 1  (* x is consumed *)
y : Int = 43

affine> x  (* error: x already consumed *)
Error: Linear variable 'x' has already been consumed
       at line 2: let y = x + 1
                          ^

affine> let file = File.open("test.txt")
file : Linear (Handle File) = <handle>

affine> let contents = File.read(file)  (* file consumed *)
contents : String = "..."

affine> File.close(file)  (* error *)
Error: 'file' was already consumed by File.read
       Linear resources must be used exactly once
```

### Ephapax

Once-only semantics REPL:

```scheme
ephapax> (once x 42)
x (once) = 42

ephapax> (use x)
42
x has been consumed

ephapax> (use x)
Error: 'x' was ephemeral and has already been used
       Ephemeral values can only be used once

ephapax> (ephemeral (lambda (y) (* y y)))
<ephemeral-function>

ephapax> (let ((sq (ephemeral (lambda (y) (* y y)))))
....       (sq 5))
25
```

## REPL Commands

### Universal Commands

| Command | Description |
|---------|-------------|
| `:help` | Show help |
| `:quit` / `:q` | Exit REPL |
| `:reset` | Clear environment |
| `:load <file>` | Load source file |
| `:reload` | Reload last file |
| `:type <expr>` | Show expression type |
| `:info <name>` | Show info about binding |

### Educational Commands (Me/Solo/Duet)

| Command | Description |
|---------|-------------|
| `:explain <expr>` | Step-by-step explanation |
| `:hint` | Get hint for current error |
| `:undo` | Undo last action |
| `:history` | Show session history |
| `:save` | Save session to file |

### Advanced Commands (Ensemble)

| Command | Description |
|---------|-------------|
| `:effects` | Show active effects |
| `:ast <expr>` | Show AST |
| `:ir <expr>` | Show IR |
| `:time <expr>` | Benchmark expression |
| `:profile` | Toggle profiling |
| `:debug` | Enter debugger |

### Echidna Commands

| Command | Description |
|---------|-------------|
| `:verify <prop>` | Verify property |
| `:prove <theorem>` | Attempt proof |
| `:check <expr>` | Type check with proofs |
| `:test <func>` | Generate tests for function |

## Implementation

### Basic REPL Loop

```rust
pub fn run_repl() -> Result<(), Error> {
    let mut env = Environment::new();
    let mut editor = Editor::new()?;

    println!("Welcome to NextGen REPL. Type :help for commands.");

    loop {
        let input = match editor.readline(">>> ") {
            Ok(line) => line,
            Err(ReadlineError::Interrupted) => continue,
            Err(ReadlineError::Eof) => break,
            Err(e) => return Err(e.into()),
        };

        editor.add_history_entry(&input);

        if input.starts_with(':') {
            handle_command(&input, &mut env)?;
            continue;
        }

        match eval_input(&input, &mut env) {
            Ok(value) => println!("{}", format_value(&value)),
            Err(e) => println!("{}", format_error(&e)),
        }
    }

    Ok(())
}
```

### Multi-Line Input

```rust
fn read_complete_input(editor: &mut Editor, prompt: &str) -> Result<String, Error> {
    let mut input = String::new();
    let mut continuation = false;

    loop {
        let line_prompt = if continuation { "....> " } else { prompt };
        let line = editor.readline(line_prompt)?;
        input.push_str(&line);
        input.push('\n');

        if is_complete(&input) {
            return Ok(input);
        }

        continuation = true;
    }
}

fn is_complete(input: &str) -> bool {
    // Check balanced brackets, complete statements, etc.
    let mut parser = Parser::new(input);
    match parser.parse() {
        Ok(_) => true,
        Err(ParseError::UnexpectedEof) => false,
        Err(_) => true, // Other errors mean input is "complete" (just wrong)
    }
}
```

### Tab Completion

```rust
impl Completer for ReplCompleter {
    fn complete(&self, line: &str, pos: usize) -> Vec<Completion> {
        let word = get_word_at(line, pos);
        let mut completions = vec![];

        // Complete from environment
        for name in self.env.names() {
            if name.starts_with(word) {
                completions.push(Completion {
                    replacement: name.clone(),
                    display: format!("{} : {}", name, self.env.type_of(name)),
                });
            }
        }

        // Complete keywords
        for kw in KEYWORDS {
            if kw.starts_with(word) {
                completions.push(Completion {
                    replacement: kw.to_string(),
                    display: format!("{} (keyword)", kw),
                });
            }
        }

        // Complete commands
        if line.starts_with(':') {
            for cmd in COMMANDS {
                if cmd.starts_with(word) {
                    completions.push(Completion {
                        replacement: cmd.to_string(),
                        display: format!("{}", cmd),
                    });
                }
            }
        }

        completions
    }
}
```

### Syntax Highlighting

```rust
impl Highlighter for ReplHighlighter {
    fn highlight(&self, line: &str) -> String {
        let tokens = Lexer::new(line).collect::<Vec<_>>();
        let mut result = String::new();

        for token in tokens {
            let styled = match token.kind {
                TokenKind::Keyword => token.text.blue().bold(),
                TokenKind::String => token.text.green(),
                TokenKind::Number => token.text.cyan(),
                TokenKind::Comment => token.text.bright_black(),
                TokenKind::Operator => token.text.yellow(),
                TokenKind::Error => token.text.red().underline(),
                _ => token.text.normal(),
            };
            result.push_str(&styled.to_string());
        }

        result
    }
}
```

## Running the REPL

```bash
# Start REPL for specific language
nextgen repl solo
nextgen repl ensemble
nextgen repl betlang

# Start with file loaded
nextgen repl solo --load examples/hello.solo

# Start with custom settings
nextgen repl ensemble --no-color --history-file ~/.ensemble_history

# Network REPL (for collaboration)
nextgen repl duet --serve 8080
nextgen repl duet --connect localhost:8080
```

## Notebook Integration

The REPL can export to and import from Jupyter-style notebooks:

```bash
# Export session to notebook
nextgen repl solo --export session.ipynb

# Run notebook in REPL mode
nextgen notebook run examples/tutorial.ipynb
```

## Related Pages

- [[Interpreter Implementation]]
- [[IDE Support]]
- [[Debugging]]
- [[Tutorial: Your First Program]]
