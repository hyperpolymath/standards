# Standard Library

The NextGen Languages share a common standard library design with language-specific implementations and age-appropriate APIs.

## Module Structure

```
std/
├── core/           # Fundamental types and operations
│   ├── types       # Basic type definitions
│   ├── ops         # Operators and traits
│   └── prelude     # Auto-imported items
│
├── collections/    # Data structures
│   ├── list        # Dynamic arrays
│   ├── map         # Hash maps
│   ├── set         # Hash sets
│   ├── queue       # FIFO queues
│   └── stack       # LIFO stacks
│
├── text/           # String handling
│   ├── string      # String operations
│   ├── regex       # Regular expressions
│   └── format      # Formatting utilities
│
├── io/             # Input/output
│   ├── console     # Terminal I/O
│   ├── file        # File operations
│   └── stream      # Stream abstractions
│
├── math/           # Mathematics
│   ├── basic       # Arithmetic operations
│   ├── trig        # Trigonometric functions
│   ├── stats       # Statistical functions
│   └── random      # Random number generation
│
├── time/           # Date and time
│   ├── instant     # Points in time
│   ├── duration    # Time spans
│   └── format      # Date formatting
│
├── net/            # Networking
│   ├── http        # HTTP client/server
│   ├── tcp         # TCP sockets
│   └── url         # URL parsing
│
├── json/           # JSON handling
│   ├── parse       # JSON parsing
│   └── serialize   # JSON generation
│
├── testing/        # Test utilities
│   ├── assert      # Assertions
│   ├── mock        # Mocking
│   └── property    # Property-based testing
│
└── debug/          # Debugging
    ├── print       # Debug printing
    ├── trace       # Stack traces
    └── profile     # Performance profiling
```

## Age-Appropriate APIs

### Me (Ages 8-12)

Simple, visual, safe:

```
-- Available modules
say        -- Output text
ask        -- Get input
remember   -- Store values
turtle     -- Draw pictures
music      -- Make sounds
games      -- Simple games

-- Examples
say "Hello, world!"

remember name = ask "What is your name?"
say "Nice to meet you, " plus name

turtle.forward 100
turtle.right 90
turtle.forward 100
```

### Solo (Ages 13-14)

Text-based, structured:

```solo
// Available modules
use std.io         // Input/output
use std.math       // Math functions
use std.text       // String operations
use std.list       // Lists/arrays
use std.games      // Game helpers

// Examples
let name = io.ask("Enter your name: ")
io.say("Hello, " + name + "!")

let numbers = [1, 2, 3, 4, 5]
let doubled = list.map(numbers, fn(x) { x * 2 })
io.say(doubled)  // [2, 4, 6, 8, 10]

let random = math.random(1, 100)
io.say("Random number: " + random)
```

### Duet (Ages 15)

Full standard library with AI assistance:

```duet
use std/io
use std/collections/map
use std/net/http
use std/json

// AI-assisted usage
@synth: fetch JSON from URL and parse it
fn fetch_json(url: String) -> Result<Json, Error> {
    let response = http.get(url)?
    json.parse(response.body)
}

// Smart completions understand context
let users = Map::new()
users.insert("alice", User { name: "Alice", age: 30 })
// Duet suggests: users.get("alice"), users.remove("alice"), etc.
```

### Ensemble (Ages 16+)

Complete professional library:

```ensemble
use std::collections::{HashMap, BTreeSet}
use std::io::{Read, Write, BufReader}
use std::net::http::{Client, Request, Response}
use std::sync::{Arc, Mutex, Channel}
use std::async::{spawn, join}

// Full async support
async fn fetch_all(urls: Vec<String>) -> Vec<Result<String, Error>> {
    let client = Client::new();
    let futures = urls.into_iter()
        .map(|url| async move {
            client.get(&url).await?.text().await
        });
    join_all(futures).await
}

// Generics and traits
fn process<T: Serialize + Debug>(items: impl Iterator<Item = T>) -> String {
    items
        .map(|item| format!("{:?}", item))
        .collect::<Vec<_>>()
        .join(", ")
}
```

## Core Modules

### std/core/types

```rust
// Primitive types (available in all languages)
type Int     // 64-bit signed integer
type Float   // 64-bit floating point
type Bool    // true/false
type String  // UTF-8 string
type Char    // Unicode character
type Unit    // () / void / nil

// Option type (Duet+)
enum Option<T> {
    Some(T),
    None,
}

// Result type (Duet+)
enum Result<T, E> {
    Ok(T),
    Err(E),
}

// List type
type List<T> = [T]

// Map type
type Map<K, V>
```

### std/collections/list

```rust
// List operations
fn new<T>() -> List<T>
fn from_items<T>(items: ...T) -> List<T>
fn len<T>(list: List<T>) -> Int
fn is_empty<T>(list: List<T>) -> Bool

fn push<T>(list: &mut List<T>, item: T)
fn pop<T>(list: &mut List<T>) -> Option<T>
fn get<T>(list: List<T>, index: Int) -> Option<T>
fn set<T>(list: &mut List<T>, index: Int, value: T)

fn map<T, U>(list: List<T>, f: fn(T) -> U) -> List<U>
fn filter<T>(list: List<T>, f: fn(T) -> Bool) -> List<T>
fn fold<T, U>(list: List<T>, init: U, f: fn(U, T) -> U) -> U
fn find<T>(list: List<T>, f: fn(T) -> Bool) -> Option<T>

fn sort<T: Ord>(list: &mut List<T>)
fn reverse<T>(list: &mut List<T>)
fn concat<T>(a: List<T>, b: List<T>) -> List<T>
```

### std/io

```rust
// Console I/O
fn print(message: String)
fn println(message: String)
fn read_line() -> String
fn read_int() -> Result<Int, ParseError>

// File I/O (Duet+)
fn read_file(path: String) -> Result<String, IoError>
fn write_file(path: String, content: String) -> Result<(), IoError>
fn append_file(path: String, content: String) -> Result<(), IoError>
fn file_exists(path: String) -> Bool
fn delete_file(path: String) -> Result<(), IoError>

// Stream I/O (Ensemble)
trait Read {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, IoError>
}

trait Write {
    fn write(&mut self, buf: &[u8]) -> Result<usize, IoError>
    fn flush(&mut self) -> Result<(), IoError>
}
```

### std/math

```rust
// Basic operations
fn abs(n: Int) -> Int
fn abs(n: Float) -> Float
fn min<T: Ord>(a: T, b: T) -> T
fn max<T: Ord>(a: T, b: T) -> T
fn clamp<T: Ord>(value: T, min: T, max: T) -> T

// Power and roots
fn pow(base: Float, exp: Float) -> Float
fn sqrt(n: Float) -> Float
fn cbrt(n: Float) -> Float

// Trigonometry (Duet+)
fn sin(x: Float) -> Float
fn cos(x: Float) -> Float
fn tan(x: Float) -> Float
fn asin(x: Float) -> Float
fn acos(x: Float) -> Float
fn atan(x: Float) -> Float
fn atan2(y: Float, x: Float) -> Float

// Constants
const PI: Float = 3.14159265358979323846
const E: Float = 2.71828182845904523536
const TAU: Float = 6.28318530717958647692

// Random numbers
fn random() -> Float                    // 0.0 to 1.0
fn random_int(min: Int, max: Int) -> Int
fn random_choice<T>(list: List<T>) -> T
fn shuffle<T>(list: &mut List<T>)
```

### std/text/string

```rust
// Basic operations
fn len(s: String) -> Int
fn is_empty(s: String) -> Bool
fn concat(a: String, b: String) -> String

// Case conversion
fn to_upper(s: String) -> String
fn to_lower(s: String) -> String
fn capitalize(s: String) -> String

// Search
fn contains(s: String, pattern: String) -> Bool
fn starts_with(s: String, prefix: String) -> Bool
fn ends_with(s: String, suffix: String) -> Bool
fn index_of(s: String, pattern: String) -> Option<Int>

// Manipulation
fn trim(s: String) -> String
fn split(s: String, delimiter: String) -> List<String>
fn join(parts: List<String>, delimiter: String) -> String
fn replace(s: String, from: String, to: String) -> String
fn substring(s: String, start: Int, end: Int) -> String

// Parsing
fn parse_int(s: String) -> Result<Int, ParseError>
fn parse_float(s: String) -> Result<Float, ParseError>
```

## Language-Specific Extensions

### betlang: Probabilistic

```racket
;; Probability distributions
(uniform-dist min max)
(normal-dist mean stddev)
(bernoulli-dist p)
(binomial-dist n p)

;; Sampling
(sample dist)
(sample-n dist n)

;; Inference
(importance-sample model evidence n)
(mcmc-sample model n)
(abc-sample model distance threshold n)

;; Statistics
(mean samples)
(variance samples)
(std-dev samples)
(percentile samples p)
```

### julia-the-viper: Security

```
// Control channel functions
fn sanitize(input: DATA) -> DATA
fn validate(input: DATA, schema: Schema) -> Result<DATA, ValidationError>
fn escape_html(s: DATA) -> DATA
fn escape_sql(s: DATA) -> DATA

// Data channel operations (all provably halt)
fn transform(data: DATA, mapping: Mapping) -> DATA
fn filter_fields(data: DATA, fields: List<String>) -> DATA
fn merge(a: DATA, b: DATA) -> DATA
```

### Phronesis: Ethics

```elixir
# Ethical evaluation
defmodule Ethics do
  def evaluate(action, context) do
    # Returns ethical assessment
  end

  def align(action, values) do
    # Check alignment with value system
  end

  def audit_trail(agent, action) do
    # Record decision for accountability
  end
end

# Agent values
defmodule Values do
  def define(value_name, weight, constraints)
  def prioritize(values, context)
end
```

### Eclexia: Resource-Aware

```rust
// Resource types
type Energy = Joules
type Time = Milliseconds
type Memory = Bytes
type Carbon = GramsCO2

// Resource constraints
#[resource(energy < 100J, time < 500ms)]
fn process(data: Data) -> Result

// Adaptive execution
adaptive fn compute(x: Float) -> Float
    @strategy(fast) { x * x }
    @strategy(precise) { precise_square(x) }
    @strategy(efficient) { lookup_table(x) }
```

### AffineScript: Linear Types

```ocaml
(* Linear type constructors *)
type Linear 'a    (* must be used exactly once *)
type Affine 'a    (* must be used at most once *)
type Unrestricted 'a  (* can be used freely *)

(* Resource management *)
val open_file : String -> Linear (Handle File)
val read : Linear (Handle File) -> (String, Linear (Handle File))
val close : Linear (Handle File) -> Unit

(* Effect types *)
type Effect 'e 'a  (* computation with effect 'e returning 'a *)
val perform : 'e -> Effect 'e Unit
val handle : Effect 'e 'a -> ('e -> 'a) -> 'a
```

### Ephapax: Ephemeral Values

```scheme
;; Ephemeral constructors
(once value)           ; Create once-only value
(ephemeral expr)       ; Create ephemeral computation

;; Linear operations
(use x)                ; Consume ephemeral value
(transfer x to-env)    ; Move ownership

;; Session types
(session
  (send type)          ; Send message of type
  (recv type)          ; Receive message of type
  (choice (label1 ...) (label2 ...))
  (end))
```

## Package Management

### Adding Dependencies

```toml
# nextgen.toml
[dependencies]
std = "1.0"
http = "2.1"
json = "1.5"

[dev-dependencies]
testing = "1.0"
benchmark = "0.9"
```

### Importing

```ensemble
// Import entire module
use std::collections

// Import specific items
use std::io::{read_file, write_file}

// Import with alias
use std::net::http as web

// Conditional imports (Ensemble)
#[cfg(target = "wasm")]
use std::wasm::memory
```

## Related Pages

- [[Package Manager]]
- [[Web Framework]]
- [[GUI Framework]]
- [[Game Framework]]
