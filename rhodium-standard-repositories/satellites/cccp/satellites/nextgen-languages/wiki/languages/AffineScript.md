# AffineScript

**Tagline:** Affine types, dependent types, and effects for WebAssembly

## Overview

AffineScript combines multiple advanced type system features into a cohesive language targeting WebAssembly:

- **Affine Types**: Values can be used at most once
- **Dependent Types**: Types can depend on runtime values
- **Row Polymorphism**: Flexible record and variant handling
- **Effect System**: Explicit side effect tracking
- **WebAssembly**: Broad deployment target

## Philosophy

AffineScript explores how advanced type theory can be made practical. By combining affine types (resource safety without GC), dependent types (precise specifications), and effect tracking (purity guarantees), it enables verified software components that run anywhere WebAssembly runs.

## Quick Start

```ocaml
(* Hello World *)
let main : Effect[IO] Unit =
  print "Hello, AffineScript!"

(* Linear file handling - no leaks possible *)
let process_file (path : String) : Effect[IO] String =
  let handle = File.open path in    (* handle : Linear (Handle File) *)
  let contents = File.read handle in (* handle consumed *)
  contents                           (* handle automatically closed *)

(* Dependent types for safe indexing *)
let safe_index : forall (n : Nat). Vector n Int -> Fin n -> Int =
  fun vec idx -> Vector.get vec idx  (* bounds check at compile time *)

(* Row polymorphism for extensible records *)
let get_name : forall r. { name : String | r } -> String =
  fun record -> record.name          (* works with any record having 'name' *)
```

## Type System

### Affine Types

Variables with affine types can be used at most once:

```ocaml
(* Linear: must use exactly once *)
type Linear a

(* Affine: use at most once (can drop) *)
type Affine a

(* Unrestricted: use any number of times *)
type Unrestricted a

(* Example: file handles are linear *)
let handle : Linear (Handle File) = File.open "data.txt"
let data = File.read handle   (* handle consumed here *)
(* File.read handle *)        (* ERROR: handle already consumed *)

(* Explicit drop for affine values *)
let unused : Affine Connection = connect server
drop unused  (* explicit cleanup *)
```

### Dependent Types

Types that depend on values:

```ocaml
(* Length-indexed vectors *)
type Vector (n : Nat) a

let empty : Vector 0 a
let cons : forall n. a -> Vector n a -> Vector (n + 1) a
let append : forall m n. Vector m a -> Vector n a -> Vector (m + n) a

(* Finite numbers (bounded naturals) *)
type Fin (n : Nat)  (* 0, 1, ..., n-1 *)

(* Safe indexing - no runtime bounds check needed *)
let index : forall n a. Vector n a -> Fin n -> a

(* Example usage *)
let vec : Vector 5 Int = [1, 2, 3, 4, 5]
let third : Int = index vec 2  (* 2 : Fin 5, statically valid *)
(* let sixth = index vec 5 *)  (* ERROR: 5 is not < 5 *)
```

### Row Polymorphism

Work with records having at least certain fields:

```ocaml
(* Function works with any record having 'x' and 'y' *)
let distance : forall r. { x : Float, y : Float | r } -> Float =
  fun point -> sqrt (point.x * point.x + point.y * point.y)

(* Can call with any compatible record *)
let p2d = { x = 3.0, y = 4.0 }
let p3d = { x = 1.0, y = 2.0, z = 3.0 }
let named = { x = 0.0, y = 1.0, name = "origin" }

distance p2d    (* 5.0 *)
distance p3d    (* works! ignores z *)
distance named  (* works! ignores name *)

(* Extending records *)
let add_z : forall r. { x : Float, y : Float | r } -> Float -> { x : Float, y : Float, z : Float | r } =
  fun record z -> { ...record, z = z }
```

### Effect System

Track and control side effects:

```ocaml
(* Effect types *)
type Effect e a  (* computation with effects 'e' returning 'a' *)

(* Common effects *)
effect IO         (* input/output *)
effect State s    (* mutable state of type s *)
effect Exception e (* can throw exception of type e *)
effect Async      (* asynchronous operations *)

(* Pure functions have no effects *)
let pure_add : Int -> Int -> Int =
  fun x y -> x + y

(* Effectful functions declare their effects *)
let read_number : Effect[IO] Int =
  let line = read_line () in
  parse_int line

(* Combine effects *)
let stateful_io : Effect[IO, State Int] Unit =
  let n = read_number () in
  put n

(* Handle effects *)
let run_state : forall s a. s -> Effect[State s] a -> (a, s) =
  fun init computation ->
    handle computation with
    | get () k -> k init
    | put s k -> let (r, _) = k () in (r, s)
```

## Syntax

### Declarations

```ocaml
(* Value binding *)
let name : Type = expr

(* Function *)
let func (arg1 : Type1) (arg2 : Type2) : ReturnType =
  body

(* Type alias *)
type Name = ExistingType

(* Algebraic data type *)
type Option a =
  | None
  | Some a

type List a =
  | Nil
  | Cons a (List a)

(* Record type *)
type Person = {
  name : String,
  age : Int,
  email : Linear String,  (* linear field *)
}

(* Effect declaration *)
effect MyEffect =
  | Operation1 : Int -> String
  | Operation2 : Unit -> Unit
```

### Expressions

```ocaml
(* Lambda *)
fun x -> x + 1
fun (x : Int) (y : Int) -> x + y

(* Application *)
f x y
f (g x)

(* Let binding *)
let x = 5 in x + 1
let f x = x * 2 in f 10

(* Pattern matching *)
match option with
| None -> "nothing"
| Some x -> "got " ++ show x

(* Records *)
{ name = "Alice", age = 30 }
record.field
{ ...record, field = new_value }

(* Conditionals *)
if condition then expr1 else expr2

(* Effects *)
perform Operation1 42
handle expr with
| Operation1 n k -> k (show n)
| Operation2 () k -> k ()
```

## WebAssembly Compilation

AffineScript compiles to efficient WebAssembly:

```ocaml
(* Source *)
let factorial (n : Nat) : Nat =
  match n with
  | 0 -> 1
  | n -> n * factorial (n - 1)

(* Compiles to WASM *)
(func $factorial (param $n i64) (result i64)
  (if (result i64) (i64.eqz (local.get $n))
    (then (i64.const 1))
    (else
      (i64.mul
        (local.get $n)
        (call $factorial (i64.sub (local.get $n) (i64.const 1)))))))
```

### Linear Types → Zero-Cost Resource Management

```ocaml
(* Source with linear types *)
let with_file (path : String) (f : Linear (Handle File) -> a) : Effect[IO] a =
  let handle = File.open path in
  let result = f handle in  (* handle must be consumed by f *)
  result

(* Compiles to WASM without GC overhead *)
(* Resources are deterministically freed at scope end *)
```

### WASM Targets

```bash
# Compile to standalone WASM
affine build --target wasm32-unknown-unknown src/main.af

# Compile for browser
affine build --target wasm32-web src/main.af

# Compile for WASI (server-side)
affine build --target wasm32-wasi src/main.af
```

## Echidna Integration

### Type System Soundness

```agda
-- Prove progress and preservation
progress : ∀ {Γ e τ} → Γ ⊢ e : τ → Value e ⊎ ∃[ e' ] (e ⟶ e')
preservation : ∀ {Γ e e' τ} → Γ ⊢ e : τ → e ⟶ e' → Γ ⊢ e' : τ

-- Prove linearity safety
linearity-sound : ∀ {Γ e τ} →
  Γ ⊢ e : Linear τ →
  UsedOnce e
```

### Property Verification

```ocaml
(* Specify properties *)
@property("vectors preserve length")
let concat_length : forall m n a.
  (v1 : Vector m a) -> (v2 : Vector n a) ->
  length (append v1 v2) = m + n

(* Echidna generates proof obligations and tests *)
@verify
let test_concat =
  let v1 = [1, 2, 3] : Vector 3 Int in
  let v2 = [4, 5] : Vector 2 Int in
  assert (length (append v1 v2) = 5)
```

## Standard Library Highlights

```ocaml
(* Linear I/O *)
module Linear.IO where
  type Handle a  (* Linear file handle *)

  val open : String -> Effect[IO] (Linear (Handle File))
  val read : Linear (Handle File) -> Effect[IO] (String, Linear (Handle File))
  val write : Linear (Handle File) -> String -> Effect[IO] (Linear (Handle File))
  val close : Linear (Handle File) -> Effect[IO] Unit

(* Dependent vectors *)
module Data.Vector where
  type Vector (n : Nat) a

  val empty : Vector 0 a
  val singleton : a -> Vector 1 a
  val cons : a -> Vector n a -> Vector (n + 1) a
  val head : Vector (n + 1) a -> a
  val tail : Vector (n + 1) a -> Vector n a
  val index : Vector n a -> Fin n -> a
  val map : (a -> b) -> Vector n a -> Vector n b

(* Effect handlers *)
module Control.Effect where
  effect State s where
    get : Unit -> s
    put : s -> Unit

  val runState : s -> Effect[State s] a -> (a, s)
  val evalState : s -> Effect[State s] a -> a
  val execState : s -> Effect[State s] a -> s
```

## Example: Type-Safe Database

```ocaml
(* Schema as types *)
type Schema = [Column]
type Column = { name : String, ty : Type }

type Users = [
  { name = "id", ty = Int },
  { name = "name", ty = String },
  { name = "email", ty = String },
]

(* Type-safe queries *)
let select : forall schema cols.
  Subset cols schema ->
  Table schema ->
  Effect[DB] (List (Record cols))

let insert : forall schema.
  Record schema ->
  Table schema ->
  Effect[DB] Unit

(* Usage *)
let get_user_names : Effect[DB] (List { name : String }) =
  select ["name"] users_table

let add_user : Effect[DB] Unit =
  insert { id = 1, name = "Alice", email = "alice@example.com" } users_table
```

## Related Pages

- [[Type System Verification]]
- [[WebAssembly Backend]]
- [[Effect System]]
- [[Linear Types]]
