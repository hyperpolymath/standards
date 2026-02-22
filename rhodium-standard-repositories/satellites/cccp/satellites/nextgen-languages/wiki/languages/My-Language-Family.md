# My-Language Family

The My-Language family is a progressive series of programming languages designed to grow with learners from age 8 through age 18.

## Philosophy

> "Meet learners where they are, then guide them forward."

The family follows a **progressive disclosure** model:
- Each language builds on concepts from the previous
- Complexity increases gradually
- Skills transfer seamlessly between levels
- No "unlearning" required when advancing

## The Languages

```
Me (ages 6-8)
 │  Visual, block-based, purely exploratory
 │
 ▼
Solo (ages 8-10)
 │  First text-based, simple syntax, immediate feedback
 │
 ▼
Duet (ages 11-14)
 │  Collaborative features, expanded capabilities
 │
 ▼
Ensemble (ages 15-18)
 │  Full-featured, industry-ready concepts
 │
 ▼
[Professional languages: Rust, Python, etc.]
```

## Shared Characteristics

All languages in the family share:

### Syntax Principles
- **Readable keywords** (not cryptic symbols)
- **Consistent structure** across complexity levels
- **Minimal boilerplate** for beginners
- **Gradual typing** (optional types that become more prominent)

### Error Handling
- **Friendly error messages** with suggestions
- **Age-appropriate explanations**
- **Visual error highlighting**
- **"Did you mean..." suggestions**

### Tooling
- **Unified REPL** experience
- **Shared LSP** foundation
- **Cross-compatible packages** (where appropriate)
- **Consistent CLI** interface

## Progression Path

### Concepts Introduced by Level

| Concept | Me | Solo | Duet | Ensemble |
|---------|----|----- |------|----------|
| Variables | Visual boxes | `let x = 5` | `let x = 5` | `let x: Int = 5` |
| Output | Drag visual | `say "Hi"` | `print("Hi")` | `println("Hi")` |
| Loops | Repeat block | `repeat 5 times` | `for i in 1..5` | `for i in 1..=5` |
| Functions | Action blocks | `fun greet()` | `fn greet()` | `fn greet() -> ()` |
| Types | None | Implicit | Gradual | Static |
| Classes | None | None | Simple | Full OOP |
| Generics | None | None | None | Yes |
| Async | None | None | Basic | Full |

### Skill Mapping

```
Solo Student Knows:        Can Do in Duet:
─────────────────────      ──────────────────────
Variables                  → Same, plus scope
Simple functions           → Functions with params
repeat loops               → for/while loops
Lists                      → Lists + dictionaries
─────────────────────      ──────────────────────

Duet Student Knows:        Can Do in Ensemble:
─────────────────────      ──────────────────────
Basic types                → Full type system
Simple classes             → Inheritance, traits
Basic async                → Futures, streams
Module imports             → Package management
─────────────────────      ──────────────────────
```

## Language Details

### [[Me Language]]
- **Target age**: 6-8
- **Paradigm**: Visual/block-based
- **Key feature**: No typing required, drag-and-drop
- **Environment**: Web playground only

### [[Solo Language]]
- **Target age**: 8-10
- **Paradigm**: Imperative, procedural
- **Key feature**: Simple syntax, immediate visual feedback
- **Environment**: Web playground, desktop REPL

### [[Duet Language]]
- **Target age**: 11-14
- **Paradigm**: Multi-paradigm (procedural + some OOP)
- **Key feature**: Collaborative coding, pair programming support
- **Environment**: Full IDE support, web, desktop

### [[Ensemble Language]]
- **Target age**: 15-18
- **Paradigm**: Full multi-paradigm
- **Key feature**: Industry-ready concepts, full type system
- **Environment**: Full toolchain, production deployable

## Related Repositories

| Repository | Purpose |
|------------|---------|
| [me](https://github.com/hyperpolymath/me) | Me language implementation |
| [solo](https://github.com/hyperpolymath/solo) | Solo language implementation |
| [duet](https://github.com/hyperpolymath/duet) | Duet language implementation |
| [ensemble](https://github.com/hyperpolymath/ensemble) | Ensemble language implementation |
| [me-dialect-playground](https://github.com/hyperpolymath/me-dialect-playground) | Experimental dialect features |
| [my-newsroom](https://github.com/hyperpolymath/my-newsroom) | Content pipeline |
| [my-ssg](https://github.com/hyperpolymath/my-ssg) | Static site generator |
| [7-tentacles](https://github.com/hyperpolymath/7-tentacles) | Orchestration |

## Migration Between Languages

### Solo → Duet

Most Solo code works in Duet with minor adjustments:

```solo
// Solo
let name = "World"
say "Hello, " + name

repeat 3 times
  say "Hi!"
end
```

```duet
// Duet (equivalent)
let name = "World"
print("Hello, " + name)

for _ in 1..3 {
  print("Hi!")
}
```

### Duet → Ensemble

```duet
// Duet
fn greet(name) {
  print("Hello, " + name)
}

class Dog {
  let name

  fn bark() {
    print(self.name + " says woof!")
  }
}
```

```ensemble
// Ensemble (equivalent)
fn greet(name: String) -> () {
  println("Hello, {name}")
}

struct Dog {
  name: String,
}

impl Dog {
  fn bark(&self) -> () {
    println("{} says woof!", self.name)
  }
}
```

## Educational Resources

- [[Tutorial: Starting with Solo]]
- [[Tutorial: Moving to Duet]]
- [[Tutorial: Mastering Ensemble]]
- [[Curriculum Guide]]
- [[Teacher Resources]]
