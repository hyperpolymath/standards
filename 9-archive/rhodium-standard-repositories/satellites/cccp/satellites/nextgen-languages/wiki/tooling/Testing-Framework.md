# Testing Framework

The NextGen Languages ecosystem uses a multi-layered testing approach, integrated with the Echidna neurosymbolic theorem proving platform for formal verification.

## Testing Layers

```
┌─────────────────────────────────────────────────────────────┐
│                    Verification Tests                        │
│            (Formal proofs via Echidna + 12 provers)         │
├─────────────────────────────────────────────────────────────┤
│                    Integration Tests                         │
│             (Cross-module, session types)                    │
├─────────────────────────────────────────────────────────────┤
│                    Property Tests                            │
│          (Property-based, fuzzing, invariants)              │
├─────────────────────────────────────────────────────────────┤
│                      Unit Tests                              │
│            (Function-level, type-driven)                     │
└─────────────────────────────────────────────────────────────┘
```

## Echidna Integration

[Echidna](https://github.com/hyperpolymath/echidna) is a neurosymbolic theorem proving platform that integrates with 12 theorem provers:

| Prover | Strength | Use Case |
|--------|----------|----------|
| Agda | Dependent types | Type system soundness |
| Coq | Inductive proofs | Compiler correctness |
| Lean | Modern DTT | Library verification |
| Isabelle | Large theories | Semantics formalization |
| Z3 | SMT solving | Property checking |
| CVC5 | SMT solving | Constraint verification |
| Metamath | Minimal logic | Foundation proofs |
| HOL Light | Lightweight | Quick verification |
| Mizar | Set theory | Mathematical libraries |
| PVS | Automation | Safety-critical |
| ACL2 | Termination | Loop proofs |
| HOL4 | Scripting | Custom tactics |

### Echidna Workflow

```
Source Code
     │
     ▼
┌─────────────┐
│   Parser    │  Extract specifications
└──────┬──────┘
       │
       ▼
┌─────────────┐
│  Echidna    │  Neural proof synthesis
│   Core      │
└──────┬──────┘
       │
   ┌───┴───┐
   ▼       ▼
┌─────┐ ┌─────┐
│Agda │ │ Z3  │  Symbolic verification
│Coq  │ │CVC5 │
│...  │ │...  │
└──┬──┘ └──┬──┘
   │       │
   └───┬───┘
       ▼
┌─────────────┐
│   Tests     │  Generated test cases
│  + Proofs   │  + Verification certificates
└─────────────┘
```

## Test Generation

### Theory Tests (from specifications)

Echidna generates tests directly from formal specifications:

```scheme
;; Specification
(define-property vector-length
  "Length of concatenated vectors equals sum of lengths"
  (forall ((v1 Vector) (v2 Vector))
    (= (length (concat v1 v2))
       (+ (length v1) (length v2)))))

;; Generated tests
(test "empty-empty" (concat [] []) => [])
(test "single-empty" (concat [1] []) => [1])
(test "empty-single" (concat [] [1]) => [1])
(test "multi-multi" (concat [1,2] [3,4]) => [1,2,3,4])
;; ... plus edge cases from SMT counterexamples
```

### Practice Tests (from implementation)

EchidnaBot analyzes code to generate implementation tests:

```python
# Implementation
def binary_search(arr, target):
    left, right = 0, len(arr) - 1
    while left <= right:
        mid = (left + right) // 2
        if arr[mid] == target:
            return mid
        elif arr[mid] < target:
            left = mid + 1
        else:
            right = mid - 1
    return -1

# Generated tests cover:
# - Empty array
# - Single element (found/not found)
# - Target at beginning/middle/end
# - Target not in array
# - Duplicate elements
# - Integer overflow in (left + right)
```

## Per-Language Requirements

| Language | Unit | Property | Integration | Verification |
|----------|------|----------|-------------|--------------|
| Me | ✓ | ○ | - | - |
| Solo | ✓ | ✓ | ○ | - |
| Duet | ✓ | ✓ | ✓ | ○ |
| Ensemble | ✓ | ✓ | ✓ | ✓ |
| betlang | ✓ | ✓* | - | - |
| julia-the-viper | ✓ | ✓ | - | ✓ |
| Phronesis | ✓ | ✓ | - | ✓ |
| Eclexia | ✓ | ✓ | - | - |
| Oblíbený | ✓ | - | - | ✓ |
| Anvomidav | ✓ | ✓ | ✓ | ✓ |
| WokeLang | ✓ | - | - | - |
| AffineScript | ✓ | ✓ | - | ✓ |
| Ephapax | ✓ | ✓ | - | ✓ |

✓ = Required, ○ = Optional, * = Statistical testing

## Special Testing Requirements

### betlang: Probabilistic Distribution Testing

```racket
;; Test that distributions match expected properties
(test-distribution
  (ternary-bet 'A 'B 'C)
  #:samples 10000
  #:expected '((A . 0.333) (B . 0.333) (C . 0.333))
  #:tolerance 0.05)
```

### julia-the-viper: Security Property Testing

```
// Test that data channel cannot execute
@verify(no-execution)
test "data-cannot-execute" {
    input = user_data("alert('xss')")
    // Must prove grammatically impossible
    assert !can_execute(input)
}
```

### Anvomidav: Timing Verification

```
// Test real-time constraints
@timing(wcet: 10ms, deadline: 15ms)
test "motor-control-deadline" {
    command = MotorCommand::new()
    response = controller.process(command)
    // Must complete within deadline
    assert elapsed() < 15ms
}
```

### AffineScript: Linearity Checking

```ocaml
(* Test that linear values are used exactly once *)
let%test "file-handle-linear" =
  let handle = File.open "test.txt" in
  (* handle must be used exactly once *)
  File.close handle
  (* using handle again should fail *)
  (* File.read handle -- compile error! *)
```

## Running Tests

### CLI Commands

```bash
# Run all tests
nextgen test

# Run specific test level
nextgen test --unit
nextgen test --property
nextgen test --integration
nextgen test --verify

# Run with Echidna verification
nextgen test --echidna

# Generate tests from specifications
echidnabot generate --spec language.spec --output tests/

# Run tests for specific language
nextgen test --lang solo

# Coverage report
nextgen test --coverage

# Mutation testing
nextgen test --mutation
```

### CI Integration

```yaml
# .github/workflows/test.yml
name: Test Suite
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: nextgen-languages/setup@v1

      - name: Unit Tests
        run: nextgen test --unit

      - name: Property Tests
        run: nextgen test --property

      - name: Echidna Verification
        run: nextgen test --echidna --timeout 600

      - name: Coverage
        run: nextgen test --coverage --min 80
```

## Writing Tests

### Unit Test Example

```rust
// Solo language
#[test]
fn test_add() {
    assert_eq!(add(2, 3), 5);
    assert_eq!(add(-1, 1), 0);
    assert_eq!(add(0, 0), 0);
}
```

### Property Test Example

```rust
// Using nextgen-prop
#[property]
fn prop_add_commutative(a: i32, b: i32) -> bool {
    add(a, b) == add(b, a)
}

#[property]
fn prop_add_associative(a: i32, b: i32, c: i32) -> bool {
    add(add(a, b), c) == add(a, add(b, c))
}
```

### Verification Test Example

```agda
-- Agda proof for type system soundness
progress : ∀ {Γ e τ} → Γ ⊢ e ∶ τ → Value e ⊎ ∃[ e' ] (e ⟶ e')
progress (T-Var x) = inj₁ V-Var  -- variables are values
progress (T-Abs e) = inj₁ V-Abs  -- abstractions are values
progress (T-App e₁ e₂) with progress e₁
... | inj₁ V-Abs = inj₂ (_ , E-AppAbs)
... | inj₂ (e₁' , step) = inj₂ (_ , E-App1 step)
```

## Best Practices

1. **Test at appropriate level** - Not everything needs formal verification
2. **Use Echidna for critical paths** - Type checkers, compilers, security code
3. **Generate tests, don't just write them** - Let EchidnaBot find edge cases
4. **Maintain proof obligations** - Update proofs when specifications change
5. **Progressive testing** - Match test rigor to language complexity level

## Related Pages

- [[Echidna Integration]]
- [[Compiler Pipeline]]
- [[Continuous Integration]]
- [[Code Coverage]]
