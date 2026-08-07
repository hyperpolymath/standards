# Zigzag Testing: Aspect-Oriented Analysis and Synthesis

## 1. Introduction
Zigzag Testing is an advanced testing methodology designed to perform aspect-oriented analysis and synthesis by charting a collection of meandering routes through a system's design.

Unlike traditional linear integration testing (which verifies a single end-to-end happy path) or pure property-based testing (which verifies invariants on a single component), Zigzag testing deliberately crosses horizontal and vertical boundaries. It validates that cross-cutting concerns (e.g., authentication, telemetry, state persistence, failure recovery) interact correctly under chaotic, stateful traversal.

## 2. Core Concepts

### 2.1 Aspect-Oriented Analysis and Synthesis
Systems are composed of overlapping "aspects" (security, networking, DB logic, domain logic). Zigzag testing analyzes these aspects individually and synthesizes tests that verify their intersections.
- **Analysis**: Decompose the system into orthogonal aspects.
- **Synthesis**: Recombine these aspects into meandering operational routes that cross boundaries.

### 2.2 Meandering Routes
A meandering route is a non-linear test execution path. Instead of `A -> B -> C`, a meandering route might trace `A -> trigger aspect X -> induce failure in B -> verify aspect Y -> C`.
- **State-Machine Traversal**: Tests are modeled as state machines where transitions represent API calls or events.
- **Random Walks**: Property-based testing engines execute random walks through the state machine.
- **Cross-Cutting Verification**: After each step, global invariants (like "no orphaned database connections" or "telemetry was emitted") are asserted.

## 3. Implementation in the Estate
The preferred languages for implementing Zigzag Tests across the estate are **Idris2** and **Elixir**.

### 3.1 Idris2 (Algebraic Modeling)
Idris2 is used to rigorously model the state machine and aspects using dependent types.
- **Algebras**: Define the system aspects as algebraic data types.
- **Proofs**: Use dependent types to prove that invalid states cannot be represented.
- **Code Generation**: Idris2 models can generate test sequences or API payloads that are guaranteed to be structurally valid.

### 3.2 Elixir (Concurrency and Fault Tolerance)
Elixir (running on the BEAM) is used as the execution engine for the meandering routes.
- **PropEr / StreamData**: Use Elixir's property-based testing libraries to generate random walks through the state transitions.
- **OTP Processes**: Spawn concurrent actors to simulate meandering routes in parallel, stress-testing aspects like race conditions and distributed state.
- **Fault Injection**: Intentionally crash GenServers (aspect failure) during a route to verify self-healing (synthesis).

## 4. Requirement (CRG Grading)
To achieve a Code Review Guidelines (CRG) Grade of **A** or **B**, core infrastructure and high-criticality services MUST implement Zigzag Testing for their critical paths.
