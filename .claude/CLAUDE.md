# CLAUDE.md - AI Assistant Instructions

## Machine-Readable Artefacts

Every Hyperpolymath repo must have `.machine_readable/` with these 6 A2ML files:

- `STATE.a2ml` - Current project state and progress
- `META.a2ml` - Architecture decisions and development practices
- `ECOSYSTEM.a2ml` - Position in the ecosystem and related projects
- `AGENTIC.a2ml` - AI agent interaction patterns
- `NEUROSYM.a2ml` - Neurosymbolic integration config
- `PLAYBOOK.a2ml` - Operational runbook

See: https://github.com/hyperpolymath/standards

---

## Language Policy (Hyperpolymath Standard)

### Terminology note — "Rust" always means "Rust/SPARK"

As of 2026-04-10, the word **Rust** in all hyperpolymath documentation is
shorthand for **Rust with SPARK integration as the default stance**. SPARK
integration is the transition direction — Rust-primary now, with SPARK/Ada
modules called via the standard Idris2-ABI / Zig-FFI pattern for the
correctness-critical parts. New Rust projects should be *designed* to admit
SPARK modules even if they do not yet contain any. See
`spec/LANGUAGE-POLICY.adoc §Terminology` in `rhodium-standard-repositories`
for the canonical statement.

### ALLOWED Languages & Tools

| Language/Tool | Use Case | Notes |
|---------------|----------|-------|
| **AffineScript** | Primary application code | Compiles to typed-wasm; affine/linear types. Replaces ReScript across the estate (RS/TS/JS → AffineScript → typed-wasm). |
| **Deno** | Runtime & package management | Replaces Node/npm/bun |
| **Rust/SPARK** | Performance-critical, systems, WASM, CLI tools, safety-critical | "Rust" always means "Rust/SPARK" per terminology note above. Preferred over Ada where reachable. |
| **Zig** | FFI layer (hyperpolymath ABI/FFI standard), memory-safe systems where Rust/SPARK is overkill | Also the migration target for V-lang |
| **Idris2** | Formal verification (primary, ABI-style proofs) | ATS2 rejected. Proven-library status in `proven` repo. |
| **Agda** | Formal verification (foundational / type-theoretic constructions) | Used by `hyperpolymath/echo-types` (loss-with-residue / proof-relevant fibers) and other foundational formalisations. Constructive only — no postulates in load-bearing tracks. |
| **echo-types library** | Loss-with-residue formalism (Agda) | `hyperpolymath/echo-types` — canonical formalisation of `Echo f y := Σ (x : A) , (f x ≡ y)`. Cite from this lib rather than reinventing in downstream code. |
| **Tauri 2.0+** | Mobile apps (iOS/Android) | Rust backend + web UI |
| **Dioxus** | Mobile apps (native UI) | Pure Rust, React-like |
| **Gleam** | Backend services | Runs on BEAM or compiles to JS |
| **Elixir** | Backend services, distributed systems | BEAM, Phoenix, OTP |
| **Haskell** | Type-heavy tools, registry validation | Scaffoldia CLI |
| **Bash/POSIX Shell** | Scripts, automation | Keep minimal |
| **JavaScript** | Only where AffineScript cannot | MCP protocol glue, Deno APIs (transitional; prefer .affine where possible) |
| **Nickel** | Configuration language | For complex configs |
| **A2ML** | State/meta files | STATE.a2ml, META.a2ml, etc. (TOML-like format) |
| **Julia** | Batch scripts, data processing | Per RSR |
| **OCaml** | AffineScript compiler | Language-specific |
| **Ada** (legacy) | Safety-critical systems where Rust/SPARK is not yet reachable | Rust/SPARK is absorbing most Ada work over time. Do not start new pure-Ada projects unless Rust/SPARK cannot reach. |

### BANNED - Do Not Use

| Banned | Replacement | Notes |
|--------|-------------|-------|
| TypeScript | AffineScript | RS/TS/JS → AffineScript → typed-wasm. |
| **ReScript** | AffineScript | Banned in new code as of 2026-04-30. Existing `.res` files migrate to `.affine` directly (do not pass through ReScript). |
| Node.js | Deno | |
| npm | Deno | |
| Bun | Deno | |
| pnpm/yarn | Deno | |
| Go | Rust/SPARK | |
| **Python** | AffineScript/Rust/SPARK/Julia | Fully banned, no exceptions (SaltStack exception removed 2026-01-03) |
| Java/Kotlin | Rust/SPARK, Tauri, Dioxus | |
| Swift | Tauri/Dioxus | |
| React Native | Tauri/Dioxus | |
| Flutter/Dart | Tauri/Dioxus | Google lock-in |
| **V-lang** | Zig | Banned 2026-04-10. "Too many things, and V does not live up to it." Detected via `v.mod` / `vpkg.json` (not `.v` files, because that collides with Verilog). Migration is a direction, not a rip-out. |
| **ATS2** | Idris2 (formal), Rust/SPARK (safety-critical operational) | Rejected in favour of Idris2 and Rust/SPARK. |
| **Makefiles** | Mustfile/justfile | |

**NOTE:** Python is fully banned. V-lang is fully banned (2026-04-10). ATS2 is
fully banned in favour of Idris2 + Rust/SPARK. ReScript is fully banned in new
code as of 2026-04-30 (use AffineScript). All four bans are enforced by
`.github/workflows/language-policy.yml`.

### Build System

All repositories use Mustfile/justfile instead of Makefiles:

- `Mustfile` - Mandatory checks definition
- `justfile` - Build recipes (https://just.systems)
- `mustfile.toml` - Configuration (optional)

See: https://github.com/hyperpolymath/mustfile

### Mobile Development

**No exceptions for Kotlin/Swift** - use Rust-first approach:

1. **Tauri 2.0+** - Web UI (AffineScript → typed-wasm) + Rust backend, MIT/Apache-2.0
2. **Dioxus** - Pure Rust native UI, MIT/Apache-2.0

Both are FOSS with independent governance (no Big Tech).

### Enforcement Rules

1. **No new TypeScript or ReScript files** - Convert existing TS/RS to AffineScript directly (`.affine`); ReScript is no longer the destination
2. **No package.json for runtime deps** - Use deno.json imports
3. **No node_modules in production** - Deno caches deps automatically
4. **No Go code** - Use Rust instead
5. **No Python** - All Python must be rewritten
6. **No Kotlin/Swift for mobile** - Use Tauri 2.0+ or Dioxus
7. **No Makefiles** - Use Mustfile/justfile instead

### Package Management

- **Primary**: Guix (guix.scm)
- **Fallback**: Nix (flake.nix)
- **JS deps**: Deno (deno.json imports)

### Documentation Format

- All docs must be `.adoc` (AsciiDoc) except GitHub-required files
- GitHub-required `.md`: SECURITY.md, CONTRIBUTING.md, CODE_OF_CONDUCT.md, CHANGELOG.md
- No duplicate formats (if `.adoc` exists, don't also have `.md`)

### Security Requirements

- No MD5/SHA1 for security (use SHA256+)
- HTTPS only (no HTTP URLs)
- No hardcoded secrets
- SHA-pinned dependencies
- SPDX license headers on all files
