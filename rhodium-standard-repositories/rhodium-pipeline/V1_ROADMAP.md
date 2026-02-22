# Rhodium Pipeline Template — V1 Roadmap

## Current State

| Asset | Status |
|-------|--------|
| HANDOVER.md | Written, not committed |
| README.md | Written, not committed |
| Executable template | Does not exist |
| Reference implementation (zerostep) | Complete, separate repo |

**The template is currently documentation-only.** To reach v1, it needs to become an actual usable template.

---

## V1 Definition

**V1 means:** A developer can run one command and get a working skeleton project that compiles, has placeholder proofs that typecheck, and demonstrates the full methodology.

---

## Required Work

### Phase 1: Foundation (Must Have for V1)

#### 1.1 Commit the Documentation
```
□ Add HANDOVER.md to repo
□ Add README.md to repo
□ Add LICENSE (MPL-2.0)
```

#### 1.2 Choose Template Engine
Pick one:

| Option | Pros | Cons |
|--------|------|------|
| **cargo-generate** | Rust-native, good for Rust projects | Less flexible |
| **cookiecutter** | Mature, widely used | Python dependency (ironic) |
| **copier** | Modern cookiecutter alternative | Python dependency |
| **Custom shell script** | No dependencies | More maintenance |

**Recommendation:** `cargo-generate` — stays in the Rust ecosystem, no Python.

#### 1.3 Create Template Structure

```
rhodium-pipeline/
├── template/                          # The actual template
│   ├── cargo-generate.toml            # Template config
│   ├── {{project-name}}/
│   │   ├── Cargo.toml.liquid
│   │   ├── src/
│   │   │   └── main.rs.liquid
│   │   ├── proofs/
│   │   │   └── {{ProjectName}}_Invariants.thy.liquid
│   │   ├── schemas/
│   │   │   └── config.cue.liquid
│   │   ├── config.ncl.liquid
│   │   ├── justfile.liquid
│   │   └── ...
├── docs/
│   ├── HANDOVER.md
│   ├── QUICKSTART.md
│   └── ARCHITECTURE.md
├── README.md
└── LICENSE
```

#### 1.4 Minimal Skeleton Files

Each file needs a working minimal version:

| File | Minimum Viable Content |
|------|----------------------|
| `Cargo.toml` | clap, serde, basic deps |
| `main.rs` | CLI skeleton with standard flag categories |
| `*_Invariants.thy` | One trivial lemma that typechecks |
| `config.cue` | Basic schema with one constraint |
| `config.ncl` | Preset pattern with dev/prod |
| `justfile` | All 6 recipe categories, placeholder commands |

#### 1.5 Dependency Check Script

```bash
scripts/check_deps.sh
```

Must verify: rust, just, nickel, cue, isabelle (with clear install instructions on failure).

---

### Phase 2: Quality (Should Have for V1)

#### 2.1 Working Example

The generated project should:
- `cargo build` without errors
- `just help` shows all recipes
- `just check-deps` validates toolchain
- `isabelle build -d proofs/` passes (proof typechecks)
- `cue vet schemas/config.cue` passes
- `nickel export config.ncl` produces valid output

#### 2.2 Documentation Polish

```
□ QUICKSTART.md — 5-minute "hello world" with the template
□ CLI_FLAGS.md — Standard flag patterns with examples
□ PROOF_PATTERNS.md — How to write the 4 core invariants
□ CONFIGURATION.md — Nickel + CUE patterns explained
```

#### 2.3 CI Pipeline

```yaml
# .gitlab-ci.yml or .github/workflows/ci.yml
- Generate a test project from template
- Verify it builds
- Verify proofs typecheck
- Verify schemas validate
```

---

### Phase 3: Polish (Nice to Have for V1)

#### 3.1 Julia Integration Toggle

Template variable `include-julia: true/false` that:
- Adds Julia project files when true
- Adds Julia recipes to justfile
- Includes Julia↔Rust FFI skeleton

#### 3.2 Proof Starter Library

Pre-written Isabelle theories for:
- Partition invariant (copy-paste ready)
- Bijection invariant
- Checksum integrity
- Ratio tolerance

#### 3.3 Nickel/CUE Starter Contracts

Reusable validation patterns:
- Path validation
- Percentage/ratio types
- Checksum format validation

---

## Work Estimates (Complexity, Not Time)

| Task | Complexity | Dependencies |
|------|------------|--------------|
| Commit docs | Trivial | None |
| Choose template engine | Decision | None |
| Cargo.toml template | Easy | Engine chosen |
| main.rs template | Medium | Cargo.toml |
| Isabelle .thy template | Medium | Domain knowledge |
| CUE schema template | Easy | None |
| Nickel config template | Easy | None |
| justfile template | Medium | All above |
| check_deps.sh | Easy | None |
| QUICKSTART.md | Medium | Working template |
| CI pipeline | Medium | Working template |
| Julia toggle | Medium | Base template |
| Proof library | Hard | Isabelle expertise |

---

## Critical Path to V1

```
1. Commit documentation (HANDOVER.md, README.md, LICENSE)
          ↓
2. Set up cargo-generate structure
          ↓
3. Create minimal Cargo.toml + main.rs templates
          ↓
4. Create minimal Isabelle .thy template (one trivial proof)
          ↓
5. Create minimal CUE + Nickel templates
          ↓
6. Create justfile with all categories
          ↓
7. Write check_deps.sh
          ↓
8. Test: generate project, verify all tools work
          ↓
9. Write QUICKSTART.md
          ↓
10. Tag v1.0.0
```

---

## Open Decisions Needed

### 1. Template Engine
**Recommendation:** cargo-generate
**Alternative:** Shell script if you want zero dependencies

### 2. Isabelle Version Pinning
Should the template specify an Isabelle version? (Proofs can break across versions)
**Recommendation:** Yes, pin to Isabelle2024

### 3. Default Checksum Algorithm
Options: SHAKE256, BLAKE3, SHA3-256
**Recommendation:** BLAKE3 (fastest, secure, good tooling)

### 4. Minimum Rust Version
**Recommendation:** 1.75+ (for cleaner async if needed later)

### 5. License for Generated Projects
Template is MPL-2.0. What should generated projects default to?
**Recommendation:** User's choice via template variable, default MPL-2.0

---

## What Zerostep Already Provides

You can extract these directly from zerostep:

| Asset | Zerostep Location | Extraction Effort |
|-------|------------------|-------------------|
| CLI flag patterns | `src/main.rs` | Low |
| Justfile structure | `justfile` | Low |
| Isabelle proof patterns | `proofs/` | Medium (generalize) |
| CUE schema patterns | `schemas/` | Low |
| Nickel config patterns | `config.ncl` | Low |
| check_deps.sh | `scripts/` | Trivial |

**Recommendation:** Do a side-by-side extraction session. For each zerostep file, create the templatized version.

---

## Success Criteria for V1

A new user should be able to:

```bash
# Install template
cargo generate --git https://github.com/hyperpolymath/rhodium-pipeline

# Answer prompts
# Project name: my-pipeline
# Description: My data processing pipeline
# Include Julia: no

# Enter generated project
cd my-pipeline

# Verify everything works
just check-deps        # All green
cargo build            # Compiles
just validate-proofs   # Isabelle passes
just config            # Shows Nickel config

# Start customizing
# Edit src/main.rs for domain logic
# Edit proofs/*.thy for invariants
# Edit schemas/config.cue for validation
```

**If this workflow works end-to-end, it's V1.**

---

## Post-V1 Roadmap

| Version | Focus |
|---------|-------|
| v1.1 | Julia integration, more proof templates |
| v1.2 | Shared Isabelle theory library |
| v1.3 | CI templates for GitLab and GitHub |
| v2.0 | Multiple pipeline archetypes (ETL, ML training, data validation) |

---

*Generated 2025-12-06 as part of methodology review*
