# Estate Proof-Debt Audit — 2026-05-26

**Scanner:** automated grep sweep across 283 estate repositories.
**Date:** 2026-05-26 (HEAD-state snapshot).
**Scope:** every `*.v`, `*.lean`, `*.agda`, `*.idr`, `*.idr2`, `*.fst`, `*.dfy`, `*.tla`, `*.ads`, `*.adb` file outside `.git/`, `target/`, `_build/`, `node_modules/`.

**What was searched:**
- Coq: `Axiom`, `Admitted`, `admit.` at line start
- Lean: `sorry`, `axiom <name>`
- Agda: `postulate` (top-level)
- Idris2: `believe_me`, `really_believe_me`, `assert_total`, top-level `partial`, `%default partial`
- F\*: `assume val`, `admit_p`
- Cross-language: `TODO PROOF`, `OWED:`, `FIXME PROOF`
- Plus Rust/Haskell `unsafePerformIO` / `unsafeCoerce` as a "soundness escape" indicator

### Headline numbers

| Language | Files scanned |
|---|---|
| Coq (`*.v`) | 554 |
| Lean (`*.lean`) | 190 |
| Agda (`*.agda`) | 1211 |
| Idris2 (`*.idr`/`*.idr2`) | 4109 |
| F\* (`*.fst`) | 7 |
| Dafny (`*.dfy`) | 2 |
| TLA+ (`*.tla`) | 68 |
| SPARK (`*.ads`+`*.adb`) | 1011 |

### Top offenders (raw counts, including archive/vendored)

```
# Proof-debt sweep 2026-05-26T12:18:05+01:00
007                                                | files=   59 | Coq-Axm/Adm=   0 | Lean-srry/ax=   0 | Agda-pst=   0 | Idr-blv=  16 | Idr-prtl=   0 | Fstr-asm=  0 | TODO=  0 | Unsafe=  0
absolute-zero                                      | files= 6638 | Coq-Axm/Adm=  72 | Lean-srry/ax= 315 | Agda-pst=   0 | Idr-blv=   0 | Idr-prtl=   0 | Fstr-asm=  0 | TODO=  0 | Unsafe=  0
affinescript                                       | files=  593 | Coq-Axm/Adm=   0 | Lean-srry/ax=   0 | Agda-pst=   0 | Idr-blv=   0 | Idr-prtl=   0 | Fstr-asm=  0 | TODO=  0 | Unsafe=  0
affinescript-stdlib-pr                             | files=   37 | Coq-Axm/Adm=   0 | Lean-srry/ax=   0 | Agda-pst=   0 | Idr-blv=   0 | Idr-prtl=   0 | Fstr-asm=  0 | TODO=  0 | Unsafe=  0
agda-stdlib                                        | files= 1229 | Coq-Axm/Adm=   0 | Lean-srry/ax=   0 | Agda-pst=  27 | Idr-blv=   0 | Idr-prtl=   0 | Fstr-asm=  0 | TODO=  0 | Unsafe=  0
ambientops                                         | files=  138 | Coq-Axm/Adm=   0 | Lean-srry/ax=   0 | Agda-pst=   0 | Idr-blv=   0 | Idr-prtl=   0 | Fstr-asm=  0 | TODO=  0 | Unsafe=  0
asdf-tool-plugins                                  | files=  567 | Coq-Axm/Adm=   0 | Lean-srry/ax=   0 | Agda-pst=   0 | Idr-blv=   0 | Idr-prtl=   0 | Fstr-asm=  0 | TODO=  0 | Unsafe=  0
betlang                                            | files=   10 | Coq-Axm/Adm=   0 | Lean-srry/ax=   5 | Agda-pst=   0 | Idr-blv=   0 | Idr-prtl=   0 | Fstr-asm=  0 | TODO=  0 | Unsafe=  0
bofig                                              | files=    6 | Coq-Axm/Adm=   0 | Lean-srry/ax=   0 | Agda-pst=   0 | Idr-blv=   0 | Idr-prtl=   0 | Fstr-asm=  0 | TODO=  0 | Unsafe=  0
bofj-kitt                                          | files=   19 | Coq-Axm/Adm=   0 | Lean-srry/ax=   0 | Agda-pst=   0 | Idr-blv=   6 | Idr-prtl=   0 | Fstr-asm=  0 | TODO=  0 | Unsafe=  0
boinc-boinc                                        | files=    7 | Coq-Axm/Adm=   0 | Lean-srry/ax=   1 | Agda-pst=   0 | Idr-blv=   0 | Idr-prtl=   0 | Fstr-asm=  0 | TODO=  0 | Unsafe=  0
boj-server                                         | files=  126 | Coq-Axm/Adm=   0 | Lean-srry/ax=   0 | Agda-pst=   0 | Idr-blv=   9 | Idr-prtl=   0 | Fstr-asm=  0 | TODO=  0 | Unsafe=  0
burble                                             | files=   55 | Coq-Axm/Adm=   0 | Lean-srry/ax=   0 | Agda-pst=   0 | Idr-blv=   2 | Idr-prtl=   0 | Fstr-asm=  0 | TODO=  0 | Unsafe=  0
civic-connect                                      | files=   12 | Coq-Axm/Adm=   0 | Lean-srry/ax=   0 | Agda-pst=   0 | Idr-blv=   0 | Idr-prtl=   0 | Fstr-asm=  0 | TODO=  0 | Unsafe=  0
developer-ecosystem                                | files=  710 | Coq-Axm/Adm=   0 | Lean-srry/ax=   0 | Agda-pst=   0 | Idr-blv=   8 | Idr-prtl=  56 | Fstr-asm=  0 | TODO=  0 | Unsafe=  0
docmatrix                                          | files=   10 | Coq-Axm/Adm=   0 | Lean-srry/ax=   0 | Agda-pst=   0 | Idr-blv=   0 | Idr-prtl=   0 | Fstr-asm=  0 | TODO=  0 | Unsafe=  0
docudactyl                                         | files=   11 | Coq-Axm/Adm=   0 | Lean-srry/ax=   0 | Agda-pst=   0 | Idr-blv=   0 | Idr-prtl=   0 | Fstr-asm=  0 | TODO=  0 | Unsafe=  0
echidna                                            | files=  103 | Coq-Axm/Adm=   0 | Lean-srry/ax=   6 | Agda-pst=   2 | Idr-blv=   0 | Idr-prtl=   0 | Fstr-asm=  0 | TODO=  0 | Unsafe= 12
echidnabot                                         | files=    7 | Coq-Axm/Adm=   1 | Lean-srry/ax=   4 | Agda-pst=   0 | Idr-blv=   1 | Idr-prtl=   0 | Fstr-asm=  0 | TODO=  0 | Unsafe=  0
echo-types                                         | files=  609 | Coq-Axm/Adm=   0 | Lean-srry/ax=   0 | Agda-pst=   0 | Idr-blv=   0 | Idr-prtl=   0 | Fstr-asm=  0 | TODO=  0 | Unsafe=  0
eclexia                                            | files=    8 | Coq-Axm/Adm=   9 | Lean-srry/ax=   0 | Agda-pst=   0 | Idr-blv=   0 | Idr-prtl=   0 | Fstr-asm=  0 | TODO=  0 | Unsafe=  0
email-octad-experiment                             | files=   14 | Coq-Axm/Adm=   0 | Lean-srry/ax=   0 | Agda-pst=   0 | Idr-blv=   9 | Idr-prtl=   0 | Fstr-asm=  0 | TODO=  0 | Unsafe=  0
ephapax                                            | files=   23 | Coq-Axm/Adm=   3 | Lean-srry/ax=   0 | Agda-pst=   0 | Idr-blv=   1 | Idr-prtl=   1 | Fstr-asm=  0 | TODO= 14 | Unsafe=  0
fireflag                                           | files=    9 | Coq-Axm/Adm=   0 | Lean-srry/ax=   0 | Agda-pst=   0 | Idr-blv=   0 | Idr-prtl=   0 | Fstr-asm=  0 | TODO=  0 | Unsafe=  0
flatracoon                                         | files=   12 | Coq-Axm/Adm=   0 | Lean-srry/ax=   0 | Agda-pst=   0 | Idr-blv=   0 | Idr-prtl=   0 | Fstr-asm=  0 | TODO=  0 | Unsafe=  0
formatrix-docs                                     | files=   10 | Coq-Axm/Adm=   0 | Lean-srry/ax=   0 | Agda-pst=   0 | Idr-blv=   0 | Idr-prtl=   0 | Fstr-asm=  0 | TODO=  0 | Unsafe=  0
frayed-knot-toolkit                                | files=    6 | Coq-Axm/Adm=   0 | Lean-srry/ax=   0 | Agda-pst=   0 | Idr-blv=   0 | Idr-prtl=   0 | Fstr-asm=  0 | TODO=  0 | Unsafe=  0
fraying-model-computational-testbed                | files=   13 | Coq-Axm/Adm=   0 | Lean-srry/ax=   0 | Agda-pst=   0 | Idr-blv=   6 | Idr-prtl=   0 | Fstr-asm=  0 | TODO=  0 | Unsafe=  0
gossamer                                           | files=   28 | Coq-Axm/Adm=   0 | Lean-srry/ax=   0 | Agda-pst=   0 | Idr-blv=  13 | Idr-prtl=   4 | Fstr-asm=  0 | TODO=  2 | Unsafe=  0
```

### Filtered by active development (excluding archive/vendored mirrors)

- **`hyperpolymath-archive`** — 878 Lean sorry/axiom, 18 partial Idris, 3 believe_me. *Archive — frozen, not active debt.*
- **`repos-monorepo`** — 170 Coq axiom/admit, 129 Lean sorry, 105 believe_me, 138 partial. *Monorepo of vendored copies — proxy debt.*
- **`absolute-zero`** — 72 Coq Axiom/Admitted, 315 Lean sorry/axiom across ~6638 proof files. **Active.** Open issue: #44 baseline glob fix + class-J axiom audit per memory.
- **`maa-framework`** — 80 Coq, 54 Lean across just 25 files. **High density. Active.**
- **`hypatia`** — 3 Coq, 6 Lean, 3 Agda postulate, 12 believe_me, 3 unsafe across 81 files. **Active.**
- **`echidna`** — 6 Lean sorry, 2 Agda postulate, 12 unsafe. **L3-resume work per memory.**
- **`betlang`** — 5 Lean sorry. `substTop_preserves_typing` axiom per memory (PR#27 closed but axiom remains).
- **`echidnabot`** — 1 Coq, 4 Lean, 1 believe_me.
- **`ephapax`** — 3 Coq Admitted (Semantics.v:4924, 5983, 6572), 1 believe_me, 1 partial, 14 TODO PROOF. **Active — closure plan owned (Item 1 in MEMORY).**
- **`vcl-ut`** — 8 believe_me, 14 TODO PROOF. Known: HOLE deeper than documented (memory).
- **`typed-wasm`** — 5 believe_me.
- **`stapeln`** — 10 believe_me, 34 partial across 102 files.
- **`proven`** — 51 believe_me, 10 partial, 1 unsafe, 372 TODO markers across 853 files. **Largest active TODO surface.**
- **`proven-servers`** — 1 believe_me, 10 partial, 1 TODO.
- **`standards`** — 4 Agda postulate, 1 believe_me, 11 partial. The canonical repo has its own proof debt.
- **`veridical-simulation-core`** — 4 believe_me.
- **`valence-shell`** — 1 Agda postulate, 8 partial.

### Cross-cutting patterns

1. **Idris2 `partial`** is the most common active marker (138 in repos-monorepo, 34 in stapeln, 11 in standards, 10 in proven, etc.). `partial` is correctness-relevant but often masquerades as ergonomic ("totality just hard to prove here").
2. **`believe_me`** clusters around extraction-boundary code (Rust↔Idris FFI, codec runtime). Pattern repeats across typed-wasm, hypatia, stapeln, vcl-ut, somethings-fishy, snifs, rsr-template-repo (each 5–12 usages).
3. **`TODO PROOF` / `OWED` markers** in `proven` (372) and `ephapax` (14) indicate that the codebases track their own debt — they're better surfaced than purely silent debt elsewhere.
4. **Agda `postulate`** in `standards` (4) and `valence-shell` (1) — needs review for whether these are necessary axioms (e.g. `funExt`) or genuine debt.
5. **Soundness-escapes outside proof languages**: `unsafePerformIO`/`unsafeCoerce` showed 12 in echidna, 3 in hypatia, 2 in somethings-fishy — these are not "proof debt" per se but soundness-relevant and warrant audit.

### Recommended next moves

| Repo | Action | Priority |
|---|---|---|
| ephapax | Close `formal/Semantics.v` 3 Admitteds via 6-9 day plan in `project_ephapax_preservation_closure_plan.md` | P0 (already on roadmap) |
| absolute-zero | Triage 72 Coq Admitteds + 315 Lean sorries — likely cluster around T0 axiom-audit territory | P0 |
| maa-framework | High density (80+54 in 25 files) — investigate whether vendored or original | P1 |
| betlang | Discharge `substTop_preserves_typing` per PR#27 recipe | P1 |
| proven | Convert the 372 TODO PROOF markers into a discharge schedule | P1 |
| standards | Audit 4 Agda postulates + 11 Idris partials — set the example for downstream | P1 |
| typed-wasm, stapeln, vcl-ut, hypatia, snifs, somethings-fishy | Each 5-15 believe_me — audit & document | P2 |
| Rest | Document in per-repo `docs/tech-debt-2026-05-26.md`; no urgent fix | P3 |

### Trusted-base reduction policy (proposed estate rule)

Reuse the [boj-server backend-assurance harness pattern](https://github.com/hyperpolymath/boj-server) for `believe_me` / `assume val` / `Trust X` blocks: each must be one of
- (a) discharged by a proof,
- (b) property-tested via QuickCheck/Verus-style adversarial tests,
- (c) annotated with a refutation budget and tracked in `docs/proof-debt.md`.

### Coverage caveat

This scan is **syntactic**, not semantic. It cannot distinguish:
- Necessary axioms (e.g. `funExt` in HoTT) from genuine debt.
- `partial` used because Idris2's totality checker is incomplete vs `partial` used to bury non-termination.
- `unsafePerformIO` used at a verified library boundary vs ad-hoc soundness break.

The per-repo PRs ask each repo's maintainer to classify each finding.

---

🤖 Generated by Claude Code estate-wide proof-debt scan (2026-05-26).
Companion docs: `2026-05-26-estate-licence-debt.md`, `2026-05-26-estate-documentation-debt.md`.
