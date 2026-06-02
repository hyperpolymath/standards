# CLAUDE.md - AI Assistant Instructions

## License Policy — Manual Only (highest-priority guardrail)

**Owner directive 2026-06-02 (verbatim):**

> "mpl-2.0 is for my sole repos, all rights reserved is for 007,
> agpl-3.0-or-later is for those shared with my son, and leave other
> people's forked stuff alone … only palimpsest license for obvious
> reasons should be talking about palipsest and palimpsest plasma, and
> consent-aware-http, but in that case prospectively"

### Five-way classification (mutually exclusive, exhaustive)

| Category | License | Applies to |
|---|---|---|
| **1. Sole owner repos (default)** | `MPL-2.0` | Every repo the owner authored alone. ~all of `hyperpolymath/*` unless one of the other categories applies. |
| **2. The 007 repo** | All Rights Reserved (ARR) | `hyperpolymath/007` specifically. Out-of-scope for any normalisation, scanning, or labelling. Surface to owner only. |
| **3. Shared with son (Joshua)** | `AGPL-3.0-or-later` | Repos with son as co-author/maintainer. Examples: `idaptik`, `paint-type`. Permanent. |
| **4. Third-party / forks** | DO NOT TOUCH | Whatever upstream chose. Never sweep, never normalise. Flag as out-of-scope if it surfaces in an audit. |
| **5. Palimpsest carve-out (extremely narrow)** | `PMPL-1.0-or-later` | EXACTLY three repos: `palimpsest-license` + `palimpsest-plasma` + `consent-aware-http`. The first two retroactively; `consent-aware-http` **prospectively only** (don't flip existing content). NOWHERE ELSE. |

### Hard rules for agents

- **NEVER generate licence-change PRs without prior owner approval.** Manual,
  per-file, owner-only. Triggered by neurophone#99 (auto-PR reverting PMPL →
  MPL-2.0 across ~140 files; closed by owner 2026-06-02).
- **NEVER sweep SPDX headers in bulk.** Every prior LLM sweep has scrambled
  identifiers, mis-licensed third-party code, or reverted owner decisions.
- **NEVER touch third-party / vendored / forked licence text or headers.**
  Flag only — never edit.
- **Findings about licence drift are FLAG-ONLY.** Hypatia / gitbot-fleet emit
  these as `:review` strategy, not `:auto_execute`. Downstream pipelines MUST
  honour the cap.
- **New files an agent creates may carry the correct SPDX from birth** (that
  is authoring, not relicensing) — but the SPDX choice must match the repo's
  classification above, not a hard-coded default.

### Canonical sources

- `~/.claude/projects/.../memory/feedback_estate_license_policy_umbrella.md` —
  canonical estate-level policy (supersedes the earlier `reference_estate_licensing_policy`).
- `~/.claude/projects/.../memory/feedback_no_automated_licence_edits.md` —
  no-sweep mandate.
- `hyperpolymath/hypatia` `lib/rules/cicd_rules.ex` — `license_finding_strategy/0`
  + `license_finding_severity_cap/1` + `license_related_finding?/1` +
  `owner_license_classification/0`: the canonical Elixir-side cap.
- `hyperpolymath/gitbot-fleet` `scripts/dispatch-runner.sh` — `execute_entry()`
  early-exit refusal gate for any licence/SPDX recipe or category.
- `hyperpolymath/gitbot-fleet` `scripts/fix-license-{hygiene,file}.sh` +
  `fix-missing-spdx.sh` — all three carry `exit 1` refusal banners at the top.

---

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
| **Zig** | **APIs, FFIs, gateways, client SDKs (estate default 2026-05-28)**, memory-safe systems where Rust/SPARK is overkill | Zig is the estate-wide default for all API/FFI/gateway/client-SDK work unless explicitly special-cased; Idris2 owns ABIs. Completed V-lang→Zig migration 2026-05-28. |
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
| **V-lang** | Zig | Banned 2026-04-10; migration **COMPLETED 2026-05-28** across 16 PRs. Detected via hypatia `cicd_rules/vlang_detected` (`*.v` files) + `cicd_rules/vmod_detected` (`v.mod` manifest) with `path_allow_prefixes` mechanism for: v-ecosystem R&D carve-out, asdf-vlang toolchain installers, Coq proof scripts (`.v` shared with Coq + Verilog), interop targets (`/v-cartridge`, `/v-adapter`, `/v-bindings`, `/v-client` — where we expose work to V consumers without writing V), archived repos (`polystack/`). |
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

### TypeScript Exemptions (Approved)

The hyperpolymath "no new TypeScript" policy has the following approved exemptions, encoded as `path_allow_prefixes` on the hypatia rule `cicd_rules/typescript_detected` (matches `*.ts`). These are *not* policy violations — they are documented carve-outs.

Existing pre-2026-04-30 `.ts`/`.tsx` outside these carve-outs is grandfathered while in-flight migration proceeds (~288 estate-authored files across ~40 repos as of 2026-05-28; see project tracker `project_estate_ts_to_affinescript_2026_05_28.md`). New TS files in non-carve-out paths are blocked.

| Path / Pattern | Class | Rationale | Unblock condition |
|---|---|---|---|
| `**/*.d.ts` | declaration | FFI/library type definitions (headers, not implementation). | Never — declaration files are the boundary, not the code. |
| `**/bindings/deno/**`, `**/bindings/typescript/**`, `**/bindings/ts/**` | interop target | We expose work to TS/Deno consumers without authoring TS as primary code path. Exemplar: `proven/bindings/deno/` (72 files — Idris2 ABI exposed as Deno-native module). Parallel to V-lang `v-cartridge`/`v-adapter`/`v-bindings`/`v-client` carve-out. | Never — these are consumer-facing bindings. |
| `avow-protocol/telegram-bot/avow-telegram-bot/**` | PERMANENT | Telegraf / node-telegram-bot-api are the canonical TS-native libraries for the Bot API; no AffineScript binding planned. | AffineScript Telegram-bot bindings (no scheduled issue). |
| `**/vite.config.ts`, `**/vitest.config.ts`, `**/tsup.config.ts`, `**/tsconfig.json` | tooling | Build orchestration, not application code. | When AffineScript ships native equivalents. |
| `affinescript-deno-test/**`, `affinescript-cli/**` | bootstrap shim | TS/JS shims used to bootstrap the AffineScript test runner / CLI. | When AffineScript self-hosts these. |
| `rescript/**`, `servers/**`, `repos-monorepo/**`, `linguist/**` | upstream fork | Not estate-authored — vendored upstream code (ReScript compiler, third-party MCP servers, mass aggregator, GitHub linguist with `samples/TypeScript/*.ts` as ML training fixtures). | Never — upstream fork. |
| `hyperpolymath-archive/**` | archived | GitHub-archived repos cannot accept PRs; TS is dormant. | Never — archived. |
| `**/deps/**` | vendored package-manager dep | Elixir Mix vendored-dep directory (also adopted by other tools). Exemplar: `tma-mark2/deps/phoenix_live_view/assets/js/phoenix_live_view/*.ts` ships Phoenix LiveView's authored TS. | Never — vendored upstream. |
| `**/vscode/**` (covers `editors/vscode/`, `extensions/vscode/`, `clients/vscode/`) | editor-host extension | VSCode extension entry points target the `vscode` extension-host API. Five estate repos (`universal-language-server-plugin`, `reposystem`, `proof-burrower`, `phronesis`, `bofj-kitt`) have a single `vscode/extension.ts`. | When AffineScript ships the VSCode-extension API binding (top-50 roadmap, unshipped). |

Adding to this list requires explicit user approval and an unblock condition (except the structural classes above, which are estate-wide policy). The detection rule and its `path_allow_prefixes` field are the single source of truth; this table mirrors that for human readability.

### ReScript Exemptions (Approved)

The hyperpolymath "no new ReScript" policy (banned 2026-05-25) has the following approved exemptions, encoded as `path_allow_prefixes` on the hypatia rules `cicd_rules/rescript_detected` (matches `*.res`) and `cicd_rules/rescript_interface_detected` (matches `*.resi`).

Existing pre-2026-05-25 `.res`/`.resi` outside these carve-outs is grandfathered while in-flight migration proceeds (~3,996 files across ~80 repos as of 2026-05-30; see project tracker `project_estate_rescript_to_affinescript_2026_05_28.md` and umbrella `hyperpolymath/standards#252`). New `.res`/`.resi` files in non-carve-out paths are blocked.

| Path / Pattern | Class | Rationale | Unblock condition |
|---|---|---|---|
| `**/bsconfig.json`, `**/*.config.res` | tooling | Build orchestration, not application code. | When AffineScript ships native equivalents. |
| `rescript/**`, `servers/**`, `repos-monorepo/**`, `linguist/**` | upstream fork | Not estate-authored — vendored upstream code (ReScript compiler, third-party MCP servers, mass aggregator, GitHub linguist samples). | Never — upstream fork. |
| `zotpress/**` | upstream fork | Upstream WordPress plugin fork (`hyperpolymath/hyperpolymath/zotpress`); not estate-authored, kept in tree as a vendored fork. Mirrors the per-repo `.hypatia-ignore` entry in hyperpolymath/hyperpolymath#20. | Never — upstream fork. |
| `hyperpolymath-archive/**` | archived | GitHub-archived repos cannot accept PRs; ReScript is dormant. | Never — archived. |
| `**/deps/**`, `**/node_modules/**` | vendored package-manager dep | Mix-style vendored deps and Node-style node_modules. | Never — vendored upstream. |
| `**/vscode/**` (covers `editors/vscode/`, `extensions/vscode/`, `clients/vscode/`) | editor-host extension | VSCode extension entry points target the `vscode` extension-host API. | When AffineScript ships the VSCode-extension API binding (top-50 roadmap, unshipped). |
| `**/lib/js/**`, `**/lib/es6/**`, `**/lib/bs/**` | compiled output | bsc (the ReScript compiler) emits to these paths. They are not source. | Never — compiler output, not source. |
| `affinescript-deno-test/**`, `affinescript-cli/**` | bootstrap shim | Bootstrap the AffineScript toolchain itself. | When AffineScript self-hosts these. |
| `avow-protocol/telegram-bot/avow-telegram-bot/**` | PERMANENT | Mirrors TS Telegraf carve-out for any `.res` file in the same directory. | Never. |

### npm Exemptions (Approved)

The hyperpolymath "npm banned" policy (2026-05-25) has the following approved exemptions on the hypatia rule `cicd_rules/nodejs_detected` (matches `package-lock.json`).

Migration substantially complete 2026-05-31 under umbrella `hyperpolymath/standards#253` (172 manifests at campaign start; all seven STEP issues #261/#262/#265/#268/#270/#273/#275 closed; ~22 physical-migration PRs landed plus three named-bucket audits closed `SUBSTANTIALLY DONE`; per-repo follow-up trackers cover the residual longtail). See `project_estate_npm_to_deno_2026_05_28.md`. Per-repo recipe: `docs/migrations/npm-to-deno-template/MIGRATION.md`.

| Path / Pattern | Class | Rationale | Unblock condition |
|---|---|---|---|
| `**/vscode/**` | VSCode extension host-required (segment) | VSCode extension toolchain runs under Node; lockfile is contractually required by the host. | When AffineScript ships the VSCode-extension API binding. |
| `vscode-` substring (`vscode-extension/`, `editors/vscode-007/`, `vscode-a2ml`, `vscode-k9`, …) | VSCode-* extension repos / subdirs | Same VSCode host-required toolchain rationale; different path-segment shape than `/vscode/`. | When AffineScript ships the VSCode-extension API binding. |
| `tree-sitter-` substring (`tree-sitter-a2ml`, `tree-sitter-k9`, `editors/tree-sitter-ephapax`, `tree-sitter-affinescript/`, …) | tree-sitter grammar npm-publish target | Class C consumer artifact — tree-sitter grammars ship via npm with `node-gyp` native binding because every consumer (Atom/Neovim/VSCode TextMate) links the native addon. | Never — npm-publishable consumer artifact with native binding. |
| `affinescript-deno-test/**`, `affinescript-cli/**` | bootstrap shim | Bootstrap the AffineScript toolchain itself. | When AffineScript self-hosts these. |
| `rescript/**`, `servers/**`, `repos-monorepo/**`, `linguist/**` | upstream fork | Not estate-authored — vendored upstream code. | Never — upstream fork. |
| `hyperpolymath-archive/**` | archived | Archived repos cannot accept PRs. | Never — archived. |
| `**/deps/**`, `**/node_modules/**` | vendored package-manager dep | Vendored deps. | Never — vendored upstream. |
| `**/.lake/**` | Lean4 vendored package | Lean4's package manager directory, parallel to `/deps/`. | Never — vendored. |
| `**/office-addin/**` | Office.js add-in host-required | Office host loads `.js` from a Node-packaged manifest, parallel to VSCode extension carve-out. | When AffineScript ships an Office.js binding. |
| `**/bindings/javascript/**`, `**/bindings/typescript/**` | consumer-facing estate export | Proven JS/TS bindings published to npm consumers, parallel to `/bindings/deno/` under `:typescript_detected`. | Never — consumer-facing bindings. |
| `**/example/**`, `**/examples/**`, `**/test-fixtures/**`, `**/fixtures/**` | example/test fixture | Demonstrates an npm consumer (e.g., showing how a library is used from a Node project) without making the repo itself an npm consumer. | Never — fixture, not own toolchain. |

### JavaScript Exemptions (Approved)

The hyperpolymath "Unnecessarily-JavaScript banned" policy (2026-05-25) has the following approved exemptions on the hypatia rules `cicd_rules/javascript_detected` (matches `*.js`) and `cicd_rules/javascript_jsx_detected` (matches `*.jsx`).

Distinct from TS/RS policy: JavaScript is *allowed* where AffineScript cannot reach. The rule targets the gap between current AS bindings and current JS usage — JS that COULD be AS today but isn't. In-flight migration tracked under `hyperpolymath/standards#254` (1,609 files across the estate; see `project_estate_unnecessary_js_2026_05_28.md`). Ship mode: HARD-BLOCK + extensive carve-outs (umbrella#254 STEP 1 design question resolved 2026-05-30). Per-PR exemption via inline pragma `// hypatia: allow cicd_rules/javascript_detected -- <reason>`.

| Path / Pattern | Class | Rationale | Unblock condition |
|---|---|---|---|
| `mcp-bridge/**`, `**/plugins/**` | host-required by ecosystem | MCP servers and plugin entry points where JS is the host contract (the host loads .js, not .affine). | When AS plugin-host bindings ship (top-50 roadmap). |
| `**/*.config.js`, `**/*.config.cjs`, `**/*.config.mjs` | tooling configs | Build orchestration. | When AS ships native equivalents. |
| `affinescript-deno-test/**`, `affinescript-cli/**` | bootstrap shim | Bootstrap the AffineScript toolchain itself. | When AS self-hosts these. |
| `rescript/**`, `servers/**`, `repos-monorepo/**`, `linguist/**` | upstream fork | Not estate-authored — vendored upstream code (linguist samples are ML training data). | Never — upstream fork. |
| `hyperpolymath-archive/**` | archived | Archived repos cannot accept PRs. | Never — archived. |
| `**/deps/**`, `**/node_modules/**` | vendored package-manager dep | Vendored deps. | Never — vendored upstream. |
| `**/out/**`, `**/lib/js/**`, `**/.deno/**` | compiled output | AS / RS / Deno-cache compile output. | Never — compiler output, not source. |
| `**/vscode/**`, `**/extensions/vscode/**` | editor-host extension entry | VSCode extension entry points (often shipped as compiled `.js` even when authored as `.ts`). | When AS VSCode-extension API binding ships (top-50 roadmap). |