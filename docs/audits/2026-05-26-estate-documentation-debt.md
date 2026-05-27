# Estate Documentation-Debt Audit — 2026-05-26

**Scanner:** automated sweep of README + docs/ + CHANGELOG + CONTRIBUTING + CODE_OF_CONDUCT + SECURITY presence across 279 git repos.
**Date:** 2026-05-26.

**Documentation-debt definition used:**
- README present? How many lines?
- `docs/` directory present? How many `.md`/`.adoc`/`.rst` files? Total LoC?
- Wiki indicator: `wiki/` dir, `.wiki` submodule, or in-repo reference?
- Project hygiene: CHANGELOG.md, CONTRIBUTING.md, CODE_OF_CONDUCT.md, SECURITY.md?

A repo has a **heavily-developed and well-organised wiki** for the purposes of this audit when it satisfies: `docs/` directory has ≥10 substantive markdown/asciidoc/rst files OR there is a `wiki/` directory/submodule.

### Severity distribution (combined)

| Severity | Count | Meaning |
|---|---|---|
| CRITICAL | 5 | no README at all |
| HIGH | 10 | stub README (<20 lines) |
| MEDIUM | 16 | README OK but no `docs/` directory |
| LOW | 123 | thin docs (<10 files in `docs/`) |
| OK | 124 | heavily-developed docs |

### Heavily-developed wikis (≥10 docs files) — exemplars

These ~50 repos meet the user's "heavily-developed and well-organised wiki" bar:

- `007                                                ` — 43 files / 11879 LoC
- `affinescript                                       ` — 91 files / 26832 LoC
- `affinescript-stdlib-pr                             ` — 85 files / 25238 LoC
- `airborne-submarine-squadron                        ` — 13 files / 1050 LoC
- `anamnesis                                          ` — 11 files / 13004 LoC
- `aspasia                                            ` — 10 files / 1040 LoC
- `betlang                                            ` — 10 files / 2837 LoC
- `bofig                                              ` — 10 files / 4175 LoC
- `bofj-kitt                                          ` — 70 files / 4218 LoC
- `boj-server                                         ` — 89 files / 25917 LoC
- `burble                                             ` — 94 files / 11625 LoC
- `cloudguard-cli                                     ` — 15 files / 1852 LoC
- `cloudguard-server                                  ` — 13 files / 1694 LoC
- `conative-gating                                    ` — 14 files / 5903 LoC
- `cookie-rebound                                     ` — 54 files / 2238 LoC
- `dictask                                            ` — 54 files / 2197 LoC
- `echidna                                            ` — 75 files / 23198 LoC
- `echo-types                                         ` — 76 files / 18112 LoC
- `eclexia                                            ` — 48 files / 18209 LoC
- `email-octad-experiment                             ` — 70 files / 5565 LoC
- `excel-economic-numbers-tool                        ` — 21 files / 7983 LoC
- `frayed-knot-toolkit                                ` — 54 files / 2238 LoC
- `fraying-model-computational-testbed                ` — 54 files / 2238 LoC
- `game-server-admin                                  ` — 55 files / 2439 LoC
- `gitbot-fleet                                       ` — 23 files / 3511 LoC
- `git-scripts                                        ` — 15 files / 1558 LoC
- `gossamer                                           ` — 61 files / 5382 LoC
- `gv-clade-index                                     ` — 55 files / 2796 LoC
- `http-capability-gateway                            ` — 12 files / 4066 LoC
- `hybrid-automation-router                           ` — 56 files / 2396 LoC
- `hypatia                                            ` — 67 files / 21970 LoC
- `i-human                                            ` — 10 files / 1040 LoC
- `intsoc-transactor                                  ` — 13 files / 1413 LoC
- `januskey                                           ` — 18 files / 6288 LoC
- `julia-the-viper                                    ` — 20 files / 5151 LoC
- `kategoria                                          ` — 61 files / 2694 LoC
- `kategoria-pipeline                                 ` — 54 files / 2238 LoC
- `krl                                                ` — 56 files / 2498 LoC
- `laniakea                                           ` — 10 files / 3494 LoC
- `lcb-website                                        ` — 18 files / 418 LoC
- `llm-grace                                          ` — 72 files / 4841 LoC
- `methodologies                                      ` — 54 files / 2238 LoC
- `mtpc-template-repo                                 ` — 54 files / 2197 LoC
- `my-lang                                            ` — 36 files / 13747 LoC
- `natsci-studio                                      ` — 55 files / 2437 LoC
- `nesy-solver                                        ` — 55 files / 2325 LoC
- `network-ambulance                                  ` — 12 files / 8678 LoC
- `nextgen-languages                                  ` — 10 files / 2387 LoC
- `nextgen-typing                                     ` — 56 files / 2436 LoC
- `npm-avoidant                                       ` — 70 files / 4218 LoC
- `oblibeny                                           ` — 32 files / 16455 LoC
- `ochrance-framework                                 ` — 18 files / 5711 LoC
- `odds-and-sods-package-manager                      ` — 31 files / 5668 LoC
- `paint-type                                         ` — 54 files / 2040 LoC
- `palimpsest-license                                 ` — 43 files / 18101 LoC
- `pandoc-a2ml                                        ` — 56 files / 2498 LoC
- `pandoc-k9                                          ` — 56 files / 2498 LoC
- `panic-attack                                       ` — 12 files / 2839 LoC
- `panll                                              ` — 62 files / 19405 LoC
- `patch-bridge                                       ` — 57 files / 3279 LoC
- `php-aegis                                          ` — 12 files / 4293 LoC
- `proof-burrower                                     ` — 62 files / 4015 LoC
- `protocol-squisher                                  ` — 17 files / 6886 LoC
- `proven                                             ` — 12 files / 3258 LoC
- `proven-servers                                     ` — 16 files / 2316 LoC
- `rattlescript                                       ` — 55 files / 2261 LoC
- `repos-monorepo                                     ` — 17 files / 3037 LoC
- `rsr-template-repo                                  ` — 70 files / 4218 LoC
- `sanctify-php                                       ` — 19 files / 6548 LoC
- `session-sentinel                                   ` — 57 files / 3387 LoC
- `snifs                                              ` — 54 files / 2238 LoC
- `somethings-fishy                                   ` — 56 files / 2498 LoC
- `squeakwell                                         ` — 54 files / 2238 LoC
- `standards                                          ` — 321 files / 22993 LoC
- `standards-as-port                                  ` — 319 files / 22705 LoC
- `stapeln                                            ` — 14 files / 2036 LoC
- `statistease                                        ` — 14 files / 1375 LoC
- `thejeffparadox                                     ` — 11 files / 1058 LoC
- `the-nash-equilibrium                               ` — 80 files / 7398 LoC
- `tma-mark2                                          ` — 13 files / 4024 LoC
- `typed-wasm                                         ` — 59 files / 3405 LoC
- `typell                                             ` — 16 files / 2478 LoC
- `valence-shell                                      ` — 50 files / 23689 LoC
- `vcl-ut                                             ` — 65 files / 6613 LoC
- `verisimdb                                          ` — 51 files / 29597 LoC
- `verisimiser                                        ` — 69 files / 4106 LoC
- `voyage-enterprise-decision-system                  ` — 13 files / 5339 LoC
- `vscode-a2ml                                        ` — 59 files / 2733 LoC
- `vscode-k9                                          ` — 58 files / 2626 LoC
- `wokelang                                           ` — 58 files / 20193 LoC

### CRITICAL — no README at all


### HIGH — stub README (<20 lines)

- `achievements-lab                                   `(2 lines)
- `asdf-tool-plugins                                  `(14 lines)
- `blog-drafts                                        `(17 lines)
- `flatracoon                                         `(19 lines)
- `git-reticulator                                    `(12 lines)
- `ipv6-tools                                         `(17 lines)
- `manifesto                                          `(15 lines)
- `my-lang                                            `(8 lines)
- `sdp-hkdf-deployment                                `(19 lines)
- `tropical-resource-typing                           `(5 lines)

### MEDIUM — good README but no `docs/` directory (55 repos)

These have substantial top-level READMEs (≥20 lines) but no `docs/` directory holding deeper material. Symptom of "README has grown to do the work of docs/". Refactor target: split into README (intro + quickstart only) + `docs/architecture.md`, `docs/usage.md`, etc.

- `a2ml_ex                                            `(README=233 lines)
- `a2ml_gleam                                         `(README=58 lines)
- `action-trust-layers                                `(README=75 lines)
- `agda-stdlib                                        `(README=71 lines)
- `ai-cli-lab                                         `(README=171 lines)
- `anvomidav                                          `(README=84 lines)
- `cafescripto                                        `(README=105 lines)
- `claude-integrations                                `(README=100 lines)
- `claude-memory                                      `(README=80 lines)
- `coord-tui                                          `(README=215 lines)
- `cyo                                                `(README=53 lines)
- `file                                               `(README=156 lines)
- `filesoup                                           `(README=386 lines)
- `format-registrations                               `(README=129 lines)
- `groove-browser-harness                             `(README=139 lines)
- `HOL                                                `(README=82 lines)
- `homebrew-tap                                       `(README=72 lines)
- `humor-ecosystem                                    `(README=64 lines)
- `hyperpolymath-archive                              `(README=56 lines)
- `hyperpolymath.github.io                            `(README=89 lines)
- `info                                               `(README=46 lines)
- `ipv6-site-enforcer                                 `(README=181 lines)
- `jaffascript                                        `(README=98 lines)
- `julia-ecosystem                                    `(README=87 lines)
- `julia-professional-registry                        `(README=79 lines)
- `k9_ex                                              `(README=114 lines)
- `k9_gleam                                           `(README=117 lines)
- `live-files                                         `(README=81 lines)
- `lua-filters                                        `(README=128 lines)
- `lucidscript                                        `(README=100 lines)
- `maa-framework                                      `(README=122 lines)
- `me-dialect                                         `(README=287 lines)
- `nafa-app                                           `(README=236 lines)
- `network-dashboard                                  `(README=274 lines)
- `nickel-augmentation                                `(README=83 lines)
- `patallm-gallery                                    `(README=204 lines)
- `polyglot-formalisms-gleam                          `(README=137 lines)
- `polysafe-gitfixer                                  `(README=216 lines)
- `polystack                                          `(README=61 lines)
- `pseudoscript                                       `(README=98 lines)
- `qubes-sdp                                          `(README=376 lines)
- `rescript-ecosystem                                 `(README=42 lines)
- `robodog-ecm                                        `(README=128 lines)
- `scripts                                            `(README=232 lines)
- `social-media-tools                                 `(README=207 lines)
- `ssg-collection                                     `(README=67 lines)
- `technical-notes                                    `(README=27 lines)
- `tentacles-agentic-syllabus                         `(README=25 lines)
- `the-metadatastician                                `(README=87 lines)
- `tree-sitter-a2ml                                   `(README=136 lines)
- `tree-sitter-k9                                     `(README=140 lines)
- `veridical-simulation-core                          `(README=173 lines)
- `vex-tools                                          `(README=83 lines)
- `wordpress-tools                                    `(README=22 lines)
- `zotero-tools                                       `(README=26 lines)

### Estate-wide hygiene file coverage

| File | Present | Missing | % |
|---|---|---|---|
| CHANGELOG.md | 99 | 180 | 35% |
| CONTRIBUTING.md | 252 | 27 | 90% |
| CODE_OF_CONDUCT.md | 234 | 45 | 84% |
| SECURITY.md | 243 | 36 | 87% |

CONTRIBUTING/CODE_OF_CONDUCT/SECURITY are well covered (likely shipped via the `rsr-template-repo` baseline). **CHANGELOG is the headline gap — 65% of repos have none.**

### Empty `docs/` directories (61 repos)

These have a `docs/` directory but no `.md`/`.adoc`/`.rst` files in it. Either delete the empty dir or seed it with the standard skeleton (architecture, usage, contributing).

### Patterns

1. **Wiki uniformity**: ~50 repos have substantive docs (≥10 files). The pattern they share — index.md, architecture.md, then topic deep-dives — suggests these are the template. Propagating that template to the other ~230 repos would close most of the doc debt with minimal hand-authoring.
2. **README-as-docs anti-pattern**: 55 repos have README ≥20 lines and zero docs/ — the README has absorbed material that belongs in a docs/ tree, hurting both discoverability (the README is too long to skim) and searchability (deep content isn't indexed under its own URL).
3. **CHANGELOG gap**: 180 repos have no CHANGELOG. Even semi-automated CHANGELOG generation (e.g. from conventional commits, or `git-cliff`) would close this.

### Recommended next moves

1. **Standards-PR** (separate, follow-up): add `docs-template/` skeleton to `rsr-template-repo` so new repos start with the heavy-docs structure pre-populated.
2. **CHANGELOG generation**: add a `CHANGELOG.md` skeleton + `git-cliff` config to `governance-reusable.yml` so it's CI-enforced. **180-repo sweep.**
3. **Per-repo PRs** (this audit): each non-OK repo gets a `docs/tech-debt-2026-05-26.md` summarizing its specific gaps + a small first contribution to its docs tree (the `docs-template/` skeleton).
4. **GitHub Wikis**: this scan cannot see GitHub-hosted wikis (separate repos). If a repo has a populated GitHub Wiki, the doc-debt classification here may be overstated. Spot-check before fixing.

### Coverage caveat

This scan counted `.md`/`.adoc`/`.rst` files but did not assess quality. A repo with 50 placeholder `# TODO` files would score "OK" here but actually have severe doc debt. The per-repo PRs ask maintainers to validate.

---

🤖 Generated by Claude Code estate-wide documentation-debt scan (2026-05-26).
Companion docs: `2026-05-26-estate-proof-debt.md`, `2026-05-26-estate-licence-debt.md`.
