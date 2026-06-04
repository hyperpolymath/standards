# T3 Archetype Specs
<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->

T3 repos share common proof patterns. Each T3 repo's spec references ONE archetype and lists repo-specific items on top.

## Archetypes

| Archetype | Description | Common Proofs |
|-----------|-------------|---------------|
| [iser](iser.md) | Code generator (`*-iser` repos) | Template substitution safety, output validity, ABI |
| [a2ml-k9](a2ml-k9.md) | A2ML/K9 parsers & validators | Grammar completeness, parser termination, serialisation roundtrip |
| [julia-pkg](julia-pkg.md) | Julia packages (`*.jl`) | Numerical bounds, API contracts, type stability |
| [webapp](webapp.md) | Web applications (ReScript) | State machine, XSS freedom, API contracts |
| [cli-tool](cli-tool.md) | CLI tools | Exit code discipline, idempotence, input validation |
| [security](security.md) | Security/crypto tools | Key handling, access control, injection-free |
| [config-infra](config-infra.md) | Config/DNS/infrastructure | Format validation, idempotence, rollback |
| [game-creative](game-creative.md) | Games/creative | State machine, scoring correctness |
| [integration-sdk](integration-sdk.md) | SDKs/clients | API compliance, auth, type safety |

## Mandatory ABI Proofs (ALL T3 repos)

Every T3 repo inherits these from rsr-template-repo:
- ABI-1: Non-null pointer safety
- ABI-2: Memory layout correctness
- ABI-3: Platform type sizes
- ABI-4: FFI return types
- ABI-5: C ABI compliance

See `~/Desktop/proof-specs/T1-critical/rsr-template-repo.md` for full ABI spec.

## How per-repo specs work

A T3 repo spec looks like:

```markdown
# Proof Spec: <repo-name>

**Archetype**: <archetype-name>
**Extra theorems**: N

## Repo-specific

<list of 0-5 bespoke theorems>
```

This gives LLMs everything needed: the archetype tells them the common proof pattern, the repo-specific list tells them what else to verify.
