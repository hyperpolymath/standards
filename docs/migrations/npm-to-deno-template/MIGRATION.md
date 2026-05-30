# npm → Deno per-repo migration recipe

Canonical procedure for migrating a hyperpolymath estate repository from
`package.json` + npm/Node to `deno.json` + Deno.

Policy: `docs/JS-RUNTIME-POLICY.adoc` (Deno > Bun > pnpm > npm).
Campaign tracker: hyperpolymath/standards#253.
Rule enforcement: hypatia `cicd_rules/nodejs_detected` + `npx_or_npm_run_in_ci`.

## 0. Decide which class the repo is in

Before touching anything, decide which of the migration classes applies.
Each class has a different end-state.

| Class | Signal | End-state |
|---|---|---|
| **A. Pure-Deno port** | Repo's `package.json` only lists dev-only Node-compatible tools (`typescript`, `vitest`, build helpers). No host contract requires Node. | Delete `package.json` + `package-lock.json`. Author `deno.json`. CI workflows swap to `deno test`/`deno task`. |
| **B. npm wrapper via Deno** | Repo wraps an npm-published tool that does not yet have a Deno-native fork (e.g., `rescript`, `vite`, `tailwindcss`). | Keep dependency expressed as `npm:pkg@semver` inside `deno.json`'s `imports`. Tasks call `deno run -A --node-modules-dir=auto npm:pkg`. No `package.json`. |
| **C. Carve-out** | One of the six classes in `cicd_rules/nodejs_detected` `path_allow_prefixes` (VSCode extension, bootstrap shim, upstream fork, archived, vendored, example/fixture). | **Skip migration.** File stays on npm; no PR. |

A given repo with multiple `package.json` files can split across classes — handle each manifest on its own merit.

## 1. Inventory the current `package.json`

```bash
# Capture starting point.
cat package.json
ls -la package-lock.json bun.lockb yarn.lock pnpm-lock.yaml 2>/dev/null
```

Record:

- Direct deps (`dependencies` + `devDependencies`).
- Scripts (`scripts.*`).
- `engines.node`, `engines.npm` — note for replacement by `engines.deno`.
- `private`, `type`, `exports` — preserved as needed.

## 2. Author `deno.json` from the canonical template

Copy `deno.json` from this directory. Adjust:

- `name` — `@hyperpolymath/<repo-name>`.
- `version` — preserve from `package.json`.
- `license` — `MPL-2.0-or-later` (estate default) unless repo policy differs.
- `compilerOptions` — preserve `strict` and friends from `tsconfig.json` if present.
- `imports` — populate from `dependencies`:
  - Deno-native: `"@std/": "https://deno.land/std@0.224.0/"` (and similar).
  - JSR: `"@scope/pkg": "jsr:@scope/pkg@^1.2.3"`.
  - npm fallback: `"pkg": "npm:pkg@^1.2.3"` (Class B only).
- `tasks` — port from `scripts`:
  - `"build": "rescript"` → `"build": "deno run -A --node-modules-dir=auto npm:rescript"`.
  - `"test": "vitest"` → `"test": "deno test -A src/"` (port tests to Deno test API where reachable; if not yet portable, `"test": "deno run -A --node-modules-dir=auto npm:vitest"`).
- `nodeModulesDir`:
  - Default `"none"` (Class A — pure Deno).
  - Set to `"auto"` only when an npm package's lifecycle requires it (Class B; rescript and most ESM-shipped npm packages are fine without it).

## 3. Delete the npm scaffolding

```bash
git rm package.json package-lock.json
# Also remove bun.lockb / yarn.lock / pnpm-lock.yaml / .npmrc if present.
git rm -f bun.lockb yarn.lock pnpm-lock.yaml .npmrc 2>/dev/null || true

# node_modules/ should already be in .gitignore.
rm -rf node_modules
```

## 4. Update `.gitignore`

Confirm these entries are present (RSR canonical template propagates them — see `docs/JS-RUNTIME-POLICY.adoc §Canonical .gitignore Entries`):

```
# npm-avoidant (standards#67): estate JS-runtime policy is Deno>Bun>pnpm>npm.
package-lock.json
**/package-lock.json
node_modules/
**/node_modules/
bun.lockb
yarn.lock
pnpm-lock.yaml
```

## 5. Migrate CI workflows

Search for any of these and replace:

| Before | After |
|---|---|
| `actions/setup-node@<sha>` | `denoland/setup-deno@<sha>` (or remove if no JS step remains) |
| `npm ci` / `npm install` | `deno cache <entrypoint>` (often unnecessary — Deno caches at first run) |
| `npm test` / `npm run test` | `deno task test` (or `deno test -A src/`) |
| `npx <tool>` | `deno run -A --node-modules-dir=auto npm:<tool>` (Class B) or Deno-native equivalent (Class A) |

Note: hypatia `cicd_rules/npx_or_npm_run_in_ci` blocks `npx` and `npm run` in CI run-blocks (added 2026-05-28). Don't leave any.

## 6. Verify locally

```bash
deno check src/
deno lint src/
deno fmt --check src/
deno test -A src/
```

Class B (npm wrapper) — exercise the wrapped tool end-to-end:

```bash
deno task build
deno task test
```

## 7. Commit pattern

```bash
git add deno.json .gitignore .github/workflows/
git rm package.json package-lock.json
git commit -m "feat(deno): migrate <repo> npm → Deno (standards#253)

<class-specific summary: deleted N npm manifests, added deno.json with
M imports, ported K tasks, CI now runs on deno test>

Class: A | B  (per docs/migrations/npm-to-deno-template/MIGRATION.md)
Carry-forward: <any blocked Deno-native equivalent, e.g. \"vitest not
portable until deno-test gets <feature>\">"
```

## 8. PR + auto-merge

Per estate convention: auto-merge with squash.

```bash
gh pr create --title "feat(deno): <repo> npm → Deno (standards#253)" \
             --body  "<paste commit body + cross-ref campaign>"
gh pr merge --auto --squash --delete-branch
```

## Carry-forward patterns observed in oikos Phase 5 + 5 follow-ups (memory)

- **ReScript wrapping** (canonical Class B): `deno run -A --node-modules-dir=auto npm:rescript@^12.0.0`. `--allow-scripts=npm:rescript` when the install lifecycle requires it.
- **Tailwind / vite / esbuild** — same pattern as rescript: `npm:<tool>@<semver>`, `--node-modules-dir=auto`.
- **`type: "module"` repos** — Deno is ESM-native, no extra step.
- **`exports` field** — preserve in `deno.json` if the package is published; otherwise drop.

## Anti-patterns

- ❌ Don't keep `package.json` "for tooling only" — `deno.json` covers fmt/lint/test/tasks.
- ❌ Don't fall back to `npm:` specifiers when a JSR or Deno-native equivalent exists (use `deno info <pkg>` to check).
- ❌ Don't commit `node_modules/` even on Class B — `--node-modules-dir=auto` regenerates at run-time.
- ❌ Don't add `"engines": {"node": "..."}` to `deno.json` — Deno doesn't honour it and it signals the repo isn't fully migrated.

## When migration is blocked

If a `package.json` cannot be removed (host-required, Node-only library, npm publish target), the path goes in the hypatia rule's `path_allow_prefixes` instead. See `standards/.claude/CLAUDE.md §npm Exemptions (Approved)` for the canonical exemption table.
