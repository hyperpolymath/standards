<!-- SPDX-License-Identifier: MPL-2.0 -->
# `signed-push` — verified commits from CI/AI tooling

Estate composite action that pushes a branch's local commits as GitHub **Verified**
commits, so they satisfy `required_signatures` branch rulesets.

## Why

Commits made by automation with a **PAT** or the default `GITHUB_TOKEN` show as
**Unverified**, and any branch whose ruleset requires signed commits then **cannot
merge** (e.g. `mergeStateStatus: BLOCKED`). The only reliable way for automation to
produce Verified commits is the GraphQL [`createCommitOnBranch`] mutation
authenticated as a **GitHub App installation** — GitHub signs those commits itself.

This action mints an App installation token and delegates the push to
[`Asana/push-signed-commits`], which diffs local vs. remote and replays the local
commits through `createCommitOnBranch`.

## Prerequisites (one-time, account owner)

1. Register a **GitHub App** on the org (Settings → Developer settings → GitHub Apps).
   Repository permissions: **Contents: Read & write**, **Workflows: Read & write**
   (to edit `.github/workflows`), optionally **Administration: Read & write** (for
   `delete_repo`).
2. **Install** the App on the org/repos and **generate a private key** (`.pem`).
3. Store **`APP_ID`** and **`APP_PRIVATE_KEY`** as org (or repo) secrets.
4. If the branch ruleset also enforces **pull-request-only merges** or a **merge
   queue**, add the App to that ruleset's **bypass list** — `required_signatures`
   is satisfied by this action, but those are separate rules.

## Usage

```yaml
jobs:
  update:
    runs-on: ubuntu-latest
    permissions:
      contents: write
    steps:
      - uses: actions/checkout@v4
      - name: Make changes
        run: |
          echo "generated" >> CHANGELOG.md
          git config user.name  "estate-bot[bot]"
          git config user.email "estate-bot[bot]@users.noreply.github.com"
          git commit -am "chore: regenerate changelog"
      - name: Push as Verified
        uses: hyperpolymath/standards/.github/actions/signed-push@main
        with:
          app-id: ${{ vars.APP_ID }}
          private-key: ${{ secrets.APP_PRIVATE_KEY }}
          # branch: defaults to the current ref
```

## Notes

- Pinned to SHAs (`create-github-app-token` v3.2.0, `push-signed-commits` v1) per
  estate supply-chain policy.
- Run **after** local commits exist in the job — the action pushes the delta
  between your local branch and its remote.

[`createCommitOnBranch`]: https://github.blog/changelog/2021-09-13-a-simpler-api-for-authoring-commits/
[`Asana/push-signed-commits`]: https://github.com/Asana/push-signed-commits
