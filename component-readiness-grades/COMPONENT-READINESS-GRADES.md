<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk> -->

# Component Readiness Grades (CRG)

**Standard:** Component Readiness Grades v1.0  
**Author:** Jonathan D.A. Jewell  
**Date:** 2026-02-28  
**Status:** Active  
**License:** PMPL-1.0-or-later  
**Part of:** Rhodium Standard Repositories (RSR)

---

## Abstract

Component Readiness Grades (CRG) is a general-purpose quality assessment scheme
for software components, features, subcommands, modules, APIs, and libraries.
It provides a uniform vocabulary for communicating the readiness of individual
components within a project, mapping each grade to a release stage and requiring
specific evidence thresholds for each level.

This standard is designed to be adopted by any software project regardless of
language, framework, or domain. It is part of the Rhodium Standard Repositories
(RSR) family of standards maintained by hyperpolymath.

---

## 1. Scope

This standard applies to:

- Individual software components (subcommands, modules, features, APIs,
  libraries, plugins, integrations).
- Any project that wishes to communicate the readiness of its parts with
  precision and honesty.
- Internal assessment (development planning) and external communication
  (release documentation, changelogs, user-facing quality indicators).

This standard does NOT apply to:

- Whole-project grading. Projects are collections of components; grade each
  component individually.
- Third-party dependency assessment. Grade your integration with a dependency,
  not the dependency itself.
- Non-software artifacts (documentation, design assets) unless the project
  chooses to extend the scheme.

---

## 2. Normative References

- **RSR (Rhodium Standard Repositories):** The repository quality framework
  within which CRG operates.
- **Semver 2.0.0:** CRG is orthogonal to semantic versioning. A component's
  CRG grade tracks validation evidence; semver tracks API compatibility.

---

## 3. Terms and Definitions

- **Component:** A discrete, assessable unit of software. This may be a CLI
  subcommand, a library module, a feature, an API endpoint, a plugin, or any
  other unit that can be tested and evaluated independently.
- **Home context:** The project's own codebase, configuration, workflow, and
  use cases. The environment in which the component was developed.
- **Dogfooding:** Using the component on the project itself.
- **Broad validation:** Testing the component on at least six diverse,
  unrelated targets outside the home context.
- **Field-proven:** Demonstrated value through real-world external use with
  feedback from users outside the development team.
- **Diverse targets:** Targets that differ in ways that matter for the
  component under test. Six variations of the same thing do not constitute
  diversity.

---

## 4. Grade Definitions

### 4.1. Grade X — Untested

**Release stage:** None

No testing has been performed. The component's status is completely unknown.
Not even a smoke test has been run. This is the default state for any new
component that has not yet been evaluated.

**Examples:**

- A subcommand that was written but never invoked after the initial
  implementation.
- A library module that compiles but has never been exercised against real
  input.
- A feature that exists in code but was never demonstrated to a user or
  developer.

**Evidence required:** None. This grade represents the absence of evidence.

### 4.2. Grade F — Harmful / Wasteful

**Release stage:** Reject, deprecate, or delegate

Tested and found to be actively harmful, a significant opportunity cost, a
waste of resources, offering nothing helpful, or redundant because someone
else does the job better and the effort should be redirected. The component
does more harm than good.

Grade F is not merely "bad quality." It encompasses strategic assessment:
even a technically functional component earns an F if the time spent
maintaining it would be better invested elsewhere, or if an existing external
tool already solves the problem more effectively.

**Examples:**

- A subcommand that silently corrupts data under certain conditions.
- A feature that duplicates what an established external tool already does,
  but worse, and maintaining it diverts effort from the project's actual
  value proposition.
- A module that introduces a heavy dependency tree for marginal benefit.
- A component whose maintenance burden exceeds its utility to any known user.

**Evidence required:**

- Documented test results showing harm, waste, or redundancy.
- Comparison with alternatives (if the F grade is for opportunity cost or
  delegation).
- A clear statement of why the component should be rejected, deprecated, or
  delegated.

### 4.3. Grade E — Minimal / Salvageable

**Release stage:** Pre-alpha (needs redesign or major work)

Does something slight. The component could be salvageable with significant
rework, but it is currently barely functional or useful. There is a kernel of
value, but it is buried under incomplete implementation, poor design, or
fundamental gaps.

**Examples:**

- A parser that handles the happy path but crashes on any malformed input.
- A CLI subcommand that works for one specific file format but fails on all
  others.
- A feature that produces output, but the output is frequently wrong or
  misleading.

**Evidence required:**

- At least one successful test case demonstrating the kernel of functionality.
- Documentation of known failures and limitations.
- A rough assessment of what rework would be needed to reach grade D.

### 4.4. Grade D — Partial / Inconsistent

**Release stage:** Alpha

Works on some inputs, some cases, or some configurations, but not
systematically. The component either needs to be narrowed in scope (so that
its documented capabilities match its actual capabilities) or needs the
inconsistencies fixed. This is the minimum grade for an Alpha release.

**Examples:**

- A formatter that handles 4 out of 7 supported languages correctly.
- A database driver that works with PostgreSQL but silently drops connections
  with MySQL.
- A validation module that catches 60% of invalid inputs but passes the rest.

**Evidence required:**

- A matrix of tested scenarios showing where the component succeeds and fails.
- Documented scope: what it claims to do vs. what it actually does.
- At least one test per claimed capability (some will be failing — that is
  expected at grade D).

### 4.5. Grade C — Self-Validated

**Release stage:** Beta

Tested on the tool or project itself (dogfooding). The component works
reliably in the home context. "Home context" means the project's own
codebase, configuration, workflow, and use cases. This is the minimum grade
for a Beta release.

The key distinction from grade D is reliability: at grade C, the component
does not just work on some things — it works consistently on everything
within its home context.

**Examples:**

- A linter that is run on its own codebase in CI and catches real issues.
- A migration tool that was used to migrate the project's own database schema.
- A CLI subcommand that the development team uses daily in their own workflow.

**Evidence required:**

- The component is actively used on the project itself (dogfooding).
- CI integration or equivalent automated validation in the home context.
- No known failures within the home context (failures outside it are
  acceptable and expected at this stage).

### 4.6. Grade B — Broadly Validated

**Release stage:** Release Candidate

Tested on at least six disparate, unrelated targets. The component
demonstrates breadth and generality. The six targets MUST be genuinely
diverse — not six variations of the same thing. This is the minimum grade
for a Release Candidate.

The number six is deliberate: it is enough to reveal assumptions baked into
the home context without being so high that it becomes a barrier to progress.

**Examples:**

- A code formatter tested on six open-source projects in different languages,
  of different sizes, with different coding styles.
- A container scanner tested against images from six different base
  distributions, frameworks, and deployment patterns.
- A database migration tool tested on six schemas of varying complexity from
  unrelated domains.

**Evidence required:**

- A list of the six (or more) targets with brief descriptions of why they
  are diverse.
- Test results for each target (pass/fail, with notes on any issues found
  and resolved).
- Evidence that issues discovered during broad validation were fed back into
  the component.

### 4.7. Grade A — Field-Proven

**Release stage:** Stable

Real-world feedback has been amassed from external use. The component has been
shown to do no harm in the wild. It is actually useful to people outside the
development team. This is the minimum grade for a Stable release.

**Grade A does NOT mean:**

- Perfection. There is always more to do.
- Completion. New features can still be added.
- Freedom from bugs. Bugs will exist; what matters is that the component has
  demonstrated net positive value.

**Grade A DOES mean:**

- External users have used it and provided feedback.
- The component has not regressed under real-world conditions.
- It has earned its grade through demonstrated value, and it has not lost
  that grade through harm or neglect.

**Examples:**

- A CLI tool with issue reports from external users, where reported bugs
  were triaged and the tool continued to deliver value.
- A library published to a registry with downloads and usage reports from
  independent projects.
- A feature that external contributors have built upon or integrated into
  their own workflows.

**Evidence required:**

- Real-world usage data (downloads, issue reports, user testimonials,
  external integrations).
- Evidence of feedback incorporation (issues addressed, documentation
  improved based on user confusion, etc.).
- No unresolved reports of the component causing harm in external
  environments.

---

## 5. Release Stage Mapping

| Grade | Release Stage      | Meaning                                           |
|-------|--------------------|---------------------------------------------------|
| X     | —                  | Not assessed                                      |
| F     | —                  | Reject / deprecate / delegate                     |
| E     | —                  | Pre-alpha (needs redesign or major work)           |
| D     | Alpha              | Functional but incomplete or inconsistent          |
| C     | Beta               | Self-validated, reliable in home context           |
| B     | Release Candidate  | Broadly validated across diverse targets           |
| A     | Stable             | Field-proven with real-world feedback              |

---

## 6. Assessment Guidelines

### 6.1. Core Principles

**Principle 1: Assess components, not projects.** A project is a collection of
components. Each component gets its own grade. A project with ten A-grade
components and one F-grade component is not an A-grade project — it is a
project with a clear candidate for deprecation.

**Principle 2: Evidence over intuition.** Every grade above X requires
evidence. "I think it works" is not evidence. "I ran it on X and here is
what happened" is evidence. The evidence bar rises with each grade.

**Principle 3: Grades are earned and can be lost.** A component at grade A
can be demoted if it regresses, if external feedback reveals harm, or if the
ecosystem shifts and an alternative becomes clearly superior. Grades are not
permanent awards.

**Principle 4: Honest assessment over aspirational grading.** Grade the
component as it is today, not as you hope it will be next week. A component
honestly graded D is more valuable than one dishonestly graded B, because the
honest grade tells you where to focus effort.

### 6.2. Assessment Checklist

When grading a component, answer these questions in order:

1. **Has it been tested at all?** (No → X)
2. **Does it cause harm, waste resources, or duplicate something better?** (Yes → F)
3. **Does it do something, however slight?** (Barely → E)
4. **Does it work on some things but not others?** (Partial → D)
5. **Does it work reliably on our own project?** (Dogfooded → C)
6. **Has it been tested on 6+ diverse external targets?** (Broad → B)
7. **Do external users confirm it works and is useful?** (Field-proven → A)

### 6.3. When to Assess

- **Before any release:** All components included in the release MUST be
  graded.
- **After significant changes:** If a component is substantially rewritten,
  re-assess from X (unless the rewrite preserved all existing test evidence).
- **Periodically:** At least once per release cycle, review all grades for
  staleness.

### 6.4. Recording Assessments

Assessments SHALL be recorded in a durable, version-controlled location.
Recommended locations (in order of preference):

1. A `READINESS.md` file in the project root.
2. A section in the project's `.machine_readable/STATE.scm` file.
3. Inline in the component's own documentation.

### 6.5. Communicating Grades Externally

When communicating grades to users:

- **A and B:** Safe to advertise. These grades have external evidence.
- **C:** Appropriate for beta documentation. Be clear about the scope of
  validation.
- **D:** Appropriate for alpha documentation. Be explicit about known gaps.
- **E, F, X:** Internal only. Do not ship components at these grades unless
  clearly marked as experimental or deprecated.

---

## 7. Grade Transitions

### 7.1. Promotion Criteria

| From | To | What Is Needed                                                  |
|------|----|-----------------------------------------------------------------|
| X    | E  | Run at least one test. Document what happened.                  |
| X    | F  | Evaluate and determine the component is harmful or wasteful.    |
| E    | D  | Fix the most critical failures. Document the scope.             |
| D    | C  | Dogfood it. Use it on your own project. Fix what breaks.        |
| C    | B  | Test on 6+ diverse external targets. Fix what breaks.           |
| B    | A  | Ship it. Collect external feedback. Demonstrate no harm.        |

**Skipping grades:** A component MAY skip grades if the evidence supports it.
A brand-new component that is immediately dogfooded and works can go straight
from X to C. A component tested on 10 external targets before any internal
use could go from X to B. The grades describe evidence thresholds, not
mandatory sequential steps.

### 7.2. Demotion Criteria

| From | To | When                                                             |
|------|----|------------------------------------------------------------------|
| A    | B  | External feedback dries up or reveals the component is no longer |
|      |    | useful in the field. No active external users remain.            |
| A    | F  | External feedback reveals the component causes harm.             |
| B    | C  | Broad validation targets reveal failures that are not fixed.     |
| C    | D  | The home context changes and the component no longer works       |
|      |    | reliably in it.                                                  |
| C    | F  | Dogfooding reveals the component is a net negative.              |
| D    | E  | The scope narrows so far that the component barely does anything.|
| Any  | F  | A better external alternative emerges and maintaining this       |
|      |    | component is now pure opportunity cost.                          |

**Demotion is not punishment.** It is an honest reassessment. A component
demoted from B to C is a component that needs more diverse testing, not a
component that has failed.

---

## 8. Template Assessment Table

Projects adopting CRG SHOULD include an assessment table. The following
templates may be copied and adapted.

### 8.1. Compact Table

```markdown
## Component Readiness Assessment

| Component           | Grade | Release Stage | Evidence Summary                     | Last Assessed |
|---------------------|-------|---------------|--------------------------------------|---------------|
| `example-command`   | C     | Beta          | Dogfooded in CI since 2026-01.       | 2026-02-28    |
| `parse-module`      | D     | Alpha         | Works on JSON/YAML, fails on TOML.   | 2026-02-28    |
| `export-feature`    | X     | —             | Not yet tested.                      | 2026-02-28    |
| `legacy-formatter`  | F     | —             | prettier does this better; removing. | 2026-02-28    |
```

### 8.2. Extended Template (with promotion path)

```markdown
## Component Readiness Assessment (Extended)

### `example-command`

- **Grade:** C (Beta)
- **Last assessed:** 2026-02-28
- **Evidence:** Used in our own CI pipeline since 2026-01-15. No failures in
  home context. 47 successful runs logged.
- **Known limitations:** Only tested on Linux x86_64. No macOS or ARM testing.
- **Promotion path to B:** Test on 6 diverse external projects. Candidates:
  project-alpha (Rust, large), project-beta (Python, small), project-gamma
  (mixed monorepo), project-delta (embedded C), project-epsilon (Gleam/BEAM),
  project-zeta (legacy Java).
- **Demotion risk:** Low. Home context is stable.
```

### 8.3. Guile Scheme Format (for STATE.scm integration)

```scheme
(component-readiness
  (version "1.0")
  (assessed "2026-02-28")
  (components
    (component
      (name "example-command")
      (grade C)
      (release-stage "beta")
      (evidence "Dogfooded in CI since 2026-01. 47 successful runs.")
      (promotion-path "Test on 6+ diverse external projects"))
    (component
      (name "parse-module")
      (grade D)
      (release-stage "alpha")
      (evidence "Works on JSON/YAML, fails on TOML.")
      (promotion-path "Fix TOML parsing, then dogfood"))))
```

---

## 9. Informative Notes

### 9.1. Relationship to Semver

CRG is orthogonal to semantic versioning. Semver tracks API compatibility.
CRG tracks quality and validation evidence. A component can be at semver
3.0.0 and grade D (if it has never been broadly validated), or at semver
0.1.0 and grade A (if it shipped early and accumulated real-world feedback).

### 9.2. Third-Party Dependencies

This scheme is for components you maintain. You SHOULD NOT grade third-party
dependencies themselves. However, you MAY assess your *integration* with a
third-party dependency — not the dependency itself, but how well your code
uses it.

### 9.3. Grade Permanence

No grade is permanent. Grade F is not a death sentence: a component graded F
because a better alternative existed can be re-evaluated if that alternative
disappears or degrades. A component graded F for causing harm can be
re-evaluated after a redesign. Grade F means "stop investing in this as it
currently stands," not "this idea is forever worthless."

Grade A is not a trophy: a component can lose its A grade through regression,
neglect, or ecosystem changes that render it obsolete.

### 9.4. Skipping from X to A

In theory, a component can go from X directly to A if it ships immediately
and external feedback is positive. In practice, this almost never happens.
The grades are evidence thresholds, not sequential gates, but accumulating
A-level evidence without passing through intermediate stages is extremely
unlikely.

---

## 10. Conformance

A project conforms to CRG if:

1. Each assessable component has an assigned grade from the set
   {X, F, E, D, C, B, A}.
2. Each grade above X is supported by the evidence described in section 4.
3. Assessments are recorded in a version-controlled location (section 6.4).
4. Assessments are reviewed at least once per release cycle (section 6.3).
5. Release stages respect the minimum grade thresholds in section 5.

---

## Revision History

| Version | Date       | Author                  | Changes          |
|---------|------------|-------------------------|------------------|
| 1.0     | 2026-02-28 | Jonathan D.A. Jewell    | Initial release  |
