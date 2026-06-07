// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>

# Decision: Hypatia As Estate Control-Plane Observer

Date: 2026-06-06

Status: proposed

## Context

The standards repository owns reusable workflow glue that many repositories in
the estate consume. During the `echidna` Hypatia triage, a key failure mode was
not in the source repo itself: the standards reusable Hypatia workflow carried
an embedded SARIF converter that could diverge from Hypatia's native SARIF
renderer. That converter was part of a self-echo path in which Hypatia could
read GitHub code-scanning alerts and then re-upload mirror findings as new
Hypatia alerts.

The standards layer is therefore not passive documentation. It is estate
control-plane infrastructure.

Related issue: <https://github.com/hyperpolymath/standards/issues/378>

## Decision

Standards should treat reusable workflows, embedded converters, SARIF
categories, and bot/farm interfaces as control-plane components. They must
preserve scanner semantics rather than re-implementing them in ways that drift.

For Hypatia integration this means:

- prefer Hypatia's native SARIF renderer when practical;
- when an embedded converter is needed, mirror Hypatia's meta-rule suppression;
- include machine-readable metadata: finding ID, category, class, route, and
  dispatch safety;
- keep empty SARIF upload behavior so stale alerts can clear;
- document the route from reusable workflow to GitHub code scanning to
  gitbot-fleet and `.git-private-farm`.

## Estate Roles

| Component | Role |
| --- | --- |
| Hypatia | Observe repo and environment; classify findings; produce work orders |
| standards | Provide reusable workflow and reporting surfaces; prevent converter drift |
| echidna | Concrete repository under triage; source of deposited evidence reports |
| gitbot-fleet | Bot intake and PR/review execution |
| `.git-private-farm` | Rate-limited estate fanout and canary orchestration |
| repossystem | Portfolio map tying repos, workflows, bots, and control surfaces together |

## Rules For Reusable Workflows

Reusable workflows should be conservative:

- no direct destructive action;
- no new auto-execute route without canary and rollback;
- no unstructured scanner output if structured metadata is available;
- no duplication of a scanner's public alert surface without dedupe;
- no silent failure of the reporting path;
- no mass fanout from a newly introduced rule.

## Action Economy

The standards layer should help preserve GitHub Actions credit and human
attention:

- if CodeQL, Scorecard, Dependabot, governance, or Hypatia is already running,
  downstream agents should be able to wait for and consume the result;
- if another tool is better suited to a finding, Hypatia should be able to hand
  off or reformat the finding for that tool;
- if another tool is producing a bad or incomplete fix, Hypatia should hold or
  route the work for review;
- repeated findings should dedupe by stable ID, not re-open equivalent work.

## Implication For `Git in the Time of NeSy Agency`

This decision frames the estate as a NeSy agency portfolio rather than a set of
independent repos. The standards repo provides common reflexes; Hypatia provides
environment-aware observation; the fleet and farm provide controlled action.
The book should treat this as a nervous-system/control-plane pattern: observe,
classify, route, wait or act, verify, and learn.
