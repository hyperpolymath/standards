#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# soft-attach.sh — reference implementation of [soft-attach] from
# launcher/launcher-standard.a2ml.
#
# Soft-attach = optional ecosystem integrations that the launcher invokes
# IF they are installed, and silently skips otherwise. Downstream
# launchers SHOULD source this script rather than re-implementing the
# "if-installed-then-invoke" pattern, so the spec stays consistent
# across the estate.
#
# All primitives are non-fatal — a missing or failing soft-attach tool
# never breaks the launcher (per the §soft-attach spec: "called if
# present, silently skipped if absent").
#
# Primitives:
#
#   hp_soft_attach_present "command"
#       Returns 0 if the command is on PATH, 1 otherwise. Building
#       block; rarely called directly.
#
#   hp_soft_attach_run "command-line"
#       If the first token of command-line is on PATH, runs the whole
#       line via `bash -c`. Otherwise silent no-op. Suitable for
#       [soft-attach].tools entries that use `command = "..."`.
#       Template substitution ({app-name}, {log-file}, {repo-dir}) is
#       the CALLER's responsibility — substitute before passing in.
#
#   hp_soft_attach_event "tool" "event-name" [extra args...]
#       If `tool` is on PATH, invokes `tool emit event-name [args]`.
#       The `emit` verb is the soft-attach convention for event-style
#       integrations (e.g. feedback-o-tron). Tools that use a different
#       verb should be called via hp_soft_attach_run with the full
#       command line.
#
# Recommended call sites:
#   - on launcher start failure: emit start_failed event to feedback-o-tron;
#     run hypatia diagnose; run panic-attack assail.
#   - on --integ failure: same pattern.
# See the comprehensive-launcher-template.sh for the full hook layout.

hp_soft_attach_present() {
    command -v "${1:?soft-attach: command required}" >/dev/null 2>&1
}

hp_soft_attach_run() {
    local cmd_line="${1:?soft-attach: command line required}"
    local first_token
    first_token=$(printf '%s' "${cmd_line}" | awk '{print $1}')
    if hp_soft_attach_present "${first_token}"; then
        bash -c "${cmd_line}" || true
    fi
}

hp_soft_attach_event() {
    local tool="${1:?soft-attach: tool required}"
    local event="${2:?soft-attach: event-name required}"
    shift 2
    if hp_soft_attach_present "${tool}"; then
        "${tool}" emit "${event}" "$@" || true
    fi
}

# CLI mode (not sourced): provide a thin wrapper for ad-hoc invocation.
#   ./soft-attach.sh run "hypatia diagnose --app foo --log /tmp/foo.log"
#   ./soft-attach.sh event feedback-o-tron launcher:start_failed
#   ./soft-attach.sh present hypatia
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    case "${1:-}" in
        run)     shift; hp_soft_attach_run "$@" ;;
        event)   shift; hp_soft_attach_event "$@" ;;
        present) shift; hp_soft_attach_present "$@" ;;
        *)
            printf 'usage: %s {run|event|present} ...\n' "${0##*/}" >&2
            exit 64  # EX_USAGE
            ;;
    esac
fi
