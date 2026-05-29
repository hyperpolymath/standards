#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# keepopen.sh — standard desktop launcher fallback ladder.
#
# Canonical location: developer-ecosystem/standards/launcher/keepopen.sh
# Deployed copy:      .desktop-tools/keepopen.sh (symlinked)
# Documented in:      standards/docs/UX-standards/launcher-standard.adoc §Fallback Ladder
#
# Its job is to turn a possibly-broken launcher into something that ALWAYS
# lands the user somewhere useful — even when every upstream hook fails.
#
# Usage:
#     keepopen.sh APP_NAME REPO_DIR "GUI_CMD" "TUI_CMD" [LOG_FILE]
#
# Fallback ladder (each fallback shows a LOUD banner so the failure is
# visible — the point is that the user CAN see a tool is broken):
#
#     1. GUI_CMD   — primary path. Silent on success. If it fails ↓
#     2. TUI_CMD   — loud yellow banner, then fallback. If it fails ↓
#     3. bash -l   — loud red banner, then cd into REPO_DIR and drop into
#                    an interactive login shell. Never just "press enter
#                    to close" — the user lands in the repo so they can
#                    actually fix the thing that's broken.
#
# Each CMD is evaluated as `bash -c "$cmd"`, so pipelines and shell quoting
# work normally. Pass an empty string to skip a stage (e.g. an app with no
# GUI can use `""` for GUI_CMD and go straight to the TUI banner → TUI).
#
# Banners are intentionally loud and ugly — visibility beats aesthetics.

set -u

APP_NAME="${1:?keepopen: APP_NAME required (arg 1)}"
REPO_DIR="${2:?keepopen: REPO_DIR required (arg 2)}"
GUI_CMD="${3:?keepopen: GUI_CMD required (arg 3) — pass '' if not applicable}"
TUI_CMD="${4:?keepopen: TUI_CMD required (arg 4) — pass '' if not applicable}"
LOG_FILE="${5:-}"

# Honour NO_COLOR (https://no-color.org/) and auto-detect non-TTY stdout.
# When set, banners and prefix labels emit no ANSI escapes — still loud
# and clearly labelled, just plain text. The freedesktop-style desktop
# launch redirects stdout to a real terminal so this rarely triggers
# automatically, but covers `keepopen.sh ... | tee` and CI captures.
if [[ -n "${NO_COLOR:-}" ]] || [[ ! -t 1 ]]; then
    C_RED='' C_YEL='' C_CYA='' C_GRN='' C_BOLD='' C_RST=''
else
    C_RED=$'\033[1;31m'
    C_YEL=$'\033[1;33m'
    C_CYA=$'\033[1;36m'
    C_GRN=$'\033[1;32m'
    C_BOLD=$'\033[1m'
    C_RST=$'\033[0m'
fi

banner() {
    # $1 = colour; $2 = title; remaining args = body lines.
    local colour="$1"; shift
    local title="$1"; shift
    echo
    echo "${colour}${C_BOLD}================================================================${C_RST}"
    echo "${colour}${C_BOLD}  ${title}${C_RST}"
    echo "${colour}${C_BOLD}================================================================${C_RST}"
    local line
    for line in "$@"; do
        [[ -z "${line}" ]] && { echo; continue; }
        echo "  ${colour}${line}${C_RST}"
    done
    echo
}

# -----------------------------------------------------------------------------
# STAGE 1 — GUI
# -----------------------------------------------------------------------------

gui_exit=0
if [[ -n "${GUI_CMD}" ]]; then
    echo "${C_CYA}[keepopen:${APP_NAME}] GUI → ${GUI_CMD}${C_RST}"
    bash -c "${GUI_CMD}"
    gui_exit=$?
    if [[ ${gui_exit} -eq 0 ]]; then
        exit 0
    fi
    banner "${C_YEL}" "FALLBACK 1/2 — GUI FAILED (exit ${gui_exit})" \
        "APP     : ${APP_NAME}" \
        "GUI cmd : ${GUI_CMD}" \
        "${LOG_FILE:+LOG FILE: ${LOG_FILE}}" \
        "" \
        "The primary GUI path exited non-zero." \
        "Something needs fixing. Falling back to the TUI path." \
        "(If this keeps happening, edit the .desktop file or the" \
        "keepopen invocation to point at a working GUI command.)"
else
    banner "${C_YEL}" "STAGE 1/2 SKIPPED — NO GUI CONFIGURED" \
        "APP : ${APP_NAME}" \
        "" \
        "This app was launched with no GUI command. Going straight to TUI."
fi

# -----------------------------------------------------------------------------
# STAGE 2 — TUI
# -----------------------------------------------------------------------------

tui_exit=0
if [[ -n "${TUI_CMD}" ]]; then
    echo "${C_CYA}[keepopen:${APP_NAME}] TUI → ${TUI_CMD}${C_RST}"
    bash -c "${TUI_CMD}"
    tui_exit=$?
    if [[ ${tui_exit} -eq 0 ]]; then
        exit 0
    fi
    banner "${C_RED}" "FALLBACK 2/2 — TUI ALSO FAILED (exit ${tui_exit})" \
        "APP     : ${APP_NAME}" \
        "GUI cmd : ${GUI_CMD:-<none>}" \
        "TUI cmd : ${TUI_CMD}" \
        "${LOG_FILE:+LOG FILE: ${LOG_FILE}}" \
        "REPO    : ${REPO_DIR}" \
        "" \
        "BOTH the GUI and the TUI paths failed." \
        "Something needs fixing — you are being dropped into a shell" \
        "at the repo root so you can investigate, not just closed out."
else
    banner "${C_RED}" "STAGE 2/2 SKIPPED — NO TUI CONFIGURED" \
        "APP : ${APP_NAME}" \
        "REPO: ${REPO_DIR}" \
        "" \
        "No TUI command was provided either. Dropping into a shell at the repo root."
fi

# -----------------------------------------------------------------------------
# STAGE 3 — interactive shell at repo root (final fallback)
# -----------------------------------------------------------------------------

if [[ -d "${REPO_DIR}" ]]; then
    cd "${REPO_DIR}" || true
    echo "${C_GRN}[keepopen:${APP_NAME}] Dropping into bash at ${REPO_DIR}${C_RST}"
else
    echo "${C_RED}[keepopen:${APP_NAME}] REPO_DIR does not exist: ${REPO_DIR}${C_RST}" >&2
    echo "${C_RED}[keepopen:${APP_NAME}] Staying in ${PWD} instead.${C_RST}" >&2
fi

cat <<EOF

${C_YEL}${C_BOLD}Hints:${C_RST}
  - You are in this shell because the launcher's GUI/TUI paths failed.
  - ${LOG_FILE:+Check the log file: ${LOG_FILE}}
  - Type 'exit' or Ctrl-D to close this window.
  - Type 'ls', 'cat README.adoc', or 'just --list' to investigate.

EOF

exec bash -l
