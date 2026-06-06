#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# gui-error.sh — reference implementation of [error-visibility] from
# launcher/launcher-standard.a2ml.
#
# When the launcher runs in a GUI context (no TTY + DISPLAY or
# WAYLAND_DISPLAY set), errors written only to stderr disappear: the
# user sees a brief terminal flash and nothing else. This script
# surfaces such errors via a graphical dialog AND stderr.
#
# Downstream launchers SHOULD source this script and call
#     hp_gui_error "Title" "message"
# rather than re-implementing the dialog ladder.
#
# Dialog ladder (matches [error-visibility].gui-dialog-chain):
#   1. kdialog       — KDE Plasma
#   2. zenity        — GNOME / Cinnamon / Xfce
#   3. notify-send   — libnotify (less prominent than a dialog, but standard)
#   4. xmessage      — X11 last resort
#
# Returns 0 if any dialog succeeded, non-zero if all failed. stderr is
# always written regardless (per [error-visibility].always-also-to-stderr).
#
# Env overrides:
#   NO_GUI_ERROR=1   — suppress the dialog attempt; stderr only.
#                      Useful in CI / scripted invocation.

hp_gui_error() {
    local title="${1:-Error}"
    local message="${2:-}"

    # Always write to stderr regardless of dialog outcome.
    printf '[%s] %s\n' "${title}" "${message}" >&2

    # Skip dialogs when we have a TTY (the user will see stderr fine)
    # or when explicitly suppressed.
    if [[ -t 2 ]] || [[ -n "${NO_GUI_ERROR:-}" ]]; then
        return 0
    fi

    # GUI requires a display.
    if [[ -z "${DISPLAY:-}" ]] && [[ -z "${WAYLAND_DISPLAY:-}" ]]; then
        return 1
    fi

    # Try the ladder; first present + successful wins.
    if command -v kdialog >/dev/null 2>&1; then
        kdialog --title "${title}" --error "${message}" >/dev/null 2>&1 && return 0
    fi
    if command -v zenity >/dev/null 2>&1; then
        zenity --title="${title}" --error --text="${message}" >/dev/null 2>&1 && return 0
    fi
    if command -v notify-send >/dev/null 2>&1; then
        notify-send -u critical "${title}" "${message}" >/dev/null 2>&1 && return 0
    fi
    if command -v xmessage >/dev/null 2>&1; then
        xmessage -title "${title}" -center "${message}" >/dev/null 2>&1 && return 0
    fi

    return 1
}

# CLI mode (not sourced): forward args to hp_gui_error.
#   ./gui-error.sh "Launcher failed" "Server died — see ~/.local/state/app/server.log"
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    hp_gui_error "$@"
fi
