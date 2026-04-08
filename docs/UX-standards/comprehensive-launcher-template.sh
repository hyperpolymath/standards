#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# 
# ============================================================================
# COMPREHENSIVE LAUNCHER TEMPLATE
# ============================================================================
# 
# D-SIP-FV-MA Compliant: Dependable, Secure, Interoperable, Performant,
# Functional, Versatile, Metaiconic, Accessible
# 
# Purpose: Provide a robust, self-healing launcher for desktop applications
# that handles process management, error recovery, and user feedback.
# 
# Usage: Customize the CONFIGURATION section and implement application-specific
# logic in the MAIN LOGIC section.
# 
# Integration Points:
#   • Feedback-o-tron: Automatic error reporting and telemetry
#   • Hypatia: LLM-assisted troubleshooting and exhausted support
#   • Dustfiles: Targeted automated repair
# 
# ============================================================================

set -euo pipefail

# ============================================================================
# CONFIGURATION - Customize for each application
# ============================================================================
APP_NAME="TemplateApp"                    # Application name
REPO_DIR="/path/to/repo"                 # Repository directory
COMMAND="command to run"                 # Command to execute
URL="http://localhost:PORT"               # URL if web app (empty if not)
PID_FILE="/tmp/${APP_NAME,,}-server.pid"  # PID file
LOG_FILE="/tmp/${APP_NAME,,}-server.log"  # Log file
MODE="${1:---auto}"                      # Default mode

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

log() {
  echo "[$APP_NAME] $1"
}

err() {
  echo "[$APP_NAME] ERROR: $1" >&2
}

is_running() {
  [ -f "$PID_FILE" ] && kill -0 "$(cat "$PID_FILE")" 2>/dev/null
}

wait_for_url() {
  local max_wait=$1
  local waited=0
  
  if [ -z "$URL" ]; then return 0; fi
  
  while [ $waited -lt $max_wait ]; do
    if curl -fsS "$URL" >/dev/null 2>&1; then
      return 0
    fi
    sleep 1
    waited=$((waited + 1))
  done
  
  return 1
}

start_server() {
  if is_running; then
    log "Server already running (PID: $(cat "$PID_FILE"))"
    return 0
  fi
  
  log "Starting $APP_NAME server..."
  
  # Start in background with nohup to prevent process from being killed
  cd "$REPO_DIR"
  nohup $COMMAND >"$LOG_FILE" 2>&1 &
  echo $! > "$PID_FILE"
  
  # Wait for server to be ready
  if ! wait_for_url 15; then
    err "Server did not start within 15 seconds"
    err "Check log: $LOG_FILE"
    err ""
    err "Troubleshooting steps:"
    err "  1. Check if port $APP_PORT is available: 'lsof -i :$APP_PORT'"
    err "  2. Review server logs: 'tail -50 $LOG_FILE'"
    err "  3. Verify dependencies: '$COMMAND --version'"
    err "  4. See documentation: https://github.com/hyperpolymath/standards/blob/main/docs/UX-standards/launcher-standard.adoc"
    err ""
    err "For LLM-assisted troubleshooting:"
    err "  hypatia diagnose --app $APP_NAME --log $LOG_FILE"
    
    # Soft-attach: Report failure to feedback-o-tron if available
    if command -v feedback-o-tron >/dev/null 2>&1; then
      feedback-o-tron --event "launcher:start_failed" \
        --app "$APP_NAME" --url "$URL" --log "$LOG_FILE" \
        --error "Timeout after 15 seconds" 2>/dev/null || true
    fi
    
    return 1
  fi
  
  log "Server started successfully"
  
  # Soft-attach: Try to integrate with feedback-o-tron if available
  if command -v feedback-o-tron >/dev/null 2>&1; then
    feedback-o-tron --event "launcher:server_started" \
      --app "$APP_NAME" --url "$URL" --pid "$(cat "$PID_FILE")" \
      --log "$LOG_FILE" 2>/dev/null || true
  fi
  
  return 0
}

stop_server() {
  if ! is_running; then
    log "No running server found"
    return 0
  fi
  
  log "Stopping $APP_NAME server..."
  kill "$(cat "$PID_FILE")" 2>/dev/null || true
  rm -f "$PID_FILE"
  log "Server stopped"
}

open_browser() {
  if [ -z "$URL" ]; then
    log "No URL configured for this application"
    return 1
  fi
  
  if ! is_running; then
    err "Server is not running"
    return 1
  fi
  
  log "Opening $APP_NAME at $URL..."
  
  if command -v xdg-open >/dev/null 2>&1; then
    xdg-open "$URL" &
  elif command -v firefox >/dev/null 2>&1; then
    firefox "$URL" &
  elif command -v chromium >/dev/null 2>&1; then
    chromium "$URL" &
  else
    warn "No browser found - please manually open: $URL"
    warn ""
    warn "If you're on a headless system or minimal environment:"
    warn "  1. Install a browser: 'sudo dnf install firefox' or 'sudo apt install chromium'"
    warn "  2. Or use curl to test: 'curl -v $URL'"
    warn "  3. See: https://github.com/hyperpolymath/standards/blob/main/docs/UX-standards/launcher-standard.adoc#browser-issues"
  fi
  
  # Soft-attach: Report browser launch attempt
  if command -v feedback-o-tron >/dev/null 2>&1; then
    feedback-o-tron --event "launcher:browser_launch" \
      --app "$APP_NAME" --url "$URL" 2>/dev/null || true
  fi
}

# ============================================================================
# MAIN SWITCH
# ============================================================================

case "$MODE" in
  --start)
    start_server
    ;;
  
  --stop)
    stop_server
    ;;
  
  --status)
    if is_running; then
      log "Server is running (PID: $(cat "$PID_FILE"))"
      if [ -n "$URL" ]; then
        log "URL: $URL"
      fi
    else
      log "Server is not running"
    fi
    ;;
  
  --browser|--web)
    start_server && open_browser
    ;;
  
  --auto|*)
    start_server && open_browser
    ;;
esac
