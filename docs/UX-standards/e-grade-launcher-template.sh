#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# E-GRADE LAUNCHER TEMPLATE
# Elite, Efficient, Elegant, Easy, Self-Healing, Self-Diagnostic, Soft-Attach

set -euo pipefail

# ============================================================================
# CONFIGURATION - Customize for each application
# ============================================================================
APP_NAME="TemplateApp"
APP_DIR="/path/to/app"
APP_PORT=4000
APP_URL="http://localhost:$APP_PORT"
LOG_FILE="/tmp/${APP_NAME,,}-launcher.log"

# ============================================================================
# CORE FUNCTIONS - Standard E-grade functionality
# ============================================================================

log() { echo -e "\033[0;32m[$APP_NAME]\033[0m $1"; }
warn() { echo -e "\033[0;33m[$APP_NAME]\033[0m $1"; }
err() { echo -e "\033[0;31m[$APP_NAME]\033[0m $1" >&2; }

is_running() {
  curl -s -o /dev/null -w "%{http_code}" "$APP_URL/" 2>/dev/null | grep -q "200\|404\|500"
}

start_server() {
  if is_running; then
    log "Server already running on $APP_URL"
    return 0
  fi

  log "Starting $APP_NAME server..."
  
  # Use nohup for reliable background process management
  cd "$APP_DIR"
  nohup command_to_start_server >"$LOG_FILE" 2>&1 &
  
  # Wait for server to be ready
  wait_for_server 15 || return 1
  
  log "Server started successfully at $APP_URL"
}

wait_for_server() {
  local max_wait=$1
  local waited=0
  
  while [ $waited -lt $max_wait ]; do
    if is_running; then
      return 0
    fi
    sleep 1
    waited=$((waited + 1))
  done
  
  err "Server did not start within $max_wait seconds"
  err "Check log: $LOG_FILE"
  return 1
}

open_browser() {
  if ! is_running; then
    warn "Server not running - browser will open to $APP_URL when ready"
  else
    log "Opening $APP_NAME at $APP_URL..."
  fi
  
  # Try multiple browser options with error handling
  if command -v xdg-open >/dev/null 2>&1; then
    if ! xdg-open "$APP_URL" 2>/dev/null; then
      warn "Failed to open browser via xdg-open"
      log "Please manually open: $APP_URL"
    fi
  elif command -v firefox >/dev/null 2>&1; then
    if ! firefox "$APP_URL" 2>/dev/null; then
      warn "Failed to open Firefox"
      log "Please manually open: $APP_URL"
    fi
  elif command -v chromium >/dev/null 2>&1; then
    if ! chromium "$APP_URL" 2>/dev/null; then
      warn "Failed to open Chromium"
      log "Please manually open: $APP_URL"
    fi
  else
    warn "No browser found - please manually open: $APP_URL"
  fi
}

run_diagnostics() {
  log "Running $APP_NAME diagnostics..."
  
  # Check if server is running
  if is_running; then
    log "✓ Server is running on $APP_URL"
  else
    err "✗ Server is not running"
    if command -v lsof >/dev/null 2>&1 && lsof -i :$APP_PORT >/dev/null 2>&1; then
      err "  Port $APP_PORT is in use by another process"
    else
      err "  Port $APP_PORT is available"
    fi
  fi
  
  # Check for errors in log
  if [ -f "$LOG_FILE" ]; then
    local errors=$(grep -i "error\|fail\|crash" "$LOG_FILE" | wc -l || echo 0)
    if [ "$errors" -gt 0 ]; then
      err "✗ Found $errors potential errors in log: $LOG_FILE"
    fi
  fi
  
  log "Diagnostics complete"
}

# ============================================================================
# MAIN LOGIC - Customize for each application
# ============================================================================

MODE="${1:---auto}"

case "$MODE" in
  --start)
    start_server
    open_browser
    ;;
  
  --stop)
    # Add stop logic here
    log "Stop functionality not yet implemented"
    ;;
  
  --status|--diagnostics)
    run_diagnostics
    ;;
  
  --help|-h)
    echo "Usage: $0 [OPTION]"
    echo ""
    echo "Options:"
    echo "  --start      Start server and open browser (default)"
    echo "  --stop       Stop the server"
    echo "  --status     Show server status"
    echo "  --diagnostics Run self-diagnostics"
    echo "  --help       Show this help"
    ;;
  
  *)
    start_server
    open_browser
    ;;
esac