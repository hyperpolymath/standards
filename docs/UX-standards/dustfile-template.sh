#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Dustfile Template - Targeted, automated repair for application launchers
# 
# Dustfiles are self-diagnostic, self-healing scripts that:
# 1. Detect specific problems
# 2. Provide targeted fixes
# 3. Offer clear explanations
# 4. Integrate with feedback systems

set -euo pipefail

# ============================================================================
# CONFIGURATION - Customize for each application
# ============================================================================
APP_NAME="TemplateApp"
APP_DIR="/path/to/app"
PID_FILE="/tmp/${APP_NAME,,}-server.pid"
LOG_FILE="/tmp/${APP_NAME,,}-server.log"
PORT=4000
COMMAND="command to run"

# ============================================================================
# DIAGNOSTIC FUNCTIONS - Targeted problem detection
# ============================================================================

log() { echo -e "\033[0;32m[$APP_NAME-Dustfile]\033[0m $1"; }
warn() { echo -e "\033[0;33m[$APP_NAME-Dustfile]\033[0m $1"; }
err() { echo -e "\033[0;31m[$APP_NAME-Dustfile]\033[0m $1" >&2; }

is_running() {
  [ -f "$PID_FILE" ] && kill -0 "$(cat "$PID_FILE")" 2>/dev/null
}

port_in_use() {
  command -v lsof >/dev/null 2>&1 && lsof -i :$PORT >/dev/null 2>&1
}

# ============================================================================
# TARGETED REPAIR FUNCTIONS - Specific fixes for known issues
# ============================================================================

repair_port_conflict() {
  log "Repairing port conflict on port $PORT..."
  
  if port_in_use; then
    local pid=$(lsof -ti :$PORT 2>/dev/null || echo "")
    local cmd=$(ps -p $pid -o cmd= 2>/dev/null || echo "unknown")
    
    warn "Port $PORT is in use by PID $pid: $cmd"
    read -rp "Kill this process? [y/N] " confirm
    
    if [[ "$confirm" =~ ^[Yy]$ ]]; then
      kill -9 "$pid" 2>/dev/null && log "Killed process $pid" || err "Failed to kill process"
      sleep 2
      return 0
    else
      err "Cannot repair - port conflict remains"
      return 1
    fi
  fi
  
  log "Port $PORT is now available"
}

repair_missing_dependencies() {
  log "Checking for missing dependencies..."
  
  local missing=()
  
  # Check for required commands
  for cmd in curl grep sed awk; do
    if ! command -v "$cmd" >/dev/null 2>&1; then
      missing+=("$cmd")
    fi
  done
  
  if [ ${#missing[@]} -gt 0 ]; then
    err "Missing dependencies: ${missing[*]}"
    
    case "$(uname -s)" in
      Linux*)
        if command -v dnf >/dev/null 2>&1; then
          log "Installing missing dependencies with dnf..."
          sudo dnf install -y "${missing[@]}" && return 0
        elif command -v apt-get >/dev/null 2>&1; then
          log "Installing missing dependencies with apt..."
          sudo apt-get update && sudo apt-get install -y "${missing[@]}" && return 0
        else
          err "Cannot automatically install - please install: ${missing[*]}"
          return 1
        fi
        ;;
      Darwin*)
        log "Installing missing dependencies with brew..."
        brew install "${missing[@]}" && return 0
        ;;
      *)
        err "Unsupported OS - please manually install: ${missing[*]}"
        return 1
        ;;
    esac
  fi
  
  log "All dependencies are installed"
}

repair_server_not_starting() {
  log "Diagnosing server startup failure..."
  
  # Check common issues
  if ! [ -x "$(command -v $COMMAND)" ]; then
    err "Command not found: $COMMAND"
    err "Please install the application or check your PATH"
    return 1
  fi
  
  if port_in_use; then
    err "Port $PORT is already in use"
    repair_port_conflict || return 1
  fi
  
  # Check logs for specific errors
  if [ -f "$LOG_FILE" ]; then
    local errors=$(grep -i "error\|fail\|exception" "$LOG_FILE" | tail -5)
    if [ -n "$errors" ]; then
      err "Found errors in log:"
      echo "$errors"
      err ""
      err "For LLM-assisted analysis:"
      err "  hypatia diagnose --app $APP_NAME --log $LOG_FILE"
      err ""
      err "For exhausted support (when you've tried everything):"
      err "  hypatia exhausted --app $APP_NAME --log $LOG_FILE --dustfile $0"
    fi
  fi
  
  log "Attempting to restart server..."
  
  # Try to start with more verbose logging
  cd "$APP_DIR"
  nohup $COMMAND --verbose >"$LOG_FILE" 2>&1 &
  echo $! > "$PID_FILE"
  
  sleep 3
  
  if is_running; then
    log "Server restarted successfully"
    return 0
  else
    err "Server still not running - check $LOG_FILE for details"
    return 1
  fi
}

# ============================================================================
# MAIN DUSTFILE INTERFACE
# ============================================================================

MODE="${1:---diagnose}"

case "$MODE" in
  --diagnose|-d)
    log "Running $APP_NAME diagnostics..."
    
    log "1. Checking if server is running..."
    if is_running; then
      log "✓ Server is running (PID: $(cat "$PID_FILE"))"
    else
      err "✗ Server is not running"
    fi
    
    log "2. Checking port availability..."
    if port_in_use; then
      err "✗ Port $PORT is in use"
    else
      log "✓ Port $PORT is available"
    fi
    
    log "3. Checking dependencies..."
    repair_missing_dependencies || true
    
    log "4. Checking logs..."
    if [ -f "$LOG_FILE" ]; then
      local errors=$(grep -c -i "error\|fail\|exception" "$LOG_FILE" 2>/dev/null || echo 0)
      if [ "$errors" -gt 0 ]; then
        err "✗ Found $errors errors in $LOG_FILE"
      else
        log "✓ No errors found in logs"
      fi
    else
      warn "? No log file found at $LOG_FILE"
    fi
    
    log ""
    log "Diagnostics complete. Use --repair to attempt fixes."
    ;;
  
  --repair|-r)
    log "Attempting automated repair..."
    
    # Try repairs in order of likelihood
    repair_missing_dependencies && \
    repair_port_conflict && \
    repair_server_not_starting
    
    if [ $? -eq 0 ]; then
      log "Repair completed successfully!"
      log "Try launching the application again."
    else
      err "Automated repair failed"
      err "Please check logs and documentation for manual repair steps."
    fi
    ;;
  
  --full-repair|-f)
    log "Attempting comprehensive repair..."
    
    # Stop any running instances
    if is_running; then
      log "Stopping existing server..."
      kill "$(cat "$PID_FILE")" 2>/dev/null || true
      rm -f "$PID_FILE"
      sleep 2
    fi
    
    # Clean up
    log "Cleaning up..."
    rm -f "$LOG_FILE"
    
    # Full repair sequence
    repair_missing_dependencies && \
    repair_port_conflict && \
    repair_server_not_starting
    
    if [ $? -eq 0 ]; then
      log "Comprehensive repair completed!"
    else
      err "Comprehensive repair failed"
    fi
    ;;
  
  --help|-h|*)
    echo "Usage: $0 [OPTION]"
    echo ""
    echo "Options:"
    echo "  --diagnose, -d    Run diagnostics only"
    echo "  --repair, -r      Attempt automated repair"
    echo "  --full-repair, -f  Comprehensive repair (stops server, cleans up)"
    echo "  --help, -h        Show this help"
    echo ""
    echo "Dustfiles are targeted repair tools. They:"
    echo "  • Detect specific problems"
    echo "  • Offer automated fixes"
    echo "  • Provide clear explanations"
    echo "  • Integrate with feedback systems"
    ;;
esac

# Soft-attach: Report dustfile usage to feedback-o-tron
if command -v feedback-o-tron >/dev/null 2>&1; then
  feedback-o-tron --event "dustfile:used" \
    --app "$APP_NAME" --mode "$MODE" 2>/dev/null || true
fi