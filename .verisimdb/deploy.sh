#!/bin/sh
# SPDX-License-Identifier: PMPL-1.0-or-later
# Deploy the standards VeriSimDB instance (port 8097).
#
# Usage: .verisimdb/deploy.sh [start|stop|status|logs|rebuild]

set -eu

ACTION="${1:-status}"
COMPOSE_FILE=".verisimdb/compose.yml"
CONTAINER="standards-verisimdb"

case "$ACTION" in
  start)
    echo "Starting $CONTAINER on port 8097..."
    podman-compose -f "$COMPOSE_FILE" up -d
    ;;
  stop)
    echo "Stopping $CONTAINER..."
    podman-compose -f "$COMPOSE_FILE" down
    ;;
  status)
    podman ps --filter "name=$CONTAINER"
    ;;
  logs)
    podman logs -f "$CONTAINER"
    ;;
  rebuild)
    echo "Rebuilding $CONTAINER..."
    podman-compose -f "$COMPOSE_FILE" down
    podman-compose -f "$COMPOSE_FILE" build --no-cache
    podman-compose -f "$COMPOSE_FILE" up -d
    ;;
  health)
    curl -fsS http://localhost:8097/health && echo " OK" || { echo " FAIL"; exit 1; }
    ;;
  *)
    echo "Usage: $0 [start|stop|status|logs|rebuild|health]" >&2
    exit 2
    ;;
esac
