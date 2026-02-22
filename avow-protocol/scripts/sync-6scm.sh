#!/usr/bin/env bash
set -euo pipefail

mkdir -p .machine_readable/6scm
for f in AGENTIC.scm ECOSYSTEM.scm META.scm NEUROSYM.scm PLAYBOOK.scm STATE.scm; do
  if [ -f ".machine_readable/$f" ]; then
    cp -f ".machine_readable/$f" ".machine_readable/6scm/$f"
  fi
done
