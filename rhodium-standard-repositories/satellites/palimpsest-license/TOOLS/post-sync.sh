#!/bin/bash

echo "🔄 Post-sync tasks starting..."

# Rebuild markdown indexes or TOC
npx markdownlint-cli "**/*.md"

# Compile Mermaid diagrams (if using mermaid-cli)
mmdc -i diagrams/license-flow.mmd -o diagrams/license-flow.svg

# Rebuild HTML docs (if using Python or Elixir tooling)
make html || mix docs

# Log timestamp
echo "🕒 Synced on $(date)" >> sync.log

echo "✅ Post-sync tasks completed."
