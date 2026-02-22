# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors
#
# 1000Langs Justfile - RSR Gold Compliant
# ========================================
# Comprehensive task automation for the super-parallel corpus crawler
# Runtime: Deno | Languages: ReScript, Elixir, Julia

set shell := ["bash", "-euo", "pipefail", "-c"]
set positional-arguments := true
set dotenv-load := true

# Default recipe - show help
default: help

# ============================================================================
# HELP & DOCUMENTATION
# ============================================================================

# Show all available recipes with descriptions
help:
    @just --list --unsorted

# Show detailed help for a specific recipe
help-recipe RECIPE:
    @just --show {{RECIPE}}

# Generate documentation for all recipes
docs:
    @echo "Generating recipe documentation..."
    @just --list --list-heading $'# Available Recipes\n\n' --list-prefix '- ' > docs/RECIPES.md

# ============================================================================
# DEVELOPMENT ENVIRONMENT
# ============================================================================

# Enter Nix development shell
shell:
    nix develop

# Install all dependencies (Deno + ReScript)
install:
    deno install

# Update all dependencies
update:
    deno install
    nix flake update

# Clean all build artifacts
clean:
    deno task clean
    rm -rf node_modules/.cache
    rm -rf coverage
    rm -rf .vitest

# Deep clean including node_modules
clean-all: clean
    rm -rf node_modules
    rm -rf .rescript

# ============================================================================
# BUILD & COMPILE
# ============================================================================

# Build ReScript to JavaScript
build:
    deno task build

# Build with watch mode
watch:
    deno task build:watch

# Build for production (optimized)
build-prod:
    NODE_ENV=production deno task build

# Type check without building
typecheck:
    npx rescript build -warn-error

# Build documentation
build-docs:
    asciidoctor docs/*.adoc -D docs/html

# ============================================================================
# TESTING
# ============================================================================

# Run all tests
test:
    deno task test

# Run tests with watch mode
test-watch:
    deno task test:watch

# Run tests with coverage
test-coverage:
    deno task test:coverage

# Run specific test file
test-file FILE:
    deno run -A npm:vitest run {{FILE}}

# Run tests matching pattern
test-grep PATTERN:
    deno run -A npm:vitest run -t "{{PATTERN}}"

# Run unit tests only
test-unit:
    deno run -A npm:vitest run test/unit

# Run integration tests only
test-integration:
    deno run -A npm:vitest run test/integration

# Run proof verification tests
test-proofs:
    deno run -A npm:vitest run test/proofs

# ============================================================================
# CODE QUALITY
# ============================================================================

# Run linter
lint:
    deno task lint

# Fix linting issues automatically
lint-fix:
    deno run -A npm:@biomejs/biome check --write .

# Format code
format:
    deno task fmt

# Check formatting without changes
format-check:
    deno run -A npm:@biomejs/biome format --check .

# Run all quality checks
check: lint format-check typecheck test

# Pre-commit checks
pre-commit: lint-fix format check

# ============================================================================
# PROOFS & VERIFICATION
# ============================================================================

# Run Echidna proof verification
prove:
    @echo "Running Echidna proof verification..."
    cd proofs && echidna verify

# Check specific proof
prove-check PROOF:
    cd proofs && echidna check {{PROOF}}

# Generate proof obligations
prove-gen:
    @echo "Generating proof obligations from specifications..."
    cd proofs && echidna generate

# Validate proof dependencies
prove-deps:
    cd proofs && echidna deps --check

# ============================================================================
# CORPUS OPERATIONS
# ============================================================================

# Crawl specific source and language
crawl SOURCE LANG:
    deno run -A src/Lang1000.res.mjs crawl --source {{SOURCE}} --lang {{LANG}}

# Crawl all sources via Elixir orchestrator
crawl-all:
    cd orchestrator && mix crawl --source all --workers 20

# Crawl with specific worker count
crawl-workers SOURCE WORKERS:
    cd orchestrator && mix crawl --source {{SOURCE}} --workers {{WORKERS}}

# List available languages
languages:
    deno run -A src/Lang1000.res.mjs list-sources

# Show language statistics
language-stats:
    deno run -A src/Lang1000.res.mjs stats --languages

# Build parallel corpus
corpus-build:
    deno run -A src/Lang1000.res.mjs corpus --build

# Validate corpus integrity via VeriSimDB
verify:
    deno task verify

# Export corpus to format
corpus-export FORMAT="json":
    deno run -A src/Lang1000.res.mjs corpus --export {{FORMAT}}

# ============================================================================
# VERISIMDB INTEGRATION
# ============================================================================

# Run VeriSimDB corpus quality analysis
verisimdb-verify:
    deno run -A src/Lang1000.res.mjs verify --output /tmp/lol-scan.json

# Ingest scan results into verisimdb-data
verisimdb-ingest:
    ./scripts/verisimdb-ingest.sh

# Full verify + ingest pipeline
verisimdb-pipeline: verisimdb-verify verisimdb-ingest

# ============================================================================
# JULIA ANALYSIS
# ============================================================================

# Run Julia analysis on corpus
analyze CORPUS:
    cd orchestrator && mix analyze --corpus {{CORPUS}}

# Run Julia analysis standalone
analyze-standalone CORPUS:
    cd analysis && julia --project=. src/server.jl analyze {{CORPUS}}

# Install Julia dependencies
julia-deps:
    cd analysis && julia --project=. -e 'using Pkg; Pkg.instantiate()'

# Run Julia tests
julia-test:
    cd analysis && julia --project=. test/runtests.jl

# ============================================================================
# ELIXIR ORCHESTRATOR
# ============================================================================

# Get Elixir dependencies
elixir-deps:
    cd orchestrator && mix deps.get

# Run Elixir tests
elixir-test:
    cd orchestrator && mix test

# Run Elixir orchestrator interactively
elixir-iex:
    cd orchestrator && iex -S mix

# ============================================================================
# CONTAINER OPERATIONS (PODMAN)
# ============================================================================

# Build container image
container-build:
    podman build -t 1000langs:latest -f Containerfile .

# Build container without cache
container-build-nocache:
    podman build --no-cache -t 1000langs:latest -f Containerfile .

# Run container
container-run:
    podman run --rm -it 1000langs:latest

# Run container with volume mount
container-dev:
    podman run --rm -it -v .:/app:Z 1000langs:latest

# Push container to registry
container-push REGISTRY:
    podman push 1000langs:latest {{REGISTRY}}/1000langs:latest

# List container images
container-list:
    podman images | grep 1000langs

# Clean container images
container-clean:
    podman rmi 1000langs:latest || true

# ============================================================================
# RSR COMPLIANCE
# ============================================================================

# Run full RSR audit
rsr-audit:
    ./scripts/rsr-audit.sh . text

# Run RSR audit with JSON output
rsr-audit-json:
    ./scripts/rsr-audit.sh . json

# Run RSR audit with HTML report
rsr-audit-html:
    ./scripts/rsr-audit.sh . html > reports/rsr-audit.html

# Check compliance level
rsr-level:
    @./scripts/rsr-audit.sh . json | jq -r '.compliance_level'

# Validate SPDX headers
spdx-check:
    @echo "Checking SPDX license headers..."
    @find src test -name "*.res" -exec grep -L "SPDX-License-Identifier" {} \; | \
        xargs -I {} echo "Missing SPDX header: {}"

# Add SPDX headers to files
spdx-fix:
    @echo "Adding SPDX headers to files..."
    ./scripts/add-spdx-headers.sh

# ============================================================================
# RELEASE & VERSIONING
# ============================================================================

# Show current version
version:
    @jq -r '.version' package.json

# Bump patch version
bump-patch:
    npm version patch --no-git-tag-version

# Bump minor version
bump-minor:
    npm version minor --no-git-tag-version

# Bump major version
bump-major:
    npm version major --no-git-tag-version

# Create release
release VERSION:
    @echo "Creating release v{{VERSION}}..."
    npm version {{VERSION}} -m "Release v{{VERSION}}"
    git push origin main --tags

# Generate changelog
changelog:
    @echo "Generating CHANGELOG.md..."
    git log --pretty=format:"- %s (%h)" --no-merges > CHANGELOG.md.tmp
    cat CHANGELOG.md.tmp >> CHANGELOG.md
    rm CHANGELOG.md.tmp

# ============================================================================
# NICKEL CONFIGURATION
# ============================================================================

# Validate Nickel configuration
nickel-check:
    nickel eval config/main.ncl

# Export Nickel to JSON
nickel-export:
    nickel export config/main.ncl > config/config.json

# Show resolved configuration
nickel-show:
    nickel eval config/main.ncl | jq .

# ============================================================================
# CI/CD HELPERS
# ============================================================================

# Setup CI environment
ci-setup:
    deno install

# Run CI checks
ci-check: ci-setup check

# Build for CI
ci-build: ci-setup build-prod test-coverage

# Deploy artifacts
ci-deploy:
    @echo "Deploying artifacts..."

# ============================================================================
# DEVELOPMENT UTILITIES
# ============================================================================

# Start development server
dev: watch

# Open project in editor
edit:
    $EDITOR .

# Show project structure
tree:
    @tree -I 'node_modules|.git|coverage|.rescript' -a --dirsfirst

# Count lines of code
loc:
    @tokei --exclude node_modules --exclude .git

# Generate TODO list from code
todos:
    @grep -rn "TODO\|FIXME\|HACK\|XXX" src test --include="*.res" || true

# Security audit
security:
    deno run -A npm:audit

# Check for outdated dependencies
outdated:
    @echo "Check deno.json imports for updates"

# ============================================================================
# DATABASE & PERSISTENCE
# ============================================================================

# Initialize database
db-init:
    @echo "Initializing corpus database..."
    mkdir -p data/corpus

# Backup database
db-backup:
    tar -czf backups/corpus-$(date +%Y%m%d-%H%M%S).tar.gz data/corpus

# Restore database from backup
db-restore BACKUP:
    tar -xzf {{BACKUP}} -C data/

# ============================================================================
# OPENCYC INTEGRATION
# ============================================================================

# Start OpenCyc connection
cyc-connect:
    @echo "Connecting to OpenCyc knowledge base..."
    deno run -A src/cyc/OpenCyc.res.mjs connect

# Query OpenCyc
cyc-query TERM:
    deno run -A src/cyc/OpenCyc.res.mjs query "{{TERM}}"

# Sync language concepts with OpenCyc
cyc-sync:
    deno run -A src/cyc/OpenCyc.res.mjs sync-languages

# ============================================================================
# GIS INTEGRATION (FUTURE)
# ============================================================================

# Generate language distribution map
gis-map:
    @echo "GIS integration planned - see FUTURE.adoc"

# Export language coordinates
gis-export:
    @echo "GIS integration planned - see FUTURE.adoc"

# ============================================================================
# MISCELLANEOUS
# ============================================================================

# Print environment info
env-info:
    @echo "Deno: $(deno --version | head -1)"
    @echo "ReScript: $(npx rescript -version)"
    @echo "Elixir: $(elixir --version | tail -1)"
    @echo "Julia: $(julia --version)"
    @echo "Podman: $(podman --version)"
    @echo "Just: $(just --version)"

# Open project repository
repo:
    xdg-open https://github.com/Hyperpolymath/1000Langs

# Open documentation
open-docs:
    xdg-open docs/html/README.html

# Benchmark performance
bench:
    @echo "Running benchmarks..."
    deno run -A src/bench.res.mjs

# Profile memory usage
profile:
    deno run -A --inspect src/Lang1000.res.mjs

# ============================================================================
# MAINTENANCE
# ============================================================================

# Update .gitignore
gitignore:
    curl -sL https://www.toptal.com/developers/gitignore/api/node,rescript,vim,emacs > .gitignore

# Prune git repository
git-prune:
    git gc --prune=now
    git remote prune origin

# Archive old branches
git-archive:
    @echo "Archiving merged branches..."
    git branch --merged | grep -v "main\|master\|\*" | xargs -r git branch -d

# All maintenance tasks
maintain: clean git-prune security outdated
