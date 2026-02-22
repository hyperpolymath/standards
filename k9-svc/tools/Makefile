# SPDX-License-Identifier: PMPL-1.0-or-later
# Makefile for K9 Tools

.PHONY: all build install clean test help

# Default target
all: build

# Build all tools
build: build-k9-init build-k9-validate build-k9-sign
	@echo "✓ All K9 tools built successfully"

build-k9-init:
	@echo "Building k9-init..."
	cd src/k9-init && cargo build --release
	@echo "✓ k9-init built"

build-k9-validate:
	@echo "Building k9-validate..."
	cd src/k9-validate && cargo build --release
	@echo "✓ k9-validate built"

build-k9-sign:
	@echo "Building k9-sign..."
	cd src/k9-sign && cargo build --release
	@echo "✓ k9-sign built"

# Install all tools
install: install-k9-init install-k9-validate install-k9-sign
	@echo "✓ All K9 tools installed"

install-k9-init: build-k9-init
	@echo "Installing k9-init..."
	sudo cp src/k9-init/target/release/k9-init /usr/local/bin/
	@echo "✓ k9-init installed to /usr/local/bin/"

install-k9-validate: build-k9-validate
	@echo "Installing k9-validate..."
	sudo cp src/k9-validate/target/release/k9-validate /usr/local/bin/
	@echo "✓ k9-validate installed to /usr/local/bin/"

install-k9-sign: build-k9-sign
	@echo "Installing k9-sign..."
	sudo cp src/k9-sign/target/release/k9-sign /usr/local/bin/
	@echo "✓ k9-sign installed to /usr/local/bin/"

# Run tests
test:
	@echo "Running tests..."
	cd src/k9-init && cargo test
	cd src/k9-validate && cargo test
	cd src/k9-sign && cargo test
	@echo "✓ All tests passed"

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	cd src/k9-init && cargo clean
	cd src/k9-validate && cargo clean
	cd src/k9-sign && cargo clean
	@echo "✓ Clean complete"

# Display help
help:
	@echo "K9 Tools - Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  build               Build all tools"
	@echo "  install             Install all tools to /usr/local/bin/"
	@echo "  test                Run all tests"
	@echo "  clean               Clean build artifacts"
	@echo "  help                Display this help"
	@echo ""
	@echo "Individual tools:"
	@echo "  build-k9-init       Build k9-init only"
	@echo "  build-k9-validate   Build k9-validate only"
	@echo "  build-k9-sign       Build k9-sign only"
	@echo "  install-k9-init     Install k9-init only"
	@echo "  install-k9-validate Install k9-validate only"
	@echo "  install-k9-sign     Install k9-sign only"
