# SPDX-License-Identifier: AGPL-3.0-or-later
# Makefile - K9 SVC Installation
#
# Usage:
#   make install          # Install to ~/.local (user)
#   make install-system   # Install to /usr/local (requires sudo)
#   make uninstall        # Remove user installation
#   make uninstall-system # Remove system installation

PREFIX ?= $(HOME)/.local
DESTDIR ?=

# Directories
BINDIR = $(DESTDIR)$(PREFIX)/bin
SHAREDIR = $(DESTDIR)$(PREFIX)/share/k9
MIMEDIR = $(DESTDIR)$(PREFIX)/share/mime/packages
MANDIR = $(DESTDIR)$(PREFIX)/share/man/man1

# Files to install
SCRIPTS = must sign.sh
NICKEL_FILES = pedigree.ncl register.ncl leash.ncl
MIME_FILES = mime/k9.xml
DOCS = README.adoc SPEC.adoc GUIDE.adoc LICENSE

.PHONY: all install install-system uninstall uninstall-system check test clean

all: check

# Check prerequisites
check:
	@echo "K9: Checking prerequisites..."
	@command -v nickel >/dev/null 2>&1 || (echo "ERROR: nickel not found" && exit 1)
	@command -v just >/dev/null 2>&1 || (echo "ERROR: just not found" && exit 1)
	@echo "K9: All prerequisites satisfied."

# Run tests
test:
	@./test.sh

# User-level installation (default)
install: check
	@echo "K9: Installing to $(PREFIX)..."
	@mkdir -p $(BINDIR)
	@mkdir -p $(SHAREDIR)
	@mkdir -p $(SHAREDIR)/examples
	@mkdir -p $(SHAREDIR)/assets
	@mkdir -p $(MIMEDIR)

	# Install scripts
	@install -m 755 must $(BINDIR)/k9-must
	@install -m 755 sign.sh $(BINDIR)/k9-sign
	@install -m 644 justfile $(SHAREDIR)/justfile

	# Install schemas
	@install -m 644 pedigree.ncl $(SHAREDIR)/
	@install -m 644 register.ncl $(SHAREDIR)/
	@install -m 644 leash.ncl $(SHAREDIR)/

	# Install examples
	@install -m 644 examples/*.k9 $(SHAREDIR)/examples/ 2>/dev/null || true
	@install -m 644 examples/*.k9.ncl $(SHAREDIR)/examples/ 2>/dev/null || true

	# Install assets
	@install -m 644 assets/*.svg $(SHAREDIR)/assets/ 2>/dev/null || true

	# Install MIME type
	@install -m 644 mime/k9.xml $(MIMEDIR)/
	@update-mime-database $(DESTDIR)$(PREFIX)/share/mime 2>/dev/null || true

	# Install docs
	@install -m 644 $(DOCS) $(SHAREDIR)/ 2>/dev/null || true

	# Create wrapper script
	@echo '#!/bin/sh' > $(BINDIR)/k9
	@echo '# K9 SVC wrapper - runs just with K9 recipes' >> $(BINDIR)/k9
	@echo 'exec just --justfile $(PREFIX)/share/k9/justfile "$$@"' >> $(BINDIR)/k9
	@chmod 755 $(BINDIR)/k9

	@echo ""
	@echo "K9: Installation complete!"
	@echo "    Commands: k9, k9-must, k9-sign"
	@echo "    Schemas:  $(SHAREDIR)/"
	@echo ""
	@echo "    Add $(BINDIR) to your PATH if not already present."

# System-wide installation
install-system:
	@$(MAKE) install PREFIX=/usr/local

# Uninstall user installation
uninstall:
	@echo "K9: Uninstalling from $(PREFIX)..."
	@rm -f $(BINDIR)/k9 $(BINDIR)/k9-must $(BINDIR)/k9-sign
	@rm -rf $(SHAREDIR)
	@rm -f $(MIMEDIR)/k9.xml
	@update-mime-database $(DESTDIR)$(PREFIX)/share/mime 2>/dev/null || true
	@echo "K9: Uninstallation complete."

# Uninstall system installation
uninstall-system:
	@$(MAKE) uninstall PREFIX=/usr/local

# Clean generated files
clean:
	@rm -rf docs/*.html
	@rm -f ~/.magic.mgc
	@echo "K9: Clean complete."

# Development helpers
.PHONY: lint fmt

lint:
	@nickel typecheck pedigree.ncl
	@nickel typecheck register.ncl
	@nickel typecheck leash.ncl
	@echo "K9: All schemas pass typecheck."

fmt:
	@nickel format pedigree.ncl 2>/dev/null || true
	@nickel format register.ncl 2>/dev/null || true
	@nickel format leash.ncl 2>/dev/null || true
	@echo "K9: Formatting complete."
