# SPDX-License-Identifier: MIT AND Palimpsest-0.8
# SPDX-FileCopyrightText: 2025 The Rhodium Standard Contributors
{
  description = "Rhodium Standard Repositories - Framework for emotionally safe, technically excellent, politically autonomous software development";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };

        rustToolchain = pkgs.rust-bin.stable.latest.default.override {
          extensions = [ "rust-src" "rust-analyzer" "rustfmt" "clippy" ];
        };

      in
      {
        devShells.default = pkgs.mkShell {
          name = "rhodium-standard-dev";

          buildInputs = with pkgs; [
            # Rust toolchain
            rustToolchain
            cargo-audit
            cargo-watch
            cargo-edit

            # Build tools
            just
            git

            # Documentation tools
            asciidoctor
            pandoc

            # Link checking
            lychee

            # Shell linting
            shellcheck

            # General utilities
            jq
            ripgrep
            fd
            tree

            # Nix tools
            nil  # Nix language server
            nixfmt
          ];

          shellHook = ''
            echo "🎖️  Rhodium Standard Development Environment"
            echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            echo "Available commands:"
            echo "  just --list       # Show all tasks"
            echo "  just validate     # Run full validation"
            echo "  just audit        # RSR compliance audit"
            echo "  just test         # Run tests"
            echo ""
            echo "Versions:"
            echo "  Rust:    $(rustc --version | cut -d' ' -f2)"
            echo "  Cargo:   $(cargo --version | cut -d' ' -f2)"
            echo "  Just:    $(just --version 2>&1 | head -1 | cut -d' ' -f2)"
            echo "  Nix:     $(nix --version | cut -d' ' -f3)"
            echo ""
            echo "Documentation: README.adoc"
            echo "Contributing: CONTRIBUTING.adoc"
            echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
          '';

          # Environment variables
          RUST_BACKTRACE = "1";
          RUST_LOG = "info";
        };

        # Package for the RSR audit script
        packages.rsr-audit = pkgs.writeShellApplication {
          name = "rsr-audit";
          runtimeInputs = with pkgs; [ bash coreutils findutils ripgrep ];
          text = builtins.readFile ./rsr-audit.sh;
        };

        packages.default = self.packages.${system}.rsr-audit;

        # CI/CD checks
        checks = {
          audit-script-shellcheck = pkgs.runCommand "shellcheck-rsr-audit" {
            buildInputs = [ pkgs.shellcheck ];
          } ''
            shellcheck ${./rsr-audit.sh}
            touch $out
          '';

          # Check that all required files exist
          required-files-check = pkgs.runCommand "check-required-files" {} ''
            cd ${./.}

            # Category 2: Documentation Standards
            test -f README.adoc || (echo "Missing README.adoc" && exit 1)
            test -f LICENSE.txt || (echo "Missing LICENSE.txt" && exit 1)
            test -f SECURITY.md || (echo "Missing SECURITY.md" && exit 1)
            test -f CODE_OF_CONDUCT.adoc || (echo "Missing CODE_OF_CONDUCT.adoc" && exit 1)
            test -f CONTRIBUTING.adoc || (echo "Missing CONTRIBUTING.adoc" && exit 1)
            test -f MAINTAINERS.md || (echo "Missing MAINTAINERS.md" && exit 1)
            test -f CHANGELOG.md || (echo "Missing CHANGELOG.md" && exit 1)
            test -f FUNDING.yml || (echo "Missing FUNDING.yml" && exit 1)
            test -f GOVERNANCE.adoc || (echo "Missing GOVERNANCE.adoc" && exit 1)

            # .well-known directory
            test -d .well-known || (echo "Missing .well-known directory" && exit 1)
            test -f .well-known/security.txt || (echo "Missing .well-known/security.txt" && exit 1)
            test -f .well-known/ai.txt || (echo "Missing .well-known/ai.txt" && exit 1)
            test -f .well-known/humans.txt || (echo "Missing .well-known/humans.txt" && exit 1)

            # Infrastructure
            test -f .gitignore || (echo "Missing .gitignore" && exit 1)
            test -f .gitattributes || (echo "Missing .gitattributes" && exit 1)
            test -f justfile || (echo "Missing justfile" && exit 1)
            test -f flake.nix || (echo "Missing flake.nix" && exit 1)

            echo "✅ All required files present"
            touch $out
          '';
        };

        # Apps for running tools
        apps = {
          audit = {
            type = "app";
            program = "${self.packages.${system}.rsr-audit}/bin/rsr-audit";
          };
        };

        # Formatter for Nix files
        formatter = pkgs.nixfmt;
      }
    );

  # Metadata
  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    ];
  };
}
