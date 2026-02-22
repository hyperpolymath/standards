# SPDX-License-Identifier: MIT AND Palimpsest-0.8
# SPDX-FileCopyrightText: 2025 The Rhodium Standard Contributors
{
  description = "rhodium-minimal - Minimal Rhodium Standard Repository example";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
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
          extensions = [ "rust-src" "rust-analyzer" ];
        };

        buildInputs = with pkgs; [
          rustToolchain
        ];

        nativeBuildInputs = with pkgs; [
          just
          ripgrep
          tokei  # Code statistics
        ];

      in
      {
        # Development shell
        devShells.default = pkgs.mkShell {
          inherit buildInputs nativeBuildInputs;

          shellHook = ''
            echo "🎖️  rhodium-minimal development environment"
            echo "Language: Rust ${rustToolchain.version}"
            echo ""
            echo "Available commands:"
            echo "  just --list    # Show all tasks"
            echo "  just build     # Build project"
            echo "  just run       # Run application"
            echo "  just test      # Run tests"
            echo "  just validate  # RSR compliance"
            echo ""
            echo "RSR Compliance: Bronze (75-89%)"
            echo "TPCF Perimeter: 3 (Community Sandbox)"
            echo ""
          '';
        };

        # Packages
        packages.default = pkgs.rustPlatform.buildRustPackage {
          pname = "rhodium-minimal";
          version = "0.1.0";
          src = ./.;

          cargoLock = {
            lockFile = ./Cargo.lock;
          };

          nativeBuildInputs = nativeBuildInputs;

          meta = with pkgs.lib; {
            description = "Minimal Rhodium Standard Repository (RSR) compliant example";
            homepage = "https://gitlab.com/hyperpolymath/rhodium-standard-repositories";
            license = with licenses; [ mit ];  # Dual: MIT + Palimpsest-0.8
            maintainers = [ "The Rhodium Standard Contributors" ];
            platforms = platforms.unix;
          };
        };

        # Apps
        apps.default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/rhodium-minimal";
        };

        # Checks (CI/CD integration)
        checks = {
          build = self.packages.${system}.default;

          # Test check
          test = pkgs.runCommand "test-rhodium-minimal" {
            buildInputs = [ rustToolchain ];
          } ''
            cd ${./.}
            cargo test
            touch $out
          '';

          # Format check
          format = pkgs.runCommand "format-rhodium-minimal" {
            buildInputs = [ rustToolchain ];
          } ''
            cd ${./.}
            cargo fmt -- --check
            touch $out
          '';

          # Lint check
          lint = pkgs.runCommand "lint-rhodium-minimal" {
            buildInputs = [ rustToolchain ];
          } ''
            cd ${./.}
            cargo clippy -- -D warnings
            touch $out
          '';
        };
      }
    );
}
