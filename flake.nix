# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
#
# Nix flake development environment for standards.
# Usage: nix develop
{
  description = "Hyperpolymath standards — A2ML, K9, Axel, Groove, eNSAID";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Deno — runtime for standard tooling and validators
            deno

            # Nickel — configuration language for eNSAID
            nickel

            # Build tooling
            gnumake
          ];

          shellHook = ''
            echo "standards dev shell — deno + nickel"
          '';
        };
      });
}
