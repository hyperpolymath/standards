# SPDX-License-Identifier: AGPL-3.0-or-later
# flake.nix - K9 SVC Nix Flake
#
# Usage:
#   nix develop          # Enter development shell
#   nix build            # Build the package
#   nix run              # Run k9 status
#   nix flake check      # Run tests
#
# To use in another flake:
#   inputs.k9-svc.url = "github:hyperpolymath/k9-svc";
#   packages = [ inputs.k9-svc.packages.${system}.default ];

{
  description = "K9 SVC - Self-Validating Components";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        k9-svc = pkgs.stdenv.mkDerivation {
          pname = "k9-svc";
          version = "1.0.0";

          src = ./.;

          nativeBuildInputs = [ pkgs.makeWrapper ];

          buildInputs = [
            pkgs.nickel
            pkgs.just
            pkgs.openssl
          ];

          installPhase = ''
            runHook preInstall

            # Create directories
            mkdir -p $out/bin
            mkdir -p $out/share/k9/{examples,assets}
            mkdir -p $out/share/mime/packages
            mkdir -p $out/share/doc/k9-svc

            # Install scripts
            install -m755 must $out/bin/k9-must
            install -m755 sign.sh $out/bin/k9-sign

            # Install schemas
            install -m644 pedigree.ncl $out/share/k9/
            install -m644 register.ncl $out/share/k9/
            install -m644 leash.ncl $out/share/k9/
            install -m644 justfile $out/share/k9/

            # Install examples
            install -m644 examples/*.k9 $out/share/k9/examples/ 2>/dev/null || true
            install -m644 examples/*.k9.ncl $out/share/k9/examples/ 2>/dev/null || true

            # Install assets
            install -m644 assets/*.svg $out/share/k9/assets/ 2>/dev/null || true

            # Install MIME type
            install -m644 mime/k9.xml $out/share/mime/packages/

            # Install documentation
            install -m644 README.adoc SPEC.adoc GUIDE.adoc LICENSE $out/share/doc/k9-svc/

            # Create wrapper script
            makeWrapper ${pkgs.just}/bin/just $out/bin/k9 \
              --add-flags "--justfile $out/share/k9/justfile" \
              --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.nickel pkgs.openssl ]}

            # Wrap sign.sh to ensure openssl is available
            wrapProgram $out/bin/k9-sign \
              --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.openssl ]}

            runHook postInstall
          '';

          meta = with pkgs.lib; {
            description = "Self-Validating Components - a file format that eats its own dog food";
            homepage = "https://github.com/hyperpolymath/k9-svc";
            license = licenses.agpl3Plus;
            maintainers = [];
            platforms = platforms.unix;
          };
        };

      in {
        packages = {
          default = k9-svc;
          k9-svc = k9-svc;
        };

        apps.default = flake-utils.lib.mkApp {
          drv = k9-svc;
          exePath = "/bin/k9";
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.nickel
            pkgs.just
            pkgs.openssl
            pkgs.podman
            pkgs.asciidoctor
            pkgs.xmllint
          ];

          shellHook = ''
            echo "K9 SVC Development Shell"
            echo "========================"
            echo "Available commands:"
            echo "  ./must status    - Check environment"
            echo "  just typecheck   - Validate schemas"
            echo "  just test        - Run test suite"
            echo "  just dogfood     - Self-validation"
            echo ""
          '';
        };

        checks.default = pkgs.runCommand "k9-svc-check" {
          buildInputs = [ pkgs.nickel k9-svc ];
        } ''
          # Typecheck schemas
          nickel typecheck ${k9-svc}/share/k9/pedigree.ncl
          nickel typecheck ${k9-svc}/share/k9/register.ncl
          nickel typecheck ${k9-svc}/share/k9/leash.ncl
          echo "All schema checks passed" > $out
        '';
      });
}
