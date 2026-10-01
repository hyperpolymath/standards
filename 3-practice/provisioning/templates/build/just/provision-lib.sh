#!/usr/bin/env bash
# SC2015: `A && pass || fail` is safe here — pass/warn/fail/info always return 0.
# shellcheck disable=SC2015
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# provision-lib.sh — the shared provisioning engine behind build/just/provision.just
#
# Canon: hyperpolymath/standards 3-practice/provisioning/PROVISIONING-STANDARD.adoc
# Vendored into every repo at build/just/provision-lib.sh by `provision-set mint`.
# DO NOT hand-edit a vendored copy: repo-specific facts live in
# .machine_readable/descriptiles/provisioning_praxis.deed, and `provision-set realign`
# overwrites this file from canon.
#
# Usage: provision-lib.sh <verb> [args]
#   verbs: langs doctor setup heal dev-shell toolchain-refresh crates-scm ai-setup
#          ai-warmup <user|dev|maintainer> eval config-show opsm lang-run <verb>
#          search <pattern> version
#   facts: guix-specs mise-tools langs guix-dir set-files guix-gaps tool-table
#          system-deps <adoc|ai>
#   predicates (print why and exit 1, or exit 0 silently):
#          guix-stub FILE, mise-lock-gaps, mise-banned
#
# Exit: 0 ok | 1 a FAIL was found / a step failed | 2 usage error
set -uo pipefail

PROVISION_LIB_VERSION="0.5.0"
ROOT="${PROVISION_ROOT:-$(pwd)}"
cd "$ROOT" || exit 2
DEED=".machine_readable/descriptiles/provisioning_praxis.deed"
MIN_JUST="1.42.0"   # root→module recipe deps (`doctor: provision::doctor`); 1.41 rejects them
# How to get mise: package managers first; the installer only as download, read, run.
MISE_INSTALL_HINT="brew install mise | Fedora: dnf copr enable jdxcode/mise, dnf install mise | winget install jdx.mise | others: https://mise.jdx.dev/installing-mise.html — or: curl -fsSLo mise-install.sh https://mise.run, read it, sh mise-install.sh"
T="timeout 15"      # some --version probes hang (observed 2026-09-30); never probe unbounded

if [ -t 1 ]; then R=$'\033[31m'; G=$'\033[32m'; Y=$'\033[33m'; B=$'\033[34m'; Z=$'\033[0m'; else R=; G=; Y=; B=; Z=; fi
PASS=0; WARN=0; FAIL=0
pass() { PASS=$((PASS+1)); printf '  %sPASS%s  %s\n' "$G" "$Z" "$*"; }
warn() { WARN=$((WARN+1)); printf '  %sWARN%s  %s\n' "$Y" "$Z" "$*"; }
fail() { FAIL=$((FAIL+1)); printf '  %sFAIL%s  %s\n' "$R" "$Z" "$*"; }
info() { printf '  %sinfo%s  %s\n' "$B" "$Z" "$*"; }
hdr()  { printf '\n%s== %s ==%s\n' "$B" "$*" "$Z"; }
have() { command -v "$1" >/dev/null 2>&1; }

# Where this repository keeps each part of the set. A file sits at the root only
# when something needs it there (launcher.sh, Justfile, mise.toml, mise.lock);
# the Guix trio and the warm-ups follow the layout the repository already has
# (rsr-template-repo keeps build/guix.scm and docs/onboarding/llm-warmup-*.adoc).
# This is the ONE answer: provision-check.sh asks it through `guix-dir` and
# `set-files` rather than keeping its own copy.
guix_dir()   { if [ -f build/guix.scm ]; then echo build; else echo .; fi; }
warmup_dir() {
  local d
  for d in . docs/onboarding docs; do
    compgen -G "$d/llm-warmup-*.adoc" >/dev/null && { echo "$d"; return; }
  done
  echo .
}
gpath() { local d; d=$(guix_dir); [ "$d" = . ] && echo "$1" || echo "$d/$1"; }
wpath() { local d; d=$(warmup_dir); [ "$d" = . ] && echo "llm-warmup-$1.adoc" || echo "$d/llm-warmup-$1.adoc"; }
set_files() {
  printf '%s\n' launcher.sh Justfile justfile mise.toml \
    "$(gpath guix.scm)" "$(gpath manifest.scm)" "$(gpath channels.scm)" \
    "$DEED" docs/SETUP.adoc docs/AI_INSTALLATION_GUIDE.adoc \
    "$(wpath user)" "$(wpath dev)" "$(wpath maintainer)"
}

# ---------------------------------------------------------------------------
# Descriptor: flat `:key "value"` reads from provisioning_praxis.deed (s-expression).
deed() { # $1 key, $2 default
  local v=""
  [ -f "$DEED" ] && v=$(grep -oE "\(:?$1[[:space:]]+\"[^\"]*\"" "$DEED" 2>/dev/null | head -1 | sed -E 's/^[^"]*"//; s/"$//')
  [ -z "$v" ] && v=$(grep -oE ":$1[[:space:]]+\"[^\"]*\"" "$DEED" 2>/dev/null | head -1 | sed -E 's/^[^"]*"//; s/"$//')
  printf '%s' "${v:-$2}"
}
repo_slug() {
  local u; u=$(git config --get remote.origin.url 2>/dev/null || true)
  u=${u%.git}; u=${u#*github.com[:/]}
  [ -n "$u" ] && printf '%s' "$u" || printf 'hyperpolymath/%s' "$(basename "$ROOT")"
}
REPO_SLUG="$(deed repo "$(repo_slug)")"
REPO_NAME="${REPO_SLUG#*/}"
ARCHETYPE="$(deed archetype "")"

# ---------------------------------------------------------------------------
# Language detection. Markers at the root or one level down (workspaces). The
# estate ABI/FFI pattern nests deeper: Idris2 to two levels down (src/abi/*.ipkg),
# Zig to three (ffi/zig/build.zig, and rsr-template-repo's src/interface/ffi/build.zig:
# 74 repos have their only build.zig at that depth, measured 2026-09-30).
# Order matters only for display. `docs` is reported when nothing else is.
first() { find . -maxdepth "${2:-2}" -not -path './.git/*' -not -path '*/node_modules/*' -not -path './target/*' -name "$1" -print -quit 2>/dev/null; }
detect_langs() {
  local out=()
  [ -n "$(first Cargo.toml)" ]          && out+=(rust)
  [ -n "$(first '*.ipkg' 3)" ]          && out+=(idris2)
  [ -n "$(first Project.toml 1)" ]      && out+=(julia)
  [ -n "$(first build.zig 4)" ]         && out+=(zig)
  [ -n "$(first mix.exs)" ]             && out+=(elixir)
  [ -n "$(first gleam.toml)" ]          && out+=(gleam)
  [ -n "$(first dune-project)" ]        && out+=(ocaml)
  { [ -n "$(first '*.cabal')" ] || [ -n "$(first stack.yaml 1)" ]; } && out+=(haskell)
  [ -n "$(first package.json)" ]        && out+=(bun)
  [ ${#out[@]} -eq 0 ] && out+=(docs)
  printf '%s\n' "${out[@]}"
}
mapfile -t LANGS < <(detect_langs)
[ -z "$ARCHETYPE" ] && { [ "${LANGS[*]}" = docs ] && ARCHETYPE=docs || ARCHETYPE=library; }

# Binaries each language needs, and how each is obtained.
# mise registry gaps (verified 2026-09-30 against mise 2026.7.5): ocaml, haskell,
# idris2 and guile are NOT mise tools. They come from opam / ghcup / pack / the
# system package manager (or Guix), and the doctor probes the binary itself.
lang_tools() {
  case "$1" in
    rust)    echo "cargo rustc" ;;
    idris2)  echo "idris2" ;;
    julia)   echo "julia" ;;
    zig)     echo "zig" ;;
    elixir)  echo "elixir mix erl" ;;
    gleam)   echo "gleam erl" ;;
    ocaml)   echo "opam dune ocaml" ;;
    haskell) echo "ghc cabal" ;;
    bun)     echo "bun" ;;
    docs)    echo "" ;;
  esac
}
# Guix package specs per language (verified 2026-09-30 against guix ae77aeb in
# docker.io/metacall/guix). Guix has NO idris2, gleam, bun or lychee, and its
# julia is 1.8.5: those come from mise, which Guix itself ships ("mise").
GUIX_BASE="git bash coreutils nss-certs just mise shellcheck"
lang_guix() {
  case "$1" in
    rust)    echo "rust rust:cargo gcc-toolchain pkg-config" ;;
    idris2)  echo "chez-scheme gmp gcc-toolchain" ;;
    zig)     echo "zig" ;;
    elixir)  echo "elixir erlang" ;;
    gleam)   echo "erlang" ;;
    ocaml)   echo "ocaml dune opam ocaml-findlib" ;;
    haskell) echo "ghc cabal-install" ;;
    docs)    echo "ruby-asciidoctor" ;;
    julia|bun) echo "" ;;
  esac
}
# Tools a Guix shell cannot supply, so mise (inside that shell) does.
lang_guix_gap() {
  case "$1" in
    idris2) echo "idris2 (via pack)" ;; julia) echo julia ;; gleam) echo gleam ;; bun) echo bun ;; docs) echo lychee ;;
  esac
}
guix_specs() {
  local l s=" $GUIX_BASE "
  for l in "${LANGS[@]}"; do for p in $(lang_guix "$l"); do case "$s" in *" $p "*) ;; *) s="$s$p ";; esac; done; done
  echo "$s" | xargs
}

# mise tools per language (registry-checked 2026-09-30, mise 2026.7.5). idris2 comes
# from pack and haskell from ghcup or Guix: neither is a mise tool. The recipes
# themselves need just and shellcheck, so those two are always declared.
MISE_BASE="just shellcheck"
# Tools a repo's own recipes call (e.g. `deps-audit` runs trivy): pinned only where used.
RECIPE_TOOLS="trivy"
lang_mise() { case "$1" in
    rust) echo "rust" ;; zig) echo "zig" ;; julia) echo "julia" ;;
    elixir) echo "erlang elixir" ;; gleam) echo "erlang gleam" ;; ocaml) echo "opam" ;;
    bun) echo "bun" ;; docs) echo "lychee" ;; esac; }
recipe_tools() {
  local t f body=""
  for f in Justfile justfile build/just/*.just; do
    [ -f "$f" ] && body+=$(grep -v '^[[:space:]]*#' "$f")$'\n'
  done
  for t in $RECIPE_TOOLS; do
    [[ "$body" =~ (^|[^[:alnum:]_-])$t([^[:alnum:]_-]|$) ]] && echo "$t"
  done
}

mise_tools() {
  local l t seen=" " out=()
  for t in $MISE_BASE $(for l in "${LANGS[@]}"; do lang_mise "$l"; done) $(recipe_tools); do
    case "$seen" in *" $t "*) ;; *) out+=("$t"); seen="$seen$t " ;; esac
  done
  printf "%s\n" "${out[*]}"
}

lang_remedy() {
  case "$1" in
    rust)    echo "mise use rust@latest  (or rustup: https://rustup.rs)" ;;
    idris2)  echo "install pack: https://github.com/stefan-hoeck/idris2-pack#installation, then: pack install-app idris2" ;;
    julia)   echo "mise use julia@latest  (or: https://julialang.org/install/ — juliaup)" ;;
    zig)     echo "mise use zig@latest" ;;
    elixir)  echo "mise use erlang@latest elixir@latest  (erlang builds from source: allow ~10 min)" ;;
    gleam)   echo "mise use gleam@latest erlang@latest" ;;
    ocaml)   echo "mise use opam@latest && opam init -y && opam switch create . --deps-only -y" ;;
    haskell) echo "ghcup: https://www.haskell.org/ghcup/  (or: just dev-shell, which uses Guix)" ;;
    bun)     echo "mise use bun@latest" ;;
  esac
}

# The per-language default for each contract verb. A repo overrides any of
# these simply by defining the root recipe itself; `provision-set` only adds
# `<verb>: provision::<verb>` where the root Justfile has no such recipe.
lang_cmd() { # $1 lang, $2 verb  -> prints a shell command, or nothing (= N/A)
  local l=$1 v=$2 ipkg
  case "$l:$v" in
    rust:deps)   echo "cargo fetch" ;;
    rust:build)  echo "cargo build --all-targets" ;;
    rust:test)   echo "cargo test --all-targets" ;;
    rust:bench)  grep -rqs '\[\[bench\]\]\|criterion\|divan' --include=Cargo.toml . && echo "cargo bench" ;;
    rust:lint)   echo "cargo clippy --all-targets -- -D warnings" ;;
    rust:fmt)    echo "cargo fmt --all" ;;
    rust:fmt-check) echo "cargo fmt --all -- --check" ;;
    rust:run)    echo "cargo run --release" ;;

    idris2:*)
      ipkg=$(first '*.ipkg' 1); [ -z "$ipkg" ] && ipkg=$(first '*.ipkg'); [ -z "$ipkg" ] && ipkg=$(first '*.ipkg' 3); ipkg=${ipkg#./}
      case "$v" in
        deps)  have pack && echo "pack install-deps $ipkg" ;;
        build) have pack && echo "pack build $ipkg" || echo "idris2 --build $ipkg" ;;
        test)  local t; t=$(find . -maxdepth 3 -name 'test*.ipkg' -print -quit 2>/dev/null)
               if [ -n "$t" ]; then have pack && echo "pack test ${t#./}" || echo "idris2 --build ${t#./}"
               else have pack && echo "pack typecheck $ipkg" || echo "idris2 --typecheck $ipkg"; fi ;;
        run)   grep -qs '^[[:space:]]*executable' "$ipkg" && { have pack && echo "pack run $ipkg" || echo "idris2 --build $ipkg && ./build/exec/*"; } ;;
      esac ;;

    julia:deps)  echo "julia --project=. -e 'using Pkg; Pkg.instantiate()'" ;;
    julia:build) echo "julia --project=. -e 'using Pkg; Pkg.precompile()'" ;;
    julia:test)  echo "julia --project=. -e 'using Pkg; Pkg.test()'" ;;
    julia:bench) [ -f benchmark/benchmarks.jl ] && echo "julia --project=benchmark -e 'using Pkg; Pkg.develop(path=\".\"); Pkg.instantiate(); include(\"benchmark/benchmarks.jl\")'" ;;

    zig:*)
      # build.zig may sit in ffi/zig/; run there, not at the root.
      local zb zd; zb=$(first build.zig 1); [ -z "$zb" ] && zb=$(first build.zig); [ -z "$zb" ] && zb=$(first build.zig 3); [ -z "$zb" ] && zb=$(first build.zig 4)
      zd=$(dirname "${zb#./}"); local cdz=""; [ "$zd" != . ] && cdz="cd '$zd' && "
      case "$v" in
        build) echo "${cdz}zig build" ;;
        test)  grep -qs '"test"' "$zb" && echo "${cdz}zig build test" ;;
        bench) grep -qs '"bench"' "$zb" && echo "${cdz}zig build bench -Doptimize=ReleaseFast" ;;
        fmt)   echo "zig fmt $zd" ;;
        fmt-check) echo "zig fmt --check $zd" ;;
        lint)  echo "zig fmt --check $zd" ;;
        run)   grep -qs '"run"' "$zb" && echo "${cdz}zig build run" ;;
      esac ;;

    elixir:deps)  echo "mix local.hex --force --if-missing && mix local.rebar --force --if-missing && mix deps.get" ;;
    elixir:build) echo "mix compile --warnings-as-errors" ;;
    elixir:test)  echo "mix test" ;;
    elixir:bench) ls bench/*.exs >/dev/null 2>&1 && echo "for f in bench/*.exs; do mix run \"\$f\"; done" ;;
    elixir:lint)  grep -qs ':credo' mix.exs && echo "mix credo --strict" || echo "mix compile --warnings-as-errors" ;;
    elixir:fmt)   echo "mix format" ;;
    elixir:fmt-check) echo "mix format --check-formatted" ;;
    elixir:run)   echo "mix run --no-halt" ;;

    gleam:deps)  echo "gleam deps download" ;;
    gleam:build) echo "gleam build" ;;
    gleam:test)  echo "gleam test" ;;
    gleam:fmt)   echo "gleam format" ;;
    gleam:fmt-check) echo "gleam format --check" ;;
    gleam:lint)  echo "gleam format --check" ;;
    gleam:run)   echo "gleam run" ;;

    ocaml:deps)  echo "opam install . --deps-only --with-test -y" ;;
    ocaml:build) echo "opam exec -- dune build" ;;
    ocaml:test)  echo "opam exec -- dune test" ;;
    ocaml:fmt)   echo "opam exec -- dune fmt" ;;
    ocaml:fmt-check) echo "opam exec -- dune build @fmt" ;;

    haskell:deps)  echo "cabal update && cabal build all --only-dependencies" ;;
    haskell:build) echo "cabal build all" ;;
    haskell:test)  echo "cabal test all" ;;
    haskell:bench) grep -qs '^benchmark' ./*.cabal && echo "cabal bench all" ;;
    haskell:run)   echo "cabal run" ;;

    bun:deps)  [ -f bun.lock ] || [ -f bun.lockb ] && echo "bun install --frozen-lockfile" || echo "bun install" ;;
    bun:build) grep -qs '"build"[[:space:]]*:' package.json && echo "bun run build" ;;
    # `bun test` exits 0 when it finds no test files, so it runs only when some exist.
    bun:test)  if grep -qs '"test"[[:space:]]*:' package.json; then echo "bun run test"
               elif git ls-files 2>/dev/null | grep -qE '(\.|_)(test|spec)\.(js|mjs|cjs|jsx)$'; then echo "bun test"; fi ;;
    bun:bench) grep -qs '"bench"[[:space:]]*:' package.json && echo "bun run bench" ;;
    bun:lint)  grep -qs '"lint"[[:space:]]*:' package.json && echo "bun run lint" ;;
    bun:fmt)   grep -qs '"fmt"[[:space:]]*:' package.json && echo "bun run fmt" ;;
    bun:fmt-check) grep -qs '"fmt-check"[[:space:]]*:' package.json && echo "bun run fmt-check" ;;
    bun:run)   grep -qs '"start"[[:space:]]*:' package.json && echo "bun run start" ;;

    docs:test) have asciidoctor && echo "for f in \$(git ls-files '*.adoc'); do asciidoctor -o /dev/null --failure-level=WARN \"\$f\" || exit 1; done" ;;
    docs:lint) have lychee && echo "lychee --offline --no-progress \$(git ls-files '*.adoc' '*.md')" ;;
  esac
}

# Run a contract verb across every detected language. N/A is reported, not
# faked green: a verb with no command for any language exits 0 but SAYS so.
lang_run() { # $1 verb
  local verb=$1 ran=0 rc=0 l cmd override
  override=$(deed "$verb" "")
  if [ -n "$override" ]; then
    info "$verb (from provisioning_praxis.deed): $override"
    bash -c "$override"; return $?
  fi
  for l in "${LANGS[@]}"; do
    cmd=$(lang_cmd "$l" "$verb")
    [ -z "$cmd" ] && continue
    ran=1
    info "$verb [$l]: $cmd"
    bash -c "$cmd" || { rc=$?; printf '  %sFAIL%s  %s [%s] exited %s\n' "$R" "$Z" "$verb" "$l" "$rc" >&2; }
  done
  [ $ran -eq 0 ] && info "$verb: N/A for ${LANGS[*]} (archetype: $ARCHETYPE) — nothing to run, and nothing was faked"
  return $rc
}

# ---------------------------------------------------------------------------
# A repo keeps its own checks as root recipes named doctor-local / setup-local /
# heal-local (provision-set renames a pre-existing custom doctor/setup/heal to
# these); the canon verbs run them, so `just doctor`, `./launcher.sh --doctor` and
# CI all see the same result.
has_recipe() { have just && just --summary 2>/dev/null | tr " " "\n" | grep -qx "$1"; }

version_ge() { [ "$(printf '%s\n%s\n' "$2" "$1" | sort -V | head -1)" = "$2" ]; }

# Tools that must never appear in a repo's toolchain (estate language policy).
BANNED_TOOLS='python|deno|node|nodejs|npm|yarn|pnpm|typescript|rescript|make|black|ruff|pip|poetry|nix|go|golang|java|kotlin'

# ---------------------------------------------------------------------------
# Shared predicates. doctor and provision-check.sh both call THESE (the check via
# `provision-lib.sh guix-stub|mise-lock-gaps|mise-banned`): a gate with its own
# copy of a test passes what doctor warns about.

# The keys of mise.toml [tools], quotes stripped ("cargo:foo" stays cargo:foo).
mise_toml_tools() {
  [ -f mise.toml ] || return 0
  awk '/^\[tools\]/{t=1;next} /^\[/{t=0} t && /=/{sub(/[ \t]*=.*/,""); gsub(/["\x27 ]/,""); print}' mise.toml
}
# Banned tools named in mise.toml; a backend prefix does not hide one ("aqua:denoland/deno" is deno).
mise_banned() {
  local t base hits=""
  for t in $(mise_toml_tools); do
    base=${t##*:}; base=${base%%@*}; base=${base##*/}
    [[ "$base" =~ ^($BANNED_TOOLS)$ ]] && hits="$hits$t "
  done
  printf '%s' "${hits% }"
}
# Why mise.lock does not pin mise.toml, or nothing when it does. Presence is not
# enough: a zero-byte lock pins nothing. Every [tools] key needs its [[tools.<key>]]
# entry, and the lock must carry checksums.
mise_lock_gaps() {
  [ -f mise.toml ] || return 0
  [ -f mise.lock ] || { echo "mise.lock missing"; return; }
  [ -s mise.lock ] || { echo "mise.lock is empty"; return; }
  local t miss=""
  # A tool is pinned when its [[tools.X]] block carries a concrete version line.
  for t in $(mise_toml_tools); do
    awk -v a="[[tools.$t]]" -v b="[[tools.\"$t\"]]" '
      $0 == a || $0 == b { inb = 1; next }
      /^\[/ { inb = 0 }
      inb && /^version = "[^"]+"/ { ok = 1 }
      END { exit !ok }' mise.lock || miss="$miss$t "
  done
  [ -n "$miss" ] && { echo "mise.lock does not pin: ${miss% }"; return; }
  # Checksums are per artefact: every [tools.X."platforms.P"] table needs its own
  # sha256, so one checksummed tool cannot vouch for another. A tool with no
  # platform tables has no artefact to checksum (core:rust installs through
  # rustup, cargo: builds from source), which is what `mise lock` writes for it.
  miss=$(awk '
    # Print the open platform table as tool/platform when it carried no sha256.
    function close_table() { if (p != "" && !c) printf "%s ", p; p = "" }
    /^\[tools\..*platforms\./ { close_table(); p = $0; c = 0
      gsub(/^\[tools\.|\]$|"/, "", p); sub(/\.platforms\./, "/", p); next }
    /^\[/ { close_table() }
    /^checksum = "sha256:[0-9a-f]+"/ { c = 1 }
    END { close_table() }' mise.lock)
  [ -z "$miss" ] || echo "mise.lock has no sha256 for: ${miss% }"
}
# Why a Guix file is a stub, or nothing when it is real. The test is positive: a
# guix.scm must define every field a package needs, not merely avoid known stub
# shapes; `(package (name "x") (source (local-file ".")))` is a stub.
guix_stub_reason() {
  local f=$1 k miss=""
  [ -f "$f" ] || { echo "missing"; return; }
  grep -qE '\{\{|__[A-Z][A-Z_]*__' "$f" && { echo "unfilled template slots"; return; }
  grep -qE '\(inputs \(list\)\)|\(source #f\)' "$f" && { echo "empty inputs or no source"; return; }
  case "${f##*/}" in
    manifest.scm) grep -q 'specifications->manifest' "$f" || echo "lists no specifications"; return ;;
    channels.scm) grep -qE '\(commit "[0-9a-f]{40}"\)' "$f" || echo "pins no commit"; return ;;
  esac
  for k in name version source build-system home-page synopsis description license; do
    grep -qE "\\(${k}[[:space:]]" "$f" || miss="$miss$k "
  done
  [ -n "$miss" ] && { echo "no package field: ${miss% }"; return; }
  if grep -q 'crates\.scm' "$f"; then
    grep -qs 'define %crate-inputs' build/guix/crates.scm || echo "build/guix/crates.scm is missing or defines no %crate-inputs"
  fi
}

# ---------------------------------------------------------------------------
# Facts the generator writes into docs/SETUP.adoc and the AI guide. They live here,
# beside lang_tools and lang_remedy, so no second per-language table exists.
lang_title() { case "$1" in
  rust) echo Rust ;; idris2) echo Idris2 ;; julia) echo Julia ;; zig) echo Zig ;; elixir) echo Elixir ;;
  gleam) echo Gleam ;; ocaml) echo OCaml ;; haskell) echo Haskell ;; bun) echo Bun ;; docs) echo Docs ;; esac; }
# What a language's route needs from the OS: "fedora|debian|macos command|why", or nothing.
lang_sysdeps() { case "$1" in
  rust)    echo "gcc pkgconf-pkg-config|build-essential pkg-config|xcode-select --install|Rust links through the system C toolchain, and pkg-config finds C libraries" ;;
  idris2)  echo "chez-scheme gmp-devel|chezscheme libgmp-dev|brew install chezscheme gmp|pack builds Idris2 on top of Chez Scheme and GMP" ;;
  elixir|gleam) echo "gcc gcc-c++ make autoconf ncurses-devel openssl-devel|build-essential autoconf m4 libncurses-dev libssl-dev|brew install autoconf openssl@3|mise builds Erlang/OTP from source (allow ~10 min); the make here is the system tool that build uses, not a Makefile in this repository" ;;
  ocaml)   echo "gcc make patch unzip bubblewrap|build-essential patch unzip bubblewrap|xcode-select --install|opam compiles OCaml and sandboxes its builds with bubblewrap" ;;
  haskell) echo "gcc gcc-c++ gmp-devel make ncurses-devel xz perl|build-essential curl libffi-dev libgmp-dev libncurses-dev|xcode-select --install|ghcup installs GHC, which links through the C toolchain and GMP" ;;
esac; }
guix_gaps() {
  local l g gaps=""
  for l in "${LANGS[@]}"; do g=$(lang_guix_gap "$l"); [ -n "$g" ] && gaps="$gaps${gaps:+, }$g"; done
  printf '%s\n' "${gaps:-none}"
}
tool_table() {
  local l t base
  for l in "${LANGS[@]}"; do
    if [ "$l" = docs ]; then echo "|docs |\`lychee\` |mise use lychee@latest"; continue; fi
    t=$(lang_tools "$l"); echo "|$l |\`${t// /\`, \`}\` |$(lang_remedy "$l")"
  done
  base="$MISE_BASE $(recipe_tools | xargs)"; base=$(echo "$base" | xargs)
  echo "|(recipes) |\`${base// /\`, \`}\` |mise install, or your OS package manager (e.g. \`dnf install just ShellCheck\`)"
}
# shellcheck disable=SC2016  # the backticks are AsciiDoc literals, not command substitution
system_deps() { # $1 adoc|ai
  local l d seen="" fed deb mac why any=0
  [ "$1" = adoc ] && printf '=== System packages\n'
  for l in "${LANGS[@]}"; do
    d=$(lang_sysdeps "$l"); [ -z "$d" ] && continue
    case "$seen" in *"|$d|"*) continue ;; esac; seen="$seen|$d|"; any=1
    IFS='|' read -r fed deb mac why <<<"$d"
    if [ "$1" = adoc ]; then
      printf '\n%s: %s.\n\n* Fedora: `sudo dnf install %s`\n* Debian/Ubuntu: `sudo apt install %s`\n* macOS: `%s`\n' \
        "$(lang_title "$l")" "$why" "$fed" "$deb" "$mac"
    else
      printf '* %s needs OS packages (%s): Fedora `sudo dnf install %s` · Debian/Ubuntu `sudo apt install %s` · macOS `%s`.\n' \
        "$(lang_title "$l")" "$why" "$fed" "$deb" "$mac"
    fi
  done
  if [ "$1" = adoc ]; then
    if [ $any -eq 1 ]; then printf '\nThe Guix development shell (`%s`) provides these itself, so inside `just dev-shell` none of them is needed.\n' "$(gpath manifest.scm)"
    else printf '\nNone beyond git and a shell: every tool this repository needs comes from mise, or from the Guix shell.\n'; fi
  elif [ $any -eq 0 ]; then printf '* No other OS packages: every tool comes from mise.\n'; fi
}

cmd_doctor() {
  printf '%s doctor — %s (%s; languages: %s)\n' "$REPO_NAME" "$REPO_SLUG" "$ARCHETYPE" "${LANGS[*]}"

  hdr "Core toolchain"
  have git && pass "git $($T git --version 2>/dev/null | awk '{print $3}')" || fail "PV-E01 git not found — install git from your OS package manager"
  if have just; then
    local jv; jv=$($T just --version 2>/dev/null | awk '{print $2}')
    version_ge "$jv" "$MIN_JUST" && pass "just $jv (>= $MIN_JUST)" || fail "PV-E02 just $jv is older than $MIN_JUST — run: mise use just@latest"
  else fail "PV-E02 just not found — run: mise use -g just@latest  (or see docs/SETUP.adoc)"; fi
  if have mise; then
    pass "mise $($T mise --version 2>/dev/null | awk '{print $1}')"
    if [ -f mise.toml ] || [ -f .mise.toml ]; then
      # Only THIS repo's declarations: `mise ls` also lists the user's global config.
      local here=${ROOT/#$HOME/\~} missing
      missing=$($T mise ls --current --missing 2>/dev/null | grep -F -e "$here/mise.toml" -e "$here/.mise.toml" -e "$ROOT/mise.toml" | awk '{print $1"@"$2}' | tr '\n' ' ')
      [ -z "${missing// /}" ] && pass "every mise tool is installed" || fail "PV-E03 mise tools not installed: $missing— run: just setup"
    fi
  else warn "PV-W01 mise not found — tools must then come from Guix or your OS; install: $MISE_INSTALL_HINT"; fi
  if have guix; then pass "guix $($T guix --version 2>/dev/null | head -1 | awk '{print $NF}') (optional reproducible path)"
  else info "guix not installed — optional; mise is the default path (docs/SETUP.adoc §Guix)"; fi

  hdr "Language toolchains"
  local l t
  for l in "${LANGS[@]}"; do
    [ "$l" = docs ] && { info "no build language detected — docs/theory archetype"; continue; }
    for t in $(lang_tools "$l"); do
      if have "$t"; then pass "$l: $t ($(command -v "$t"))"
      else fail "PV-E10 $l: '$t' not found — run: just setup   (manual: $(lang_remedy "$l"))"; fi
    done
  done
  local extra; extra=$(deed system-deps "")
  for t in $extra; do have "$t" && pass "system dep: $t" || fail "PV-E11 system dependency '$t' not found (declared in $DEED) — see docs/SETUP.adoc §Prerequisites"; done

  hdr "Repository provisioning files"
  [ -f mise.toml ] && pass "mise.toml" || fail "PV-E20 mise.toml missing — the toolchain is undeclared"
  local lg; lg=$(mise_lock_gaps)
  [ -z "$lg" ] && pass "mise.lock pins every mise.toml tool (latest → concrete, checksummed)" || warn "PV-W20 $lg — run: just toolchain-refresh"
  [ -f .mise.toml ] && [ -f mise.toml ] && warn "PV-W21 both mise.toml and .mise.toml — mise merges them; keep only mise.toml"
  [ -f .tool-versions ] && warn "PV-W22 .tool-versions present — a second toolchain source; fold it into mise.toml"
  if [ -f mise.toml ]; then
    local bad; bad=$(mise_banned)
    [ -z "$bad" ] && pass "mise.toml pins no banned tool" || warn "PV-W23 mise.toml pins banned tool(s): $bad (language policy: bun, no python/deno/node/make)"
  fi
  # hypatia guix_not_stub reads guix.scm AND build/guix.scm; an unfilled __PLACEHOLDER__ is a stub too.
  local g r gstub=""
  for g in guix.scm build/guix.scm "$(gpath manifest.scm)" "$(gpath channels.scm)"; do
    [ -f "$g" ] || continue
    r=$(guix_stub_reason "$g"); [ -n "$r" ] && gstub="$gstub$g ($r) "
  done
  local gs gm; gs=$(gpath guix.scm); gm=$(gpath manifest.scm)
  [ -f guix.scm ] && [ -f build/guix.scm ] && warn "PV-W35 both guix.scm and build/guix.scm exist — two Guix sources; keep one (build/ is used)"
  if [ -f "$gs" ]; then
    if [ -n "$gstub" ]; then warn "PV-W24 template stub in: $gstub— the Guix path does not build this repo (just heal cannot fix this; provision-set realign does)"
    else pass "$gs (non-stub)"; fi
  else warn "PV-W25 guix.scm missing — no reproducible Guix path"; fi
  [ -f "$gm" ] && pass "$gm (guix shell -m $gm)" || warn "PV-W26 $gm missing — 'just dev-shell' falls back to mise"
  [ -x launcher.sh ] && pass "launcher.sh (executable)" || { [ -f launcher.sh ] && fail "PV-E27 launcher.sh not executable — run: just heal" || warn "PV-W27 launcher.sh missing"; }
  local d
  for d in docs/SETUP.adoc docs/AI_INSTALLATION_GUIDE.adoc "$(wpath user)" "$(wpath dev)" "$(wpath maintainer)"; do
    if [ ! -f "$d" ]; then warn "PV-W28 $d missing"
    elif grep -qE "__[A-Z][A-Z_]*__" "$d"; then warn "PV-W29 $d still has unfilled template slots (__SPEC_…__) — replace each with the facts for this repo"
    else pass "$d"; fi
  done

  hdr "Estate policy drift"
  local f any=0
  for f in deno.json deno.jsonc deno.lock import_map.json; do
    [ -f "$f" ] && { warn "PV-W30 $f — Deno leftover; the estate runtime is bun (migration issue tracks it)"; any=1; }
  done
  for f in flake.nix flake.lock shell.nix default.nix; do [ -f "$f" ] && { warn "PV-W31 $f — Nix is not an estate toolchain; Guix is"; any=1; }; done
  for f in Makefile GNUmakefile makefile; do [ -f "$f" ] && { warn "PV-W32 $f — Justfile is the only task runner"; any=1; }; done
  for f in requirements.txt pyproject.toml setup.py Pipfile; do [ -f "$f" ] && { warn "PV-W33 $f — Python is not an estate language"; any=1; }; done
  for f in package-lock.json yarn.lock pnpm-lock.yaml tsconfig.json; do [ -f "$f" ] && { warn "PV-W34 $f — npm/yarn/pnpm/TypeScript leftover; bun only"; any=1; }; done
  [ $any -eq 0 ] && pass "no deno / nix / make / python / npm leftovers"

  local hook=build/just/doctor-local.sh
  if [ -f "$hook" ]; then
    hdr "Repo-specific checks ($hook)"
    # Sourced so the hook can call pass/warn/fail, but in a subshell so an `exit`
    # or `set -e` in it cannot end the doctor before the summary. The EXIT trap
    # hands the hook's tally back; an early exit is itself a FAIL.
    local tally qtally hp w f rc
    tally=$(mktemp)
    printf -v qtally '%q' "$tally"
    # shellcheck disable=SC2030,SC2031  # the subshell's counts return via $tally
    ( PASS=0 WARN=0 FAIL=0 hook_done=0
      # The path is baked in now: when `set -e` trips, bash unwinds this
      # function's locals before the EXIT trap runs, so $tally is gone by then.
      # shellcheck disable=SC2064
      trap "printf '%d %d %d %d\n' \"\$PASS\" \"\$WARN\" \"\$FAIL\" \"\$hook_done\" > $qtally" EXIT
      # shellcheck source=/dev/null
      . "$hook"
      hook_done=1 )
    rc=$?
    read -r hp w f hook_done < "$tally" || { hp=0 w=0 f=0 hook_done=0; }
    rm -f "$tally"
    # shellcheck disable=SC2031
    PASS=$((PASS+hp)) WARN=$((WARN+w)) FAIL=$((FAIL+f))
    [ "$hook_done" = 1 ] || fail "PV-E51 $hook exited (status $rc) before it finished; its later checks did not run"
  fi
  if has_recipe doctor-local; then
    hdr "Repo-specific checks (just doctor-local)"
    just doctor-local && pass "doctor-local" || fail "PV-E50 the repo-specific doctor-local recipe failed (output above)"
  fi

  printf '\n%s: %s%d PASS%s, %s%d WARN%s, %s%d FAIL%s\n' "$REPO_NAME" "$G" "$PASS" "$Z" "$Y" "$WARN" "$Z" "$R" "$FAIL" "$Z"
  if [ "$FAIL" -gt 0 ]; then
    echo "Next: 'just heal' fixes what is safe to fix automatically; each FAIL code is explained in docs/SETUP.adoc §Troubleshooting."
    return 1
  fi
  return 0
}

cmd_setup() {
  printf '%s setup — installing everything this repository needs\n' "$REPO_NAME"
  local rc=0
  if have mise; then
    hdr "mise toolchain"
    $T mise trust -q . 2>/dev/null || true
    mise install || rc=1
  else
    warn "mise not found. Install it ($MISE_INSTALL_HINT), or enter the Guix shell with 'just dev-shell', then re-run: just setup"
  fi
  local l
  for l in "${LANGS[@]}"; do
    case "$l" in
      idris2) have pack || warn "pack not found — Idris2 comes from pack, not mise: $(lang_remedy idris2)" ;;
      haskell) have ghcup || have ghc || warn "ghc not found — $(lang_remedy haskell)" ;;
      ocaml) have opam && { opam switch show >/dev/null 2>&1 || opam init -y --bare; } ;;
    esac
  done
  hdr "Project dependencies"
  # Put this repo's mise tools on PATH for the dependency step.
  if have mise; then PATH="$(mise bin-paths 2>/dev/null | paste -sd: -):$PATH"; export PATH; fi
  lang_run deps || rc=1
  local hook=build/just/setup-local.sh
  [ -f "$hook" ] && { hdr "Repo-specific setup ($hook)"; bash "$hook" || rc=1; }
  has_recipe setup-local && { hdr "Repo-specific setup (just setup-local)"; just setup-local || rc=1; }
  [ -f launcher.sh ] && chmod +x launcher.sh
  hdr "Verification"
  cmd_doctor || rc=1
  [ $rc -eq 0 ] && printf '\n%sReady.%s Try: just --list\n' "$G" "$Z" || printf '\n%sSetup incomplete%s — read the FAIL lines above; docs/SETUP.adoc has the manual route.\n' "$R" "$Z"
  return $rc
}

cmd_heal() {
  printf '%s heal — applying safe, reversible fixes, then re-checking\n' "$REPO_NAME"
  hdr "Fixes"
  [ -f launcher.sh ] && [ ! -x launcher.sh ] && chmod +x launcher.sh && info "made launcher.sh executable"
  for f in build/just/*.sh scripts/*.sh; do [ -f "$f" ] && [ ! -x "$f" ] && chmod +x "$f" && info "made $f executable"; done
  if have mise; then
    $T mise trust -q . 2>/dev/null && info "mise config trusted"
    mise install && info "mise tools installed"
    [ -f mise.lock ] || { $T mise lock 2>/dev/null && info "mise.lock generated"; }
  fi
  if printf '%s\n' "${LANGS[@]}" | grep -qx elixir && have mix; then mix deps.get >/dev/null 2>&1 && info "mix deps fetched"; fi
  if printf '%s\n' "${LANGS[@]}" | grep -qx bun && have bun; then bun install >/dev/null 2>&1 && info "bun deps installed"; fi
  if printf '%s\n' "${LANGS[@]}" | grep -qx julia && have julia; then julia --project=. -e 'using Pkg; Pkg.instantiate()' >/dev/null 2>&1 && info "julia deps instantiated"; fi
  local hook=build/just/heal-local.sh
  [ -f "$hook" ] && { info "running $hook"; bash "$hook" || true; }
  has_recipe heal-local && { info "running just heal-local"; just heal-local || true; }
  echo "Not touched (never automatic): source files, git history, Deno/Nix/Make leftovers — those are migrations, tracked as issues."
  hdr "Re-check"
  cmd_doctor
}

cmd_dev_shell() {
  local gm; gm=$(gpath manifest.scm)
  if have guix && [ -f "$gm" ] && ! grep -q '{{' "$gm"; then
    info "entering: guix shell -m $gm  (exit to leave)"
    exec guix shell -m "$gm"
  elif have mise; then
    info "guix/manifest.scm unavailable — entering the mise environment instead (exit to leave)"
    exec mise exec -- "${SHELL:-bash}"
  else
    fail "PV-E40 neither guix nor mise is installed — see docs/SETUP.adoc"; return 1
  fi
}

# Weekly toolchain refresh: bump mise pins and the lock, re-pin the Guix channel,
# regenerate build/guix/crates.scm when guix.scm loads it, then show the diff
# for a signed commit.
cmd_toolchain_refresh() {
  hdr "mise: bump 'latest' resolutions and re-lock"
  have mise || { fail "mise not found"; return 1; }
  mise up --bump || return 1
  $T mise lock 2>/dev/null || mise lock || return 1
  local ch; ch=$(gpath channels.scm)
  if [ -f "$ch" ]; then
    hdr "guix: channel pin"
    if have guix; then
      guix pull --channels="$ch" --dry-run >/dev/null 2>&1 || true
      guix describe --format=channels > "$ch.new" 2>/dev/null && mv "$ch.new" "$ch" && info "$ch re-pinned to the current guix commit"
    else info "guix not installed — $ch left as-is (CI re-pins it)"; fi
  fi
  local cr=""
  if [ -f Cargo.lock ] && grep -qs 'crates\.scm' "$(gpath guix.scm)"; then
    hdr "guix: crate inputs from Cargo.lock"
    cr=build/guix/crates.scm
    if have "${GUIX%% *}"; then cmd_crates_scm || return 1
    else info "guix not installed — $cr left as-is (CI regenerates it)"; fi
  fi
  git --no-pager diff --stat -- mise.toml mise.lock "$ch" $cr 2>/dev/null
  echo "Commit the diff above (signed) as: chore(toolchain): weekly refresh"
}

# The guix command (default: guix), e.g. a wrapper that runs it in a container
# with this directory mounted.
GUIX="${GUIX:-guix}"

# Write build/guix/crates.scm: every registry crate in Cargo.lock as a Guix
# origin, and %crate-inputs listing them for guix.scm. Written whole or not at
# all. The importer's output is accepted only when it defines exactly one crate
# source per registry package in Cargo.lock: run through a container, guix's exit
# status is lost, so the count is the check.
cmd_crates_scm() {
  local dst=build/guix/crates.scm want got tmp spdx
  [ -f Cargo.lock ] || { info "no Cargo.lock — no crate inputs"; return 0; }
  want=$(grep -c '^source = "registry+' Cargo.lock)
  tmp=$(mktemp) || return 1
  if [ "$want" -gt 0 ]; then
    # GUIX is split on purpose: it may be a command with arguments.
    # shellcheck disable=SC2086
    timeout 1800 $GUIX import crate --lockfile=Cargo.lock "$REPO_NAME" >"$tmp" 2>"$tmp.err"
    got=$(grep -c '^(define rust-' "$tmp")
    if [ "$got" != "$want" ]; then
      fail "PV-E41 guix import crate defined $got of the $want registry crates in Cargo.lock; $dst left as-is: $(grep -m1 -i 'error' "$tmp.err" || grep -v '^+' "$tmp.err" | tail -1)"
      rm -f "$tmp" "$tmp.err"; return 1
    fi
  fi
  # The licence line is guix.scm's own, so the file matches its repository.
  spdx=$(grep -m1 'SPDX-License-Identifier' "$(gpath guix.scm)" 2>/dev/null)
  mkdir -p build/guix
  {
    [ -n "$spdx" ] && printf '%s\n' "$spdx"
    # shellcheck disable=SC2016  # the backticks are literal text
    printf ';; Generated from Cargo.lock by `just toolchain-refresh` (guix import crate\n'
    printf ';; --lockfile); never hand-edit it. guix.scm loads it for %%crate-inputs.\n\n'
    grep -v '^guix (GNU Guix)' "$tmp"
    printf '\n(define %%crate-inputs\n  (list'
    grep -o '^(define rust-[^ ]*' "$tmp" | awk '{printf "\n    %s", $2}'
    printf '))\n'
  } >"$dst.new" && mv "$dst.new" "$dst"
  rm -f "$tmp" "$tmp.err"
  info "$dst: $want crate source(s)"
}

# The sentence people are told to say lives in one place a reader sees: the first
# listing block of the README's [[ai-install]] section. The deed's ai-say-it and
# the generic line are fallbacks for a README without that section.
just_say_it() {
  local line="" r
  for r in README.adoc README.md; do
    [ -f "$r" ] || continue
    line=$(awk '/^\[\[ai-install\]\]/{on=1} on && /^----$/{if (inb) exit; inb=1; next} inb && NF{print; exit}' "$r")
    [ -n "$line" ] && break
  done
  [ -z "$line" ] && line=$(deed ai-say-it "")
  [ -z "$line" ] && line="Set up $REPO_NAME from https://github.com/$REPO_SLUG — follow docs/AI_INSTALLATION_GUIDE.adoc in that repo."
  printf '%s' "$line"
}
clip() { # best-effort; silent when no clipboard exists (CI, SSH)
  # wl-copy and xclip fork a daemon that inherits stdout; left attached, it
  # holds any pipeline open forever (observed 2026-09-30). Detach and bound it.
  local c=()
  if have clip.exe; then c=(clip.exe)                       # WSL: the Windows clipboard
  elif have pbcopy; then c=(pbcopy)
  elif [ -n "${WAYLAND_DISPLAY:-}" ] && have wl-copy; then c=(wl-copy)
  elif [ -n "${DISPLAY:-}" ] && have xclip; then c=(xclip -selection clipboard)
  else cat >/dev/null; return 1; fi
  timeout 3 "${c[@]}" >/dev/null 2>&1
}
cmd_ai_setup() {
  cat <<EOF
AI-assisted setup for $REPO_NAME
================================
Give any AI assistant (Claude, ChatGPT, Gemini, a local model…) this one line:

    $(just_say_it)

It will read docs/AI_INSTALLATION_GUIDE.adoc, ask you the few questions listed
there, run 'just setup', and prove the result with 'just doctor'. Nothing is
vendor-specific; the guide is plain AsciiDoc any agent can follow.

Prefer to do it yourself?  docs/SETUP.adoc is the same route as manual steps.
Working on the code with an AI?  just ai-warmup dev   (or: user, maintainer)
EOF
  just_say_it | clip && echo "(copied the line to your clipboard)" || true
}
cmd_ai_warmup() {
  local who=${1:-user} f
  case "$who" in user|dev|maintainer) ;; *) echo "usage: just ai-warmup <user|dev|maintainer>" >&2; return 2 ;; esac
  for f in "$(wpath "$who")" "llm-warmup-$who.adoc" "docs/onboarding/llm-warmup-$who.adoc" "docs/llm-warmup-$who.adoc"; do
    if [ -f "$f" ]; then
      cat "$f"; clip < "$f" && echo "--- (copied $f to your clipboard — paste it as your first message to any AI)" >&2 || true
      return 0
    fi
  done
  fail "llm-warmup-$who.adoc not found"; return 1
}

cmd_eval() {
  local logf rc=0 s e v r o st
  logf=".eval/$(date -u +%Y%m%dT%H%M%SZ).txt"
  mkdir -p .eval
  { echo "# $REPO_NAME evaluation — $(date -u +%FT%TZ) — $(git rev-parse --short HEAD 2>/dev/null || echo no-git)"
    echo "# languages: ${LANGS[*]}   archetype: $ARCHETYPE"; } > "$logf"
  for v in test bench; do
    # The root recipe if the repo defines one (its own override), else the module default.
    if just --summary 2>/dev/null | tr " " "\n" | grep -qx "$v"; then r=$v; else r="provision::$v"; fi
    s=$(date +%s); o=$(mktemp)
    if just "$r" >"$o" 2>&1; then st=PASS; else st=FAIL; rc=1; fi
    e=$(( $(date +%s) - s )); cat "$o" >>"$logf"
    # A skip is not a pass: lang_run exits 0 on N/A (users see success), eval says N/A.
    [ "$st" = PASS ] && grep -q "nothing was faked" "$o" && st=N/A
    rm -f "$o"; echo "$v: $st (${e}s)" | tee -a "$logf"
  done
  echo "full log: $logf"
  return $rc
}

cmd_config_show() {
  echo "repo:        $REPO_SLUG"
  echo "archetype:   $ARCHETYPE"
  echo "languages:   ${LANGS[*]}"
  echo "descriptor:  $DEED $([ -f "$DEED" ] && echo '(present)' || echo '(absent — defaults in use)')"
  echo "config:      $(deed config "none declared")"
  echo "run:         $(deed run "$(for l in "${LANGS[@]}"; do lang_cmd "$l" run; done | head -1)")"
  echo "ports:       $(deed ports "none")"
  echo "mise:        $([ -f mise.toml ] && echo mise.toml) $([ -f mise.lock ] && echo mise.lock)"
  echo "guix:        $(for f in guix.scm manifest.scm channels.scm; do f=$(gpath "$f"); [ -f "$f" ] && printf "%s " "$f"; done)"
  echo "lib:         provision-lib $PROVISION_LIB_VERSION"
}

cmd_opsm() {
  cat <<EOF
Get $REPO_NAME with OPSM (the odds-and-sods package manager):

    opsm install https://github.com/$REPO_SLUG.git --registry git

Then provision it from the checkout: ./launcher.sh --setup   (or: just setup)

$(deed opsm-note "No registry package is published for $REPO_NAME, so OPSM fetches it through its git registry adapter.")

OPSM is not installed? It builds from source (Erlang/OTP 26+, Elixir 1.16+):

    git clone https://github.com/hyperpolymath/odds-and-sods-package-manager.git
    cd odds-and-sods-package-manager/opsm_ex
    mix deps.get && mix escript.build
    install -m 0755 opsm ~/.local/bin/opsm     # or: sudo cp opsm /usr/local/bin/
    opsm --version

(Syntax verified 2026-09-30 against odds-and-sods-package-manager README.adoc §Installation
and §Install Packages at 9aff310.)
EOF
}

cmd_search() {
  local p=${1:?usage: just search <pattern>}
  if have agrep; then git ls-files -z | xargs -0 agrep -n -1 -- "$p" 2>/dev/null
  else git grep -n -i -- "$p"; fi
}

case "${1:-}" in
  guix-specs)         guix_specs ;;
  mise-tools)         mise_tools ;;
  langs)              printf '%s\n' "${LANGS[@]}" ;;
  doctor)             cmd_doctor ;;
  setup)              cmd_setup ;;
  heal)               cmd_heal ;;
  dev-shell)          cmd_dev_shell ;;
  toolchain-refresh)  cmd_toolchain_refresh ;;
  crates-scm)         cmd_crates_scm ;;
  ai-setup)           cmd_ai_setup ;;
  ai-warmup)          shift; cmd_ai_warmup "${1:-user}" ;;
  eval)               cmd_eval ;;
  config-show)        cmd_config_show ;;
  opsm)               cmd_opsm ;;
  search)             shift; cmd_search "${1:-}" ;;
  lang-run)           shift; lang_run "${1:?verb}" ;;
  version)            echo "provision-lib $PROVISION_LIB_VERSION" ;;
  guix-dir)           guix_dir ;;
  set-files)          set_files ;;
  guix-gaps)          guix_gaps ;;
  tool-table)         tool_table ;;
  system-deps)        shift; system_deps "${1:-adoc}" ;;
  # Predicates: print why, exit 1; print nothing, exit 0.
  guix-stub)          shift; r=$(guix_stub_reason "${1:?usage: guix-stub FILE}"); [ -z "$r" ] || { echo "$r"; exit 1; } ;;
  mise-lock-gaps)     r=$(mise_lock_gaps); [ -z "$r" ] || { echo "$r"; exit 1; } ;;
  mise-banned)        r=$(mise_banned); [ -z "$r" ] || { echo "$r"; exit 1; } ;;
  *) sed -n '2,20p' "$0"; exit 2 ;;
esac
