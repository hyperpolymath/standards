# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# containerfile-to-stapeln.jl — Convert Containerfiles to stapeln.toml service definitions
#
# Usage:
#   julia scripts/containerfile-to-stapeln.jl [--dry-run] [--commit]
#
# Scans all repos for Containerfiles, generates stapeln.toml alongside them.
# Does NOT overwrite existing stapeln.toml files.

const REPOS_DIR = joinpath("/var", "mnt", "eclipse", "repos")

# Repos that already have stapeln.toml
const SKIP_REPOS = Set(["verisimdb", "nextgen-databases"])

function parse_containerfile(path::String)::Dict{String,Any}
    content = read(path, String)
    lines = split(content, "\n")

    info = Dict{String,Any}(
        "from" => "cgr.dev/chainguard/wolfi-base:latest",
        "packages" => String[],
        "env" => Dict{String,String}(),
        "expose" => Int[],
        "workdir" => "/app",
        "copy_commands" => String[],
        "run_commands" => String[],
        "entrypoint" => String[],
        "cmd" => String[],
        "user" => "",
        "language" => "unknown",
    )

    for line in lines
        stripped = strip(line)
        isempty(stripped) && continue
        startswith(stripped, "#") && continue

        if startswith(uppercase(stripped), "FROM ")
            info["from"] = strip(stripped[5:end])
            # Detect language from base image
            lower_from = lowercase(info["from"])
            if occursin("rust", lower_from) || occursin("cargo", lower_from)
                info["language"] = "rust"
            elseif occursin("elixir", lower_from) || occursin("erlang", lower_from)
                info["language"] = "elixir"
            elseif occursin("node", lower_from) || occursin("deno", lower_from)
                info["language"] = "deno"
            elseif occursin("zig", lower_from)
                info["language"] = "zig"
            elseif occursin("haskell", lower_from) || occursin("ghc", lower_from)
                info["language"] = "haskell"
            elseif occursin("julia", lower_from)
                info["language"] = "julia"
            elseif occursin("wolfi", lower_from) || occursin("chainguard", lower_from)
                info["language"] = "chainguard"
            end
        elseif startswith(uppercase(stripped), "RUN ")
            cmd = strip(stripped[5:end])
            push!(info["run_commands"], cmd)
            # Detect packages from apk/apt
            if occursin("apk add", cmd)
                pkgs = match(r"apk add\s+(?:--no-cache\s+)?(.*)", cmd)
                if pkgs !== nothing
                    append!(info["packages"], split(strip(pkgs.captures[1])))
                end
            end
        elseif startswith(uppercase(stripped), "ENV ")
            m = match(r"ENV\s+(\w+)\s*=?\s*(.*)", stripped)
            if m !== nothing
                info["env"][m.captures[1]] = strip(m.captures[2], ['"', '\''])
            end
        elseif startswith(uppercase(stripped), "EXPOSE ")
            for p in split(strip(stripped[8:end]))
                port = tryparse(Int, replace(p, r"/.*" => ""))
                port !== nothing && push!(info["expose"], port)
            end
        elseif startswith(uppercase(stripped), "WORKDIR ")
            info["workdir"] = strip(stripped[9:end])
        elseif startswith(uppercase(stripped), "COPY ")
            push!(info["copy_commands"], strip(stripped[6:end]))
        elseif startswith(uppercase(stripped), "ENTRYPOINT ")
            info["entrypoint"] = [strip(stripped[12:end])]
        elseif startswith(uppercase(stripped), "CMD ")
            info["cmd"] = [strip(stripped[5:end])]
        elseif startswith(uppercase(stripped), "USER ")
            info["user"] = strip(stripped[6:end])
        end
    end

    # Detect language from RUN commands if not found from base image
    if info["language"] == "unknown" || info["language"] == "chainguard"
        all_cmds = join(info["run_commands"], " ")
        if occursin("cargo", all_cmds) || occursin("rustc", all_cmds)
            info["language"] = "rust"
        elseif occursin("mix ", all_cmds) || occursin("elixir", all_cmds)
            info["language"] = "elixir"
        elseif occursin("deno", all_cmds)
            info["language"] = "deno"
        elseif occursin("zig build", all_cmds)
            info["language"] = "zig"
        elseif occursin("cabal", all_cmds) || occursin("stack build", all_cmds)
            info["language"] = "haskell"
        elseif occursin("julia", all_cmds)
            info["language"] = "julia"
        elseif occursin("gleam", all_cmds)
            info["language"] = "gleam"
        elseif occursin("idris2", all_cmds)
            info["language"] = "idris2"
        end
    end

    return info
end

function generate_stapeln_toml(repo_name::String, info::Dict{String,Any})::String
    lang = info["language"]
    user = isempty(info["user"]) ? repo_name : info["user"]
    expose_str = isempty(info["expose"]) ? "" : "\nexpose = [$(join(info["expose"], ", "))]"

    # Build env section
    env_pairs = String[]
    for (k, v) in info["env"]
        push!(env_pairs, "$(k) = \"$(v)\"")
    end
    env_str = isempty(env_pairs) ? "" : "\nenv = { $(join(env_pairs, ", ")) }"

    # Build packages section
    pkg_str = isempty(info["packages"]) ? "[]" :
        "[" * join(map(p -> "\"$p\"", info["packages"]), ", ") * "]"

    # Build layer based on language
    toolchain_layer = ""
    build_layer = ""
    cache_key = ""

    if lang == "rust"
        toolchain_layer = """

[layers.rust-toolchain]
description = "Rust compiler and build dependencies"
extends = "base"
packages = ["rust", "pkgconf", "build-base"]
cache = true

[layers.rust-deps]
description = "Cargo dependency fetch"
extends = "rust-toolchain"
commands = ["cargo fetch --locked"]
cache-key = "Cargo.lock"
cache = true
"""
        build_layer = """
[layers.build]
description = "$repo_name Rust compilation"
extends = "rust-deps"
commands = ["cargo build --release"]
artifacts = [
    { src = "target/release/$repo_name", dst = "/app/$repo_name" },
]
"""
    elseif lang == "elixir"
        toolchain_layer = """

[layers.elixir-toolchain]
description = "Elixir/OTP runtime"
extends = "base"
packages = ["erl27-elixir-1.18", "erlang-27", "git", "build-base"]
cache = true

[layers.elixir-deps]
description = "Mix dependency fetch"
extends = "elixir-toolchain"
env = { MIX_ENV = "prod" }
commands = [
    "mix local.hex --force",
    "mix local.rebar --force",
    "mix deps.get --only prod",
    "mix compile",
]
cache-key = "mix.lock"
cache = true
"""
        build_layer = """
[layers.build]
description = "$repo_name Elixir release"
extends = "elixir-deps"
commands = ["mix release"]
artifacts = [
    { src = "_build/prod/rel/$repo_name", dst = "/app/$repo_name/" },
]
"""
    elseif lang == "deno"
        toolchain_layer = """

[layers.deno-toolchain]
description = "Deno runtime"
extends = "base"
packages = ["deno"]
cache = true
"""
        build_layer = """
[layers.build]
description = "$repo_name Deno build"
extends = "deno-toolchain"
commands = ["deno cache src/main.ts || true"]
"""
    elseif lang == "zig"
        toolchain_layer = """

[layers.zig-toolchain]
description = "Zig compiler"
extends = "base"
packages = ["zig"]
cache = true
"""
        build_layer = """
[layers.build]
description = "$repo_name Zig compilation"
extends = "zig-toolchain"
commands = ["zig build -Doptimize=ReleaseSafe"]
artifacts = [
    { src = "zig-out/bin/$repo_name", dst = "/app/$repo_name" },
]
"""
    elseif lang == "haskell"
        toolchain_layer = """

[layers.haskell-toolchain]
description = "GHC and Cabal"
extends = "base"
packages = ["ghc", "cabal-install"]
cache = true

[layers.haskell-deps]
description = "Cabal dependency fetch"
extends = "haskell-toolchain"
commands = ["cabal update", "cabal build --only-dependencies"]
cache-key = "*.cabal"
cache = true
"""
        build_layer = """
[layers.build]
description = "$repo_name Haskell compilation"
extends = "haskell-deps"
commands = ["cabal build"]
"""
    else
        # Generic layer
        toolchain_layer = """

[layers.toolchain]
description = "Build tools"
extends = "base"
packages = $pkg_str
cache = true
"""
        build_layer = """
[layers.build]
description = "$repo_name build"
extends = "toolchain"
commands = $(isempty(info["run_commands"]) ? "[]" : "[\"$(join(info["run_commands"][1:min(3, length(info["run_commands"]))], "\", \""))\"]")
"""
    end

    entrypoint_str = if !isempty(info["entrypoint"])
        "\nentrypoint = [\"$(join(info["entrypoint"], "\", \""))\"]"
    elseif !isempty(info["cmd"])
        "\nentrypoint = [\"$(join(info["cmd"], "\", \""))\"]"
    else
        "\nentrypoint = [\"/app/$repo_name\"]"
    end

    return """# SPDX-License-Identifier: PMPL-1.0-or-later
# stapeln.toml — Layer-based container build for $repo_name
#
# stapeln builds containers as composable layers (German: "to stack").
# Each layer is independently cacheable, verifiable, and signable.

[metadata]
name = "$repo_name"
version = "0.1.0"
description = "$repo_name container service"
author = "Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>"
license = "PMPL-1.0-or-later"
registry = "ghcr.io/hyperpolymath"

[build]
containerfile = "Containerfile"
context = "."
runtime = "podman"

# ── Layer Definitions ──────────────────────────────────────────

[layers.base]
description = "Chainguard Wolfi minimal base"
from = "cgr.dev/chainguard/wolfi-base:latest"
cache = true
verify = true
$toolchain_layer
$build_layer
[layers.runtime]
description = "Minimal runtime"
from = "cgr.dev/chainguard/wolfi-base:latest"
packages = ["ca-certificates", "curl"]
copy-from = [
    { layer = "build", src = "/app/", dst = "/app/" },
]$entrypoint_str
user = "$user"$expose_str$env_str

# ── Security ───────────────────────────────────────────────────

[security]
non-root = true
read-only-root = false
no-new-privileges = true
cap-drop = ["ALL"]
seccomp-profile = "default"

[security.signing]
algorithm = "ML-DSA-87"
provider = "cerro-torre"

[security.sbom]
format = "spdx-json"
output = "sbom.spdx.json"
include-deps = true

# ── Verification ───────────────────────────────────────────────

[verify]
vordr = true
svalinn = true
scan-on-build = true
fail-on = ["critical", "high"]

# ── Targets ────────────────────────────────────────────────────

[targets.development]
layers = ["base", "$(lang == "unknown" ? "toolchain" : lang * "-toolchain")", "build"]
env = { LOG_LEVEL = "debug" }

[targets.production]
layers = ["runtime"]
env = { LOG_LEVEL = "info" }

[targets.test]
layers = ["base", "$(lang == "unknown" ? "toolchain" : lang * "-toolchain")", "build"]
env = { LOG_LEVEL = "debug" }
"""
end

function main()
    dry_run = "--dry-run" in ARGS
    do_commit = "--commit" in ARGS

    println("╔══════════════════════════════════════════════════════════════╗")
    println("║  Containerfile → Stapeln Conversion                         ║")
    println("║  $(dry_run ? "DRY RUN" : "LIVE")                                                      ║")
    println("╚══════════════════════════════════════════════════════════════╝")
    println()

    converted = 0
    skipped = 0

    for entry in sort(readdir(REPOS_DIR))
        repo_path = joinpath(REPOS_DIR, entry)
        !isdir(repo_path) && continue
        entry in SKIP_REPOS && continue

        containerfile = joinpath(repo_path, "Containerfile")
        if !isfile(containerfile)
            containerfile = joinpath(repo_path, "Dockerfile")
            !isfile(containerfile) && continue
        end

        # Skip if stapeln.toml already exists
        stapeln_path = joinpath(repo_path, "stapeln.toml")
        if isfile(stapeln_path)
            skipped += 1
            continue
        end

        println("[$entry] Converting $(basename(containerfile))...")

        info = parse_containerfile(containerfile)
        toml_content = generate_stapeln_toml(entry, info)

        if dry_run
            println("  [DRY] Would create stapeln.toml ($(info["language"]) detected)")
        else
            write(stapeln_path, toml_content)
            println("  [ADD] Created stapeln.toml ($(info["language"]) detected)")

            if do_commit && isdir(joinpath(repo_path, ".git"))
                cd(repo_path) do
                    try
                        run(`git add stapeln.toml`)
                        run(`git commit -m "feat: add stapeln.toml layer-based container definition\n\nConverted from existing Containerfile to stapeln format.\nIncludes Chainguard base, security hardening, SBOM generation.\n\nCo-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>"`)
                        println("  [GIT] Committed")
                    catch e
                        @warn "Git failed: $e"
                    end
                end
            end
        end

        converted += 1
    end

    println()
    println("═══════════════════════════════════════════════════")
    println("  Converted: $converted")
    println("  Skipped (already have stapeln.toml): $skipped")
    println("═══════════════════════════════════════════════════")
end

main()
