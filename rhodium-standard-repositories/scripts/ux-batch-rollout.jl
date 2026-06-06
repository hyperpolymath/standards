# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# ux-batch-rollout.jl — Batch rollout UX Manifesto infrastructure to all repos
#
# Usage:
#   julia scripts/ux-batch-rollout.jl [--dry-run] [--fix-paths] [--deploy-templates] [--all]
#
# Actions:
#   --deploy-templates  Deploy quickstarts, contractiles, setup.sh, doctor/heal recipes
#   --fix-paths         Fix hardcoded absolute paths
#   --all               Do everything
#   --dry-run           Show what would be changed without making changes
#   --repo <name>       Target a specific repo only
#   --commit            Auto-commit changes per repo
#   --push              Auto-push after commit

using Dates

# ── Configuration ──────────────────────────────────────────────────
# NOTE: Paths are built from segments to prevent this script from modifying itself
# when run with --fix-paths. Do NOT inline these as string literals.

const REPOS_DIR = joinpath("/var", "mnt", "eclipse", "repos")
const TEMPLATE_DIR = joinpath(@__DIR__, "..", "templates")

# Files that should never be modified (binary, generated, vendored)
const SKIP_EXTENSIONS = Set([
    ".png", ".jpg", ".jpeg", ".gif", ".svg", ".ico", ".webp", ".bmp",
    ".woff", ".woff2", ".ttf", ".eot", ".otf",
    ".zip", ".tar", ".gz", ".bz2", ".xz", ".7z", ".zst",
    ".pdf", ".doc", ".docx", ".xls", ".xlsx",
    ".wasm", ".so", ".dylib", ".dll", ".o", ".a",
    ".pyc", ".pyo", ".class",
    ".mp3", ".mp4", ".wav", ".ogg", ".webm", ".flac",
    ".db", ".sqlite", ".sqlite3",
    ".lock",  # Lock files managed by tools
    ".cmi", ".cmt", ".cmti", ".cmo", ".cmx", ".cmxa",  # OCaml compiled
    ".beam",  # Erlang compiled
    ".res.mjs",  # ReScript compiled output
    ".jl",  # Julia scripts (prevent self-modification)
    ".ast",  # ReScript AST files (binary-ish)
])

const SKIP_DIRS = Set([
    ".git", "node_modules", "_build", "target", "dist", "build",
    "__pycache__", ".lake", "vendor", "generated", ".zig-cache",
    "_opam", ".stack-work", ".cabal", "elm-stuff", "deps",
    ".deno", ".cache", "public/assets", "lib/bs", "lib/ocaml",
])

# Repos that already have full UX treatment (skip them)
const ALREADY_TREATED = Set([
    "hypatia", "proven", "echidna", "panll", "boj-server",
    "ambientops", "verisimdb", "idaptik", "gossamer",
    "groove-browser-harness", "tma-mark2", "polyglot-i18n",
])

# Build path patterns at runtime (segments joined, not literals)
const PATH_PATTERNS = let
    home_prefix = joinpath("/home", "hyper")
    mnt_repos = joinpath("/mnt", "eclipse", "repos")
    var_mnt_repos = joinpath("/var", "mnt", "eclipse", "repos")
    mnt_generated = joinpath("/mnt", "eclipse", "generated")
    mnt_eclipse = joinpath("/mnt", "eclipse")
    [
        Regex(replace(home_prefix, "/" => "\\/") * "(?!/\\.)") => "\$HOME",
        Regex(replace(var_mnt_repos, "/" => "\\/")) => "\$REPOS_DIR",
        Regex(replace(mnt_repos, "/" => "\\/")) => "\$REPOS_DIR",
        Regex(replace(mnt_generated, "/" => "\\/")) => "\$GENERATED_DIR",
        Regex(replace(mnt_eclipse, "/" => "\\/")) => "\$ECLIPSE_DIR",
    ]
end

# Files where hardcoded paths are intentional (don't fix)
const PATH_EXCEPTIONS = Set([
    "CLAUDE.md",           # AI instructions reference real paths
    "MEMORY.md",           # Memory files reference real paths
    ".claude/CLAUDE.md",   # Same
    "setup.sh",            # Setup detects paths dynamically
])

# ── Template loading ───────────────────────────────────────────────

function load_template(name::String)::String
    path = joinpath(TEMPLATE_DIR, name)
    if !isfile(path)
        @warn "Template not found: $name"
        return ""
    end
    return read(path, String)
end

function fill_template(content::String, repo_name::String)::String
    description = replace(repo_name, "-" => " ") |> titlecase
    main_command = "just run"

    replacements = Dict(
        "{{REPO}}" => repo_name,
        "{{DESCRIPTION}}" => "$description — See README.adoc for details.",
        "{{MAIN_COMMAND}}" => main_command,
        "{{EXAMPLE_OUTPUT}}" => "$description started successfully.",
        "{{LINUX_PREREQS}}" => "See README.adoc",
        "{{MACOS_PREREQS}}" => "See README.adoc",
        "{{WINDOWS_PREREQS}}" => "See README.adoc",
        "{{SPECIFIC_INVARIANT}}" => "# Add project-specific invariants here",
        "{project-name}" => repo_name,
        "{version}" => "0.1.0",
        "{primary-language}" => detect_language(joinpath(REPOS_DIR, repo_name)),
    )

    result = content
    for (k, v) in replacements
        result = replace(result, k => v)
    end
    return result
end

function detect_language(repo_path::String)::String
    if isfile(joinpath(repo_path, "rescript.json"))
        return "rescript"
    elseif isfile(joinpath(repo_path, "Cargo.toml"))
        return "rust"
    elseif isfile(joinpath(repo_path, "mix.exs"))
        return "elixir"
    elseif isfile(joinpath(repo_path, "gleam.toml"))
        return "gleam"
    elseif isfile(joinpath(repo_path, "deno.json")) || isfile(joinpath(repo_path, "deno.jsonc"))
        return "deno"
    elseif isdir(joinpath(repo_path, "src", "abi"))
        return "idris2"
    elseif isfile(joinpath(repo_path, "build.zig"))
        return "zig"
    elseif isfile(joinpath(repo_path, "package.json"))
        return "javascript"
    else
        return "mixed"
    end
end

# ── Path fixing ────────────────────────────────────────────────────

function should_fix_file(filepath::String)::Bool
    ext = lowercase(splitext(filepath)[2])
    ext in SKIP_EXTENSIONS && return false

    parts = splitpath(filepath)
    for part in parts
        part in SKIP_DIRS && return false
    end

    basename_path = basename(filepath)
    basename_path in PATH_EXCEPTIONS && return false

    for exc in PATH_EXCEPTIONS
        endswith(filepath, exc) && return false
    end

    return true
end

function fix_hardcoded_paths(repo_path::String; dry_run::Bool=false)::Int
    fixed_count = 0

    for (root, dirs, files) in walkdir(repo_path; onerror=(_)->nothing)
        filter!(d -> !(d in SKIP_DIRS), dirs)

        for file in files
            filepath = joinpath(root, file)
            relpath_str = relpath(filepath, repo_path)

            !should_fix_file(relpath_str) && continue

            content = try
                read(filepath, String)
            catch
                continue
            end

            # Skip binary files (check for null bytes in first 512 bytes)
            check_len = min(512, length(content))
            if check_len > 0 && any(c == '\0' for c in content[1:check_len])
                continue
            end

            new_content = content
            for (pattern, replacement) in PATH_PATTERNS
                new_content = replace(new_content, pattern => replacement)
            end

            if new_content != content
                fixed_count += 1
                if dry_run
                    println("  [DRY] Would fix paths in: $relpath_str")
                else
                    write(filepath, new_content)
                    println("  [FIX] Fixed paths in: $relpath_str")
                end
            end
        end
    end

    return fixed_count
end

# ── Template deployment ────────────────────────────────────────────

function deploy_templates(repo_path::String; dry_run::Bool=false)::Int
    deployed = 0
    repo_name = basename(repo_path)

    deployments = [
        ("QUICKSTART-USER.adoc.template", "QUICKSTART-USER.adoc"),
        ("QUICKSTART-DEV.adoc.template", "QUICKSTART-DEV.adoc"),
        ("QUICKSTART-MAINTAINER.adoc.template", "QUICKSTART-MAINTAINER.adoc"),
        ("setup.sh.template", "setup.sh"),
        ("MUST.contractile.template", joinpath(".machine_readable", "MUST.contractile")),
        ("TRUST.contractile.template", joinpath(".machine_readable", "TRUST.contractile")),
        ("INTENT.contractile.template", joinpath(".machine_readable", "INTENT.contractile")),
        ("ADJUST.contractile.template", joinpath(".machine_readable", "ADJUST.contractile")),
    ]

    for (template_name, target_rel) in deployments
        target_path = joinpath(repo_path, target_rel)

        isfile(target_path) && continue

        template_content = load_template(template_name)
        isempty(template_content) && continue

        filled = fill_template(template_content, repo_name)

        if dry_run
            println("  [DRY] Would deploy: $target_rel")
        else
            mkpath(dirname(target_path))
            write(target_path, filled)
            if endswith(target_rel, "setup.sh")
                chmod(target_path, 0o755)
            end
            println("  [ADD] Deployed: $target_rel")
        end
        deployed += 1
    end

    # Deploy LLM warmup scripts if missing
    for warmup in ["llm-warmup-user.md", "llm-warmup-dev.md"]
        target = joinpath(repo_path, warmup)
        if !isfile(target)
            audience = warmup == "llm-warmup-user.md" ? "User" : "Developer"
            content = """
            # LLM Warmup — $repo_name ($audience)

            ## What is $repo_name?
            See README.adoc for overview.

            ## Key Commands
            - `just setup` — set up development environment
            - `just build` — build the project
            - `just test` — run tests
            - `just doctor` — diagnose issues
            - `just heal` — attempt auto-repair

            ## Quick Context
            - License: PMPL-1.0-or-later
            - Part of hyperpolymath ecosystem
            - See EXPLAINME.adoc for architecture
            """
            if dry_run
                println("  [DRY] Would deploy: $warmup")
            else
                write(target, content)
                println("  [ADD] Deployed: $warmup")
            end
            deployed += 1
        end
    end

    # Add doctor/heal/tour recipes to Justfile if they exist but lack them
    justfile_path = joinpath(repo_path, "Justfile")
    if !isfile(justfile_path)
        justfile_path = joinpath(repo_path, "justfile")
    end

    if isfile(justfile_path)
        justfile_content = read(justfile_path, String)
        recipes_to_add = String[]

        # Build grep pattern from segments to avoid self-match
        grep_pattern = joinpath("/home", "hyper") * "\\\\|" * joinpath("/mnt", "eclipse")

        if !occursin("doctor", justfile_content)
            push!(recipes_to_add, "\n# Self-diagnostic — checks dependencies, permissions, paths\ndoctor:\n    @echo \"Running diagnostics for $(repo_name)...\"\n    @echo \"Checking required tools...\"\n    @command -v just >/dev/null 2>&1 && echo \"  [OK] just\" || echo \"  [FAIL] just not found\"\n    @command -v git >/dev/null 2>&1 && echo \"  [OK] git\" || echo \"  [FAIL] git not found\"\n    @echo \"Checking for hardcoded paths...\"\n    @grep -rn '$(grep_pattern)' --include='*.rs' --include='*.ex' --include='*.res' --include='*.gleam' --include='*.sh' . 2>/dev/null | head -5 || echo \"  [OK] No hardcoded paths\"\n    @echo \"Diagnostics complete.\"\n")
        end

        if !occursin("heal", justfile_content)
            push!(recipes_to_add, "\n# Auto-repair common issues\nheal:\n    @echo \"Attempting auto-repair for $(repo_name)...\"\n    @echo \"Fixing permissions...\"\n    @find . -name \"*.sh\" -exec chmod +x {} \\\\; 2>/dev/null || true\n    @echo \"Cleaning stale caches...\"\n    @rm -rf .cache/stale 2>/dev/null || true\n    @echo \"Repair complete.\"\n")
        end

        if !occursin("tour:", justfile_content)
            push!(recipes_to_add, "\n# Guided tour of key features\ntour:\n    @echo \"=== $(repo_name) Tour ===\"\n    @echo \"\"\n    @echo \"1. Project structure:\"\n    @ls -la\n    @echo \"\"\n    @echo \"2. Available commands: just --list\"\n    @echo \"3. Read README.adoc for full overview\"\n    @echo \"4. Read EXPLAINME.adoc for architecture decisions\"\n    @echo \"5. Run 'just doctor' to check your setup\"\n    @echo \"\"\n    @echo \"Tour complete! Try 'just --list' to see all available commands.\"\n")
        end

        if !occursin("help-me", justfile_content)
            push!(recipes_to_add, "\n# Open feedback channel with diagnostic context\nhelp-me:\n    @echo \"=== $(repo_name) Help ===\"\n    @echo \"Platform: \\$(uname -s) \\$(uname -m)\"\n    @echo \"Shell: \\$SHELL\"\n    @echo \"\"\n    @echo \"To report an issue:\"\n    @echo \"  https://github.com/hyperpolymath/$(repo_name)/issues/new\"\n    @echo \"\"\n    @echo \"Include the output of 'just doctor' in your report.\"\n")
        end

        if !isempty(recipes_to_add)
            if dry_run
                println("  [DRY] Would add $(length(recipes_to_add)) Justfile recipes")
            else
                open(justfile_path, "a") do f
                    for recipe in recipes_to_add
                        write(f, recipe)
                    end
                end
                println("  [ADD] Added $(length(recipes_to_add)) Justfile recipes")
            end
            deployed += length(recipes_to_add)
        end
    end

    # Ensure .machine_readable/ exists
    mr_dir = joinpath(repo_path, ".machine_readable")
    if !isdir(mr_dir) && !dry_run
        mkpath(mr_dir)
        println("  [ADD] Created .machine_readable/")
        deployed += 1
    end

    # Ensure guix.scm exists
    guix_path = joinpath(repo_path, "guix.scm")
    if !isfile(guix_path)
        guix_content = """; SPDX-License-Identifier: AGPL-3.0-or-later
;; guix.scm — GNU Guix package definition for $repo_name
;; Usage: guix shell -f guix.scm

(use-modules (guix packages)
             (guix build-system gnu)
             (guix licenses))

(package
  (name "$repo_name")
  (version "0.1.0")
  (source #f)
  (build-system gnu-build-system)
  (synopsis "$repo_name")
  (description "$repo_name — part of the hyperpolymath ecosystem.")
  (home-page "https://github.com/hyperpolymath/$repo_name")
  (license ((@@ (guix licenses) license) "PMPL-1.0-or-later"
             "https://github.com/hyperpolymath/palimpsest-license")))
"""
        if dry_run
            println("  [DRY] Would deploy: guix.scm")
        else
            write(guix_path, guix_content)
            println("  [ADD] Deployed: guix.scm")
        end
        deployed += 1
    end

    # Ensure flake.nix exists
    flake_path = joinpath(repo_path, "flake.nix")
    if !isfile(flake_path)
        flake_template = load_template("flake.nix.template")
        if !isempty(flake_template)
            filled = fill_template(flake_template, repo_name)
            if dry_run
                println("  [DRY] Would deploy: flake.nix")
            else
                write(flake_path, filled)
                println("  [ADD] Deployed: flake.nix")
            end
            deployed += 1
        end
    end

    return deployed
end

# ── Git operations ─────────────────────────────────────────────────

function git_commit_and_push(repo_path::String; push::Bool=false)
    repo_name = basename(repo_path)

    if !isdir(joinpath(repo_path, ".git"))
        return
    end

    cd(repo_path) do
        status = try
            read(`git status --porcelain`, String)
        catch
            return
        end
        if isempty(strip(status))
            return
        end

        ux_files = [
            "QUICKSTART-USER.adoc", "QUICKSTART-DEV.adoc", "QUICKSTART-MAINTAINER.adoc",
            "setup.sh", "guix.scm", "flake.nix",
            "llm-warmup-user.md", "llm-warmup-dev.md",
            ".machine_readable/MUST.contractile", ".machine_readable/TRUST.contractile",
            ".machine_readable/INTENT.contractile", ".machine_readable/ADJUST.contractile",
            "Justfile", "justfile",
        ]

        for f in ux_files
            if isfile(f) && occursin(f, status)
                try run(`git add $f`) catch; end
            end
        end

        for line in split(status, "\n")
            stripped = strip(line)
            if startswith(stripped, " M ") || startswith(stripped, "M ")
                file = strip(stripped[3:end])
                try run(`git add $file`) catch; end
            end
        end

        for line in split(status, "\n")
            stripped = strip(line)
            if startswith(stripped, "?? ")
                file = rstrip(strip(stripped[4:end]), '/')
                if any(endswith(file, ext) for ext in [".adoc", ".contractile", ".md", ".scm", ".nix", ".sh"])
                    try run(`git add $file`) catch; end
                end
            end
        end

        staged = try
            read(`git diff --cached --name-only`, String)
        catch
            return
        end
        if isempty(strip(staged))
            return
        end

        msg = "feat: deploy UX Manifesto infrastructure\n\nAdds quickstart guides, contractiles, doctor/heal/tour recipes,\nLLM warmup scripts, guix.scm, and fixes hardcoded paths.\n\nPart of the UX Manifesto \"Impossible to Criticise\" campaign.\n\nCo-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>"
        try
            run(`git commit -m $msg`)
            println("  [GIT] Committed UX infrastructure for $repo_name")
        catch e
            @warn "Commit failed for $repo_name: $e"
        end

        if push
            try
                run(`git push`)
                println("  [GIT] Pushed $repo_name")
            catch e
                @warn "Push failed for $repo_name: $e"
            end
        end
    end
end

# ── Main ───────────────────────────────────────────────────────────

function main()
    dry_run = "--dry-run" in ARGS
    fix_paths = "--fix-paths" in ARGS || "--all" in ARGS
    deploy = "--deploy-templates" in ARGS || "--all" in ARGS
    do_commit = "--commit" in ARGS
    do_push = "--push" in ARGS

    target_repo = nothing
    for (i, arg) in enumerate(ARGS)
        if arg == "--repo" && i < length(ARGS)
            target_repo = ARGS[i + 1]
        end
    end

    if !fix_paths && !deploy
        println("Usage: julia ux-batch-rollout.jl [--deploy-templates] [--fix-paths] [--all] [--dry-run] [--commit] [--push] [--repo <name>]")
        return
    end

    println("╔══════════════════════════════════════════════════════════════╗")
    println("║  UX Manifesto Batch Rollout                                 ║")
    println("║  $(dry_run ? "DRY RUN — no changes will be made" : "LIVE — changes will be written")                       ║")
    println("╚══════════════════════════════════════════════════════════════╝")
    println()

    repos = String[]
    for entry in readdir(REPOS_DIR)
        repo_path = joinpath(REPOS_DIR, entry)
        !isdir(repo_path) && continue
        !isdir(joinpath(repo_path, ".git")) && continue
        entry in ALREADY_TREATED && continue

        if target_repo !== nothing && entry != target_repo
            continue
        end

        push!(repos, entry)
    end

    sort!(repos)
    println("Found $(length(repos)) repos to process")
    println()

    total_deployed = 0
    total_fixed = 0
    repos_modified = 0

    for (i, repo_name) in enumerate(repos)
        repo_path = joinpath(REPOS_DIR, repo_name)
        println("[$i/$(length(repos))] $repo_name")

        deployed = 0
        fixed = 0

        if deploy
            deployed = deploy_templates(repo_path; dry_run)
        end

        if fix_paths
            fixed = fix_hardcoded_paths(repo_path; dry_run)
        end

        if deployed > 0 || fixed > 0
            repos_modified += 1
            total_deployed += deployed
            total_fixed += fixed

            if do_commit && !dry_run
                git_commit_and_push(repo_path; push=do_push)
            end
        end

        println()
    end

    println("═══════════════════════════════════════════════════")
    println("  Repos processed: $(length(repos))")
    println("  Repos modified:  $repos_modified")
    println("  Templates deployed: $total_deployed")
    println("  Paths fixed:     $total_fixed")
    println("═══════════════════════════════════════════════════")
end

main()
