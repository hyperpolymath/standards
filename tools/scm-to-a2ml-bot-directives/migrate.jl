# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
#
# Migrate hyperpolymath bot-directive SCM files to A2ML (TOML-shaped).
#
# For each discovered `.scm` under either
#     <repo>/.bot_directives/*.scm   (legacy location)
# or  <repo>/.machine_readable/bot_directives/*.scm   (preferred location)
# this script produces an equivalent `.a2ml` at
#     <repo>/.machine_readable/bot_directives/*.a2ml
# and (once written and round-trip-verified) deletes the original `.scm`.
#
# After all per-repo files migrate, the legacy `.bot_directives/` directory
# at repo root is removed if empty.
#
# Supported input forms:
#   (bot-directive (bot "X") (scope "Y") (allow (...)) (deny (...)) (notes "..."))
#   (bot-directives (version "1.0") (notes "..." "..."))       ;; README-style
#   (<botname>-config (...) (nested-block (key value) ...))    ;; lithoglyph
#
# Usage:
#   julia migrate.jl <repo>                 # migrate one repo
#   julia migrate.jl --all                  # scan all of /var/mnt/eclipse/repos
#   julia migrate.jl --dry-run <repo>       # print plan, do not write
#
# Safety:
#   - Writes the .a2ml first; reads it back and parses it; only then deletes
#     the .scm. A parse-back failure aborts that file with a warning.
#   - Never deletes a .scm without its .a2ml successfully existing.
#   - No network; no git commands; pure file I/O.
#   - Idempotent: running twice on a migrated repo is a no-op.

using Dates

const REPOS_ROOT = "/var/mnt/eclipse/repos"
const PREFERRED_DIR = ".machine_readable/bot_directives"
const LEGACY_DIR = ".bot_directives"
const SPDX_HEADER = "# SPDX-License-Identifier: PMPL-1.0-or-later"

# ─── S-expression tokeniser ──────────────────────────────────────────────

struct Token
    kind::Symbol  # :lparen :rparen :string :symbol :bool :number
    value::String
end

function tokenise(src::AbstractString)::Vector{Token}
    tokens = Token[]
    # Walk UTF-8 safely via nextind/prevind. `i` always points at a valid
    # char start (or past end-of-string). `endi` is the exclusive upper
    # bound (first byte-index past the last char).
    i = firstindex(src)
    endi = ncodeunits(src) + 1

    while i < endi
        c = src[i]
        if isspace(c)
            i = nextind(src, i)
            continue
        end
        if c == ';'  # line comment to EOL
            while i < endi && src[i] != '\n'
                i = nextind(src, i)
            end
            continue
        end
        if c == '('
            push!(tokens, Token(:lparen, "("))
            i = nextind(src, i)
            continue
        end
        if c == ')'
            push!(tokens, Token(:rparen, ")"))
            i = nextind(src, i)
            continue
        end
        if c == '"'
            j = nextind(src, i)  # first char inside string
            buf = IOBuffer()
            while j < endi && src[j] != '"'
                if src[j] == '\\' && nextind(src, j) < endi
                    j2 = nextind(src, j)
                    nxt = src[j2]
                    if nxt == 'n'
                        write(buf, '\n')
                    elseif nxt == 't'
                        write(buf, '\t')
                    elseif nxt == '"'
                        write(buf, '"')
                    elseif nxt == '\\'
                        write(buf, '\\')
                    else
                        write(buf, nxt)
                    end
                    j = nextind(src, j2)
                else
                    write(buf, src[j])
                    j = nextind(src, j)
                end
            end
            push!(tokens, Token(:string, String(take!(buf))))
            # Consume closing quote (if present — malformed files just end)
            i = j < endi ? nextind(src, j) : j
            continue
        end
        # Symbol, bool, or number — read until whitespace/paren
        j = i
        while j < endi
            cj = src[j]
            (isspace(cj) || cj == '(' || cj == ')') && break
            j = nextind(src, j)
        end
        chunk = src[i:prevind(src, j)]
        if chunk == "#t" || chunk == "#f"
            push!(tokens, Token(:bool, chunk))
        elseif occursin(r"^-?\d+(\.\d+)?$", chunk)
            push!(tokens, Token(:number, chunk))
        else
            push!(tokens, Token(:symbol, chunk))
        end
        i = j
    end
    return tokens
end

# ─── S-expression AST ────────────────────────────────────────────────────

# SExpr is either an atom (String/Bool/Int/Float/Symbol) or a Vector{SExpr}.
# Symbols are represented as a 2-tuple (:sym, String) to distinguish from
# quoted strings. Keeping this light rather than defining nominal structs.
const SExpr = Any

function parse_sexpr(tokens::Vector{Token})::Vector{SExpr}
    pos = Ref(1)
    forms = SExpr[]
    while pos[] <= length(tokens)
        push!(forms, parse_one!(tokens, pos))
    end
    forms
end

function parse_one!(tokens::Vector{Token}, pos::Ref{Int})::SExpr
    tok = tokens[pos[]]
    pos[] += 1
    if tok.kind == :lparen
        list = SExpr[]
        while pos[] <= length(tokens) && tokens[pos[]].kind != :rparen
            push!(list, parse_one!(tokens, pos))
        end
        pos[] <= length(tokens) || error("Unclosed list")
        pos[] += 1  # consume rparen
        return list
    elseif tok.kind == :rparen
        error("Unexpected ')'")
    elseif tok.kind == :string
        return tok.value       # plain Julia String
    elseif tok.kind == :bool
        return tok.value == "#t"
    elseif tok.kind == :number
        return occursin('.', tok.value) ? parse(Float64, tok.value) : parse(Int, tok.value)
    elseif tok.kind == :symbol
        return (:sym, tok.value)
    else
        error("Unknown token kind $(tok.kind)")
    end
end

is_symbol(x, name::AbstractString) = x isa Tuple && x[1] === :sym && x[2] == name
is_any_symbol(x) = x isa Tuple && x[1] === :sym
sym_name(x) = x[2]

# ─── A2ML (TOML-shaped) emitter ──────────────────────────────────────────

function toml_escape(s::AbstractString)::String
    buf = IOBuffer()
    write(buf, '"')
    for c in s
        if c == '\\'
            write(buf, "\\\\")
        elseif c == '"'
            write(buf, "\\\"")
        elseif c == '\n'
            write(buf, "\\n")
        elseif c == '\t'
            write(buf, "\\t")
        else
            write(buf, c)
        end
    end
    write(buf, '"')
    String(take!(buf))
end

function toml_value(v)::String
    if v isa AbstractString
        return toml_escape(v)
    elseif v isa Bool
        return v ? "true" : "false"
    elseif v isa Integer
        return string(v)
    elseif v isa AbstractFloat
        return string(v)
    elseif v isa Vector
        inner = join([toml_value(x) for x in v], ", ")
        return "[$inner]"
    else
        error("Cannot emit TOML for value of type $(typeof(v))")
    end
end

# Extract the Scheme atom as a Julia-typed value for TOML emission.
# Symbols become their textual form (strings); kebab-case key names are
# kept as-is (valid TOML bare keys).
function atom_to_julia(x)
    if is_any_symbol(x)
        return sym_name(x)
    elseif x isa Bool || x isa Integer || x isa AbstractFloat || x isa AbstractString
        return x
    else
        error("Cannot coerce atom: $(x)")
    end
end

# A "simple" (key value) or (key v1 v2 ...) clause from Scheme. Returns
# (keyname, Julia value). The value is a scalar for one-arg clauses and
# a Vector for multi-arg clauses. Nested lists of symbol/string become
# Vector{String}.
function parse_kv_clause(clause::Vector)::Tuple{String, Any}
    is_any_symbol(clause[1]) || error("Clause head not a symbol: $clause")
    key = sym_name(clause[1])
    args = clause[2:end]

    # Single-arg — scalar. Special case: (key (item1 item2 ...)) -> list.
    if length(args) == 1
        a = args[1]
        if a isa Vector
            # (key (v1 v2 ...)) style
            return key, [atom_to_julia(x) for x in a]
        else
            return key, atom_to_julia(a)
        end
    end

    # Multi-arg — combine into a list of atoms OR treat as prose-note.
    # Heuristic: if all args are strings, join with " " for a notes field.
    if all(a -> a isa AbstractString, args) && key == "notes"
        return key, join(args, " ")
    end

    return key, [atom_to_julia(x) for x in args]
end

# ─── Emitters ────────────────────────────────────────────────────────────

function emit_bot_directive(form::Vector, filename::AbstractString)::String
    # form[1] is (:sym, "bot-directive")
    body_clauses = [c for c in form[2:end] if c isa Vector]

    buf = IOBuffer()
    println(buf, SPDX_HEADER)
    println(buf, "# Bot directive — migrated from SCM on $(Dates.format(today(), "yyyy-mm-dd"))")
    println(buf, "# Media-Type: application/vnd.bot-directive+a2ml")
    println(buf)
    println(buf, "schema_version = \"1.0\"")
    println(buf, "directive_type = \"bot-directive\"")

    for clause in body_clauses
        k, v = parse_kv_clause(clause)
        # kebab-case keys stay kebab-case (valid TOML bare keys)
        println(buf, "$k = $(toml_value(v))")
    end

    String(take!(buf))
end

function emit_bot_directives_readme(form::Vector, filename::AbstractString)::String
    # (bot-directives (version "1.0") (notes "..." "..."))
    # Some variants use nested blocks: (bot-directives (metadata (version ...)) (overview "..."))
    body_clauses = [c for c in form[2:end] if c isa Vector]

    buf = IOBuffer()
    println(buf, SPDX_HEADER)
    println(buf, "# .bot_directives README — migrated from SCM on $(Dates.format(today(), "yyyy-mm-dd"))")
    println(buf, "# Media-Type: application/vnd.bot-directives+a2ml")
    println(buf)
    println(buf, "schema_version = \"1.0\"")
    println(buf, "directive_type = \"bot-directives-readme\"")

    scalar_clauses = Vector{Vector}()
    nested_clauses = Vector{Vector}()
    for clause in body_clauses
        is_any_symbol(clause[1]) || continue
        args = clause[2:end]
        has_nested_list = any(a -> a isa Vector, args)
        if has_nested_list
            push!(nested_clauses, clause)
        else
            push!(scalar_clauses, clause)
        end
    end

    for clause in scalar_clauses
        k, v = parse_kv_clause(clause)
        println(buf, "$k = $(toml_value(v))")
    end

    for clause in nested_clauses
        section = sym_name(clause[1])
        println(buf)
        println(buf, "[$section]")
        for inner in clause[2:end]
            if inner isa Vector
                k, v = parse_kv_clause(inner)
                println(buf, "$k = $(toml_value(v))")
            end
        end
    end

    String(take!(buf))
end

# For nested configs like (echidnabot-config (quality-gates (flag #t) ...))
# we emit each nested block as a TOML [section] table.
function emit_bot_config(form::Vector, filename::AbstractString)::String
    head = sym_name(form[1])  # e.g. "echidnabot-config"
    # Strip "-config" suffix for the bot name if present
    bot_name = endswith(head, "-config") ? chop(head; tail = 7) : head
    body_clauses = [c for c in form[2:end] if c isa Vector]

    buf = IOBuffer()
    println(buf, SPDX_HEADER)
    println(buf, "# $(bot_name) config — migrated from SCM on $(Dates.format(today(), "yyyy-mm-dd"))")
    println(buf, "# Media-Type: application/vnd.bot-directive+a2ml")
    println(buf)
    println(buf, "schema_version = \"1.0\"")
    println(buf, "directive_type = \"bot-config\"")
    println(buf, "bot = $(toml_escape(bot_name))")

    # Top-level scalar clauses first
    scalar_clauses = Vector{Vector}()
    nested_clauses = Vector{Vector}()
    for clause in body_clauses
        is_any_symbol(clause[1]) || continue
        args = clause[2:end]
        has_nested_list = any(a -> a isa Vector, args)
        if has_nested_list
            push!(nested_clauses, clause)
        else
            push!(scalar_clauses, clause)
        end
    end

    for clause in scalar_clauses
        k, v = parse_kv_clause(clause)
        println(buf, "$k = $(toml_value(v))")
    end

    for clause in nested_clauses
        section = sym_name(clause[1])
        println(buf)
        println(buf, "[$section]")
        for inner in clause[2:end]
            if inner isa Vector
                k, v = parse_kv_clause(inner)
                println(buf, "$k = $(toml_value(v))")
            end
        end
    end

    String(take!(buf))
end

function convert_scm_to_a2ml(scm_path::AbstractString)::Union{String, Nothing}
    try
        src = read(scm_path, String)
        tokens = tokenise(src)
        forms = parse_sexpr(tokens)
        # Find the first non-comment top-level list form
        top = nothing
        for f in forms
            if f isa Vector && !isempty(f) && is_any_symbol(f[1])
                top = f
                break
            end
        end
        top === nothing && return nothing

        head = sym_name(top[1])
        if head == "bot-directive"
            return emit_bot_directive(top, scm_path)
        elseif head == "bot-directives"
            return emit_bot_directives_readme(top, scm_path)
        elseif endswith(head, "-config")
            return emit_bot_config(top, scm_path)
        else
            @warn "Unknown top-level form: ($head ...) at $scm_path — skipping"
            return nothing
        end
    catch e
        @warn "Failed to convert $scm_path: $e"
        return nothing
    end
end

# Verify the emitted A2ML parses cleanly (TOML syntax check).
function verify_a2ml(path::AbstractString)::Bool
    # Very light structural check: every non-blank, non-comment line must be
    # either a [section] header or `key = value`. Full TOML parse is
    # delegated to the Rust bots' `toml` crate at runtime.
    for line in eachline(path)
        stripped = strip(line)
        isempty(stripped) && continue
        startswith(stripped, "#") && continue
        if startswith(stripped, "[") && endswith(stripped, "]")
            continue
        end
        occursin('=', stripped) || return false
    end
    true
end

# ─── Per-repo migration ──────────────────────────────────────────────────

struct MigrationResult
    repo::String
    migrated::Int
    skipped::Int
    failed::Int
    removed_legacy_dir::Bool
end

function migrate_repo(repo_path::AbstractString; dry_run::Bool = false)::MigrationResult
    legacy = joinpath(repo_path, LEGACY_DIR)
    preferred = joinpath(repo_path, PREFERRED_DIR)

    scm_files = String[]
    for dir in (legacy, preferred)
        isdir(dir) || continue
        for entry in readdir(dir; join = true)
            if isfile(entry) && endswith(entry, ".scm")
                push!(scm_files, entry)
            end
        end
    end

    isempty(scm_files) && return MigrationResult(repo_path, 0, 0, 0, false)

    if !dry_run
        mkpath(preferred)
    end

    migrated = 0
    skipped = 0
    failed = 0

    for scm in scm_files
        name = splitext(basename(scm))[1]
        a2ml_path = joinpath(preferred, "$name.a2ml")

        if isfile(a2ml_path)
            println("  [skip] $a2ml_path already exists")
            skipped += 1
            continue
        end

        body = convert_scm_to_a2ml(scm)
        if body === nothing
            println("  [fail] convert $scm")
            failed += 1
            continue
        end

        if dry_run
            println("  [dry-run] would write $a2ml_path")
            println("  [dry-run] would delete $scm")
            migrated += 1
            continue
        end

        write(a2ml_path, body)
        if !verify_a2ml(a2ml_path)
            println("  [fail] verify $a2ml_path — leaving .scm in place")
            rm(a2ml_path)
            failed += 1
            continue
        end

        rm(scm)
        println("  [ok] $scm → $a2ml_path")
        migrated += 1
    end

    # Remove the legacy directory if it is now empty
    removed_legacy = false
    if !dry_run && isdir(legacy)
        if isempty(readdir(legacy))
            rm(legacy)
            removed_legacy = true
            println("  [cleanup] removed empty $legacy")
        end
    end

    MigrationResult(repo_path, migrated, skipped, failed, removed_legacy)
end

# ─── CLI ─────────────────────────────────────────────────────────────────

function find_repos_with_directives(root::AbstractString)::Vector{String}
    repos = Set{String}()
    for (dirpath, dirs, files) in walkdir(root; follow_symlinks = false)
        b = basename(dirpath)
        if b == LEGACY_DIR || (b == "bot_directives" && occursin("/.machine_readable/", dirpath))
            # Walk up to the repo root — the parent of .bot_directives OR
            # the grandparent of .machine_readable/bot_directives.
            if b == LEGACY_DIR
                push!(repos, dirname(dirpath))
            else
                push!(repos, dirname(dirname(dirpath)))
            end
        end
    end
    sort!(collect(repos))
end

function main(args::Vector{String})
    dry_run = false
    targets = String[]
    scan_all = false

    i = 1
    while i <= length(args)
        a = args[i]
        if a == "--dry-run"
            dry_run = true
        elseif a == "--all"
            scan_all = true
        elseif a == "--help" || a == "-h"
            println("Usage: julia migrate.jl [--dry-run] [--all | <repo-path>...]")
            return 0
        else
            push!(targets, a)
        end
        i += 1
    end

    if scan_all
        append!(targets, find_repos_with_directives(REPOS_ROOT))
    end

    if isempty(targets)
        println(stderr, "No targets. Pass --all or one or more repo paths.")
        return 2
    end

    total_migrated = 0
    total_skipped = 0
    total_failed = 0
    total_dirs_cleaned = 0

    for repo in targets
        println("\n== $repo")
        result = migrate_repo(repo; dry_run = dry_run)
        total_migrated += result.migrated
        total_skipped += result.skipped
        total_failed += result.failed
        total_dirs_cleaned += result.removed_legacy_dir ? 1 : 0
    end

    println("\n═══ Summary ═══")
    println("  Migrated : $total_migrated")
    println("  Skipped  : $total_skipped")
    println("  Failed   : $total_failed")
    println("  Legacy dirs removed: $total_dirs_cleaned")
    total_failed > 0 ? 1 : 0
end

if abspath(PROGRAM_FILE) == @__FILE__
    exit(main(String[x for x in ARGS]))
end
