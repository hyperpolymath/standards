#!/usr/bin/env julia
# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# state-scm-to-v2.jl — convert a v1 STATE.scm (s-expr machine state) to the
# canonical STATE.a2ml v2 "thin session journal" directive format.
#
# Spec: standards/a2ml-templates/STATE.a2ml.v2.spec.adoc
# v2 deliberately discards everything derivable; keeps only:
#   phase, next_action, last_action, updated, and @blockers entries.
#
# Usage:
#   state-scm-to-v2.jl <STATE.scm>            # print v2 to stdout (dry-run)
#   state-scm-to-v2.jl <STATE.scm> --write    # write .machine_readable/STATE.a2ml
#
# Exit codes: 0 ok, 2 parse/usage error, 3 not a (state ...) form.
#
# Ported from the original Python implementation (Python is banned estate-wide);
# behaviour and output are identical. A node is either a Vector (a list) or a
# leaf tuple (:str, value) / (:atom, value).

using Dates

# --- s-expression reader ----------------------------------------------------

function tokenize(s::AbstractString)
    # Strip ;; line comments, then tokenize parens and "strings" and atoms.
    s = replace(s, r";;[^\n]*" => "")
    toks = Any[]
    cs = collect(s)
    i, n = 1, length(cs)
    while i <= n
        c = cs[i]
        if c == '(' || c == ')'
            push!(toks, c == '(' ? :lparen : :rparen); i += 1
        elseif c == '"'
            j = i + 1; buf = IOBuffer()
            while j <= n && cs[j] != '"'
                if cs[j] == '\\' && j + 1 <= n
                    print(buf, cs[j+1]); j += 2
                else
                    print(buf, cs[j]); j += 1
                end
            end
            push!(toks, (:str, String(take!(buf)))); i = j + 1
        elseif isspace(c)
            i += 1
        else
            j = i
            while j <= n && !isspace(cs[j]) && cs[j] != '(' && cs[j] != ')' && cs[j] != '"'
                j += 1
            end
            push!(toks, (:atom, String(cs[i:j-1]))); i = j
        end
    end
    return toks
end

function parse_forms(toks)
    # Returns nested Vectors; strings -> (:str, v), atoms -> (:atom, v).
    pos = Ref(1)
    function rd()
        t = toks[pos[]]; pos[] += 1
        if t === :lparen
            lst = Any[]
            while toks[pos[]] !== :rparen
                push!(lst, rd())
            end
            pos[] += 1
            return lst
        elseif t === :rparen
            error("unexpected )")
        end
        return t  # (:str, ..) or (:atom, ..)
    end
    forms = Any[]
    while pos[] <= length(toks)
        push!(forms, rd())
    end
    return forms
end

# --- query helpers (mirror the Python truthiness: "" counts as absent) ------

isblank(x) = x === nothing || x == ""

head(node) =
    (node isa Vector && !isempty(node) && node[1] isa Tuple) ? node[1][2] : nothing

function findform(node, name)
    # First child list whose head atom == name.
    node isa Vector || return nothing
    for ch in node
        if ch isa Vector && !isempty(ch) && ch[1] isa Tuple && ch[1][2] == name
            return ch
        end
    end
    return nothing
end

first_string(::Nothing) = nothing
function first_string(node)
    # First (non-empty) string literal anywhere under node, depth-first.
    if node isa Tuple
        return node[1] === :str ? node[2] : nothing
    elseif node isa Vector
        for ch in node
            r = first_string(ch)
            if !isblank(r)
                return r
            end
        end
    end
    return nothing
end

function all_strings(node)
    out = String[]
    if node isa Tuple
        node[1] === :str && push!(out, node[2])
    elseif node isa Vector
        for ch in node
            append!(out, all_strings(ch))
        end
    end
    return out
end

# Escape backslash then double-quote, in that order (mirrors the original).
q(s) = "\"" * replace(replace(string(s), "\\" => "\\\\"), "\"" => "\\\"") * "\""

function slug(s)
    t = replace(lowercase(string(s)), r"[^a-z0-9]+" => "-")
    t = strip(t, '-')
    t = first(t, 40)
    return isempty(t) ? "blocker" : String(t)
end

# pick the first truthy (non-blank) candidate, falling back to `dflt`
firsttruthy(dflt, cands...) = (for c in cands; isblank(c) || return c; end; dflt)

# --- conversion -------------------------------------------------------------

function convert(path)
    src = read(path, String)
    forms = parse_forms(tokenize(src))
    state = nothing
    for f in forms
        if head(f) == "state"
            state = f; break
        end
    end
    if state === nothing
        print(stderr, "ERR: no (state ...) form in $path\n")
        exit(3)
    end

    # phase  <- (current-position (phase "..")) | (current-phase "..")
    phase = nothing
    cp = findform(state, "current-position")
    if cp !== nothing
        ph = findform(cp, "phase")
        ph !== nothing && (phase = first_string(ph))
    end
    if isblank(phase)
        cph = findform(state, "current-phase")
        cph !== nothing && (phase = first_string(cph))
    end
    isblank(phase) && (phase = "unknown")

    # next_action <- critical-next-actions, first string (immediate first)
    next_action = "TODO — review and set"
    cna = findform(state, "critical-next-actions")
    if cna !== nothing
        imm = findform(cna, "immediate")
        next_action = firsttruthy(next_action, first_string(imm), first_string(cna))
    end

    # last_action <- last session-history entry's first accomplishment string
    last_action = "migrated from v1 STATE.scm"
    sh = findform(state, "session-history")
    if sh !== nothing
        sessions = [c for c in sh if c isa Vector && head(c) == "session"]
        if !isempty(sessions)
            acc = findform(sessions[end], "accomplishments")
            last_action = firsttruthy(last_action, first_string(acc), first_string(sessions[end]))
        end
    end

    # updated <- (metadata (updated|last-updated "..")) else today
    updated = nothing
    md = findform(state, "metadata")
    if md !== nothing
        for key in ("updated", "last-updated")
            u = findform(md, key)
            if u !== nothing
                updated = first_string(u); break
            end
        end
    end
    isblank(updated) && (updated = string(Dates.today()))

    # blockers <- (blockers-and-issues (critical ..)(high ..)(medium ..)(low ..))
    blockers = Tuple{String,String}[]
    bi = findform(state, "blockers-and-issues")
    if bi !== nothing
        for sev in ("critical", "high", "medium", "low")
            grp = findform(bi, sev)
            if grp !== nothing
                for desc in all_strings(grp)
                    push!(blockers, (sev, desc))
                end
            end
        end
    end

    lines = String[]
    push!(lines, "# SPDX-License-Identifier: AGPL-3.0-or-later")
    push!(lines, "# Migrated from $(basename(path)) by state-scm-to-v2.jl on " *
                 "$(Dates.today())")
    push!(lines, "")
    push!(lines, "@state(version=\"2.0\"):")
    push!(lines, "phase: $(q(phase))")
    push!(lines, "next_action: $(q(next_action))")
    push!(lines, "last_action: $(q(last_action))")
    push!(lines, "updated: $(updated)")
    if !isempty(blockers)
        push!(lines, "")
        push!(lines, "@blockers:")
        for (sev, desc) in blockers
            push!(lines, "- id: $(slug(desc))")
            push!(lines, "  description: $(q(desc))")
            push!(lines, "  waiting_on: $(q(sev * "-priority — internal"))")
            push!(lines, "  since: $(updated)")
        end
        push!(lines, "@end")
    end
    push!(lines, "")
    push!(lines, "@end")
    return join(lines, "\n") * "\n"
end

function main()
    write_mode = "--write" in ARGS
    args = [a for a in ARGS if a != "--write"]
    if length(args) != 1
        print(stderr, "usage: state-scm-to-v2.jl <STATE.scm> [--write]\n")
        exit(2)
    end
    scm = args[1]
    out = convert(scm)
    if write_mode
        dest = joinpath(dirname(scm), "STATE.a2ml")
        open(io -> Base.write(io, out), dest, "w")
        print(stderr, "wrote $dest\n")
    else
        Base.write(stdout, out)
    end
end

main()
