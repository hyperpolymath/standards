# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

"""
Elixir bridge server: JSON-over-stdio protocol.

Reads JSON commands from stdin, dispatches to LolAnalysis functions,
returns JSON results on stdout.

Commands:
  {"cmd": "analyze", "corpus": {"eng": "text...", "fra": "text..."}}
  {"cmd": "distance_matrix", "corpus": {...}, "extra": "jensen_shannon"}
  {"cmd": "cluster", "corpus": {...}, "extra": "upgma"}
  {"cmd": "frequency_spectra", "corpus": {...}}
  {"cmd": "quality", "corpus": {...}}
"""

using JSON3

# Include the main module (use @__DIR__ for reliable path resolution)
include(joinpath(@__DIR__, "LolAnalysis.jl"))
using .LolAnalysis

function handle_command(request::Dict{String,Any})
    cmd = get(request, "cmd", "")
    corpus_raw = get(request, "corpus", Dict())
    extra = get(request, "extra", nothing)

    # Parse corpus: either a JSON string or already a dict
    corpus = if corpus_raw isa String
        try
            JSON3.read(corpus_raw, Dict{String,String})
        catch
            Dict{String,String}()
        end
    elseif corpus_raw isa Dict
        Dict{String,String}(string(k) => string(v) for (k, v) in corpus_raw)
    else
        Dict{String,String}()
    end

    try
        if cmd == "analyze"
            results = LolAnalysis.analyze_corpus(corpus)
            return Dict("status" => "ok", "results" => results)

        elseif cmd == "distance_matrix"
            metric = extra !== nothing ? Symbol(extra) : :jensen_shannon
            distances = LolAnalysis.compute_distances(corpus; metric=metric)
            langs = collect(keys(corpus))
            return Dict(
                "status" => "ok",
                "languages" => langs,
                "matrix" => [distances[i, :] for i in 1:size(distances, 1)]
            )

        elseif cmd == "cluster"
            method = extra !== nothing ? Symbol(extra) : :upgma
            tree = LolAnalysis.cluster_languages(corpus; method=method)
            return Dict("status" => "ok", "newick" => tree)

        elseif cmd == "frequency_spectra"
            spectra = LolAnalysis.frequency_spectra(corpus)
            return Dict("status" => "ok", "results" => spectra)

        elseif cmd == "quality"
            weak_points = LolAnalysis.assess_quality(corpus)
            return Dict("status" => "ok", "weak_points" => weak_points)

        else
            return Dict("status" => "error", "message" => "Unknown command: $cmd")
        end
    catch e
        return Dict("status" => "error", "message" => string(e))
    end
end

function main()
    # Signal readiness
    println(stderr, "LolAnalysis server ready")

    while !eof(stdin)
        line = readline(stdin)
        isempty(line) && continue

        try
            request = JSON3.read(line, Dict{String,Any})
            response = handle_command(request)
            println(stdout, JSON3.write(response))
            flush(stdout)
        catch e
            error_response = Dict("status" => "error", "message" => string(e))
            println(stdout, JSON3.write(error_response))
            flush(stdout)
        end
    end
end

# Run server if executed directly, or handle CLI args
if abspath(PROGRAM_FILE) == @__FILE__
    if length(ARGS) >= 2 && ARGS[1] == "analyze"
        # CLI mode: analyze a corpus file
        corpus_path = ARGS[2]
        corpus_json = read(corpus_path, String)
        corpus = JSON3.read(corpus_json, Dict{String,String})
        results = LolAnalysis.analyze_corpus(corpus)
        println(JSON3.write(results))
    else
        # Server mode
        main()
    end
end
