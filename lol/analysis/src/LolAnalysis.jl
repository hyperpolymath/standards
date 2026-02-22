# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

"""
    LolAnalysis

Julia analysis layer for 1000Langs corpus data.
Provides statistical analysis, language distance computation,
phylogenetic clustering, and quality assessment.

Integrates with Elixir orchestrator via JSON-over-stdio bridge.
"""
module LolAnalysis

include("corpus_stats.jl")
include("language_distance.jl")
include("frequency_analysis.jl")
include("phylogenetic.jl")
include("quality_assessment.jl")
include("zero_frequency.jl")

using .CorpusStats
using .LanguageDistance
using .FrequencyAnalysis
using .Phylogenetic
using .QualityAssessment
using .ZeroFrequency

export analyze_corpus, compute_distances, cluster_languages,
       assess_quality, frequency_spectra

"""
    analyze_corpus(texts::Dict{String,String}) -> Dict

Run full statistical analysis on a corpus of language texts.
Returns per-language metrics including entropy, TTR, and quality scores.
"""
function analyze_corpus(texts::Dict{String,String})
    results = Dict{String,Any}()

    for (lang, text) in texts
        stats = CorpusStats.compute_all(text)
        quality = QualityAssessment.quality_score(text)

        results[lang] = Dict(
            "character_entropy" => stats.character_entropy,
            "word_entropy" => stats.word_entropy,
            "type_token_ratio" => stats.type_token_ratio,
            "hapax_ratio" => stats.hapax_ratio,
            "mean_word_length" => stats.mean_word_length,
            "quality_score" => quality
        )
    end

    return results
end

"""
    compute_distances(texts::Dict{String,String}; metric=:jensen_shannon) -> Matrix

Compute pairwise distance matrix across all languages in the corpus.
"""
function compute_distances(texts::Dict{String,String}; metric::Symbol=:jensen_shannon)
    langs = collect(keys(texts))
    n = length(langs)

    # Build character n-gram profiles
    profiles = Dict{String,Dict{String,Float64}}()
    for (lang, text) in texts
        profiles[lang] = FrequencyAnalysis.ngram_profile(text, 3)
    end

    # Compute distance matrix
    return LanguageDistance.pairwise_distances(profiles, langs, metric)
end

"""
    cluster_languages(texts::Dict{String,String}; method=:upgma) -> String

Cluster languages and return a Newick-format tree.
"""
function cluster_languages(texts::Dict{String,String}; method::Symbol=:upgma)
    distances = compute_distances(texts)
    langs = collect(keys(texts))
    return Phylogenetic.build_tree(distances, langs, method)
end

"""
    assess_quality(texts::Dict{String,String}) -> Vector{Dict}

Assess corpus quality and return VeriSimDB-compatible weak points.
"""
function assess_quality(texts::Dict{String,String})
    weak_points = Dict{String,Any}[]

    for (lang, text) in texts
        lang_wps = QualityAssessment.detect_issues(text, lang)
        append!(weak_points, lang_wps)
    end

    return weak_points
end

"""
    frequency_spectra(texts::Dict{String,String}) -> Dict

Compute frequency spectra for each language in the corpus.
"""
function frequency_spectra(texts::Dict{String,String})
    results = Dict{String,Any}()

    for (lang, text) in texts
        spectrum = FrequencyAnalysis.frequency_spectrum(text)
        zipf = FrequencyAnalysis.zipf_coefficient(text)

        results[lang] = Dict(
            "spectrum" => spectrum,
            "zipf_coefficient" => zipf
        )
    end

    return results
end

"""
    test()

Quick self-test to verify module loads correctly.
"""
function test()
    println("LolAnalysis v0.1.0 loaded successfully")

    # Basic smoke test
    sample = Dict("eng" => "In the beginning God created the heavens and the earth",
                   "fra" => "Au commencement Dieu crea les cieux et la terre")

    results = analyze_corpus(sample)
    println("Analyzed $(length(results)) languages")

    for (lang, stats) in results
        println("  $lang: entropy=$(round(stats["character_entropy"], digits=3)), " *
                "quality=$(round(stats["quality_score"], digits=3))")
    end

    println("All tests passed")
end

end # module
