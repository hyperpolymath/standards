# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

"""
    CorpusStats

Enhanced statistical analysis beyond the ReScript Statistics module.
Provides character-level and word-level metrics for corpus quality.
"""
module CorpusStats

using StatsBase

export compute_all, character_entropy, word_entropy,
       type_token_ratio, hapax_legomena_ratio

struct CorpusStatistics
    character_entropy::Float64
    word_entropy::Float64
    type_token_ratio::Float64
    hapax_ratio::Float64
    mean_word_length::Float64
    total_chars::Int
    total_words::Int
    unique_words::Int
end

"""
    character_entropy(text::String) -> Float64

Compute Shannon entropy of character distribution.
Higher entropy suggests more complex orthography.
"""
function character_entropy(text::String)
    isempty(text) && return 0.0

    chars = collect(text)
    counts = countmap(chars)
    total = length(chars)

    entropy = 0.0
    for (_, count) in counts
        p = count / total
        if p > 0
            entropy -= p * log2(p)
        end
    end

    return entropy
end

"""
    word_entropy(text::String) -> Float64

Compute Shannon entropy at the word level.
"""
function word_entropy(text::String)
    words = split(lowercase(text))
    isempty(words) && return 0.0

    counts = countmap(words)
    total = length(words)

    entropy = 0.0
    for (_, count) in counts
        p = count / total
        if p > 0
            entropy -= p * log2(p)
        end
    end

    return entropy
end

"""
    type_token_ratio(text::String) -> Float64

Vocabulary richness: unique words / total words.
Higher values indicate more diverse vocabulary.
"""
function type_token_ratio(text::String)
    words = split(lowercase(text))
    isempty(words) && return 0.0

    unique = length(Set(words))
    return unique / length(words)
end

"""
    hapax_legomena_ratio(text::String) -> Float64

Proportion of words occurring exactly once.
Indicator of vocabulary breadth in the sample.
"""
function hapax_legomena_ratio(text::String)
    words = split(lowercase(text))
    isempty(words) && return 0.0

    counts = countmap(words)
    hapax = count(v -> v == 1, values(counts))

    return hapax / length(words)
end

"""
    compute_all(text::String) -> CorpusStatistics

Compute all statistics for a text.
"""
function compute_all(text::String)
    words = split(lowercase(text))
    unique_words = length(Set(words))
    total_words = length(words)

    word_lengths = [length(w) for w in words]
    mean_wl = isempty(word_lengths) ? 0.0 : mean(word_lengths)

    return CorpusStatistics(
        character_entropy(text),
        word_entropy(text),
        total_words > 0 ? unique_words / total_words : 0.0,
        hapax_legomena_ratio(text),
        mean_wl,
        length(text),
        total_words,
        unique_words
    )
end

end # module
