# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

"""
    FrequencyAnalysis

N-gram and word frequency analysis for corpus texts.
Includes Zipf's law coefficient estimation and vocabulary growth curves.
"""
module FrequencyAnalysis

using StatsBase

export ngram_profile, frequency_spectrum, zipf_coefficient,
       vocabulary_growth, ngram_frequencies

"""
    ngram_profile(text::String, n::Int) -> Dict{String,Float64}

Build a normalized character n-gram frequency profile.
Returns probabilities that sum to 1.0.
"""
function ngram_profile(text::String, n::Int)
    isempty(text) && return Dict{String,Float64}()

    chars = collect(lowercase(text))
    total_ngrams = length(chars) - n + 1
    total_ngrams <= 0 && return Dict{String,Float64}()

    counts = Dict{String,Int}()
    for i in 1:total_ngrams
        ngram = String(chars[i:i+n-1])
        counts[ngram] = get(counts, ngram, 0) + 1
    end

    # Normalize to probabilities
    profile = Dict{String,Float64}()
    for (ngram, count) in counts
        profile[ngram] = count / total_ngrams
    end

    return profile
end

"""
    ngram_frequencies(text::String, n::Int) -> Dict{String,Int}

Build a raw character n-gram frequency count.
"""
function ngram_frequencies(text::String, n::Int)
    isempty(text) && return Dict{String,Int}()

    chars = collect(lowercase(text))
    total_ngrams = length(chars) - n + 1
    total_ngrams <= 0 && return Dict{String,Int}()

    counts = Dict{String,Int}()
    for i in 1:total_ngrams
        ngram = String(chars[i:i+n-1])
        counts[ngram] = get(counts, ngram, 0) + 1
    end

    return counts
end

"""
    frequency_spectrum(text::String) -> Vector{Tuple{Int,Int}}

Compute Baayen-style frequency spectrum: (frequency, count_of_words_with_that_frequency).
V(1) = hapax legomena, V(2) = dis legomena, etc.
"""
function frequency_spectrum(text::String)
    words = split(lowercase(text))
    isempty(words) && return Tuple{Int,Int}[]

    word_counts = countmap(words)
    freq_counts = countmap(values(word_counts))

    spectrum = [(freq, count) for (freq, count) in freq_counts]
    sort!(spectrum, by=x -> x[1])

    return spectrum
end

"""
    zipf_coefficient(text::String) -> Float64

Estimate Zipf's law exponent from word frequency distribution.
Natural language typically has coefficient near 1.0.
Deviations may indicate data quality issues.
"""
function zipf_coefficient(text::String)
    words = split(lowercase(text))
    isempty(words) && return 0.0

    word_counts = countmap(words)
    frequencies = sort(collect(values(word_counts)), rev=true)
    n = length(frequencies)
    n < 2 && return 0.0

    # Linear regression on log-log scale: log(freq) ~ -alpha * log(rank)
    log_ranks = [log(i) for i in 1:n]
    log_freqs = [log(f) for f in frequencies]

    # Least squares: alpha = -cov(log_rank, log_freq) / var(log_rank)
    mean_lr = mean(log_ranks)
    mean_lf = mean(log_freqs)

    cov_sum = 0.0
    var_sum = 0.0
    for i in 1:n
        cov_sum += (log_ranks[i] - mean_lr) * (log_freqs[i] - mean_lf)
        var_sum += (log_ranks[i] - mean_lr)^2
    end

    var_sum == 0.0 && return 0.0
    alpha = -cov_sum / var_sum

    return alpha
end

"""
    vocabulary_growth(text::String; step=100) -> Vector{Tuple{Int,Int}}

Compute vocabulary growth curve (Heaps' law).
Returns (tokens_seen, unique_types) at each step.
"""
function vocabulary_growth(text::String; step::Int=100)
    words = split(lowercase(text))
    isempty(words) && return Tuple{Int,Int}[]

    seen = Set{String}()
    curve = Tuple{Int,Int}[]

    for (i, word) in enumerate(words)
        push!(seen, word)
        if i % step == 0 || i == length(words)
            push!(curve, (i, length(seen)))
        end
    end

    return curve
end

end # module
