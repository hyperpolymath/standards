# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

"""
    ZeroFrequency

Sparse language handling using zero-probability estimation.
Inspired by ZeroProb.jl concepts for density ratio estimation.

Provides smoothing methods for handling unseen n-grams in
low-resource languages where corpus data is limited.
"""
module ZeroFrequency

using StatsBase

export good_turing_smooth, add_k_smooth, estimate_unseen,
       kneser_ney_smooth

"""
    good_turing_smooth(counts::Dict{String,Int}) -> Dict{String,Float64}

Good-Turing frequency estimation. Re-estimates probabilities
by using frequency-of-frequency counts to smooth the distribution.
"""
function good_turing_smooth(counts::Dict{String,Int})
    isempty(counts) && return Dict{String,Float64}()

    total = sum(values(counts))

    # Compute frequency of frequencies (N_r)
    freq_of_freq = countmap(values(counts))

    smoothed = Dict{String,Float64}()

    for (ngram, count) in counts
        nr = get(freq_of_freq, count, 0)
        nr1 = get(freq_of_freq, count + 1, 0)

        if nr1 > 0 && nr > 0
            # Good-Turing estimate: c* = (c+1) * N_{c+1} / N_c
            adjusted = (count + 1) * nr1 / nr
            smoothed[ngram] = adjusted / total
        else
            smoothed[ngram] = count / total
        end
    end

    # Estimate probability mass for unseen events
    n1 = get(freq_of_freq, 1, 0)
    p_unseen = n1 / total

    # Normalize
    total_p = sum(values(smoothed))
    scale = (1.0 - p_unseen) / total_p

    for (k, v) in smoothed
        smoothed[k] = v * scale
    end

    return smoothed
end

"""
    add_k_smooth(counts::Dict{String,Int}, vocab_size::Int; k=1.0) -> Dict{String,Float64}

Add-k (Laplace) smoothing. Adds k to every count including unseen items.
Simple but effective for small vocabularies.
"""
function add_k_smooth(counts::Dict{String,Int}, vocab_size::Int; k::Float64=1.0)
    total = sum(values(counts)) + k * vocab_size

    smoothed = Dict{String,Float64}()
    for (ngram, count) in counts
        smoothed[ngram] = (count + k) / total
    end

    return smoothed
end

"""
    kneser_ney_smooth(counts::Dict{String,Int}; d=0.75) -> Dict{String,Float64}

Modified Kneser-Ney smoothing. Uses absolute discounting with
a continuation probability for unseen n-grams.
"""
function kneser_ney_smooth(counts::Dict{String,Int}; d::Float64=0.75)
    isempty(counts) && return Dict{String,Float64}()

    total = sum(values(counts))
    n_unique = length(counts)  # Number of unique n-grams seen

    smoothed = Dict{String,Float64}()

    for (ngram, count) in counts
        # Discounted probability
        p_disc = max(count - d, 0.0) / total

        # Interpolation weight
        lambda = d * n_unique / total

        # Continuation probability (uniform for simplicity)
        p_cont = 1.0 / max(n_unique, 1)

        smoothed[ngram] = p_disc + lambda * p_cont
    end

    return smoothed
end

"""
    estimate_unseen(counts::Dict{String,Int}) -> NamedTuple

Estimate vocabulary coverage metrics for a language corpus.
Returns estimates of unseen vocabulary size and coverage.
"""
function estimate_unseen(counts::Dict{String,Int})
    isempty(counts) && return (
        observed_types=0,
        estimated_total=0,
        coverage=0.0,
        unseen_estimate=0,
        hapax_count=0,
        singletons_ratio=0.0
    )

    observed = length(counts)
    total_tokens = sum(values(counts))

    # Frequency of frequencies
    freq_of_freq = countmap(values(counts))
    hapax = get(freq_of_freq, 1, 0)  # Words seen once
    dis = get(freq_of_freq, 2, 0)    # Words seen twice

    # Good-Turing estimate of unseen types
    # E[unseen] ≈ N₁² / (2 * N₂) when N₂ > 0
    unseen_estimate = if dis > 0
        round(Int, hapax^2 / (2 * dis))
    else
        hapax  # Conservative estimate
    end

    estimated_total = observed + unseen_estimate
    coverage = observed / max(estimated_total, 1)

    return (
        observed_types=observed,
        estimated_total=estimated_total,
        coverage=round(coverage, digits=4),
        unseen_estimate=unseen_estimate,
        hapax_count=hapax,
        singletons_ratio=round(hapax / max(observed, 1), digits=4)
    )
end

end # module
