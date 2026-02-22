# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

"""
    LanguageDistance

Pairwise distance computation across 1500+ languages.
Uses character n-gram profiles with multiple distance metrics.
"""
module LanguageDistance

using Distances: cosine_dist, euclidean

export pairwise_distances, nearest_neighbors, jensen_shannon,
       jaccard_ngram, cosine_ngram

"""
    jensen_shannon(p::Dict, q::Dict) -> Float64

Jensen-Shannon divergence between two n-gram distributions.
Symmetric and bounded in [0, 1] when using log2.
"""
function jensen_shannon(p::Dict{String,Float64}, q::Dict{String,Float64})
    all_keys = union(keys(p), keys(q))

    kl_pm = 0.0
    kl_qm = 0.0

    for key in all_keys
        pi = get(p, key, 0.0)
        qi = get(q, key, 0.0)
        mi = (pi + qi) / 2.0

        if pi > 0 && mi > 0
            kl_pm += pi * log2(pi / mi)
        end
        if qi > 0 && mi > 0
            kl_qm += qi * log2(qi / mi)
        end
    end

    return (kl_pm + kl_qm) / 2.0
end

"""
    jaccard_ngram(p::Dict, q::Dict) -> Float64

Jaccard distance between two n-gram sets.
"""
function jaccard_ngram(p::Dict{String,Float64}, q::Dict{String,Float64})
    p_keys = Set(keys(p))
    q_keys = Set(keys(q))

    intersection_size = length(intersect(p_keys, q_keys))
    union_size = length(union(p_keys, q_keys))

    union_size == 0 && return 0.0
    return 1.0 - intersection_size / union_size
end

"""
    cosine_ngram(p::Dict, q::Dict) -> Float64

Cosine distance between two n-gram frequency vectors.
"""
function cosine_ngram(p::Dict{String,Float64}, q::Dict{String,Float64})
    all_keys = union(keys(p), keys(q))

    dot_product = 0.0
    norm_p = 0.0
    norm_q = 0.0

    for key in all_keys
        pi = get(p, key, 0.0)
        qi = get(q, key, 0.0)
        dot_product += pi * qi
        norm_p += pi^2
        norm_q += qi^2
    end

    denom = sqrt(norm_p) * sqrt(norm_q)
    denom == 0 && return 0.0

    return 1.0 - dot_product / denom
end

"""
    pairwise_distances(profiles, langs, metric) -> Matrix{Float64}

Compute N x N pairwise distance matrix for language profiles.
"""
function pairwise_distances(
    profiles::Dict{String,Dict{String,Float64}},
    langs::Vector{String},
    metric::Symbol=:jensen_shannon
)
    n = length(langs)
    D = zeros(Float64, n, n)

    dist_fn = if metric == :jensen_shannon
        jensen_shannon
    elseif metric == :cosine
        cosine_ngram
    elseif metric == :jaccard
        jaccard_ngram
    else
        jensen_shannon
    end

    for i in 1:n
        for j in (i+1):n
            d = dist_fn(profiles[langs[i]], profiles[langs[j]])
            D[i, j] = d
            D[j, i] = d  # Symmetric
        end
    end

    return D
end

"""
    nearest_neighbors(lang, profiles, langs, k; metric) -> Vector{Tuple{String,Float64}}

Find k most similar languages to the given language.
"""
function nearest_neighbors(
    lang::String,
    profiles::Dict{String,Dict{String,Float64}},
    langs::Vector{String},
    k::Int;
    metric::Symbol=:jensen_shannon
)
    !(lang in langs) && return Tuple{String,Float64}[]

    dist_fn = metric == :jensen_shannon ? jensen_shannon :
              metric == :cosine ? cosine_ngram : jaccard_ngram

    distances = Tuple{String,Float64}[]
    for other in langs
        other == lang && continue
        d = dist_fn(profiles[lang], profiles[other])
        push!(distances, (other, d))
    end

    sort!(distances, by=x -> x[2])
    return distances[1:min(k, length(distances))]
end

end # module
