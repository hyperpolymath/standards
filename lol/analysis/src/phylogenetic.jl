# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell and Contributors

"""
    Phylogenetic

Language clustering and phylogenetic tree construction from
statistical distances. Inspired by Cladistics.jl patterns.
Methods: UPGMA, neighbor-joining, Ward's.
"""
module Phylogenetic

using Clustering

export build_tree, upgma, neighbor_joining, to_newick

"""
    upgma(D::Matrix{Float64}, labels::Vector{String}) -> String

UPGMA (Unweighted Pair Group Method with Arithmetic Mean) clustering.
Returns Newick-format tree string.
"""
function upgma(D::Matrix{Float64}, labels::Vector{String})
    n = length(labels)
    n == 0 && return ";"
    n == 1 && return "$(labels[1]);"

    # Use hierarchical clustering from Clustering.jl
    # Convert distance matrix to condensed form
    dists = Float64[]
    for i in 1:n
        for j in (i+1):n
            push!(dists, D[i, j])
        end
    end

    # Build tree using average linkage (UPGMA)
    result = hclust(D, linkage=:average)

    # Convert to Newick format
    return hclust_to_newick(result, labels)
end

"""
    neighbor_joining(D::Matrix{Float64}, labels::Vector{String}) -> String

Neighbor-joining algorithm for phylogenetic tree construction.
Returns Newick-format tree string.
"""
function neighbor_joining(D::Matrix{Float64}, labels::Vector{String})
    n = length(labels)
    n == 0 && return ";"
    n == 1 && return "$(labels[1]);"
    n == 2 && return "($(labels[1]):$(D[1,2]/2),$(labels[2]):$(D[1,2]/2));"

    # Working copies
    dist = copy(D)
    active = collect(1:n)
    node_labels = copy(labels)
    next_id = n + 1

    while length(active) > 2
        m = length(active)

        # Compute Q matrix
        r = zeros(m)
        for i in 1:m
            for j in 1:m
                if i != j
                    r[i] += dist[active[i], active[j]]
                end
            end
        end

        # Find minimum Q entry
        min_q = Inf
        min_i, min_j = 1, 2
        for i in 1:m
            for j in (i+1):m
                q = (m - 2) * dist[active[i], active[j]] - r[i] - r[j]
                if q < min_q
                    min_q = q
                    min_i, min_j = i, j
                end
            end
        end

        # Compute branch lengths
        ai, aj = active[min_i], active[min_j]
        d_ij = dist[ai, aj]
        li = d_ij / 2 + (r[min_i] - r[min_j]) / (2 * (m - 2))
        lj = d_ij - li

        li = max(li, 0.0)
        lj = max(lj, 0.0)

        # Create new node label
        new_label = "($(node_labels[ai]):$(round(li, digits=6)),$(node_labels[aj]):$(round(lj, digits=6)))"

        # Update distance matrix for new node
        new_dists = zeros(size(dist, 1) + 1, size(dist, 2) + 1)
        new_dists[1:size(dist,1), 1:size(dist,2)] = dist

        for k in active
            if k != ai && k != aj
                d_new = (dist[ai, k] + dist[aj, k] - d_ij) / 2
                d_new = max(d_new, 0.0)
                new_dists[next_id, k] = d_new
                new_dists[k, next_id] = d_new
            end
        end

        dist = new_dists
        push!(node_labels, new_label)
        filter!(x -> x != ai && x != aj, active)
        push!(active, next_id)
        next_id += 1
    end

    # Final join
    if length(active) == 2
        a, b = active
        d = dist[a, b] / 2
        return "($(node_labels[a]):$(round(d, digits=6)),$(node_labels[b]):$(round(d, digits=6)));"
    end

    return "$(node_labels[active[1]]);"
end

"""
    build_tree(D::Matrix{Float64}, labels::Vector{String}, method::Symbol) -> String

Build phylogenetic tree using the specified method.
Returns Newick-format tree string.
"""
function build_tree(D::Matrix{Float64}, labels::Vector{String}, method::Symbol=:upgma)
    if method == :upgma
        return upgma(D, labels)
    elseif method == :nj || method == :neighbor_joining
        return neighbor_joining(D, labels)
    elseif method == :ward
        result = hclust(D, linkage=:ward)
        return hclust_to_newick(result, labels)
    else
        return upgma(D, labels)
    end
end

"""
Convert Clustering.jl hclust result to Newick format string.
"""
function hclust_to_newick(result, labels::Vector{String})
    n = length(labels)
    node_labels = Dict{Int,String}()

    for i in 1:n
        node_labels[i] = labels[i]
    end

    for (k, (i, j)) in enumerate(zip(result.merge[:, 1], result.merge[:, 2]))
        # In hclust, negative indices are leaves, positive are internal nodes
        left = i < 0 ? node_labels[-i] : node_labels[n + i]
        right = j < 0 ? node_labels[-j] : node_labels[n + j]
        height = result.heights[k]

        node_labels[n + k] = "($(left):$(round(height/2, digits=6)),$(right):$(round(height/2, digits=6)))"
    end

    last_key = n + size(result.merge, 1)
    return "$(node_labels[last_key]);"
end

end # module
