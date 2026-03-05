# SPDX-License-Identifier: MIT OR GPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

#=
AIBDP + HTTP 430 Middleware for Julia
Fully ported from Python.
=#

using HTTP
using JSON
using Dates

# AI User-Agent patterns for detection
const AI_USER_AGENTS = [
    r"GPTBot"i,
    r"ChatGPT-User"i,
    r"Claude-Web"i,
    r"anthropic-ai"i,
    r"Google-Extended"i,
    r"CCBot"i,
    r"Googlebot"i,
    r"Bingbot"i,
    r"Slurp"i,
    r"DuckDuckBot"i,
    r"Baiduspider"i,
    r"YandexBot"i,
    r"PerplexityBot"i,
    r"Diffbot"i,
]

mutable struct AIBDPManifest
    path::String
    cache_duration::Millisecond
    manifest::Union{Dict, Nothing}
    load_time::Union{DateTime, Nothing}
end

AIBDPManifest(path::String; cache_duration=Hour(1)) = AIBDPManifest(path, Millisecond(cache_duration), nothing, nothing)

function load_manifest!(m::AIBDPManifest)
    now_time = now()
    
    if m.manifest !== nothing && m.load_time !== nothing
        if (now_time - m.load_time) < m.cache_duration
            return m.manifest
        end
    end

    if !isfile(m.path)
        return nothing
    end

    try
        m.manifest = JSON.parsefile(m.path)
        m.load_time = now_time
        return m.manifest
    catch e
        @warn "Failed to load AIBDP manifest: $e"
        return nothing
    end
end

function is_ai_user_agent(user_agent::String)
    return any(p -> occursin(p, user_agent), AI_USER_AGENTS)
end

function extract_ai_purpose(headers::Dict)
    # Check custom AI-Purpose header
    if haskey(headers, "AI-Purpose")
        return lowercase(headers["AI-Purpose"])
    end

    ua = get(headers, "User-Agent", "")
    
    if occursin(r"GPTBot"i, ua); return "training"; end
    if occursin(r"Claude-Web"i, ua); return "indexing"; end
    if occursin(r"Google-Extended"i, ua); return "training"; end
    if occursin(r"Googlebot"i, ua); return "indexing"; end

    return "unknown"
end

function path_matches(request_path::String, pattern::String)
    if pattern == "all"; return true; end
    
    regex_pattern = pattern |>
        p -> replace(p, "." => "\\.") |>
        p -> replace(p, "**" => ".*") |>
        p -> replace(p, "*" => "[^/]*") |>
        p -> replace(p, "?" => ".")
        
    return occursin(Regex("^$regex_pattern\$"), request_path)
end

function get_applicable_policy(manifest::Dict, purpose::String, request_path::String)
    policies = get(manifest, "policies", nothing)
    if policies === nothing; return nothing; end
    
    policy = get(policies, purpose, nothing)
    if policy === nothing; return nothing; end
    
    # Check scope
    scope = get(policy, "scope", nothing)
    if scope isa Vector
        if !any(p -> path_matches(request_path, p), scope)
            return nothing
        end
    end
    
    # Check exceptions
    exceptions = get(policy, "exceptions", [])
    for ex in exceptions
        if path_matches(request_path, ex["path"])
            return ex
        end
    end
    
    return policy
end

function create_430_response(manifest::Dict, policy::Dict, purpose::String)
    manifest_uri = get(manifest, "canonical_uri", "/.well-known/aibdp.json")
    
    data = Dict(
        "error" => "AI usage boundaries declared in AIBDP manifest not satisfied",
        "manifest" => manifest_uri,
        "violated_policy" => purpose,
        "policy_status" => policy["status"],
        "required_conditions" => get(policy, "conditions", []),
        "rationale" => get(policy, "rationale", "No info"),
        "contact" => get(manifest, "contact", "")
    )
    
    headers = [
        "Content-Type" => "application/json",
        "Link" => "<$manifest_uri>; rel=\"blocked-by-consent\"",
        "Retry-After" => "86400"
    ]
    
    return HTTP.Response(430, headers, body=JSON.json(data))
end

# Example usage function for an HTTP server
function aibdp_enforce(req::HTTP.Request, manifest_loader::AIBDPManifest)
    manifest = load_manifest!(manifest_loader)
    if manifest === nothing; return nothing; end
    
    ua = HTTP.header(req, "User-Agent", "")
    if !is_ai_user_agent(ua); return nothing; end
    
    headers_dict = Dict(k => v for (k, v) in req.headers)
    purpose = extract_ai_purpose(headers_dict)
    policy = get_applicable_policy(manifest, purpose, req.target)
    
    if policy === nothing; return nothing; end
    
    if policy["status"] == "refused"
        return create_430_response(manifest, policy, purpose)
    end
    
    return nothing
end
