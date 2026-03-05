#!/usr/bin/env julia
#=
Repo Reconciliation Tool
Compares GitLab and GitHub repositories to identify sync gaps.
Fully ported to Julia.
=#

using HTTP
using JSON
using Dates

# Configuration
const GITLAB_USER = "hyperpolymath"
const GITLAB_GROUPS = ["maa-framework"]
const GITHUB_USER = "hyperpolymath"

struct Repo
    name::String
    platform::String
    url::String
    last_activity::String
    description::String
end

function get_gitlab_repos(token::String)
    repos = Repo[]
    headers = ["PRIVATE-TOKEN" => token]
    
    # User repos
    page = 1
    while true
        url = "https://gitlab.com/api/v4/users/$GITLAB_USER/projects?per_page=100&page=$page"
        resp = HTTP.get(url, headers)
        if resp.status != 200
            println("GitLab API error: $(resp.status)")
            break
        end
        data = JSON.parse(String(resp.body))
        if isempty(data)
            break
        end
        for r in data
            push!(repos, Repo(
                r["path"],
                "gitlab",
                r["web_url"],
                get(r, "last_activity_at", ""),
                get(r, "description", "") === nothing ? "" : r["description"]
            ))
        end
        page += 1
    end

    # Group repos
    for group in GITLAB_GROUPS
        page = 1
        while true
            url = "https://gitlab.com/api/v4/groups/$group/projects?per_page=100&page=$page&include_subgroups=true"
            try
                resp = HTTP.get(url, headers)
                if resp.status != 200; break; end
                data = JSON.parse(String(resp.body))
                if isempty(data); break; end
                for r in data
                    push!(repos, Repo(
                        r["path"],
                        "gitlab",
                        r["web_url"],
                        get(r, "last_activity_at", ""),
                        get(r, "description", "") === nothing ? "" : r["description"]
                    ))
                end
                page += 1
            catch
                break
            end
        end
    end
    return repos
end

function get_github_repos(token::String)
    repos = Repo[]
    headers = ["Authorization" => "token $token"]
    
    page = 1
    while true
        url = "https://api.github.com/users/$GITHUB_USER/repos?per_page=100&page=$page"
        resp = HTTP.get(url, headers)
        if resp.status != 200
            println("GitHub API error: $(resp.status)")
            break
        end
        data = JSON.parse(String(resp.body))
        if isempty(data)
            break
        end
        for r in data
            push!(repos, Repo(
                r["name"],
                "github",
                r["html_url"],
                get(r, "pushed_at", ""),
                get(r, "description", "") === nothing ? "" : r["description"]
            ))
        end
        page += 1
    end
    return repos
end

function reconcile(gitlab_repos, github_repos)
    gl_map = Dict(r.name => r for r in gitlab_repos)
    gh_map = Dict(r.name => r for r in github_repos)
    
    all_names = sort(collect(union(keys(gl_map), keys(gh_map))))
    
    result = Dict(
        "gitlab_only" => [],
        "github_only" => [],
        "both_synced" => [],
        "both_diverged" => [],
        "unknown" => []
    )

    for name in all_names
        gl = get(gl_map, name, nothing)
        gh = get(gh_map, name, nothing)

        if gl !== nothing && gh === nothing
            push!(result["gitlab_only"], Dict("name" => name, "url" => gl.url, "action" => "Mirror to GitHub"))
        elseif gh !== nothing && gl === nothing
            push!(result["github_only"], Dict("name" => name, "url" => gh.url, "action" => "New on GitHub"))
        else
            # Both exist - check dates
            try
                gl_dt = DateTime(replace(gl.last_activity, "Z" => ""), dateformat"yyyy-mm-ddTHH:MM:SS")
                gh_dt = DateTime(replace(gh.last_activity, "Z" => ""), dateformat"yyyy-mm-ddTHH:MM:SS")
                
                diff_days = abs(Dates.value(Dates.Day(gl_dt - gh_dt)))
                
                if diff_days > 1
                    push!(result["both_diverged"], Dict("name" => name, "diff" => diff_days, "ahead" => gl_dt > gh_dt ? "GitLab" : "GitHub"))
                else
                    push!(result["both_synced"], Dict("name" => name))
                end
            catch
                push!(result["unknown"], Dict("name" => name))
            end
        end
    end
    return result
end

function main()
    gl_token = get(ENV, "GITLAB_TOKEN", "")
    gh_token = get(ENV, "GITHUB_TOKEN", "")

    if isempty(gl_token) || isempty(gh_token)
        println("Please set GITLAB_TOKEN and GITHUB_TOKEN environment variables")
        return
    end

    println("Fetching repos...")
    gl_repos = get_gitlab_repos(gl_token)
    gh_repos = get_github_repos(gh_token)

    println("Reconciling...")
    res = reconcile(gl_repos, gh_repos)

    println("\n" * "="^40)
    println("SUMMARY")
    println("-"^40)
    println("GitLab only: $(length(res["gitlab_only"]))")
    println("GitHub only: $(length(res["github_only"]))")
    println("Diverged:    $(length(res["both_diverged"]))")
    println("Synced:      $(length(res["both_synced"]))")
    println("="^40)
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
