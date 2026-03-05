# SPDX-License-Identifier: MIT OR GPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

#=
Example Julia server demonstrating AIBDP + HTTP 430 middleware
=#

using HTTP
using JSON
include("aibdp_middleware.jl")

const PORT = 5000
const MANIFEST_PATH = joinpath(@__DIR__, "example-aibdp.json")
const loader = AIBDPManifest(MANIFEST_PATH)

function index_handler(req::HTTP.Request)
    return HTTP.Response(200, ["Content-Type" => "text/html"], body="""
    <html>
      <head><title>Consent-Aware HTTP Example (Julia/HTTP.jl)</title></head>
      <body>
        <h1>Welcome to Consent-Aware HTTP (Julia)</h1>
        <p>This Julia server implements HTTP 430 + AIBDP.</p>
        <ul>
          <li><a href="/article">Article</a> - Protected content</li>
          <li><a href="/public">Public content</a> - Allowed for all</li>
          <li><a href="/.well-known/aibdp.json">AIBDP Manifest</a></li>
        </ul>
      </body>
    </html>
    """)
end

function article_handler(req::HTTP.Request)
    return HTTP.Response(200, ["Content-Type" => "text/html"], body="""
    <html>
      <head><title>Protected Article (Julia)</title></head>
      <body>
        <h1>Protected Article</h1>
        <p>This content has AI usage boundaries declared via AIBDP.</p>
      </body>
    </html>
    """)
end

function public_handler(req::HTTP.Request)
    return HTTP.Response(200, ["Content-Type" => "text/html"], body="""
    <html>
      <head><title>Public Content (Julia)</title></head>
      <body>
        <h1>Public Content</h1>
        <p>This content is available for all purposes.</p>
      </body>
    </html>
    """)
end

function manifest_handler(req::HTTP.Request)
    manifest = load_manifest!(loader)
    if manifest === nothing
        return HTTP.Response(404, body="Manifest not found")
    end
    return HTTP.Response(200, ["Content-Type" => "application/aibdp+json"], body=JSON.json(manifest))
end

const ROUTER = HTTP.Router()
HTTP.register!(ROUTER, "GET", "/", index_handler)
HTTP.register!(ROUTER, "GET", "/article", article_handler)
HTTP.register!(ROUTER, "GET", "/public", public_handler)
HTTP.register!(ROUTER, "GET", "/.well-known/aibdp.json", manifest_handler)

function middleware_handler(req::HTTP.Request)
    # AIBDP Enforcement
    resp = aibdp_enforce(req, loader)
    if resp !== nothing
        println("[AIBDP] Blocked request: $(req.target)")
        return resp
    end
    
    return ROUTER(req)
end

function main()
    println("🚀 Consent-Aware HTTP server (Julia) starting on http://localhost:$PORT")
    HTTP.serve(middleware_handler, "0.0.0.0", PORT)
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
