// SPDX-License-Identifier: MIT OR GPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

// AIBDP + HTTP 430 Middleware for Express
// Implements AI Boundary Declaration Protocol enforcement

@val @scope("console") external warn: string => unit = "warn"
@val @scope("console") external error: string => unit = "error"
@val @scope("JSON") external parse: string => 'a = "parse"

// AI User-Agent patterns
let aiUserAgentPatterns = [
  %re("/GPTBot/i"),
  %re("/ChatGPT-User/i"),
  %re("/Claude-Web/i"),
  %re("/anthropic-ai/i"),
  %re("/Google-Extended/i"),
  %re("/CCBot/i"),
  %re("/Googlebot/i"),
  %re("/Bingbot/i"),
  %re("/Slurp/i"),
  %re("/DuckDuckBot/i"),
  %re("/Baiduspider/i"),
  %re("/YandexBot/i"),
  %re("/Sogou/i"),
  %re("/Exabot/i"),
  %re("/facebookexternalhit/i"),
  %re("/ia_archiver/i"),
  %re("/PerplexityBot/i"),
  %re("/Omgilibot/i"),
  %re("/Diffbot/i"),
]

type policyStatus = Allowed | Refused | Conditional

type policy = {
  status: policyStatus,
  scope: option<array<string>>,
  conditions: option<array<string>>,
  rationale: option<string>,
  exceptions: option<array<{path: string, status: policyStatus}>>,
}

type manifest = {
  canonical_uri: option<string>,
  policies: Js.Dict.t<policy>,
  contact: option<string>,
}

type response430 = {
  statusCode: int,
  headers: Js.Dict.t<string>,
  body: {..},
}

// Load and parse AIBDP manifest
let loadManifest = async (manifestPath) => {
  try {
    let content = await Node.Fs.readFile(manifestPath, "utf-8")
    Some(parse(content))
  } catch {
  | Exn.Error(e) => {
      warn(`Failed to load AIBDP manifest: ${Exn.message(e)->Option.getOr("unknown error")}`)
      None
    }
  }
}

// Check if User-Agent matches known AI systems
let isAIUserAgent = (userAgent) => {
  switch userAgent {
  | None => false
  | Some(ua) => aiUserAgentPatterns->Array.some(pattern => Js.Re.test_(pattern, ua))
  }
}

// Extract AI purpose from request headers
let extractAIPurpose = (headers: Js.Dict.t<string>) => {
  switch headers->Js.Dict.get("ai-purpose") {
  | Some(purpose) => Js.String.toLowerCase(purpose)
  | None => {
      let ua = headers->Js.Dict.get("user-agent")->Option.getOr("")
      if Js.Re.test_(%re("/GPTBot/i"), ua) {
        "training"
      } else if Js.Re.test_(%re("/Claude-Web/i"), ua) {
        "indexing"
      } else if Js.Re.test_(%re("/Google-Extended/i"), ua) {
        "training"
      } else if Js.Re.test_(%re("/Googlebot/i"), ua) {
        "indexing"
      } else {
        "unknown"
      }
    }
  }
}

// Check if path matches pattern (glob-style)
let pathMatches = (requestPath, pattern) => {
  if pattern == "all" {
    true
  } else {
    let regexPattern = pattern
      ->Js.String.replaceByRe(%re("/\./g"), "\\.")
      ->Js.String.replaceByRe(%re("/\*\*/g"), ".*")
      ->Js.String.replaceByRe(%re("/\*/g"), "[^/]*")
      ->Js.String.replaceByRe(%re("/\?/g"), ".")
    
    let regex = Js.Re.fromString("^" ++ regexPattern ++ "$")
    Js.Re.test_(regex, requestPath)
  }
}

// Create HTTP 430 response
let create430Response = (manifest: manifest, policy: policy, purpose: string) => {
  let manifestUri = manifest.canonical_uri->Option.getOr("/.well-known/aibdp.json")
  
  let headers = Js.Dict.empty()
  headers->Js.Dict.set("Content-Type", "application/json")
  headers->Js.Dict.set("Link", `<${manifestUri}>; rel="blocked-by-consent"`)
  headers->Js.Dict.set("Retry-After", "86400")
  
  {
    statusCode: 430,
    headers,
    body: {
      "error": "AI usage boundaries declared in AIBDP manifest not satisfied",
      "manifest": manifestUri,
      "violated_policy": purpose,
      "policy_status": switch policy.status {
        | Allowed => "allowed"
        | Refused => "refused"
        | Conditional => "conditional"
      },
      "required_conditions": policy.conditions->Option.getOr([]),
      "rationale": policy.rationale->Option.getOr("No additional information provided"),
      "contact": manifest.contact,
    },
  }
}

type middlewareOptions = {
  manifestPath: option<string>,
  enforceForAll: option<bool>,
  onViolation: option<(Express.req, policy, string) => unit>,
}

// Express middleware factory
let aibdpMiddleware = (options: middlewareOptions) => {
  let manifestPath = options.manifestPath->Option.getOr(".well-known/aibdp.json")
  let enforceForAll = options.enforceForAll->Option.getOr(false)
  
  let manifestRef: ref<option<manifest>> = ref(None)
  let manifestLoadTimeRef = ref(0.0)
  let cacheDuration = 3600000.0 // 1 hour
  
  async (req: Express.req, res: Express.res, next: Express.next) => {
    try {
      let now = Node.dateNow()
      if manifestRef.contents->Option.isNone || now -. manifestLoadTimeRef.contents > cacheDuration {
        manifestRef := await loadManifest(manifestPath)
        manifestLoadTimeRef := now
      }
      
      switch manifestRef.contents {
      | None => next()
      | Some(manifest) => {
          let userAgent = req.headers->Js.Dict.get("user-agent")
          let isAI = enforceForAll || isAIUserAgent(userAgent)
          
          if !isAI {
            next()
          } else {
            let purpose = extractAIPurpose(req.headers)
            
            switch manifest.policies->Js.Dict.get(purpose) {
            | None => next()
            | Some(policy) => {
                switch policy.status {
                | Refused => {
                    let response = create430Response(manifest, policy, purpose)
                    options.onViolation->Option.forEach(fn => fn(req, policy, purpose))
                    
                    let r = res->Express.status(response.statusCode)
                    response.headers->Js.Dict.entries->Array.forEach(((k, v)) => {
                      r->Express.header(k, v)->ignore
                    })
                    r->Express.json(response.body)
                  }
                | Allowed => next()
                | Conditional => next() // Simplified - full implementation would check conditions
                }
              }
            }
          }
        }
      }
    } catch {
    | Exn.Error(e) => {
        error(`AIBDP middleware error: ${Exn.message(e)->Option.getOr("unknown")}`)
        next()
      }
    }
  }
}

// Serve manifest endpoint
let serveManifest = (manifestPath) => {
  let path = manifestPath->Option.getOr(".well-known/aibdp.json")
  
  async (req: Express.req, res: Express.res, next: Express.next) => {
    if req.path != "/.well-known/aibdp.json" {
      next()
    } else {
      switch await loadManifest(path) {
      | None => {
          res->Express.status(404)->Express.json({"error": "Manifest not found"})
        }
      | Some(manifest) => {
          res
            ->Express.header("Content-Type", "application/aibdp+json")
            ->Express.header("Cache-Control", "public, max-age=3600")
            ->Express.header("Access-Control-Allow-Origin", "*")
            ->Express.json(manifest)
        }
      }
    }
  }
}
