// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// Service detection — auto-detect project type, name, version, routes, and
// capabilities from repository contents. Used by `groove init`.

use anyhow::Result;
use regex::Regex;
use std::fs;
use std::path::Path;

/// Detected project type.
#[derive(Debug, Clone, PartialEq)]
pub enum ProjectType {
    Rust,
    Elixir,
    Deno,
    Zig,
    Cli,     // No HTTP server detected — passive mode
    Unknown,
}

/// Information detected from the repository.
#[derive(Debug, Clone)]
pub struct DetectedInfo {
    pub project_type: ProjectType,
    pub service_id: String,
    pub version: String,
    pub has_http_server: bool,
    pub detected_routes: Vec<DetectedRoute>,
    pub detected_deps: Vec<String>,
    pub suggested_capabilities: Vec<SuggestedCap>,
    pub suggested_consumes: Vec<String>,
}

/// A detected HTTP route.
#[derive(Debug, Clone)]
pub struct DetectedRoute {
    pub method: String,
    pub path: String,
    pub file: String,
    pub line: u32,
}

/// A suggested capability based on route/dependency analysis.
#[derive(Debug, Clone)]
pub struct SuggestedCap {
    pub cap_type: String,
    pub endpoint: String,
    pub protocol: String,
    pub reason: String,
}

/// Detect everything about a repo from its contents.
pub fn detect(path: &Path) -> Result<DetectedInfo> {
    let project_type = detect_project_type(path);
    let (service_id, version) = detect_name_version(path, &project_type);
    let has_http_server = detect_http_server(path, &project_type);
    let detected_routes = detect_routes(path, &project_type);
    let detected_deps = detect_dependencies(path, &project_type);
    let suggested_capabilities = suggest_capabilities(&detected_routes, &detected_deps);
    let suggested_consumes = suggest_consumes(&detected_deps);

    Ok(DetectedInfo {
        project_type: if has_http_server {
            project_type
        } else {
            ProjectType::Cli
        },
        service_id,
        version,
        has_http_server,
        detected_routes,
        detected_deps,
        suggested_capabilities,
        suggested_consumes,
    })
}

/// Detect the primary project type from files present.
fn detect_project_type(path: &Path) -> ProjectType {
    if path.join("Cargo.toml").exists() {
        ProjectType::Rust
    } else if path.join("mix.exs").exists() {
        ProjectType::Elixir
    } else if path.join("deno.json").exists() || path.join("deno.jsonc").exists() {
        ProjectType::Deno
    } else if path.join("build.zig").exists() {
        ProjectType::Zig
    } else {
        ProjectType::Unknown
    }
}

/// Extract project name and version from manifest files.
fn detect_name_version(path: &Path, project_type: &ProjectType) -> (String, String) {
    match project_type {
        ProjectType::Rust => {
            let cargo = path.join("Cargo.toml");
            if let Ok(content) = fs::read_to_string(&cargo) {
                let name = extract_toml_value(&content, "name").unwrap_or_default();
                let version = extract_toml_value(&content, "version").unwrap_or("0.1.0".into());
                return (name, version);
            }
        }
        ProjectType::Elixir => {
            let mix = path.join("mix.exs");
            if let Ok(content) = fs::read_to_string(&mix) {
                let name = extract_elixir_app(&content).unwrap_or_default();
                let version = extract_elixir_version(&content).unwrap_or("0.1.0".into());
                return (name, version);
            }
        }
        ProjectType::Deno => {
            for fname in &["deno.json", "deno.jsonc"] {
                let deno = path.join(fname);
                if let Ok(content) = fs::read_to_string(&deno) {
                    if let Ok(val) = serde_json::from_str::<serde_json::Value>(&content) {
                        let name = val["name"]
                            .as_str()
                            .unwrap_or("")
                            .trim_start_matches("@hyperpolymath/")
                            .to_string();
                        let version = val["version"]
                            .as_str()
                            .unwrap_or("0.1.0")
                            .to_string();
                        return (name, version);
                    }
                }
            }
        }
        _ => {}
    }

    // Fallback: use directory name
    let dir_name = path
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("unknown")
        .to_string();
    (dir_name, "0.1.0".into())
}

/// Detect if the project has an HTTP server.
fn detect_http_server(path: &Path, project_type: &ProjectType) -> bool {
    match project_type {
        ProjectType::Rust => {
            // Search for axum, actix_web, warp, hyper server patterns
            grep_files_recursive(
                path,
                &["rs"],
                &[
                    "axum::serve",
                    "axum::Router",
                    "actix_web",
                    "warp::serve",
                    "TcpListener::bind",
                ],
            )
        }
        ProjectType::Elixir => {
            grep_files_recursive(
                path,
                &["ex", "exs"],
                &["Plug.Cowboy", "Bandit", "Phoenix.Endpoint"],
            )
        }
        ProjectType::Deno => {
            grep_files_recursive(path, &["js", "ts", "res"], &["Deno.serve", "oak", "hono"])
        }
        ProjectType::Zig => {
            grep_files_recursive(path, &["zig"], &["std.http.Server", "httpz"])
        }
        _ => false,
    }
}

/// Detect HTTP routes from source code.
fn detect_routes(path: &Path, project_type: &ProjectType) -> Vec<DetectedRoute> {
    let mut routes = Vec::new();

    match project_type {
        ProjectType::Rust => {
            // Scan for axum .route() calls
            let re = Regex::new(r#"\.route\(\s*"([^"]+)"\s*,\s*(get|post|put|delete|patch)\("#)
                .unwrap();
            scan_files_for_pattern(path, &["rs"], &re, &mut routes);
        }
        ProjectType::Elixir => {
            // Scan for Phoenix/Plug route macros
            let re = Regex::new(r#"(get|post|put|delete|patch)\s+"([^"]+)""#).unwrap();
            scan_files_for_pattern_elixir(path, &["ex"], &re, &mut routes);
        }
        _ => {}
    }

    routes
}

/// Detect dependencies.
fn detect_dependencies(path: &Path, project_type: &ProjectType) -> Vec<String> {
    let mut deps = Vec::new();

    match project_type {
        ProjectType::Rust => {
            if let Ok(content) = fs::read_to_string(path.join("Cargo.toml")) {
                for dep in &[
                    "verisimdb",
                    "panic-attack",
                    "hypatia",
                    "echidna",
                    "feedback-o-tron",
                ] {
                    if content.contains(dep) {
                        deps.push(dep.to_string());
                    }
                }
            }
        }
        ProjectType::Elixir => {
            if let Ok(content) = fs::read_to_string(path.join("mix.exs")) {
                for dep in &[
                    "verisim_client",
                    "hypatia",
                    "echidna",
                    "feedback_o_tron",
                ] {
                    if content.contains(dep) {
                        deps.push(dep.to_string());
                    }
                }
            }
        }
        _ => {}
    }

    // Also check source code for VeriSimDB references
    if grep_files_recursive(path, &["rs", "ex", "js", "ts", "res"], &["verisimdb", "VeriSimDB"])
    {
        if !deps.contains(&"verisimdb".to_string()) {
            deps.push("verisimdb".to_string());
        }
    }

    deps
}

/// Suggest capabilities based on detected routes and dependencies.
fn suggest_capabilities(
    routes: &[DetectedRoute],
    deps: &[String],
) -> Vec<SuggestedCap> {
    let mut caps = Vec::new();

    for route in routes {
        let cap = match route.path.as_str() {
            p if p.contains("/scan") || p.contains("/assail") => Some(SuggestedCap {
                cap_type: "static-analysis".into(),
                endpoint: route.path.clone(),
                protocol: "http".into(),
                reason: format!("Route '{}' suggests scanning capability", route.path),
            }),
            p if p.contains("/prove") || p.contains("/verify") => Some(SuggestedCap {
                cap_type: "theorem-proving".into(),
                endpoint: route.path.clone(),
                protocol: "http".into(),
                reason: format!("Route '{}' suggests proving capability", route.path),
            }),
            p if p.contains("/voice") || p.contains("/rtc") => Some(SuggestedCap {
                cap_type: "voice".into(),
                endpoint: route.path.clone(),
                protocol: "webrtc".into(),
                reason: format!("Route '{}' suggests voice capability", route.path),
            }),
            p if p.contains("/dispatch") || p.contains("/bot") => Some(SuggestedCap {
                cap_type: "bot-orchestration".into(),
                endpoint: route.path.clone(),
                protocol: "http".into(),
                reason: format!("Route '{}' suggests bot orchestration", route.path),
            }),
            p if p.contains("/graphql") => Some(SuggestedCap {
                cap_type: "custom".into(),
                endpoint: route.path.clone(),
                protocol: "http".into(),
                reason: format!("GraphQL endpoint at '{}'", route.path),
            }),
            _ => None,
        };
        if let Some(c) = cap {
            caps.push(c);
        }
    }

    // Infer from deps even without explicit routes
    if deps.iter().any(|d| d.contains("verisimdb")) && caps.is_empty() {
        // VeriSimDB dependency suggests octad-storage if this IS verisimdb
        // or octad-storage consumer if it's a client
    }

    caps
}

/// Suggest consumes array from dependencies.
fn suggest_consumes(deps: &[String]) -> Vec<String> {
    let mut consumes = Vec::new();

    for dep in deps {
        match dep.as_str() {
            d if d.contains("verisimdb") || d.contains("verisim") => {
                consumes.push("octad-storage".into());
            }
            d if d.contains("panic") || d.contains("hypatia") => {
                consumes.push("scanning".into());
            }
            d if d.contains("echidna") => {
                consumes.push("theorem-proving".into());
            }
            d if d.contains("feedback") => {
                consumes.push("bug-reporting".into());
            }
            _ => {}
        }
    }

    consumes.sort();
    consumes.dedup();
    consumes
}

// --- Utility functions ---

/// Extract a simple string value from a TOML file (naive parser — no full TOML dependency).
fn extract_toml_value(content: &str, key: &str) -> Option<String> {
    let re = Regex::new(&format!(r#"^\s*{}\s*=\s*"([^"]+)""#, regex::escape(key))).ok()?;
    for line in content.lines() {
        if let Some(caps) = re.captures(line) {
            return Some(caps[1].to_string());
        }
    }
    None
}

/// Extract Elixir app name from mix.exs.
fn extract_elixir_app(content: &str) -> Option<String> {
    let re = Regex::new(r"app:\s*:(\w+)").ok()?;
    re.captures(content).map(|c| c[1].to_string())
}

/// Extract Elixir version from mix.exs.
fn extract_elixir_version(content: &str) -> Option<String> {
    let re = Regex::new(r#"version:\s*"([^"]+)""#).ok()?;
    re.captures(content).map(|c| c[1].to_string())
}

/// Recursively grep files for any of the given patterns.
fn grep_files_recursive(path: &Path, extensions: &[&str], patterns: &[&str]) -> bool {
    let walker = walkdir::WalkDir::new(path)
        .max_depth(5)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| {
            let p = e.path();
            // Skip .git, node_modules, target, _build, deps, external_corpora
            let path_str = p.to_string_lossy();
            !path_str.contains("/.git/")
                && !path_str.contains("/node_modules/")
                && !path_str.contains("/target/")
                && !path_str.contains("/_build/")
                && !path_str.contains("/deps/")
                && !path_str.contains("/external_corpora/")
                && !path_str.contains("/.deno/")
        });

    for entry in walker {
        let p = entry.path();
        if let Some(ext) = p.extension().and_then(|e| e.to_str()) {
            if extensions.contains(&ext) {
                if let Ok(content) = fs::read_to_string(p) {
                    for pattern in patterns {
                        if content.contains(pattern) {
                            return true;
                        }
                    }
                }
            }
        }
    }
    false
}

/// Scan files for route patterns and collect detected routes.
fn scan_files_for_pattern(
    path: &Path,
    extensions: &[&str],
    re: &Regex,
    routes: &mut Vec<DetectedRoute>,
) {
    let walker = walkdir::WalkDir::new(path)
        .max_depth(5)
        .into_iter()
        .filter_map(|e| e.ok());

    for entry in walker {
        let p = entry.path();
        let path_str = p.to_string_lossy();
        if path_str.contains("/.git/")
            || path_str.contains("/target/")
            || path_str.contains("/external_corpora/")
        {
            continue;
        }
        if let Some(ext) = p.extension().and_then(|e| e.to_str()) {
            if extensions.contains(&ext) {
                if let Ok(content) = fs::read_to_string(p) {
                    for (line_num, line) in content.lines().enumerate() {
                        if let Some(caps) = re.captures(line) {
                            routes.push(DetectedRoute {
                                path: caps[1].to_string(),
                                method: caps[2].to_uppercase(),
                                file: p.to_string_lossy().to_string(),
                                line: (line_num + 1) as u32,
                            });
                        }
                    }
                }
            }
        }
    }
}

/// Scan Elixir files for route patterns.
fn scan_files_for_pattern_elixir(
    path: &Path,
    extensions: &[&str],
    re: &Regex,
    routes: &mut Vec<DetectedRoute>,
) {
    let walker = walkdir::WalkDir::new(path)
        .max_depth(5)
        .into_iter()
        .filter_map(|e| e.ok());

    for entry in walker {
        let p = entry.path();
        let path_str = p.to_string_lossy();
        if path_str.contains("/.git/")
            || path_str.contains("/_build/")
            || path_str.contains("/deps/")
        {
            continue;
        }
        if let Some(ext) = p.extension().and_then(|e| e.to_str()) {
            if extensions.contains(&ext) {
                if let Ok(content) = fs::read_to_string(p) {
                    for (line_num, line) in content.lines().enumerate() {
                        if let Some(caps) = re.captures(line) {
                            routes.push(DetectedRoute {
                                method: caps[1].to_uppercase(),
                                path: caps[2].to_string(),
                                file: p.to_string_lossy().to_string(),
                                line: (line_num + 1) as u32,
                            });
                        }
                    }
                }
            }
        }
    }
}
