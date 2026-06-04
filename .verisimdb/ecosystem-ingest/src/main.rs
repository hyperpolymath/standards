// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
//
// ecosystem-ingest — derive ecosystem-link octads from dependency manifests.
//
// Five stages: scan → parse → normalise → resolve → emit.
// Output: A2ML octads written to stdout or --out, matching the
// ecosystem-link schema in .verisimdb/config.toml.
//
// Supported evidence sources (v0.1):
//   - Cargo.toml [dependencies]
//   - deno.json imports
//   - mix.exs deps (Elixir/Gleam)
//   - .github/workflows/*.yml — uses: references
//   - Justfile — cross-repo `just -d ../other` invocations

use std::collections::{BTreeMap, BTreeSet};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use std::time::{SystemTime, UNIX_EPOCH};

use walkdir::WalkDir;

const TOOL_VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Edge {
    source_repo: String,
    target_repo: String,
    link_type: String,  // cargo-dep, deno-import, mix-dep, action-ref, build-ref
    strength: u8,       // 0-100 (avoid floats for Ord)
    evidence_file: String,
    evidence_line: u32,
}

fn main() -> ExitCode {
    let args: Vec<String> = env::args().skip(1).collect();
    let (root, out_path, alias_path) = match parse_args(&args) {
        Ok(x) => x,
        Err(msg) => {
            eprintln!("ecosystem-ingest: {msg}");
            eprintln!(
                "usage: ecosystem-ingest [--out PATH] [--aliases PATH] <ROOT>\n  ROOT is scanned recursively; each top-level directory is treated as a repo"
            );
            return ExitCode::from(2);
        }
    };

    let aliases = load_aliases(alias_path.as_deref());
    let edges = scan_tree(&root, &aliases);
    let rendered = render_octads(&edges);

    match out_path {
        Some(p) => {
            if let Err(e) = fs::write(&p, rendered) {
                eprintln!("ecosystem-ingest: write {}: {e}", p.display());
                return ExitCode::FAILURE;
            }
            eprintln!("ecosystem-ingest: {} edges → {}", edges.len(), p.display());
        }
        None => {
            print!("{rendered}");
            eprintln!("ecosystem-ingest: {} edges", edges.len());
        }
    }
    ExitCode::SUCCESS
}

fn parse_args(args: &[String]) -> Result<(PathBuf, Option<PathBuf>, Option<PathBuf>), String> {
    let mut out: Option<PathBuf> = None;
    let mut aliases: Option<PathBuf> = None;
    let mut root: Option<PathBuf> = None;
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--out" => {
                i += 1;
                out = Some(PathBuf::from(args.get(i).ok_or("missing value after --out")?));
            }
            "--aliases" => {
                i += 1;
                aliases = Some(PathBuf::from(
                    args.get(i).ok_or("missing value after --aliases")?,
                ));
            }
            "-h" | "--help" => return Err("help".into()),
            s if s.starts_with("--") => return Err(format!("unknown flag: {s}")),
            s => {
                if root.is_some() {
                    return Err("only one ROOT allowed".into());
                }
                root = Some(PathBuf::from(s));
            }
        }
        i += 1;
    }
    Ok((root.ok_or("ROOT is required")?, out, aliases))
}

/// Load a simple key=value alias map (`dep_name = canonical_repo`).
fn load_aliases(path: Option<&Path>) -> BTreeMap<String, String> {
    let mut m = BTreeMap::new();
    let Some(p) = path else { return m };
    let Ok(text) = fs::read_to_string(p) else {
        return m;
    };
    for line in text.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        if let Some(eq) = line.find('=') {
            let k = line[..eq].trim().to_string();
            let v = line[eq + 1..].trim().trim_matches('"').to_string();
            if !k.is_empty() && !v.is_empty() {
                m.insert(k, v);
            }
        }
    }
    m
}

/// Walk each top-level directory under `root` as a repo and scan it.
fn scan_tree(root: &Path, aliases: &BTreeMap<String, String>) -> Vec<Edge> {
    let mut edges = Vec::new();
    let repos = list_repos(root);
    for repo_dir in repos {
        let Some(repo_name) = repo_dir.file_name().and_then(|s| s.to_str()) else {
            continue;
        };
        scan_repo(&repo_dir, repo_name, aliases, &mut edges);
    }
    edges.sort();
    edges.dedup();
    edges
}

/// If `root` itself contains a dependency manifest, treat it as a single repo.
/// Otherwise list every direct child directory that looks like a repo.
fn list_repos(root: &Path) -> Vec<PathBuf> {
    if looks_like_repo(root) {
        return vec![root.to_path_buf()];
    }
    let Ok(rd) = fs::read_dir(root) else {
        return Vec::new();
    };
    let mut out = Vec::new();
    for entry in rd.flatten() {
        let p = entry.path();
        if p.is_dir() && !is_hidden(&p) && looks_like_repo(&p) {
            out.push(p);
        }
    }
    out.sort();
    out
}

fn looks_like_repo(p: &Path) -> bool {
    p.join("Cargo.toml").exists()
        || p.join("deno.json").exists()
        || p.join("mix.exs").exists()
        || p.join("Justfile").exists()
        || p.join("justfile").exists()
        || p.join(".git").exists()
        || p.join(".github").is_dir()
}

fn is_hidden(p: &Path) -> bool {
    p.file_name()
        .and_then(|s| s.to_str())
        .map(|n| n.starts_with('.'))
        .unwrap_or(false)
}

fn scan_repo(
    repo_dir: &Path,
    repo_name: &str,
    aliases: &BTreeMap<String, String>,
    out: &mut Vec<Edge>,
) {
    // Cargo.toml files (top-level only to keep noise down)
    if let Some(p) = find_first(repo_dir, "Cargo.toml", 2) {
        if let Ok(text) = fs::read_to_string(&p) {
            parse_cargo_toml(&text, &p, repo_name, aliases, out);
        }
    }
    // deno.json
    if let Some(p) = find_first(repo_dir, "deno.json", 2) {
        if let Ok(text) = fs::read_to_string(&p) {
            parse_deno_json(&text, &p, repo_name, aliases, out);
        }
    }
    // mix.exs
    if let Some(p) = find_first(repo_dir, "mix.exs", 2) {
        if let Ok(text) = fs::read_to_string(&p) {
            parse_mix_exs(&text, &p, repo_name, aliases, out);
        }
    }
    // GitHub workflows
    let wf_dir = repo_dir.join(".github").join("workflows");
    if wf_dir.is_dir() {
        for e in WalkDir::new(&wf_dir).max_depth(2).into_iter().flatten() {
            if !e.file_type().is_file() {
                continue;
            }
            let ext = e.path().extension().and_then(|s| s.to_str()).unwrap_or("");
            if ext == "yml" || ext == "yaml" {
                if let Ok(text) = fs::read_to_string(e.path()) {
                    parse_workflow(&text, e.path(), repo_name, out);
                }
            }
        }
    }
}

/// Depth-limited search for a filename.
fn find_first(dir: &Path, name: &str, max_depth: usize) -> Option<PathBuf> {
    for e in WalkDir::new(dir)
        .max_depth(max_depth)
        .into_iter()
        .flatten()
    {
        if e.file_type().is_file() && e.file_name() == name {
            return Some(e.path().to_path_buf());
        }
    }
    None
}

// ───────────────────────────────────────────────────────────────────────
// Parsers
// ───────────────────────────────────────────────────────────────────────

fn parse_cargo_toml(
    text: &str,
    path: &Path,
    repo: &str,
    aliases: &BTreeMap<String, String>,
    out: &mut Vec<Edge>,
) {
    // Find [dependencies] sections and read `name = ...` lines until next [
    let sections = ["[dependencies]", "[dev-dependencies]", "[build-dependencies]"];
    for section in &sections {
        if let Some(start) = text.find(section) {
            let after = &text[start + section.len()..];
            let end = after.find("\n[").map(|i| i + 1).unwrap_or(after.len());
            let region = &after[..end];
            for (i, line) in region.lines().enumerate() {
                let line = line.trim();
                if line.is_empty() || line.starts_with('#') {
                    continue;
                }
                if let Some(eq) = line.find('=') {
                    let name = line[..eq].trim();
                    if !name.is_empty() && !name.contains('.') && name_looks_sane(name) {
                        let target = resolve(name, aliases);
                        out.push(Edge {
                            source_repo: repo.to_string(),
                            target_repo: target,
                            link_type: "cargo-dep".into(),
                            strength: 100,
                            evidence_file: display(path),
                            evidence_line: (i + 1) as u32,
                        });
                    }
                }
            }
        }
    }
}

fn parse_deno_json(
    text: &str,
    path: &Path,
    repo: &str,
    aliases: &BTreeMap<String, String>,
    out: &mut Vec<Edge>,
) {
    // Find "imports": { ... } and extract values on the RHS of `: "..."`
    let Some(start) = text.find("\"imports\"") else {
        return;
    };
    let after = &text[start..];
    let Some(ob) = after.find('{') else {
        return;
    };
    // balanced braces
    let bytes = after.as_bytes();
    let mut depth = 0i32;
    let mut end = bytes.len();
    for i in ob..bytes.len() {
        match bytes[i] {
            b'{' => depth += 1,
            b'}' => {
                depth -= 1;
                if depth == 0 {
                    end = i + 1;
                    break;
                }
            }
            _ => {}
        }
    }
    let block = &after[ob..end];
    for (idx, line) in block.lines().enumerate() {
        // "alias": "jsr:@scope/pkg" or "npm:..." or "https://..."
        if let Some(colon) = line.find(':') {
            let rhs = line[colon + 1..].trim().trim_end_matches(',').trim();
            if let Some(val) = rhs.strip_prefix('"').and_then(|s| s.strip_suffix('"')) {
                let target = normalise_deno_target(val);
                if let Some(t) = target {
                    let resolved = resolve(&t, aliases);
                    out.push(Edge {
                        source_repo: repo.to_string(),
                        target_repo: resolved,
                        link_type: "deno-import".into(),
                        strength: 100,
                        evidence_file: display(path),
                        evidence_line: (idx + 1) as u32,
                    });
                }
            }
        }
    }
}

fn normalise_deno_target(raw: &str) -> Option<String> {
    // jsr:@std/path@1.0.0  → jsr:@std/path
    // npm:lodash@4.17.0    → npm:lodash
    // npm:@scope/pkg@ver   → npm:@scope/pkg
    // https://deno.land/x/utils@1.0/mod.ts → https://deno.land/x/utils
    if let Some(rest) = raw.strip_prefix("jsr:") {
        // Strip leading @ from scoped name so the version-splitter works.
        let scoped = rest.starts_with('@');
        let body = if scoped { &rest[1..] } else { rest };
        let base = body.split('@').next()?;
        let out = if scoped { format!("@{base}") } else { base.to_string() };
        return Some(format!("jsr:{out}"));
    }
    if let Some(rest) = raw.strip_prefix("npm:") {
        let scoped = rest.starts_with('@');
        let body = if scoped { &rest[1..] } else { rest };
        let base = body.split('@').next()?;
        let out = if scoped { format!("@{base}") } else { base.to_string() };
        return Some(format!("npm:{out}"));
    }
    if let Some(rest) = raw.strip_prefix("https://") {
        // host/path/...
        let host_and_path = rest.split('?').next()?;
        let stripped: String = host_and_path
            .split('/')
            .take(3)
            .collect::<Vec<_>>()
            .join("/");
        return Some(format!("https://{}", strip_version(&stripped)));
    }
    None
}

fn strip_version(s: &str) -> String {
    s.split('@').next().unwrap_or(s).to_string()
}

fn parse_mix_exs(
    text: &str,
    path: &Path,
    repo: &str,
    aliases: &BTreeMap<String, String>,
    out: &mut Vec<Edge>,
) {
    // Match `{:name, "~> x.y"}` or `{:name, ...}`
    let bytes = text.as_bytes();
    let mut line_no: u32 = 1;
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'\n' {
            line_no += 1;
            i += 1;
            continue;
        }
        // look for `{:`
        if i + 1 < bytes.len() && bytes[i] == b'{' && bytes[i + 1] == b':' {
            let start = i + 2;
            let mut j = start;
            while j < bytes.len() {
                let c = bytes[j];
                if c.is_ascii_alphanumeric() || c == b'_' {
                    j += 1;
                } else {
                    break;
                }
            }
            if j > start {
                let name = &text[start..j];
                if name_looks_sane(name) {
                    let resolved = resolve(name, aliases);
                    out.push(Edge {
                        source_repo: repo.to_string(),
                        target_repo: resolved,
                        link_type: "mix-dep".into(),
                        strength: 100,
                        evidence_file: display(path),
                        evidence_line: line_no,
                    });
                }
            }
            i = j;
            continue;
        }
        i += 1;
    }
}

fn parse_workflow(text: &str, path: &Path, repo: &str, out: &mut Vec<Edge>) {
    for (idx, line) in text.lines().enumerate() {
        let t = line.trim_start().trim_start_matches('-').trim_start();
        // uses: owner/repo@sha-or-ref  (skip `uses: ./local`)
        let Some(rest) = t.strip_prefix("uses:") else {
            continue;
        };
        let rest = rest.trim().trim_matches('"').trim_matches('\'');
        if rest.starts_with('.') || rest.starts_with('/') {
            continue;
        }
        // strip @<ref>
        let base = rest.split('@').next().unwrap_or(rest);
        if base.contains('/') {
            out.push(Edge {
                source_repo: repo.to_string(),
                target_repo: format!("gh:{base}"),
                link_type: "action-ref".into(),
                strength: 70,
                evidence_file: display(path),
                evidence_line: (idx + 1) as u32,
            });
        }
    }
}

fn name_looks_sane(n: &str) -> bool {
    !n.is_empty()
        && n.chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
}

fn resolve(name: &str, aliases: &BTreeMap<String, String>) -> String {
    aliases.get(name).cloned().unwrap_or_else(|| format!("pkg:{name}"))
}

fn display(p: &Path) -> String {
    p.to_string_lossy().into_owned()
}

// ───────────────────────────────────────────────────────────────────────
// Emit
// ───────────────────────────────────────────────────────────────────────

fn render_octads(edges: &[Edge]) -> String {
    let now = iso8601_now();
    let mut s = String::new();
    s.push_str("# SPDX-License-Identifier: MPL-2.0\n");
    s.push_str("# ecosystem-link octads — emitted by ecosystem-ingest\n\n");
    s.push_str("@metadata:\n");
    s.push_str("tool: ecosystem-ingest\n");
    s.push_str(&format!("tool_version: {TOOL_VERSION}\n"));
    s.push_str(&format!("extracted_at: {now}\n"));
    s.push_str(&format!("edge_count: {}\n", edges.len()));
    s.push_str("@end\n\n");

    // Source-repo summary
    let mut source_counts: BTreeMap<&str, usize> = BTreeMap::new();
    let mut targets: BTreeSet<&str> = BTreeSet::new();
    for e in edges {
        *source_counts.entry(&e.source_repo).or_insert(0) += 1;
        targets.insert(&e.target_repo);
    }
    s.push_str("@summary:\n");
    s.push_str(&format!("unique_sources: {}\n", source_counts.len()));
    s.push_str(&format!("unique_targets: {}\n", targets.len()));
    s.push_str("@end\n\n");

    for (i, e) in edges.iter().enumerate() {
        s.push_str(&format!(
            "@octad(entity=\"ecosystem-link\", id=\"link-{i:06}\"):\n"
        ));
        s.push_str("@modality(name=\"Semantic\"):\n");
        s.push_str(&format!("source_repo: {}\n", e.source_repo));
        s.push_str(&format!("target_repo: {}\n", e.target_repo));
        s.push_str(&format!("link_type: {}\n", e.link_type));
        s.push_str(&format!(
            "strength: {:.2}\n",
            (e.strength as f32) / 100.0
        ));
        s.push_str("@end\n");
        s.push_str("@modality(name=\"Temporal\"):\n");
        s.push_str(&format!("first_seen: {now}\n"));
        s.push_str(&format!("last_confirmed: {now}\n"));
        s.push_str("stale_after_days: 30\n");
        s.push_str("@end\n");
        s.push_str("@modality(name=\"Provenance\"):\n");
        s.push_str("detection_method: manifest-scan\n");
        s.push_str(&format!("evidence_file: {}\n", e.evidence_file));
        s.push_str(&format!("evidence_line: {}\n", e.evidence_line));
        s.push_str("@end\n");
        s.push_str("@end\n\n");
    }
    s
}

fn iso8601_now() -> String {
    let secs = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0);
    let (y, mo, d, h, mi, se) = epoch_to_ymd_hms(secs);
    format!("{y:04}-{mo:02}-{d:02}T{h:02}:{mi:02}:{se:02}Z")
}

fn epoch_to_ymd_hms(secs: u64) -> (i64, u32, u32, u32, u32, u32) {
    let days = (secs / 86_400) as i64;
    let rem = secs % 86_400;
    let h = (rem / 3_600) as u32;
    let mi = ((rem % 3_600) / 60) as u32;
    let se = (rem % 60) as u32;
    let z = days + 719_468;
    let era = if z >= 0 { z } else { z - 146_096 } / 146_097;
    let doe = (z - era * 146_097) as u64;
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146_096) / 365;
    let y = yoe as i64 + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = (doy - (153 * mp + 2) / 5 + 1) as u32;
    let m = if mp < 10 { mp + 3 } else { mp - 9 } as u32;
    let y = if m <= 2 { y + 1 } else { y };
    (y, m, d, h, mi, se)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_cargo_dependencies() {
        let src = r#"
[package]
name = "demo"

[dependencies]
serde = "1"
walkdir = { version = "2" }
# comment
tokio = "1.0"

[dev-dependencies]
proptest = "1"
"#;
        let mut out = Vec::new();
        parse_cargo_toml(src, Path::new("Cargo.toml"), "demo", &BTreeMap::new(), &mut out);
        let names: Vec<&str> = out.iter().map(|e| e.target_repo.as_str()).collect();
        assert!(names.contains(&"pkg:serde"));
        assert!(names.contains(&"pkg:walkdir"));
        assert!(names.contains(&"pkg:tokio"));
        assert!(names.contains(&"pkg:proptest"));
    }

    #[test]
    fn parses_deno_imports() {
        let src = r#"
{
  "imports": {
    "std/": "jsr:@std/path@1.0.0",
    "lodash": "npm:lodash@4.17.0",
    "deno-utils": "https://deno.land/x/utils@1.0/mod.ts"
  }
}"#;
        let mut out = Vec::new();
        parse_deno_json(src, Path::new("deno.json"), "demo", &BTreeMap::new(), &mut out);
        let targets: Vec<&str> = out.iter().map(|e| e.target_repo.as_str()).collect();
        assert!(targets.iter().any(|t| t.contains("jsr:@std/path")));
        assert!(targets.iter().any(|t| t.contains("npm:lodash")));
    }

    #[test]
    fn parses_mix_deps() {
        let src = r#"
defp deps do
  [
    {:phoenix, "~> 1.7"},
    {:ecto, "~> 3.10"},
    {:jason, "~> 1.4"}
  ]
end
"#;
        let mut out = Vec::new();
        parse_mix_exs(src, Path::new("mix.exs"), "demo", &BTreeMap::new(), &mut out);
        let names: Vec<&str> = out.iter().map(|e| e.target_repo.as_str()).collect();
        assert!(names.contains(&"pkg:phoenix"));
        assert!(names.contains(&"pkg:ecto"));
        assert!(names.contains(&"pkg:jason"));
    }

    #[test]
    fn parses_workflow_uses() {
        let src = r#"
jobs:
  test:
    steps:
      - uses: actions/checkout@v4
      - uses: "actions/setup-rust@abc123"
      - uses: ./local-action
"#;
        let mut out = Vec::new();
        parse_workflow(src, Path::new("wf.yml"), "demo", &mut out);
        assert_eq!(out.len(), 2);
        assert!(out.iter().any(|e| e.target_repo == "gh:actions/checkout"));
        assert!(out.iter().any(|e| e.target_repo == "gh:actions/setup-rust"));
    }

    #[test]
    fn aliases_resolve() {
        let mut a = BTreeMap::new();
        a.insert("serde".to_string(), "serde-rs/serde".to_string());
        assert_eq!(resolve("serde", &a), "serde-rs/serde");
        assert_eq!(resolve("other", &a), "pkg:other");
    }

    #[test]
    fn render_produces_octads() {
        let edges = vec![Edge {
            source_repo: "demo".into(),
            target_repo: "pkg:serde".into(),
            link_type: "cargo-dep".into(),
            strength: 100,
            evidence_file: "Cargo.toml".into(),
            evidence_line: 5,
        }];
        let out = render_octads(&edges);
        assert!(out.contains("@octad(entity=\"ecosystem-link\""));
        assert!(out.contains("source_repo: demo"));
        assert!(out.contains("target_repo: pkg:serde"));
        assert!(out.contains("strength: 1.00"));
        assert!(out.contains("edge_count: 1"));
    }
}
