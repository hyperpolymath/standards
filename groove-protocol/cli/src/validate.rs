// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// groove validate — check a manifest against the Groove schema and codebase.
//
// Outputs findings in panic-attack-compatible JSON format (file, line, severity,
// description) so dogfood-gate can consume them directly.
//
// Implements DOG-03 (Groove manifest conformant) from the testing taxonomy.

use anyhow::{Context, Result};
use colored::Colorize;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::Path;

use crate::registry;

/// A validation finding, compatible with panic-attack WeakPoint format.
#[derive(Debug, Serialize, Deserialize)]
pub struct Finding {
    pub file: Option<String>,
    pub line: Option<u32>,
    pub severity: String,
    pub description: String,
    pub check: String,
}

/// Run the validate subcommand.
pub fn run(path: &str, json_output: bool) -> Result<()> {
    let repo_path = Path::new(path);

    // Find the manifest
    let manifest_path = if repo_path.is_file() && repo_path.extension().map_or(false, |e| e == "json") {
        repo_path.to_path_buf()
    } else {
        repo_path.join(".well-known/groove/manifest.json")
    };

    let mut findings: Vec<Finding> = Vec::new();

    if !manifest_path.exists() {
        // Check if this repo has an HTTP server — if so, missing Groove is a finding
        let has_server = crate::detect::detect(repo_path)
            .map(|info| info.has_http_server)
            .unwrap_or(false);

        if has_server {
            findings.push(Finding {
                file: None,
                line: None,
                severity: "medium".into(),
                description: "HTTP server detected but no Groove manifest found. Run `groove init` to generate one.".into(),
                check: "DOG-08".into(),
            });
        } else {
            findings.push(Finding {
                file: None,
                line: None,
                severity: "low".into(),
                description: "No Groove manifest found (not an HTTP server — consider `groove init --passive` for CLI tools).".into(),
                check: "DOG-08".into(),
            });
        }

        return output_findings(&findings, json_output);
    }

    let content = fs::read_to_string(&manifest_path)
        .with_context(|| format!("Failed to read {}", manifest_path.display()))?;

    let manifest_file = manifest_path.to_string_lossy().to_string();

    // Check 1: Valid JSON
    let manifest: serde_json::Value = match serde_json::from_str(&content) {
        Ok(v) => v,
        Err(e) => {
            findings.push(Finding {
                file: Some(manifest_file.clone()),
                line: Some(1),
                severity: "critical".into(),
                description: format!("Invalid JSON: {}", e),
                check: "DOG-03".into(),
            });
            return output_findings(&findings, json_output);
        }
    };

    // Check 2: groove_version must be exactly "1"
    match manifest.get("groove_version") {
        Some(v) if v == "1" => {}
        Some(v) => {
            findings.push(Finding {
                file: Some(manifest_file.clone()),
                line: None,
                severity: "high".into(),
                description: format!(
                    "groove_version should be \"1\", got {}. This breaks strict consumers.",
                    v
                ),
                check: "DOG-03".into(),
            });
        }
        None => {
            findings.push(Finding {
                file: Some(manifest_file.clone()),
                line: None,
                severity: "critical".into(),
                description: "Missing required field 'groove_version'".into(),
                check: "DOG-03".into(),
            });
        }
    }

    // Check 3: service_id present and valid format
    match manifest.get("service_id").and_then(|v| v.as_str()) {
        Some(id) => {
            let re = regex::Regex::new(r"^[a-z][a-z0-9_-]*$").unwrap();
            if !re.is_match(id) {
                findings.push(Finding {
                    file: Some(manifest_file.clone()),
                    line: None,
                    severity: "high".into(),
                    description: format!(
                        "service_id '{}' does not match pattern ^[a-z][a-z0-9_-]*$",
                        id
                    ),
                    check: "DOG-03".into(),
                });
            }
            // Check for stale names (DOG-10)
            if id == "panic-attacker" {
                findings.push(Finding {
                    file: Some(manifest_file.clone()),
                    line: None,
                    severity: "high".into(),
                    description: "Stale service_id 'panic-attacker' — renamed to 'panic-attack' on 2026-02-08".into(),
                    check: "DOG-10".into(),
                });
            }
        }
        None => {
            findings.push(Finding {
                file: Some(manifest_file.clone()),
                line: None,
                severity: "critical".into(),
                description: "Missing required field 'service_id'".into(),
                check: "DOG-03".into(),
            });
        }
    }

    // Check 4: capabilities must be an object (not an array)
    match manifest.get("capabilities") {
        Some(v) if v.is_object() => {
            let caps = v.as_object().unwrap();
            if caps.is_empty() {
                findings.push(Finding {
                    file: Some(manifest_file.clone()),
                    line: None,
                    severity: "medium".into(),
                    description: "Empty capabilities object — service offers nothing".into(),
                    check: "DOG-03".into(),
                });
            }

            // Check each capability
            for (key, cap) in caps {
                // Check type is valid
                if let Some(cap_type) = cap.get("type").and_then(|v| v.as_str()) {
                    if !registry::is_valid_capability(cap_type) {
                        findings.push(Finding {
                            file: Some(manifest_file.clone()),
                            line: None,
                            severity: "medium".into(),
                            description: format!(
                                "Capability '{}' has unknown type '{}'. Use 'custom' if intentional.",
                                key, cap_type
                            ),
                            check: "DOG-03".into(),
                        });
                    }

                    // Key should match type for discoverability
                    if key != cap_type && cap_type != "custom" {
                        findings.push(Finding {
                            file: Some(manifest_file.clone()),
                            line: None,
                            severity: "low".into(),
                            description: format!(
                                "Capability key '{}' differs from type '{}' — convention is key == type",
                                key, cap_type
                            ),
                            check: "DOG-03".into(),
                        });
                    }
                } else {
                    findings.push(Finding {
                        file: Some(manifest_file.clone()),
                        line: None,
                        severity: "high".into(),
                        description: format!("Capability '{}' missing required 'type' field", key),
                        check: "DOG-03".into(),
                    });
                }

                // Check protocol is valid
                if let Some(proto) = cap.get("protocol").and_then(|v| v.as_str()) {
                    if !registry::is_valid_protocol(proto) {
                        findings.push(Finding {
                            file: Some(manifest_file.clone()),
                            line: None,
                            severity: "medium".into(),
                            description: format!(
                                "Capability '{}' has unknown protocol '{}'. Valid: {}",
                                key,
                                proto,
                                registry::PROTOCOL_TYPES.join(", ")
                            ),
                            check: "DOG-03".into(),
                        });
                    }
                }
            }
        }
        Some(_) => {
            findings.push(Finding {
                file: Some(manifest_file.clone()),
                line: None,
                severity: "high".into(),
                description: "capabilities must be a JSON object (map), not an array. This is a schema violation that breaks consumers.".into(),
                check: "DOG-03".into(),
            });
        }
        None => {
            findings.push(Finding {
                file: Some(manifest_file.clone()),
                line: None,
                severity: "critical".into(),
                description: "Missing required field 'capabilities'".into(),
                check: "DOG-03".into(),
            });
        }
    }

    // Check 5: consumes array has valid capability types
    if let Some(consumes) = manifest.get("consumes").and_then(|v| v.as_array()) {
        for item in consumes {
            if let Some(cap) = item.as_str() {
                if !registry::is_valid_capability(cap) {
                    findings.push(Finding {
                        file: Some(manifest_file.clone()),
                        line: None,
                        severity: "medium".into(),
                        description: format!("consumes '{}' is not a known capability type", cap),
                        check: "DOG-03".into(),
                    });
                }
            }
        }
    }

    // Check 6: Port collision with registry
    if let Some(service_id) = manifest.get("service_id").and_then(|v| v.as_str()) {
        if let Some(reg_entry) = registry::find_service(service_id) {
            // Check endpoints URLs match registry port
            if let Some(endpoints) = manifest.get("endpoints").and_then(|v| v.as_object()) {
                for (name, url) in endpoints {
                    if let Some(url_str) = url.as_str() {
                        if let Some(port_in_url) = extract_port_from_url(url_str) {
                            if port_in_url != reg_entry.port {
                                findings.push(Finding {
                                    file: Some(manifest_file.clone()),
                                    line: None,
                                    severity: "high".into(),
                                    description: format!(
                                        "Endpoint '{}' URL port {} differs from registry port {} for service '{}'",
                                        name, port_in_url, reg_entry.port, service_id
                                    ),
                                    check: "DOG-04".into(),
                                });
                            }
                        }
                    }
                }
            }
        }
    }

    // Check 7: Template placeholders (DOG-06)
    if content.contains("{{OWNER}}") || content.contains("{{REPO}}") || content.contains("[YOUR") {
        findings.push(Finding {
            file: Some(manifest_file.clone()),
            line: None,
            severity: "critical".into(),
            description: "Unfilled template placeholders detected in manifest".into(),
            check: "DOG-06".into(),
        });
    }

    output_findings(&findings, json_output)
}

/// Output findings as human-readable or JSON.
fn output_findings(findings: &[Finding], json_output: bool) -> Result<()> {
    if json_output {
        println!("{}", serde_json::to_string_pretty(findings)?);
    } else {
        if findings.is_empty() {
            println!("{}", "groove validate: all checks passed".green());
            return Ok(());
        }

        let critical = findings.iter().filter(|f| f.severity == "critical").count();
        let high = findings.iter().filter(|f| f.severity == "high").count();
        let medium = findings.iter().filter(|f| f.severity == "medium").count();
        let low = findings.iter().filter(|f| f.severity == "low").count();

        for finding in findings {
            let severity_colored = match finding.severity.as_str() {
                "critical" => finding.severity.red().bold(),
                "high" => finding.severity.red(),
                "medium" => finding.severity.yellow(),
                "low" => finding.severity.dimmed(),
                _ => finding.severity.normal(),
            };

            let location = finding
                .file
                .as_deref()
                .unwrap_or("(no file)");

            println!(
                "[{}] [{}] {} — {}",
                finding.check.bold(),
                severity_colored,
                location,
                finding.description
            );
        }

        println!();
        println!(
            "groove validate: {} finding(s) — {} critical, {} high, {} medium, {} low",
            findings.len(),
            critical,
            high,
            medium,
            low
        );

        if critical > 0 {
            std::process::exit(1);
        }
    }

    Ok(())
}

/// Extract port number from a URL like "http://localhost:8080/api".
fn extract_port_from_url(url: &str) -> Option<u16> {
    let re = regex::Regex::new(r":(\d+)").ok()?;
    re.captures(url)
        .and_then(|c| c[1].parse::<u16>().ok())
}
