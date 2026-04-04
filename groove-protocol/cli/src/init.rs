// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// groove init — generate a .well-known/groove/manifest.json from repo analysis.
//
// Detects service type, extracts routes, infers capabilities, and writes
// a correct manifest. Supports --passive for CLI tools and libraries.

use anyhow::{Context, Result};
use serde_json::json;
use std::fs;
use std::path::Path;

use crate::detect::{self, ProjectType};
use crate::registry;

/// Run the init subcommand.
pub fn run(
    path: &str,
    service_id_override: Option<&str>,
    port_override: Option<u16>,
    passive: bool,
) -> Result<()> {
    let repo_path = Path::new(path);
    let info = detect::detect(repo_path)?;

    let service_id = service_id_override
        .map(String::from)
        .unwrap_or(info.service_id.clone());

    let is_passive = passive || info.project_type == ProjectType::Cli;

    // Determine port
    let port = if is_passive {
        None
    } else {
        port_override.or_else(|| {
            // Check if this service is in the canonical registry
            registry::find_service(&service_id).map(|e| e.port)
        })
    };

    // Build capabilities
    let mut capabilities = serde_json::Map::new();

    if !info.suggested_capabilities.is_empty() {
        for cap in &info.suggested_capabilities {
            let mut cap_obj = serde_json::Map::new();
            cap_obj.insert("type".into(), json!(cap.cap_type));
            cap_obj.insert(
                "description".into(),
                json!(format!("Auto-detected: {}", cap.reason)),
            );
            cap_obj.insert("protocol".into(), json!(cap.protocol));
            if !is_passive {
                cap_obj.insert("endpoint".into(), json!(cap.endpoint));
            }
            cap_obj.insert("requires_auth".into(), json!(false));
            cap_obj.insert("panel_compatible".into(), json!(true));

            // Use the cap type as the key (hyphenated)
            capabilities.insert(cap.cap_type.clone(), json!(cap_obj));
        }
    }

    // If no capabilities detected, add a placeholder
    if capabilities.is_empty() {
        let mut placeholder = serde_json::Map::new();
        placeholder.insert("type".into(), json!("custom"));
        placeholder.insert(
            "description".into(),
            json!("TODO: describe this service's primary capability"),
        );
        placeholder.insert(
            "protocol".into(),
            json!(if is_passive { "cli" } else { "http" }),
        );
        if !is_passive {
            placeholder.insert("endpoint".into(), json!("/api"));
        }
        placeholder.insert("requires_auth".into(), json!(false));
        placeholder.insert("panel_compatible".into(), json!(true));
        capabilities.insert("primary".into(), json!(placeholder));
    }

    // Build the manifest
    let mut manifest = serde_json::Map::new();
    manifest.insert("groove_version".into(), json!("1"));
    manifest.insert("service_id".into(), json!(service_id));
    manifest.insert("service_version".into(), json!(info.version));

    if is_passive {
        manifest.insert("mode".into(), json!("passive"));
    }

    manifest.insert("capabilities".into(), json!(capabilities));
    manifest.insert("consumes".into(), json!(info.suggested_consumes));

    if is_passive {
        // Add invoke_patterns for CLI tools
        manifest.insert(
            "invoke_patterns".into(),
            json!({
                "ci": format!("{} --ci --format json", service_id),
                "local": format!("{} --interactive", service_id),
                "mcp": format!("boj cartridge_invoke {}", service_id)
            }),
        );
        manifest.insert("health".into(), json!(null));
    } else {
        // Active service endpoints
        let mut endpoints = serde_json::Map::new();
        if let Some(p) = port {
            endpoints.insert("api".into(), json!(format!("http://localhost:{}/api", p)));
            endpoints.insert(
                "health".into(),
                json!(format!("http://localhost:{}/health", p)),
            );
        }
        manifest.insert("endpoints".into(), json!(endpoints));
        manifest.insert("health".into(), json!("/health"));
    }

    manifest.insert("applicability".into(), json!(["individual", "team"]));

    // Write the manifest
    let output_dir = repo_path.join(".well-known/groove");
    fs::create_dir_all(&output_dir)
        .with_context(|| format!("Failed to create {}", output_dir.display()))?;

    let output_path = output_dir.join("manifest.json");
    let json_string = serde_json::to_string_pretty(&manifest)?;
    fs::write(&output_path, &json_string)
        .with_context(|| format!("Failed to write {}", output_path.display()))?;

    println!("Created {}", output_path.display());
    println!();
    println!("  service_id:   {}", service_id);
    println!("  version:      {}", info.version);
    println!(
        "  mode:         {}",
        if is_passive { "passive" } else { "active" }
    );
    println!("  capabilities: {}", capabilities.len());
    println!(
        "  consumes:     {}",
        info.suggested_consumes.join(", ")
    );

    if !info.detected_routes.is_empty() {
        println!();
        println!("  Detected routes:");
        for route in &info.detected_routes {
            println!(
                "    {} {} ({}:{})",
                route.method, route.path, route.file, route.line
            );
        }
    }

    // Warnings
    if capabilities.values().any(|v| v["type"] == "custom") {
        println!();
        println!(
            "  WARNING: Could not auto-detect capabilities. Edit the manifest to replace"
        );
        println!("           the 'custom' placeholder with the correct capability type.");
        println!(
            "           Valid types: {}",
            registry::CAPABILITY_TYPES.join(", ")
        );
    }

    if port.is_none() && !is_passive {
        println!();
        println!("  WARNING: Could not detect port. Use --port to specify, or edit the manifest.");
    }

    Ok(())
}
