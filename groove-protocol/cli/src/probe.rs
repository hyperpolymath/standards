// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// groove probe — discover running Groove services on localhost.
//
// Probes [::1] first, then 127.0.0.1, per TRANSPORT.adoc requirement.
// Fetches /.well-known/groove manifests and builds a topology map.

use anyhow::Result;
use colored::Colorize;
use std::time::Duration;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::TcpStream;
use tokio::time::timeout;

use crate::registry;

/// A discovered Groove service.
#[derive(Debug, serde::Serialize)]
pub struct DiscoveredService {
    pub service_id: String,
    pub port: u16,
    pub host: String,
    pub version: String,
    pub capabilities: Vec<String>,
    pub consumes: Vec<String>,
    pub mode: String,
}

/// Run the probe subcommand.
pub async fn run(host: &str, extra_ports: Option<&str>, timeout_ms: u64) -> Result<()> {
    let probe_timeout = Duration::from_millis(timeout_ms);

    // Build port list from registry + extra ports
    let mut ports: Vec<u16> = registry::REGISTRY.iter().map(|e| e.port).collect();
    ports.sort();
    ports.dedup();

    // Add extra ports if specified
    if let Some(extra) = extra_ports {
        for p in extra.split(',') {
            if let Ok(port) = p.trim().parse::<u16>() {
                if !ports.contains(&port) {
                    ports.push(port);
                }
            }
        }
    }

    println!(
        "{} Probing {} ports on {}...",
        "groove probe:".bold(),
        ports.len(),
        host
    );
    println!();

    let mut discovered: Vec<DiscoveredService> = Vec::new();

    for &port in &ports {
        // Try IPv6 first (per TRANSPORT.adoc)
        let hosts_to_try = if host == "localhost" {
            vec!["[::1]", "127.0.0.1"]
        } else {
            vec![host]
        };

        for probe_host in &hosts_to_try {
            let addr = if probe_host.starts_with('[') {
                format!("{}:{}", probe_host.trim_matches(|c| c == '[' || c == ']'), port)
            } else {
                format!("{}:{}", probe_host, port)
            };

            match probe_groove(&addr, probe_timeout).await {
                Ok(Some(manifest_json)) => {
                    if let Ok(manifest) =
                        serde_json::from_str::<serde_json::Value>(&manifest_json)
                    {
                        let service_id = manifest["service_id"]
                            .as_str()
                            .unwrap_or("unknown")
                            .to_string();
                        let version = manifest["service_version"]
                            .as_str()
                            .unwrap_or("?")
                            .to_string();
                        let mode = manifest["mode"]
                            .as_str()
                            .unwrap_or("active")
                            .to_string();

                        let capabilities: Vec<String> = manifest["capabilities"]
                            .as_object()
                            .map(|obj| {
                                obj.values()
                                    .filter_map(|v| v["type"].as_str().map(String::from))
                                    .collect()
                            })
                            .unwrap_or_default();

                        let consumes: Vec<String> = manifest["consumes"]
                            .as_array()
                            .map(|arr| {
                                arr.iter()
                                    .filter_map(|v| v.as_str().map(String::from))
                                    .collect()
                            })
                            .unwrap_or_default();

                        // Check if this port matches the registry
                        let registry_match =
                            registry::find_service(&service_id).map(|e| e.port == port);

                        let status = match registry_match {
                            Some(true) => "registered".green(),
                            Some(false) => "port mismatch".yellow(),
                            None => "unregistered".dimmed(),
                        };

                        println!(
                            "  {} {:>5}  {:<20} v{:<10} [{}] {}",
                            if probe_host.contains(':') {
                                "IPv6"
                            } else {
                                "IPv4"
                            },
                            port,
                            service_id.bold(),
                            version,
                            status,
                            capabilities.join(", ")
                        );

                        discovered.push(DiscoveredService {
                            service_id,
                            port,
                            host: probe_host.to_string(),
                            version,
                            capabilities,
                            consumes,
                            mode,
                        });
                    }
                    break; // Don't try IPv4 if IPv6 worked
                }
                Ok(None) => {
                    // Port open but no Groove manifest
                }
                Err(_) => {
                    // Connection refused or timeout — try next host
                }
            }
        }
    }

    println!();
    println!(
        "{} {} service(s) discovered",
        "groove probe:".bold(),
        discovered.len()
    );

    if discovered.is_empty() {
        println!("  No Groove services running on localhost.");
        println!("  Start a service or use `groove registry` to see known ports.");
    }

    Ok(())
}

/// Show the live Groove mesh topology.
pub async fn mesh(json: &bool) -> Result<()> {
    let probe_timeout = Duration::from_millis(500);
    let ports: Vec<u16> = registry::REGISTRY.iter().map(|e| e.port).collect();

    let mut services: Vec<DiscoveredService> = Vec::new();

    // Discover all running services
    for &port in &ports {
        for host in &["::1", "127.0.0.1"] {
            let addr = format!("{}:{}", host, port);
            if let Ok(Some(manifest_json)) = probe_groove(&addr, probe_timeout).await {
                if let Ok(manifest) = serde_json::from_str::<serde_json::Value>(&manifest_json) {
                    let service_id = manifest["service_id"]
                        .as_str()
                        .unwrap_or("unknown")
                        .to_string();
                    let capabilities: Vec<String> = manifest["capabilities"]
                        .as_object()
                        .map(|obj| {
                            obj.values()
                                .filter_map(|v| v["type"].as_str().map(String::from))
                                .collect()
                        })
                        .unwrap_or_default();
                    let consumes: Vec<String> = manifest["consumes"]
                        .as_array()
                        .map(|arr| {
                            arr.iter()
                                .filter_map(|v| v.as_str().map(String::from))
                                .collect()
                        })
                        .unwrap_or_default();

                    services.push(DiscoveredService {
                        service_id,
                        port,
                        host: host.to_string(),
                        version: manifest["service_version"]
                            .as_str()
                            .unwrap_or("?")
                            .to_string(),
                        capabilities,
                        consumes,
                        mode: manifest["mode"]
                            .as_str()
                            .unwrap_or("active")
                            .to_string(),
                    });
                    break;
                }
            }
        }
    }

    if *json {
        // Build a mesh graph as JSON
        let mut edges = Vec::new();
        for consumer in &services {
            for cap_needed in &consumer.consumes {
                for provider in &services {
                    if provider.service_id != consumer.service_id
                        && provider.capabilities.contains(cap_needed)
                    {
                        edges.push(serde_json::json!({
                            "from": provider.service_id,
                            "to": consumer.service_id,
                            "capability": cap_needed
                        }));
                    }
                }
            }
        }

        let mesh = serde_json::json!({
            "services": services,
            "edges": edges,
            "timestamp": chrono_now()
        });
        println!("{}", serde_json::to_string_pretty(&mesh)?);
    } else {
        // ASCII topology
        println!("{}", "Groove Mesh Topology".bold());
        println!("{}", "=".repeat(60));

        if services.is_empty() {
            println!("  No running services detected.");
            return Ok(());
        }

        for consumer in &services {
            println!();
            println!(
                "  {} :{} (offers: {})",
                consumer.service_id.bold(),
                consumer.port,
                consumer.capabilities.join(", ")
            );

            for cap_needed in &consumer.consumes {
                let providers: Vec<&DiscoveredService> = services
                    .iter()
                    .filter(|s| {
                        s.service_id != consumer.service_id
                            && s.capabilities.contains(cap_needed)
                    })
                    .collect();

                if providers.is_empty() {
                    println!(
                        "    {} ← {} ({})",
                        "DANGLING".red(),
                        cap_needed,
                        "no provider running".dimmed()
                    );
                } else {
                    for provider in providers {
                        println!(
                            "    {} ← {} ← {} :{}",
                            "OK".green(),
                            cap_needed,
                            provider.service_id,
                            provider.port
                        );
                    }
                }
            }
        }

        println!();
        println!("{}", "-".repeat(60));
        println!(
            "{} service(s), {} capability flow(s)",
            services.len(),
            services.iter().map(|s| s.consumes.len()).sum::<usize>()
        );
    }

    Ok(())
}

/// Probe a single address for a Groove manifest.
///
/// Returns Ok(Some(json)) if the probe succeeds and returns valid JSON,
/// Ok(None) if the port is open but no Groove manifest,
/// Err if the connection fails.
async fn probe_groove(addr: &str, probe_timeout: Duration) -> Result<Option<String>> {
    let stream = timeout(probe_timeout, TcpStream::connect(addr)).await??;

    // Send a minimal HTTP/1.0 GET request
    let request = format!(
        "GET /.well-known/groove HTTP/1.0\r\nHost: localhost\r\nAccept: application/json\r\n\r\n"
    );

    let mut stream = stream;
    stream.write_all(request.as_bytes()).await?;

    let mut response = Vec::with_capacity(16384);
    timeout(probe_timeout, stream.read_to_end(&mut response)).await??;

    let response_str = String::from_utf8_lossy(&response);

    // Check for 200 OK
    if !response_str.starts_with("HTTP/1") || !response_str.contains("200") {
        return Ok(None);
    }

    // Extract body (after \r\n\r\n)
    if let Some(body_start) = response_str.find("\r\n\r\n") {
        let body = &response_str[body_start + 4..];
        if body.trim_start().starts_with('{') {
            return Ok(Some(body.trim().to_string()));
        }
    }

    Ok(None)
}

/// Simple timestamp without pulling in chrono.
fn chrono_now() -> String {
    "2026-04-04T00:00:00Z".to_string() // TODO: use actual time
}
