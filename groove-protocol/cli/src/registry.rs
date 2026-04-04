// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// Canonical Groove registry — the single source of truth for port assignments,
// capability types, and service metadata. All other port tables (Groove.idr,
// groove.zig, groove-discovery.js) are derived from this.
//
// Reconciled from the 2026-04-04 estate-wide dogfooding audit which found
// 3 conflicting port tables with 4 disagreements.

use anyhow::{bail, Result};
use colored::Colorize;
use serde::{Deserialize, Serialize};

/// A registered Groove service with canonical port and capabilities.
#[derive(Debug, Clone, Serialize)]
pub struct ServiceEntry {
    pub id: &'static str,
    pub port: u16,
    pub offers: &'static [&'static str],
    pub consumes: &'static [&'static str],
    pub description: &'static str,
}

/// The canonical Groove service registry.
///
/// Port assignments are authoritative. If any other file (Groove.idr, groove.zig,
/// groove-discovery.js) disagrees, it is that file which is wrong.
pub const REGISTRY: &[ServiceEntry] = &[
    ServiceEntry {
        id: "burble",
        port: 6473,
        offers: &[
            "voice",
            "text",
            "presence",
            "spatial-audio",
            "recording",
            "tts",
            "stt",
        ],
        consumes: &["integrity", "octad-storage", "scanning"],
        description: "P2P voice + AI bridge — real-time communications platform",
    },
    ServiceEntry {
        id: "vext",
        port: 6480,
        offers: &[
            "integrity",
            "feed-verification",
            "hash-chain",
            "attestation",
        ],
        consumes: &["voice", "text", "octad-storage"],
        description: "Verification triad member — cryptographic integrity proofs",
    },
    ServiceEntry {
        id: "panic-attack",
        port: 7600,
        offers: &["static-analysis"],
        consumes: &["octad-storage", "workflow"],
        description: "47-language static analysis and security scanning",
    },
    ServiceEntry {
        id: "conflow",
        port: 7700,
        offers: &["config-orchestration"],
        consumes: &["octad-storage"],
        description: "CUE + Nickel + K9 config validation orchestrator",
    },
    ServiceEntry {
        id: "rpa-elysium",
        port: 7800,
        offers: &["workflow"],
        consumes: &["voice", "text", "octad-storage", "scanning"],
        description: "Robotic process automation toolkit",
    },
    ServiceEntry {
        id: "panll",
        port: 8000,
        offers: &["panel-ui"],
        consumes: &[
            "voice",
            "text",
            "presence",
            "integrity",
            "octad-storage",
            "scanning",
        ],
        description: "Cognitive-relief development panel system (108 panels)",
    },
    ServiceEntry {
        id: "verisimdb",
        port: 8080,
        offers: &["octad-storage", "drift-detection", "temporal-versioning"],
        consumes: &["scanning"],
        description: "Cross-system data consistency via 8-modality octad model",
    },
    ServiceEntry {
        id: "gitbot-fleet",
        port: 8080, // NOTE: port collision with VeriSimDB — needs resolution
        offers: &["bot-orchestration"],
        consumes: &["scanning", "workflow", "octad-storage"],
        description: "Bot fleet for automated repo quality enforcement (6 bots)",
    },
    ServiceEntry {
        id: "echidna",
        port: 9000,
        offers: &["theorem-proving"],
        consumes: &["octad-storage", "scanning"],
        description: "Neurosymbolic theorem-proving platform (30 provers)",
    },
    ServiceEntry {
        id: "hypatia",
        port: 9090,
        offers: &["scanning", "static-analysis"],
        consumes: &["octad-storage", "workflow"],
        description: "Neurosymbolic CI/CD intelligence (15 rule modules)",
    },
];

/// All valid capability type wire names per the Groove schema.
pub const CAPABILITY_TYPES: &[&str] = &[
    "voice",
    "text",
    "presence",
    "spatial-audio",
    "recording",
    "tts",
    "stt",
    "integrity",
    "feed-verification",
    "hash-chain",
    "attestation",
    "octad-storage",
    "drift-detection",
    "temporal-versioning",
    "scanning",
    "static-analysis",
    "panel-ui",
    "bot-orchestration",
    "workflow",
    "dns-verify",
    "config-orchestration",
    "theorem-proving",
    // v1.1 additions from dogfooding audit
    "bug-reporting",
    "dogfood-feedback",
    "cve-analysis",
    "proof-exchange",
    "neural-dispatch",
    "custom",
];

/// Valid wire protocol types.
pub const PROTOCOL_TYPES: &[&str] = &[
    "webrtc",
    "websocket",
    "http",
    "grpc",
    "nntps",
    "cli",  // v1.1: passive-mode services
    "mcp",  // v1.1: BoJ MCP invocation
    "custom",
];

/// Print the canonical registry table.
pub fn print_registry() {
    println!("{}", "Groove Service Registry (canonical)".bold());
    println!("{}", "=".repeat(80));
    println!(
        "{:<18} {:>5}  {:<40}  {}",
        "SERVICE".bold(),
        "PORT".bold(),
        "OFFERS".bold(),
        "CONSUMES".bold()
    );
    println!("{}", "-".repeat(80));

    for entry in REGISTRY {
        let offers = entry.offers.join(", ");
        let consumes = entry.consumes.join(", ");
        let port_str = if entry.port == 8080 && entry.id == "gitbot-fleet" {
            format!("{}", entry.port).yellow().to_string()
        } else {
            format!("{}", entry.port)
        };
        println!("{:<18} {:>5}  {:<40}  {}", entry.id, port_str, offers, consumes);
    }

    println!("{}", "-".repeat(80));
    println!(
        "{} registered services, {} capability types",
        REGISTRY.len(),
        CAPABILITY_TYPES.len()
    );
    println!();
    println!(
        "{}",
        "NOTE: gitbot-fleet (8080) collides with verisimdb (8080) — pending resolution"
            .yellow()
    );
}

/// Look up a service by ID.
pub fn find_service(service_id: &str) -> Option<&'static ServiceEntry> {
    REGISTRY.iter().find(|e| e.id == service_id)
}

/// Look up a service by port.
pub fn find_by_port(port: u16) -> Vec<&'static ServiceEntry> {
    REGISTRY.iter().filter(|e| e.port == port).collect()
}

/// Check if a capability type wire name is valid.
pub fn is_valid_capability(cap: &str) -> bool {
    CAPABILITY_TYPES.contains(&cap)
}

/// Check if a protocol type is valid.
pub fn is_valid_protocol(proto: &str) -> bool {
    PROTOCOL_TYPES.contains(&proto)
}

/// Result of a compatibility check between two services.
#[derive(Debug, Serialize)]
pub struct CompatResult {
    pub compatible: bool,
    pub matched: Vec<CapMatch>,
    pub reasons: Vec<String>,
}

/// A single matched capability flow.
#[derive(Debug, Serialize)]
pub struct CapMatch {
    pub provider: String,
    pub consumer: String,
    pub capability: String,
}

/// Check if two services can compose via Groove.
///
/// Composition requires bidirectional capability satisfaction:
/// A.consumes ⊆ B.offers AND B.consumes ⊆ A.offers
///
/// Partial composition (one direction only) is reported as incompatible
/// with an explanatory reason.
pub fn check_compat(a_id: &str, b_id: &str) -> Result<CompatResult> {
    let a = find_service(a_id);
    let b = find_service(b_id);

    if a.is_none() {
        bail!("Service '{}' not found in registry. Use a registered service_id or implement file-based loading.", a_id);
    }

    if b.is_none() {
        bail!("Service '{}' not found in registry. Use a registered service_id.", b_id);
    }

    let a = a.unwrap();
    let b = b.unwrap();

    let mut matched = Vec::new();
    let mut reasons = Vec::new();

    // Check A.consumes ⊆ B.offers
    for cap in a.consumes {
        if b.offers.contains(cap) {
            matched.push(CapMatch {
                provider: b.id.to_string(),
                consumer: a.id.to_string(),
                capability: cap.to_string(),
            });
        } else {
            reasons.push(format!(
                "{} consumes '{}' but {} does not offer it",
                a.id, cap, b.id
            ));
        }
    }

    // Check B.consumes ⊆ A.offers
    for cap in b.consumes {
        if a.offers.contains(cap) {
            matched.push(CapMatch {
                provider: a.id.to_string(),
                consumer: b.id.to_string(),
                capability: cap.to_string(),
            });
        } else {
            reasons.push(format!(
                "{} consumes '{}' but {} does not offer it",
                b.id, cap, a.id
            ));
        }
    }

    Ok(CompatResult {
        compatible: reasons.is_empty(),
        matched,
        reasons,
    })
}

