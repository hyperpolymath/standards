// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// groove CLI — service discovery protocol tooling.
//
// Subcommands:
//   init       — generate a .well-known/groove/manifest.json from repo analysis
//   validate   — check manifest against schema and codebase for drift
//   probe      — discover running Groove services on localhost
//   registry   — show the canonical port/capability registry
//   check-compat — test if two services can compose
//   mesh       — show the live Groove mesh topology

#![forbid(unsafe_code)]

use anyhow::Result;
use clap::{Parser, Subcommand};

mod detect;
mod init;
mod probe;
mod registry;
mod validate;

/// groove — Groove protocol service discovery tooling
#[derive(Parser)]
#[command(name = "groove", version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Generate a .well-known/groove/manifest.json for this repo.
    Init {
        /// Directory to analyse (default: current directory)
        #[arg(short, long, default_value = ".")]
        path: String,

        /// Service ID override (default: auto-detected from project name)
        #[arg(long)]
        service_id: Option<String>,

        /// Port override (default: auto-detected or prompted)
        #[arg(long)]
        port: Option<u16>,

        /// Generate a passive (CLI/library) manifest instead of active (HTTP server)
        #[arg(long)]
        passive: bool,
    },

    /// Validate an existing groove manifest against the schema and codebase.
    Validate {
        /// Path to the manifest or repo root (default: current directory)
        #[arg(short, long, default_value = ".")]
        path: String,

        /// Output findings as JSON (panic-attack compatible format)
        #[arg(long)]
        json: bool,
    },

    /// Probe localhost for running Groove services.
    Probe {
        /// Host to probe (default: localhost)
        #[arg(long, default_value = "localhost")]
        host: String,

        /// Additional ports to probe (comma-separated)
        #[arg(long)]
        extra_ports: Option<String>,

        /// Timeout per probe in milliseconds
        #[arg(long, default_value = "500")]
        timeout_ms: u64,
    },

    /// Show the canonical Groove port/capability registry.
    Registry,

    /// Check if two services can compose via Groove.
    CheckCompat {
        /// First service (path to manifest.json or service_id for live probe)
        service_a: String,
        /// Second service (path to manifest.json or service_id for live probe)
        service_b: String,
    },

    /// Show the live Groove mesh topology.
    Mesh {
        /// Output as JSON instead of ASCII
        #[arg(long)]
        json: bool,
    },
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Init {
            path,
            service_id,
            port,
            passive,
        } => init::run(&path, service_id.as_deref(), port, passive)?,

        Commands::Validate { path, json } => validate::run(&path, json)?,

        Commands::Probe {
            host,
            extra_ports,
            timeout_ms,
        } => probe::run(&host, extra_ports.as_deref(), timeout_ms).await?,

        Commands::Registry => registry::print_registry(),

        Commands::CheckCompat {
            service_a,
            service_b,
        } => {
            let compat = registry::check_compat(&service_a, &service_b)?;
            if compat.compatible {
                println!(
                    "COMPATIBLE: {} <-> {} ({} matched capabilities)",
                    service_a,
                    service_b,
                    compat.matched.len()
                );
                for cap in &compat.matched {
                    println!("  {} -> {}", cap.provider, cap.capability);
                }
            } else {
                println!("INCOMPATIBLE: {} <-> {}", service_a, service_b);
                for reason in &compat.reasons {
                    println!("  {}", reason);
                }
            }
        }

        Commands::Mesh { json } => probe::mesh(&json).await?,
    }

    Ok(())
}
