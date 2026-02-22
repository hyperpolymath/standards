// SPDX-License-Identifier: PMPL-1.0-or-later

//! Repository Guardian Filesystem
//!
//! FUSE filesystem wrapper that enforces AI.a2ml manifest acknowledgment
//! before allowing any file access in repositories.
//!
//! Universal enforcement for ANY AI agent (not just MCP-compatible).

use anyhow::{Context, Result};
use clap::Parser;
use fuse3::path::Session;
use fuse3::MountOptions;
use std::path::PathBuf;
use tracing::{info, Level};

mod filesystem;
mod manifest;
mod session_manager;

use filesystem::GuardianFs;

/// Repository Guardian Filesystem - Universal AI manifest enforcement
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Source directory containing repositories
    #[arg(short, long)]
    source: PathBuf,

    /// Mount point where guarded filesystem will be accessible
    #[arg(short, long)]
    mount: PathBuf,

    /// Strict mode - block ALL operations until acknowledgment
    #[arg(long, default_value_t = true)]
    strict: bool,

    /// Session timeout in seconds
    #[arg(long, default_value_t = 3600)]
    session_timeout: u64,

    /// Allow root to access filesystem
    #[arg(long)]
    allow_root: bool,

    /// Allow other users to access filesystem
    #[arg(long)]
    allow_other: bool,
}

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_max_level(Level::INFO)
        .with_target(false)
        .init();

    let args = Args::parse();

    info!("Repository Guardian Filesystem starting");
    info!("Source: {:?}", args.source);
    info!("Mount: {:?}", args.mount);
    info!("Strict mode: {}", args.strict);

    // Verify source directory exists
    if !args.source.exists() {
        anyhow::bail!("Source directory does not exist: {:?}", args.source);
    }

    // Verify mount point exists
    if !args.mount.exists() {
        anyhow::bail!("Mount point does not exist: {:?}", args.mount);
    }

    // Create guardian filesystem
    let fs = GuardianFs::new(args.source.clone(), args.strict, args.session_timeout)?;

    // Configure mount options
    let mut mount_options = MountOptions::default();

    if args.allow_root {
        mount_options.allow_root(true);
    }

    if args.allow_other {
        mount_options.allow_other(true);
    }

    // Set filesystem name
    mount_options.fs_name("repo-guardian-fs");

    info!("Mounting filesystem...");

    // Mount and run
    Session::new(mount_options)
        .mount_with_unprivileged(fs, &args.mount)
        .await
        .context("Failed to mount filesystem")?
        .await
        .context("Filesystem error")?;

    info!("Filesystem unmounted");

    Ok(())
}
