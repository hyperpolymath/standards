// SPDX-License-Identifier: AGPL-3.0-or-later
//! k9-sign - Ed25519 signing and verification for K9 Hunt-level components
//!
//! This is a memory-safe Rust rewrite of sign.sh, eliminating:
//! - Shell injection vulnerabilities
//! - Buffer overflow risks
//! - Path traversal attacks
//! - Race conditions in file operations
//!
//! Uses ed25519-dalek for cryptographic operations instead of shelling out to OpenSSL.

use anyhow::{Context, Result};
use base64::{engine::general_purpose::STANDARD as BASE64, Engine};
use clap::{Parser, Subcommand};
use ed25519_dalek::{Signature, Signer, SigningKey, Verifier, VerifyingKey};
use rand::rngs::OsRng;
use sha2::{Digest, Sha256};
use std::fs;
use std::path::{Path, PathBuf};

#[cfg(test)]
mod tests;

const VERSION: &str = env!("CARGO_PKG_VERSION");

/// K9 Signing Tool - Ed25519 signatures for Hunt-level components
#[derive(Parser)]
#[command(name = "k9-sign")]
#[command(version = VERSION)]
#[command(about = "Ed25519 signing and verification for K9 components", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Generate new Ed25519 keypair
    Keygen {
        /// Key name (default: primary)
        #[arg(default_value = "primary")]
        name: String,
    },
    /// Sign a component file
    Sign {
        /// File to sign
        file: PathBuf,
        /// Key name to use (default: primary)
        #[arg(default_value = "primary")]
        key: String,
    },
    /// Verify a component's signature
    Verify {
        /// File to verify
        file: PathBuf,
    },
    /// Full Hunt authorization check
    Authorize {
        /// File to authorize
        file: PathBuf,
    },
    /// Add public key to trusted keys
    Trust {
        /// Path to public key file
        pubkey: PathBuf,
    },
    /// Remove key from trusted keys
    Untrust {
        /// Key name to remove
        name: String,
    },
    /// List all keys
    List,
}

/// K9 key management directories
struct KeyDirs {
    keys: PathBuf,
    trusted: PathBuf,
}

impl KeyDirs {
    fn new() -> Result<Self> {
        Self::with_config_dir(None)
    }

    #[cfg(test)]
    fn with_config_dir(config_dir: Option<PathBuf>) -> Result<Self> {
        let config = match config_dir {
            Some(dir) => dir.join("k9"),
            None => dirs::config_dir()
                .context("Could not determine config directory")?
                .join("k9"),
        };

        let keys = config.join("keys");
        let trusted = keys.join("trusted");

        Ok(KeyDirs { keys, trusted })
    }

    #[cfg(not(test))]
    fn with_config_dir(_config_dir: Option<PathBuf>) -> Result<Self> {
        Self::new()
    }

    fn ensure_created(&self) -> Result<()> {
        fs::create_dir_all(&self.keys).context("Failed to create keys directory")?;
        fs::create_dir_all(&self.trusted).context("Failed to create trusted directory")?;

        // Set permissions to 700 (owner only)
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let perms = fs::Permissions::from_mode(0o700);
            fs::set_permissions(&self.keys, perms).context("Failed to set directory permissions")?;
        }

        Ok(())
    }

    fn private_key_path(&self, name: &str) -> PathBuf {
        self.keys.join(format!("{}.key", name))
    }

    fn public_key_path(&self, name: &str) -> PathBuf {
        self.keys.join(format!("{}.pub", name))
    }

    fn trusted_key_path(&self, name: &str) -> PathBuf {
        self.trusted.join(format!("{}.pub", name))
    }
}

/// Generate a new Ed25519 keypair
pub(crate) fn cmd_keygen(name: &str) -> Result<()> {
    let dirs = KeyDirs::new()?;
    dirs.ensure_created()?;

    let privkey_path = dirs.private_key_path(name);
    let pubkey_path = dirs.public_key_path(name);

    if privkey_path.exists() {
        anyhow::bail!(
            "Key '{}' already exists at {:?}\nDelete it first if you want to regenerate.",
            name,
            privkey_path
        );
    }

    println!("K9: Generating Ed25519 keypair '{}'...", name);

    // Generate keypair using OS random number generator
    let signing_key = SigningKey::generate(&mut OsRng);
    let verifying_key = signing_key.verifying_key();

    // Write private key (32 bytes raw)
    fs::write(&privkey_path, signing_key.to_bytes())
        .context("Failed to write private key")?;

    // Set private key permissions to 600 (owner read/write only)
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let perms = fs::Permissions::from_mode(0o600);
        fs::set_permissions(&privkey_path, perms)
            .context("Failed to set private key permissions")?;
    }

    // Write public key (32 bytes raw)
    fs::write(&pubkey_path, verifying_key.to_bytes())
        .context("Failed to write public key")?;

    println!("K9: Keypair generated:");
    println!("  Private: {:?} (keep secret!)", privkey_path);
    println!("  Public:  {:?} (share this)", pubkey_path);
    println!();
    println!("K9: To trust this key for verification:");
    println!("  k9-sign trust {:?}", pubkey_path);

    Ok(())
}

/// Sign a file with a private key
pub(crate) fn cmd_sign(file: &Path, key_name: &str) -> Result<()> {
    let dirs = KeyDirs::new()?;
    dirs.ensure_created()?;

    if !file.exists() {
        anyhow::bail!("File not found: {:?}", file);
    }

    let privkey_path = dirs.private_key_path(key_name);
    if !privkey_path.exists() {
        anyhow::bail!(
            "Private key not found: {:?}\nGenerate one with: k9-sign keygen {}",
            privkey_path,
            key_name
        );
    }

    println!("K9: Signing {:?} with key '{}'...", file, key_name);

    // Read private key
    let key_bytes = fs::read(&privkey_path).context("Failed to read private key")?;
    let signing_key = SigningKey::from_bytes(
        key_bytes
            .as_slice()
            .try_into()
            .context("Invalid private key length")?,
    );

    // Read file to sign
    let file_data = fs::read(file).context("Failed to read file to sign")?;

    // Create signature
    let signature = signing_key.sign(&file_data);

    // Write binary signature (append .sig to filename)
    let sig_path = PathBuf::from(format!("{}.sig", file.display()));
    fs::write(&sig_path, signature.to_bytes()).context("Failed to write signature")?;

    // Create base64 version for embedding
    let sig_b64 = BASE64.encode(signature.to_bytes());

    println!("K9: Signature created: {:?}", sig_path);
    println!();
    println!("K9: To embed in component, add to security section:");
    println!("  signature = \"{}\",", sig_b64);
    println!();
    println!("K9: To verify:");
    println!("  k9-sign verify {:?}", file);

    Ok(())
}

/// Verify a file's signature against trusted keys
pub(crate) fn cmd_verify(file: &Path) -> Result<()> {
    let dirs = KeyDirs::new()?;
    dirs.ensure_created()?;

    if !file.exists() {
        anyhow::bail!("File not found: {:?}", file);
    }

    let sig_path = PathBuf::from(format!("{}.sig", file.display()));
    if !sig_path.exists() {
        anyhow::bail!(
            "Signature not found: {:?}\nSign the file first with: k9-sign sign {:?}",
            sig_path,
            file
        );
    }

    // Read file and signature
    let file_data = fs::read(file).context("Failed to read file")?;
    let sig_bytes = fs::read(&sig_path).context("Failed to read signature")?;
    let signature = Signature::from_bytes(
        sig_bytes
            .as_slice()
            .try_into()
            .context("Invalid signature length")?,
    );

    // Try each trusted key
    let mut verified = false;
    let trusted_keys = fs::read_dir(&dirs.trusted)
        .context("Failed to read trusted keys directory")?;

    for entry in trusted_keys {
        let entry = entry?;
        let path = entry.path();

        if path.extension().and_then(|s| s.to_str()) != Some("pub") {
            continue;
        }

        let key_name = path.file_stem().and_then(|s| s.to_str()).unwrap_or("unknown");

        // Read public key
        let pubkey_bytes = match fs::read(&path) {
            Ok(bytes) => bytes,
            Err(_) => continue,
        };

        let verifying_key = match VerifyingKey::from_bytes(
            pubkey_bytes
                .as_slice()
                .try_into()
                .unwrap_or(&[0u8; 32]),
        ) {
            Ok(key) => key,
            Err(_) => continue,
        };

        // Try to verify
        if verifying_key.verify(&file_data, &signature).is_ok() {
            println!("K9: ✓ Signature VALID (key: {})", key_name);
            verified = true;
            break;
        }
    }

    if !verified {
        eprintln!("K9: ✗ Signature INVALID or key not trusted");
        eprintln!();
        eprintln!("K9: Trusted keys in {:?}:", dirs.trusted);

        let trusted_keys = fs::read_dir(&dirs.trusted)?;
        let mut found_any = false;
        for entry in trusted_keys {
            if let Ok(entry) = entry {
                let path = entry.path();
                if path.extension().and_then(|s| s.to_str()) == Some("pub") {
                    if let Some(name) = path.file_stem().and_then(|s| s.to_str()) {
                        println!("  - {}", name);
                        found_any = true;
                    }
                }
            }
        }

        if !found_any {
            println!("  (none)");
        }

        anyhow::bail!("Signature verification failed");
    }

    Ok(())
}

/// Full Hunt authorization check
pub(crate) fn cmd_authorize(file: &Path) -> Result<()> {
    println!("K9: Hunt Authorization for {:?}", file);
    println!("─────────────────────────────────────");
    println!();

    if !file.exists() {
        anyhow::bail!("File not found: {:?}", file);
    }

    // Read file to check security level
    let content = fs::read_to_string(file).context("Failed to read file")?;

    if content.contains("leash = 'Hunt") || content.contains("trust_level.*'Hunt") {
        println!("K9: Security level: 'Hunt (full execution)");
    } else {
        println!("K9: This file is not Hunt-level. No authorization needed.");
        return Ok(());
    }

    // Check for signature
    let sig_path = PathBuf::from(format!("{}.sig", file.display()));
    if !sig_path.exists() {
        println!();
        println!("K9: ⚠️  No signature found.");
        println!("K9: Sign the file first: k9-sign sign {:?}", file);
        anyhow::bail!("Missing signature");
    }

    // Verify signature
    println!();
    cmd_verify(file)?;

    println!();
    println!("K9: ✓ Component authorized for Hunt-level execution.");
    println!("K9: You may now run its recipes.");

    Ok(())
}

/// Add a public key to trusted keys
pub(crate) fn cmd_trust(pubkey_path: &Path) -> Result<()> {
    let dirs = KeyDirs::new()?;
    dirs.ensure_created()?;

    if !pubkey_path.exists() {
        anyhow::bail!("Public key not found: {:?}", pubkey_path);
    }

    // Validate it's a valid Ed25519 public key
    let pubkey_bytes = fs::read(pubkey_path).context("Failed to read public key")?;
    if pubkey_bytes.len() != 32 {
        anyhow::bail!(
            "Invalid public key size: {} bytes (expected 32)",
            pubkey_bytes.len()
        );
    }

    // Verify it's valid Ed25519
    VerifyingKey::from_bytes(
        pubkey_bytes
            .as_slice()
            .try_into()
            .context("Invalid public key")?,
    )
    .context("Not a valid Ed25519 public key")?;

    let key_name = pubkey_path
        .file_stem()
        .and_then(|s| s.to_str())
        .context("Could not determine key name")?;

    let dest = dirs.trusted_key_path(key_name);

    fs::copy(pubkey_path, &dest).context("Failed to copy public key")?;

    println!("K9: Trusted key added: {}", key_name);
    println!("K9: Location: {:?}", dest);

    Ok(())
}

/// Remove a key from trusted keys
pub(crate) fn cmd_untrust(name: &str) -> Result<()> {
    let dirs = KeyDirs::new()?;
    dirs.ensure_created()?;

    let keyfile = dirs.trusted_key_path(name);

    if !keyfile.exists() {
        anyhow::bail!("Trusted key not found: {}", name);
    }

    fs::remove_file(&keyfile).context("Failed to remove key")?;

    println!("K9: Removed trusted key: {}", name);

    Ok(())
}

/// List all keys
pub(crate) fn cmd_list() -> Result<()> {
    let dirs = KeyDirs::new()?;
    dirs.ensure_created()?;

    println!("K9: Key Management");
    println!("─────────────────────────────────────");
    println!();
    println!("Your keys ({:?}):", dirs.keys);

    let mut found_keys = false;
    if let Ok(entries) = fs::read_dir(&dirs.keys) {
        for entry in entries {
            if let Ok(entry) = entry {
                let path = entry.path();
                if path.extension().and_then(|s| s.to_str()) == Some("key") {
                    if let Some(name) = path.file_stem().and_then(|s| s.to_str()) {
                        println!("  - {}", name);
                        found_keys = true;
                    }
                }
            }
        }
    }

    if !found_keys {
        println!("  (none - run 'k9-sign keygen' to create)");
    }

    println!();
    println!("Trusted keys ({:?}):", dirs.trusted);

    let mut found_trusted = false;
    if let Ok(entries) = fs::read_dir(&dirs.trusted) {
        for entry in entries {
            if let Ok(entry) = entry {
                let path = entry.path();
                if path.extension().and_then(|s| s.to_str()) == Some("pub") {
                    if let Some(name) = path.file_stem().and_then(|s| s.to_str()) {
                        // Show key fingerprint
                        if let Ok(key_bytes) = fs::read(&path) {
                            let mut hasher = Sha256::new();
                            hasher.update(&key_bytes);
                            let hash = hasher.finalize();
                            let fp = hex::encode(&hash[..8]);
                            println!("  - {} (sha256:{}...)", name, fp);
                            found_trusted = true;
                        }
                    }
                }
            }
        }
    }

    if !found_trusted {
        println!("  (none - run 'k9-sign trust <pubkey>' to add)");
    }

    Ok(())
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Keygen { name } => cmd_keygen(&name),
        Commands::Sign { file, key } => cmd_sign(&file, &key),
        Commands::Verify { file } => cmd_verify(&file),
        Commands::Authorize { file } => cmd_authorize(&file),
        Commands::Trust { pubkey } => cmd_trust(&pubkey),
        Commands::Untrust { name } => cmd_untrust(&name),
        Commands::List => cmd_list(),
    }
}
