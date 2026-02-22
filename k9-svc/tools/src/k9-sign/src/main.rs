// SPDX-License-Identifier: PMPL-1.0-or-later
//! k9-sign: Sign and verify K9 contractiles with Ed25519

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use ed25519_dalek::{Signature, Signer, SigningKey, Verifier, VerifyingKey};
use rand::rngs::OsRng;
use std::fs;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(name = "k9-sign")]
#[command(about = "Sign and verify K9 contractiles", long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Generate new Ed25519 keypair
    Keygen {
        /// Output directory for keys
        #[arg(short, long, default_value = "~/.k9/keys")]
        output: PathBuf,
    },
    /// Sign a contractile
    Sign {
        /// Contractile file to sign
        file: PathBuf,

        /// Private key file
        #[arg(short, long)]
        key_file: Option<PathBuf>,
    },
    /// Verify contractile signature
    Verify {
        /// Contractile file to verify
        file: PathBuf,

        /// Public key file
        #[arg(short, long)]
        key_file: Option<PathBuf>,
    },
}

fn main() -> Result<()> {
    let args = Args::parse();

    match args.command {
        Commands::Keygen { output } => keygen(&output)?,
        Commands::Sign { file, key_file } => sign(&file, key_file.as_deref())?,
        Commands::Verify { file, key_file } => verify(&file, key_file.as_deref())?,
    }

    Ok(())
}

fn keygen(output_dir: &PathBuf) -> Result<()> {
    println!("Generating Ed25519 keypair...");

    let mut csprng = OsRng;
    let signing_key = SigningKey::generate(&mut csprng);
    let verifying_key = signing_key.verifying_key();

    // Expand ~ in path
    let output_dir = if output_dir.starts_with("~") {
        let home = dirs::home_dir().context("Could not determine home directory")?;
        home.join(output_dir.strip_prefix("~").unwrap())
    } else {
        output_dir.clone()
    };

    fs::create_dir_all(&output_dir)
        .context("Failed to create output directory")?;

    let private_key_path = output_dir.join("private.key");
    let public_key_path = output_dir.join("public.key");

    // Save private key
    fs::write(&private_key_path, signing_key.to_bytes())
        .context("Failed to write private key")?;
    println!("✓ Private key: {}", private_key_path.display());

    // Save public key
    fs::write(&public_key_path, verifying_key.to_bytes())
        .context("Failed to write public key")?;
    println!("✓ Public key: {}", public_key_path.display());

    println!();
    println!("⚠  Keep your private key secure!");
    println!("   Private key: {}", private_key_path.display());
    println!("   Public key:  {}", public_key_path.display());

    Ok(())
}

fn sign(file: &PathBuf, key_file: Option<&PathBuf>) -> Result<()> {
    println!("Signing contractile: {}", file.display());

    let content = fs::read_to_string(file)
        .context("Failed to read contractile file")?;

    // Load private key
    let key_path = key_file.map(|p| p.clone()).unwrap_or_else(|| {
        dirs::home_dir()
            .unwrap()
            .join(".k9/keys/private.key")
    });

    let key_bytes = fs::read(&key_path)
        .context("Failed to read private key")?;
    let signing_key = SigningKey::from_bytes(
        key_bytes.as_slice().try_into()
            .context("Invalid private key format")?
    );

    // Sign content
    let signature = signing_key.sign(content.as_bytes());
    let signature_b64 = base64::encode(signature.to_bytes());

    // Append signature to contractile
    let signed_content = format!(
        "{}\n\n# Signature (Ed25519)\n# {}\n# Signed: {}\n",
        content,
        signature_b64,
        chrono::Local::now().to_rfc3339()
    );

    fs::write(file, signed_content)
        .context("Failed to write signed contractile")?;

    println!("✓ Contractile signed successfully");
    println!("  Signature: {}", &signature_b64[..32]);

    Ok(())
}

fn verify(file: &PathBuf, key_file: Option<&PathBuf>) -> Result<()> {
    println!("Verifying contractile: {}", file.display());

    let content = fs::read_to_string(file)
        .context("Failed to read contractile file")?;

    // Extract signature from comments
    let signature_b64 = content
        .lines()
        .find(|line| line.starts_with("# ") && line.len() > 80)
        .and_then(|line| line.strip_prefix("# "))
        .context("No signature found in contractile")?;

    let signature_bytes = base64::decode(signature_b64)
        .context("Invalid signature format")?;
    let signature = Signature::from_bytes(
        signature_bytes.as_slice().try_into()
            .context("Invalid signature")?
    );

    // Remove signature from content for verification
    let content_without_sig: String = content
        .lines()
        .take_while(|line| !line.starts_with("# Signature"))
        .collect::<Vec<_>>()
        .join("\n");

    // Load public key
    let key_path = key_file.map(|p| p.clone()).unwrap_or_else(|| {
        dirs::home_dir()
            .unwrap()
            .join(".k9/keys/public.key")
    });

    let key_bytes = fs::read(&key_path)
        .context("Failed to read public key")?;
    let verifying_key = VerifyingKey::from_bytes(
        key_bytes.as_slice().try_into()
            .context("Invalid public key format")?
    ).context("Failed to parse public key")?;

    // Verify signature
    match verifying_key.verify(content_without_sig.as_bytes(), &signature) {
        Ok(_) => {
            println!("✓ Signature valid");
            Ok(())
        }
        Err(_) => {
            println!("✗ Signature invalid");
            anyhow::bail!("Signature verification failed")
        }
    }
}
