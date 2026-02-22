// SPDX-License-Identifier: AGPL-3.0-or-later
//! Comprehensive tests for k9-sign cryptographic operations

#[cfg(test)]
mod tests {
    use super::super::*;
    use std::fs;
    use tempfile::TempDir;

    /// Create a temporary test directory and KeyDirs for testing
    fn setup_test_env() -> (TempDir, KeyDirs) {
        let dir = TempDir::new().expect("Failed to create temp dir");
        let keys_dir = KeyDirs::with_config_dir(Some(dir.path().to_path_buf()))
            .expect("Failed to create KeyDirs");
        keys_dir.ensure_created().expect("Failed to ensure dirs");
        (dir, keys_dir)
    }

    /// Create a temporary test directory (deprecated, use setup_test_env)
    fn setup_test_dir() -> TempDir {
        TempDir::new().expect("Failed to create temp dir")
    }

    /// Create a test file with content
    fn create_test_file(dir: &TempDir, name: &str, content: &[u8]) -> PathBuf {
        let path = dir.path().join(name);
        fs::write(&path, content).expect("Failed to write test file");
        path
    }

    #[test]
    fn test_keygen_creates_keypair() {
        let dir = setup_test_dir();
        let key_name = "test-key";

        // Override K9 directories for testing
        std::env::set_var("XDG_CONFIG_HOME", dir.path());

        let result = cmd_keygen(key_name);
        assert!(result.is_ok(), "Keygen should succeed");

        let keys_dir = dir.path().join("k9/keys");
        let privkey = keys_dir.join(format!("{}.key", key_name));
        let pubkey = keys_dir.join(format!("{}.pub", key_name));

        assert!(privkey.exists(), "Private key should exist");
        assert!(pubkey.exists(), "Public key should exist");

        // Check key sizes (Ed25519 is 32 bytes)
        let priv_bytes = fs::read(&privkey).expect("Failed to read private key");
        let pub_bytes = fs::read(&pubkey).expect("Failed to read public key");

        assert_eq!(priv_bytes.len(), 32, "Private key should be 32 bytes");
        assert_eq!(pub_bytes.len(), 32, "Public key should be 32 bytes");
    }

    #[test]
    fn test_keygen_prevents_overwrite() {
        let dir = setup_test_dir();
        let key_name = "test-key";

        std::env::set_var("XDG_CONFIG_HOME", dir.path());

        // Generate once
        cmd_keygen(key_name).expect("First keygen should succeed");

        // Try to generate again
        let result = cmd_keygen(key_name);
        assert!(result.is_err(), "Second keygen should fail");
    }

    #[test]
    fn test_sign_and_verify_roundtrip() {
        let dir = setup_test_dir();
        let key_name = "test-key";

        std::env::set_var("XDG_CONFIG_HOME", dir.path());

        // Generate keypair
        cmd_keygen(key_name).expect("Keygen failed");

        // Create test file
        let test_file = create_test_file(&dir, "test.txt", b"Test data for signing");

        // Sign the file
        let result = cmd_sign(&test_file, key_name);
        assert!(result.is_ok(), "Sign should succeed");

        // Check signature file exists
        let sig_file = test_file.with_extension("txt.sig");
        assert!(sig_file.exists(), "Signature file should exist");

        // Trust the key
        let keys_dir = dir.path().join("k9/keys");
        let pubkey = keys_dir.join(format!("{}.pub", key_name));
        cmd_trust(&pubkey).expect("Trust should succeed");

        // Verify the signature
        let result = cmd_verify(&test_file);
        assert!(result.is_ok(), "Verify should succeed");
    }

    #[test]
    fn test_verify_fails_with_wrong_signature() {
        let dir = setup_test_dir();
        let key_name = "test-key";

        std::env::set_var("XDG_CONFIG_HOME", dir.path());

        // Generate keypair and trust it
        cmd_keygen(key_name).expect("Keygen failed");
        let keys_dir = dir.path().join("k9/keys");
        let pubkey = keys_dir.join(format!("{}.pub", key_name));
        cmd_trust(&pubkey).expect("Trust should succeed");

        // Create and sign test file
        let test_file = create_test_file(&dir, "test.txt", b"Original content");
        cmd_sign(&test_file, key_name).expect("Sign failed");

        // Modify the file (breaks signature)
        fs::write(&test_file, b"Modified content").expect("Failed to modify file");

        // Verify should fail
        let result = cmd_verify(&test_file);
        assert!(result.is_err(), "Verify should fail for modified content");
    }

    #[test]
    fn test_verify_fails_without_trusted_key() {
        let dir = setup_test_dir();
        let key_name = "test-key";

        std::env::set_var("XDG_CONFIG_HOME", dir.path());

        // Generate keypair (but don't trust it)
        cmd_keygen(key_name).expect("Keygen failed");

        // Create and sign test file
        let test_file = create_test_file(&dir, "test.txt", b"Test data");
        cmd_sign(&test_file, key_name).expect("Sign failed");

        // Verify should fail (key not trusted)
        let result = cmd_verify(&test_file);
        assert!(result.is_err(), "Verify should fail without trusted key");
    }

    #[test]
    fn test_trust_validates_key_size() {
        let dir = setup_test_dir();
        std::env::set_var("XDG_CONFIG_HOME", dir.path());

        // Create invalid key (wrong size)
        let invalid_key = create_test_file(&dir, "invalid.pub", b"too short");

        let result = cmd_trust(&invalid_key);
        assert!(result.is_err(), "Trust should reject invalid key size");
    }

    #[test]
    fn test_untrust_removes_key() {
        let dir = setup_test_dir();
        let key_name = "test-key";

        std::env::set_var("XDG_CONFIG_HOME", dir.path());

        // Generate and trust a key
        cmd_keygen(key_name).expect("Keygen failed");
        let keys_dir = dir.path().join("k9/keys");
        let pubkey = keys_dir.join(format!("{}.pub", key_name));
        cmd_trust(&pubkey).expect("Trust failed");

        // Verify key is trusted
        let trusted_dir = keys_dir.join("trusted");
        let trusted_key = trusted_dir.join(format!("{}.pub", key_name));
        assert!(trusted_key.exists(), "Trusted key should exist");

        // Untrust the key
        cmd_untrust(key_name).expect("Untrust failed");

        // Verify key is removed
        assert!(!trusted_key.exists(), "Trusted key should be removed");
    }

    #[test]
    fn test_authorize_detects_hunt_level() {
        let dir = setup_test_dir();
        let key_name = "test-key";

        std::env::set_var("XDG_CONFIG_HOME", dir.path());

        // Generate keypair and trust it
        cmd_keygen(key_name).expect("Keygen failed");
        let keys_dir = dir.path().join("k9/keys");
        let pubkey = keys_dir.join(format!("{}.pub", key_name));
        cmd_trust(&pubkey).expect("Trust should succeed");

        // Create Hunt-level component
        let hunt_content = b"K9!\nleash = 'Hunt\nsome config data";
        let hunt_file = create_test_file(&dir, "hunt.k9.ncl", hunt_content);

        // Sign it
        cmd_sign(&hunt_file, key_name).expect("Sign failed");

        // Authorize should succeed
        let result = cmd_authorize(&hunt_file);
        assert!(result.is_ok(), "Authorize should succeed for signed Hunt component");
    }

    #[test]
    fn test_authorize_accepts_non_hunt_without_signature() {
        let dir = setup_test_dir();
        std::env::set_var("XDG_CONFIG_HOME", dir.path());

        // Create Kennel-level component (no signature needed)
        let kennel_content = b"K9!\nleash = 'Kennel\nsome config data";
        let kennel_file = create_test_file(&dir, "kennel.k9.ncl", kennel_content);

        // Authorize should succeed without signature
        let result = cmd_authorize(&kennel_file);
        assert!(result.is_ok(), "Authorize should succeed for non-Hunt component");
    }

    #[test]
    fn test_signature_is_deterministic() {
        let dir = setup_test_dir();
        let key_name = "test-key";

        std::env::set_var("XDG_CONFIG_HOME", dir.path());

        // Generate keypair
        cmd_keygen(key_name).expect("Keygen failed");

        // Create test file
        let test_file = create_test_file(&dir, "test.txt", b"Deterministic test");

        // Sign twice
        cmd_sign(&test_file, key_name).expect("First sign failed");
        let sig1 = fs::read(test_file.with_extension("txt.sig")).expect("Failed to read sig1");

        fs::remove_file(test_file.with_extension("txt.sig")).expect("Failed to remove sig");

        cmd_sign(&test_file, key_name).expect("Second sign failed");
        let sig2 = fs::read(test_file.with_extension("txt.sig")).expect("Failed to read sig2");

        // Ed25519 signatures are deterministic
        assert_eq!(sig1, sig2, "Signatures should be deterministic");
    }

    #[test]
    fn test_different_keys_produce_different_signatures() {
        let dir = setup_test_dir();
        std::env::set_var("XDG_CONFIG_HOME", dir.path());

        // Generate two keypairs
        cmd_keygen("key1").expect("Keygen 1 failed");
        cmd_keygen("key2").expect("Keygen 2 failed");

        // Create test file
        let test_file = create_test_file(&dir, "test.txt", b"Test data");

        // Sign with first key
        cmd_sign(&test_file, "key1").expect("Sign with key1 failed");
        let sig1 = fs::read(test_file.with_extension("txt.sig")).expect("Failed to read sig1");

        // Sign with second key
        fs::remove_file(test_file.with_extension("txt.sig")).expect("Failed to remove sig");
        cmd_sign(&test_file, "key2").expect("Sign with key2 failed");
        let sig2 = fs::read(test_file.with_extension("txt.sig")).expect("Failed to read sig2");

        // Signatures should be different
        assert_ne!(sig1, sig2, "Different keys should produce different signatures");
    }

    #[test]
    fn test_list_shows_keys() {
        let dir = setup_test_dir();
        std::env::set_var("XDG_CONFIG_HOME", dir.path());

        // Generate some keys
        cmd_keygen("key1").expect("Keygen failed");
        cmd_keygen("key2").expect("Keygen failed");

        // Trust one of them
        let keys_dir = dir.path().join("k9/keys");
        let pubkey = keys_dir.join("key1.pub");
        cmd_trust(&pubkey).expect("Trust failed");

        // List should succeed (we can't easily test output, but verify it doesn't panic)
        let result = cmd_list();
        assert!(result.is_ok(), "List should succeed");
    }

    #[test]
    fn test_private_key_permissions_are_restricted() {
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;

            let dir = setup_test_dir();
            std::env::set_var("XDG_CONFIG_HOME", dir.path());

            cmd_keygen("test-key").expect("Keygen failed");

            let keys_dir = dir.path().join("k9/keys");
            let privkey = keys_dir.join("test-key.key");

            let metadata = fs::metadata(&privkey).expect("Failed to get metadata");
            let mode = metadata.permissions().mode();

            // Check that only owner can read/write (0o600)
            assert_eq!(mode & 0o777, 0o600, "Private key should have 0o600 permissions");
        }
    }

    #[test]
    fn test_empty_file_can_be_signed_and_verified() {
        let dir = setup_test_dir();
        let key_name = "test-key";

        std::env::set_var("XDG_CONFIG_HOME", dir.path());

        // Generate keypair and trust it
        cmd_keygen(key_name).expect("Keygen failed");
        let keys_dir = dir.path().join("k9/keys");
        let pubkey = keys_dir.join(format!("{}.pub", key_name));
        cmd_trust(&pubkey).expect("Trust should succeed");

        // Create empty file
        let test_file = create_test_file(&dir, "empty.txt", b"");

        // Sign and verify
        cmd_sign(&test_file, key_name).expect("Sign empty file failed");
        let result = cmd_verify(&test_file);
        assert!(result.is_ok(), "Verify empty file should succeed");
    }

    #[test]
    fn test_large_file_can_be_signed() {
        let dir = setup_test_dir();
        let key_name = "test-key";

        std::env::set_var("XDG_CONFIG_HOME", dir.path());

        cmd_keygen(key_name).expect("Keygen failed");

        // Create 1MB file
        let large_data = vec![0xAA; 1024 * 1024];
        let test_file = create_test_file(&dir, "large.bin", &large_data);

        // Sign should handle large files
        let result = cmd_sign(&test_file, key_name);
        assert!(result.is_ok(), "Sign should handle large files");

        // Signature should still be 64 bytes
        let sig = fs::read(test_file.with_extension("bin.sig")).expect("Failed to read sig");
        assert_eq!(sig.len(), 64, "Signature should be 64 bytes regardless of file size");
    }
}
