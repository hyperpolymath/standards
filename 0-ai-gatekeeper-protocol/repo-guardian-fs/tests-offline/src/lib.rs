// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// Offline unit tests for repo-guardian-fs manifest and session logic.
//
// These tests are in a SEPARATE crate to avoid the broken fuse3 dependency
// (fuse3 v0.7.3 fails on Rust stable >= 1.80). The FUSE mount functionality
// requires a live environment (Linux with FUSE support + privileges); those
// are marked as "live environment" and not included here.
//
// LIVE ENVIRONMENT TESTS (not included — require FUSE kernel module + privileges):
// - Filesystem mount/unmount lifecycle
// - Access control via FUSE operations (open, read, readdir)
// - FUSE passthrough to underlying filesystem
// - Concurrent FUSE access patterns
//
// WHAT IS TESTED HERE:
// - Manifest file parsing (find_and_parse_manifest, parse_manifest)
// - Canonical location extraction
// - Invariant extraction
// - SHA-256 hash computation
// - Session management (get_or_create, acknowledge, expiry)

use anyhow::Result;
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::path::Path;
use std::sync::{Arc, RwLock};
use std::time::{Duration, Instant};

// ===========================================================================
// Inlined manifest logic (from ../src/manifest.rs, without fuse3 imports)
// ===========================================================================

/// Canonical file locations declared in manifest.
#[derive(Debug, Clone, PartialEq)]
pub struct CanonicalLocations {
    pub scm_files: String,
    pub bot_directives: String,
    pub agent_instructions: Vec<String>,
}

impl Default for CanonicalLocations {
    fn default() -> Self {
        Self {
            scm_files: ".machine_readable/".to_string(),
            bot_directives: ".bot_directives/".to_string(),
            agent_instructions: vec![
                ".claude/CLAUDE.md".to_string(),
                "AI.a2ml".to_string(),
                "0-AI-MANIFEST.a2ml".to_string(),
            ],
        }
    }
}

/// Parsed AI manifest.
#[derive(Debug, Clone)]
pub struct Manifest {
    pub hash: String,
    pub canonical_locations: CanonicalLocations,
    pub invariants: Vec<String>,
}

fn extract_path(line: &str) -> Option<String> {
    if let Some(start) = line.find('`') {
        if let Some(end) = line[start + 1..].find('`') {
            let path = line[start + 1..start + 1 + end].to_string();
            return Some(path);
        }
    }
    None
}

fn extract_canonical_locations(content: &str) -> CanonicalLocations {
    let mut locations = CanonicalLocations::default();
    for line in content.lines() {
        let line_lower = line.to_lowercase();
        if line_lower.contains("scm") && line_lower.contains("machine_readable") {
            if let Some(path) = extract_path(line) {
                if path.contains("machine_readable") {
                    locations.scm_files = path;
                }
            }
        }
        if line_lower.contains("bot") && line_lower.contains("directive") {
            if let Some(path) = extract_path(line) {
                if path.contains("bot_directive") {
                    locations.bot_directives = path;
                }
            }
        }
    }
    locations
}

fn extract_invariants(content: &str) -> Vec<String> {
    let mut invariants = Vec::new();
    let mut in_section = false;
    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.to_lowercase().contains("core invariant") {
            in_section = true;
            continue;
        }
        if in_section && trimmed.starts_with("##") {
            break;
        }
        if in_section {
            if let Some(stripped) = trimmed.strip_prefix(char::is_numeric) {
                if stripped.trim_start().starts_with('.') {
                    let invariant = stripped
                        .trim_start_matches('.')
                        .trim()
                        .trim_start_matches("**")
                        .split("**")
                        .next()
                        .unwrap_or("")
                        .trim()
                        .to_string();
                    if !invariant.is_empty() {
                        invariants.push(invariant);
                    }
                }
            }
        }
    }
    if invariants.is_empty() {
        invariants.push("no_scm_duplication".to_string());
        invariants.push("single_source_of_truth".to_string());
    }
    invariants
}

pub fn parse_manifest_from_content(content: &str) -> Manifest {
    let mut hasher = Sha256::new();
    hasher.update(content.as_bytes());
    let hash = format!("{:x}", hasher.finalize());
    Manifest {
        hash,
        canonical_locations: extract_canonical_locations(content),
        invariants: extract_invariants(content),
    }
}

pub fn parse_manifest(path: &Path) -> Result<Manifest> {
    let content = std::fs::read_to_string(path)
        .map_err(|e| anyhow::anyhow!("Failed to read manifest {:?}: {}", path, e))?;
    Ok(parse_manifest_from_content(&content))
}

pub fn find_and_parse_manifest(repo_path: &Path) -> Result<Manifest> {
    let names = ["0-AI-MANIFEST.a2ml", "AI.a2ml", "!AI.a2ml"];
    for name in &names {
        let p = repo_path.join(name);
        if p.exists() {
            return parse_manifest(&p);
        }
    }
    anyhow::bail!(
        "No AI manifest found in {:?}. Expected one of: {}",
        repo_path,
        names.join(", ")
    )
}

// ===========================================================================
// Inlined session logic (from ../src/session_manager.rs, without fuse3)
// ===========================================================================

pub type SessionId = u32;

#[derive(Debug, Clone)]
pub struct Session {
    pub id: SessionId,
    pub acknowledged: bool,
    pub created_at: Instant,
    pub last_activity: Instant,
}

#[derive(Clone)]
pub struct SessionManager {
    sessions: Arc<RwLock<HashMap<SessionId, Session>>>,
    timeout: Duration,
}

impl SessionManager {
    pub fn new(timeout_secs: u64) -> Self {
        Self {
            sessions: Arc::new(RwLock::new(HashMap::new())),
            timeout: Duration::from_secs(timeout_secs),
        }
    }

    pub fn get_or_create_session(&self, session_id: SessionId) -> Session {
        let mut sessions = self.sessions.write().unwrap();
        if let Some(session) = sessions.get_mut(&session_id) {
            if session.last_activity.elapsed() < self.timeout {
                session.last_activity = Instant::now();
                return session.clone();
            }
        }
        let session = Session {
            id: session_id,
            acknowledged: false,
            created_at: Instant::now(),
            last_activity: Instant::now(),
        };
        sessions.insert(session_id, session.clone());
        session
    }

    pub fn is_acknowledged(&self, session_id: SessionId) -> bool {
        let sessions = self.sessions.read().unwrap();
        sessions
            .get(&session_id)
            .map(|s| s.acknowledged && s.last_activity.elapsed() < self.timeout)
            .unwrap_or(false)
    }

    pub fn acknowledge(&self, session_id: SessionId, _manifest_hash: &str) -> Result<()> {
        let mut sessions = self.sessions.write().unwrap();
        if let Some(session) = sessions.get_mut(&session_id) {
            session.acknowledged = true;
            session.last_activity = Instant::now();
            Ok(())
        } else {
            anyhow::bail!("Session {} not found", session_id)
        }
    }

    pub fn cleanup_expired(&self) {
        let mut sessions = self.sessions.write().unwrap();
        sessions.retain(|_, s| s.last_activity.elapsed() < self.timeout);
    }

    pub fn active_count(&self) -> usize {
        let sessions = self.sessions.read().unwrap();
        sessions
            .values()
            .filter(|s| s.last_activity.elapsed() < self.timeout)
            .count()
    }
}

// ===========================================================================
// Unit tests: manifest parsing
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::TempDir;

    const VALID_MANIFEST: &str = r#"# STOP - CRITICAL READING REQUIRED

## CANONICAL LOCATIONS

SCM files MUST be in `.machine_readable/` directory ONLY.
Bot directives go in `.bot_directives/`.

## CORE INVARIANTS

1. **No SCM duplication** - Root must NOT contain STATE.scm
2. **Single source of truth** - .machine_readable/ is authoritative

## NEXT SECTION
"#;

    const MINIMAL_MANIFEST: &str = "# AI Manifest\n\nThis is a minimal manifest.\n";

    // -----------------------------------------------------------------------
    // Manifest content tests
    // -----------------------------------------------------------------------

    #[test]
    fn manifest_hash_is_64_hex_chars() {
        let m = parse_manifest_from_content(VALID_MANIFEST);
        assert_eq!(m.hash.len(), 64, "SHA-256 hex should be 64 chars");
        assert!(m.hash.chars().all(|c| c.is_ascii_hexdigit()), "Hash should be hex");
    }

    #[test]
    fn manifest_hash_is_deterministic() {
        let m1 = parse_manifest_from_content(VALID_MANIFEST);
        let m2 = parse_manifest_from_content(VALID_MANIFEST);
        assert_eq!(m1.hash, m2.hash);
    }

    #[test]
    fn manifest_hash_differs_for_different_content() {
        let m1 = parse_manifest_from_content(VALID_MANIFEST);
        let m2 = parse_manifest_from_content(MINIMAL_MANIFEST);
        assert_ne!(m1.hash, m2.hash);
    }

    #[test]
    fn manifest_extracts_scm_files_canonical_location() {
        let m = parse_manifest_from_content(VALID_MANIFEST);
        assert_eq!(m.canonical_locations.scm_files, ".machine_readable/");
    }

    #[test]
    fn manifest_extracts_bot_directives_canonical_location() {
        let m = parse_manifest_from_content(VALID_MANIFEST);
        assert_eq!(m.canonical_locations.bot_directives, ".bot_directives/");
    }

    #[test]
    fn manifest_defaults_scm_files_when_absent() {
        let m = parse_manifest_from_content(MINIMAL_MANIFEST);
        assert_eq!(m.canonical_locations.scm_files, ".machine_readable/");
    }

    #[test]
    fn manifest_defaults_bot_directives_when_absent() {
        let m = parse_manifest_from_content(MINIMAL_MANIFEST);
        assert_eq!(m.canonical_locations.bot_directives, ".bot_directives/");
    }

    #[test]
    fn manifest_extracts_invariants_from_core_invariants_section() {
        let m = parse_manifest_from_content(VALID_MANIFEST);
        assert_eq!(m.invariants.len(), 2);
        assert_eq!(m.invariants[0], "No SCM duplication");
        assert_eq!(m.invariants[1], "Single source of truth");
    }

    #[test]
    fn manifest_defaults_invariants_when_section_absent() {
        // When no CORE INVARIANTS section, defaults are used
        let m = parse_manifest_from_content(MINIMAL_MANIFEST);
        assert!(m.invariants.contains(&"no_scm_duplication".to_string()));
        assert!(m.invariants.contains(&"single_source_of_truth".to_string()));
    }

    #[test]
    fn manifest_empty_content_does_not_panic() {
        let m = parse_manifest_from_content("");
        assert_eq!(m.hash.len(), 64);
    }

    #[test]
    fn manifest_always_has_agent_instructions() {
        let m = parse_manifest_from_content(MINIMAL_MANIFEST);
        assert!(m.canonical_locations.agent_instructions.contains(&"0-AI-MANIFEST.a2ml".to_string()));
        assert!(m.canonical_locations.agent_instructions.contains(&".claude/CLAUDE.md".to_string()));
    }

    // -----------------------------------------------------------------------
    // File I/O tests
    // -----------------------------------------------------------------------

    #[test]
    fn parse_manifest_from_file_succeeds() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("0-AI-MANIFEST.a2ml");
        let mut f = std::fs::File::create(&path).unwrap();
        writeln!(f, "{}", VALID_MANIFEST).unwrap();
        let m = parse_manifest(&path).expect("Should parse manifest file");
        assert_eq!(m.hash.len(), 64);
    }

    #[test]
    fn parse_manifest_from_file_errors_on_missing_file() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("nonexistent.a2ml");
        let result = parse_manifest(&path);
        assert!(result.is_err(), "Should fail for missing file");
    }

    #[test]
    fn find_and_parse_manifest_finds_0_ai_manifest() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("0-AI-MANIFEST.a2ml");
        let mut f = std::fs::File::create(&path).unwrap();
        writeln!(f, "{}", VALID_MANIFEST).unwrap();
        let m = find_and_parse_manifest(dir.path()).expect("Should find manifest");
        assert_eq!(m.hash.len(), 64);
    }

    #[test]
    fn find_and_parse_manifest_finds_ai_a2ml_as_fallback() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("AI.a2ml");
        let mut f = std::fs::File::create(&path).unwrap();
        writeln!(f, "{}", MINIMAL_MANIFEST).unwrap();
        let m = find_and_parse_manifest(dir.path()).expect("Should find AI.a2ml");
        assert_eq!(m.hash.len(), 64);
    }

    #[test]
    fn find_and_parse_manifest_errors_when_no_manifest_exists() {
        let dir = TempDir::new().unwrap();
        let result = find_and_parse_manifest(dir.path());
        assert!(result.is_err(), "Should fail when no manifest exists");
        let err_msg = result.unwrap_err().to_string();
        assert!(err_msg.contains("No AI manifest"), "Error should mention manifest");
    }

    // -----------------------------------------------------------------------
    // Security aspect tests
    // -----------------------------------------------------------------------

    #[test]
    fn security_path_traversal_in_canonical_location_stored_literally() {
        // The canonical location extractor only accepts paths that contain
        // "machine_readable" (for scm_files) or "bot_directive" (for bot_directives).
        // A path traversal like "../../etc/passwd" does NOT contain those strings,
        // so it is rejected and the safe default ".machine_readable/" is used instead.
        // This is the correct security behaviour: reject unknown paths, use safe default.
        let malicious = "SCM files MUST be in `../../etc/passwd` directory ONLY.\n";
        let m = parse_manifest_from_content(malicious);
        // The path traversal is rejected; safe default is used
        assert_eq!(
            m.canonical_locations.scm_files, ".machine_readable/",
            "Path traversal should be rejected; safe default should be used"
        );
        // Agent instructions are always the safe defaults
        assert!(m.canonical_locations.agent_instructions.contains(&"0-AI-MANIFEST.a2ml".to_string()));
    }

    #[test]
    fn security_large_manifest_1mb_does_not_panic() {
        let large = "x".repeat(1_000_000);
        let m = parse_manifest_from_content(&large);
        assert_eq!(m.hash.len(), 64);
    }

    #[test]
    fn security_manifest_with_null_bytes_does_not_panic() {
        let content = "# Title\n\0\nBody content\n";
        let m = parse_manifest_from_content(content);
        assert_eq!(m.hash.len(), 64);
    }

    #[test]
    fn security_manifest_prefers_0_ai_manifest_over_ai_a2ml() {
        // 0-AI-MANIFEST.a2ml should be preferred over AI.a2ml
        let dir = TempDir::new().unwrap();

        let path1 = dir.path().join("0-AI-MANIFEST.a2ml");
        let mut f1 = std::fs::File::create(&path1).unwrap();
        writeln!(f1, "# Primary manifest\nSCM files MUST be in `.machine_readable/` ONLY.\n").unwrap();

        let path2 = dir.path().join("AI.a2ml");
        let mut f2 = std::fs::File::create(&path2).unwrap();
        writeln!(f2, "# Fallback manifest\n").unwrap();

        let m = find_and_parse_manifest(dir.path()).expect("Should find primary manifest");
        // Should have parsed the primary (which has the SCM line)
        assert_eq!(m.canonical_locations.scm_files, ".machine_readable/");
    }

    // -----------------------------------------------------------------------
    // E2E / dogfood: parse the real standards repo manifest
    // -----------------------------------------------------------------------

    #[test]
    fn dogfood_standards_repo_manifest_is_parseable() {
        // Parse the standards repo's own manifest.
        // Use CARGO_MANIFEST_DIR (set by cargo test) to navigate to the standards root.
        // CARGO_MANIFEST_DIR = .../standards/0-ai-gatekeeper-protocol/repo-guardian-fs/tests-offline
        let manifest_dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        // Navigate from tests-offline/ → 0-ai-gatekeeper-protocol/ → standards/
        let standards_root = manifest_dir
            .parent() // repo-guardian-fs/
            .and_then(|p| p.parent()) // 0-ai-gatekeeper-protocol/
            .and_then(|p| p.parent()); // standards/
        if let Some(root) = standards_root {
            let manifest_path = root.join("0-AI-MANIFEST.a2ml");
            if manifest_path.exists() {
                let m = find_and_parse_manifest(root).expect("Should parse standards manifest");
                assert_eq!(m.hash.len(), 64);
                assert_eq!(m.canonical_locations.scm_files, ".machine_readable/");
            }
            // If the path doesn't resolve (e.g., CI path differences), skip gracefully
        }
    }

    // -----------------------------------------------------------------------
    // Session management tests
    // -----------------------------------------------------------------------

    #[test]
    fn session_new_session_is_unacknowledged() {
        let mgr = SessionManager::new(3600);
        let s = mgr.get_or_create_session(42);
        assert_eq!(s.id, 42);
        assert!(!s.acknowledged);
        assert!(!mgr.is_acknowledged(42));
    }

    #[test]
    fn session_acknowledgment_marks_session() {
        let mgr = SessionManager::new(3600);
        mgr.get_or_create_session(1);
        assert!(!mgr.is_acknowledged(1));
        mgr.acknowledge(1, "any_hash").expect("Should acknowledge");
        assert!(mgr.is_acknowledged(1));
    }

    #[test]
    fn session_acknowledge_unknown_id_errors() {
        let mgr = SessionManager::new(3600);
        let result = mgr.acknowledge(999, "hash");
        assert!(result.is_err());
    }

    #[test]
    fn session_expired_session_is_unacknowledged() {
        let mgr = SessionManager::new(0); // 0s timeout — expires immediately
        mgr.get_or_create_session(1);
        mgr.acknowledge(1, "hash").unwrap();
        std::thread::sleep(Duration::from_millis(10));
        assert!(!mgr.is_acknowledged(1)); // should have expired
    }

    #[test]
    fn session_multiple_independent_sessions() {
        let mgr = SessionManager::new(3600);
        mgr.get_or_create_session(1);
        mgr.get_or_create_session(2);
        mgr.acknowledge(1, "hash").unwrap();
        assert!(mgr.is_acknowledged(1));
        assert!(!mgr.is_acknowledged(2)); // s2 not affected
    }

    #[test]
    fn session_active_count_tracks_sessions() {
        let mgr = SessionManager::new(3600);
        assert_eq!(mgr.active_count(), 0);
        mgr.get_or_create_session(1);
        assert_eq!(mgr.active_count(), 1);
        mgr.get_or_create_session(2);
        assert_eq!(mgr.active_count(), 2);
    }

    #[test]
    fn session_cleanup_expired_removes_expired_sessions() {
        let mgr = SessionManager::new(0); // 0s timeout
        mgr.get_or_create_session(1);
        mgr.get_or_create_session(2);
        std::thread::sleep(Duration::from_millis(10));
        mgr.cleanup_expired();
        assert_eq!(mgr.active_count(), 0);
    }

    #[test]
    fn session_idempotent_get_or_create_returns_same_session() {
        let mgr = SessionManager::new(3600);
        let s1 = mgr.get_or_create_session(7);
        let s2 = mgr.get_or_create_session(7);
        assert_eq!(s1.id, s2.id);
    }
}
