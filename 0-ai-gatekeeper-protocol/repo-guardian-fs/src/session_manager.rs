// SPDX-License-Identifier: PMPL-1.0-or-later

//! Session management for AI agent access control

use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use std::time::{Duration, Instant};

/// Session identifier (process ID or similar)
pub type SessionId = u32;

/// Session state for an AI agent/process
#[derive(Debug, Clone)]
pub struct Session {
    /// Unique session ID (typically process ID)
    pub id: SessionId,
    /// Whether the agent has acknowledged the manifest
    pub acknowledged: bool,
    /// Timestamp when session was created
    pub created_at: Instant,
    /// Timestamp of last activity
    pub last_activity: Instant,
}

/// Manages sessions and manifest acknowledgments
#[derive(Clone)]
pub struct SessionManager {
    sessions: Arc<RwLock<HashMap<SessionId, Session>>>,
    timeout: Duration,
}

impl SessionManager {
    /// Create a new session manager
    pub fn new(timeout_secs: u64) -> Self {
        Self {
            sessions: Arc::new(RwLock::new(HashMap::new())),
            timeout: Duration::from_secs(timeout_secs),
        }
    }

    /// Get or create a session for the given ID
    pub fn get_or_create_session(&self, session_id: SessionId) -> Session {
        let mut sessions = self.sessions.write().unwrap();

        // Check if session exists and is not expired
        if let Some(session) = sessions.get_mut(&session_id) {
            if session.last_activity.elapsed() < self.timeout {
                session.last_activity = Instant::now();
                return session.clone();
            }
            // Session expired, will create new one
        }

        // Create new session
        let session = Session {
            id: session_id,
            acknowledged: false,
            created_at: Instant::now(),
            last_activity: Instant::now(),
        };

        sessions.insert(session_id, session.clone());
        session
    }

    /// Check if a session has acknowledged the manifest
    pub fn is_acknowledged(&self, session_id: SessionId) -> bool {
        let sessions = self.sessions.read().unwrap();
        sessions
            .get(&session_id)
            .map(|s| s.acknowledged && s.last_activity.elapsed() < self.timeout)
            .unwrap_or(false)
    }

    /// Acknowledge manifest for a session
    pub fn acknowledge(&self, session_id: SessionId, manifest_hash: &str) -> Result<(), String> {
        let mut sessions = self.sessions.write().unwrap();

        if let Some(session) = sessions.get_mut(&session_id) {
            // In a real implementation, would verify manifest_hash matches
            // For now, just mark as acknowledged
            session.acknowledged = true;
            session.last_activity = Instant::now();
            Ok(())
        } else {
            Err(format!("Session {} not found", session_id))
        }
    }

    /// Clean up expired sessions
    pub fn cleanup_expired(&self) {
        let mut sessions = self.sessions.write().unwrap();
        sessions.retain(|_, session| session.last_activity.elapsed() < self.timeout);
    }

    /// Get number of active sessions
    pub fn active_count(&self) -> usize {
        let sessions = self.sessions.read().unwrap();
        sessions
            .values()
            .filter(|s| s.last_activity.elapsed() < self.timeout)
            .count()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_session_creation() {
        let manager = SessionManager::new(3600);
        let session = manager.get_or_create_session(1234);

        assert_eq!(session.id, 1234);
        assert!(!session.acknowledged);
    }

    #[test]
    fn test_acknowledgment() {
        let manager = SessionManager::new(3600);
        manager.get_or_create_session(1234);

        assert!(!manager.is_acknowledged(1234));

        manager
            .acknowledge(1234, "test_hash")
            .expect("Failed to acknowledge");

        assert!(manager.is_acknowledged(1234));
    }

    #[test]
    fn test_session_timeout() {
        let manager = SessionManager::new(0); // 0 second timeout
        manager.get_or_create_session(1234);
        manager
            .acknowledge(1234, "test_hash")
            .expect("Failed to acknowledge");

        std::thread::sleep(Duration::from_millis(10));

        // Session should be expired
        assert!(!manager.is_acknowledged(1234));
    }
}
