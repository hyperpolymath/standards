// SPDX-License-Identifier: PMPL-1.0-or-later

//! Guardian FUSE filesystem implementation

use anyhow::Result;
use fuse3::path::prelude::*;
use fuse3::{Errno, Result as FuseResult};
use std::path::PathBuf;
use std::time::SystemTime;
use tracing::{debug, warn};

use crate::manifest::{find_and_parse_manifest, Manifest};
use crate::session_manager::{SessionId, SessionManager};

/// Guardian FUSE filesystem
pub struct GuardianFs {
    /// Source directory (actual repo location)
    source: PathBuf,
    /// Session manager for tracking acknowledgments
    session_manager: SessionManager,
    /// Strict mode - block all operations until acknowledgment
    strict_mode: bool,
    /// Parsed manifest (if available)
    manifest: Option<Manifest>,
}

impl GuardianFs {
    /// Create a new guardian filesystem
    pub fn new(source: PathBuf, strict_mode: bool, session_timeout: u64) -> Result<Self> {
        // Try to parse manifest from source
        let manifest = find_and_parse_manifest(&source).ok();

        if manifest.is_none() {
            warn!("No manifest found in {:?}, enforcement disabled", source);
        }

        Ok(Self {
            source,
            session_manager: SessionManager::new(session_timeout),
            strict_mode,
            manifest,
        })
    }

    /// Check if session has access (acknowledged manifest or non-strict mode)
    fn check_access(&self, session_id: SessionId) -> FuseResult<()> {
        // If no manifest, allow access
        if self.manifest.is_none() {
            return Ok(());
        }

        // If not strict mode, allow access
        if !self.strict_mode {
            return Ok(());
        }

        // Check if session has acknowledged
        if self.session_manager.is_acknowledged(session_id) {
            Ok(())
        } else {
            debug!("Access denied for session {}: manifest not acknowledged", session_id);
            Err(Errno::from(libc::EACCES))
        }
    }

    /// Translate virtual path to real path
    fn real_path(&self, path: &Path) -> PathBuf {
        self.source.join(path.strip_prefix("/").unwrap_or(path))
    }
}

impl PathFilesystem for GuardianFs {
    type DirEntryStream = ReaddirStream;

    async fn init(&self, _req: Request) -> FuseResult<()> {
        debug!("Filesystem initialized");
        Ok(())
    }

    async fn destroy(&self, _req: Request) {
        debug!("Filesystem destroyed");
    }

    async fn lookup(&self, req: Request, parent: &Path, name: &OsStr) -> FuseResult<ReplyEntry> {
        // Check access for this session
        self.check_access(req.uid)?;

        let path = parent.join(name);
        let real_path = self.real_path(&path);

        match tokio::fs::metadata(&real_path).await {
            Ok(metadata) => {
                let attr = FileAttr {
                    ino: 1, // Would use real inode in production
                    size: metadata.len(),
                    blocks: metadata.len() / 512,
                    atime: metadata.accessed().unwrap_or_else(|_| SystemTime::now()),
                    mtime: metadata.modified().unwrap_or_else(|_| SystemTime::now()),
                    ctime: metadata.modified().unwrap_or_else(|_| SystemTime::now()),
                    kind: if metadata.is_dir() {
                        FileType::Directory
                    } else {
                        FileType::RegularFile
                    },
                    perm: if metadata.is_dir() { 0o755 } else { 0o644 },
                    nlink: 1,
                    uid: req.uid,
                    gid: req.gid,
                    rdev: 0,
                    blksize: 4096,
                };

                Ok(ReplyEntry {
                    ttl: std::time::Duration::from_secs(1),
                    attr,
                })
            }
            Err(e) => Err(Errno::from(e.raw_os_error().unwrap_or(libc::ENOENT))),
        }
    }

    async fn getattr(&self, req: Request, path: &Path, _fh: Option<u64>, _flags: u32) -> FuseResult<ReplyAttr> {
        // Check access for this session
        self.check_access(req.uid)?;

        let real_path = self.real_path(path);

        match tokio::fs::metadata(&real_path).await {
            Ok(metadata) => {
                let attr = FileAttr {
                    ino: 1,
                    size: metadata.len(),
                    blocks: metadata.len() / 512,
                    atime: metadata.accessed().unwrap_or_else(|_| SystemTime::now()),
                    mtime: metadata.modified().unwrap_or_else(|_| SystemTime::now()),
                    ctime: metadata.modified().unwrap_or_else(|_| SystemTime::now()),
                    kind: if metadata.is_dir() {
                        FileType::Directory
                    } else {
                        FileType::RegularFile
                    },
                    perm: if metadata.is_dir() { 0o755 } else { 0o644 },
                    nlink: 1,
                    uid: req.uid,
                    gid: req.gid,
                    rdev: 0,
                    blksize: 4096,
                };

                Ok(ReplyAttr {
                    ttl: std::time::Duration::from_secs(1),
                    attr,
                })
            }
            Err(e) => Err(Errno::from(e.raw_os_error().unwrap_or(libc::ENOENT))),
        }
    }

    async fn read(&self, req: Request, path: &Path, _fh: u64, offset: u64, size: u32) -> FuseResult<ReplyData> {
        // Check access for this session
        self.check_access(req.uid)?;

        let real_path = self.real_path(path);

        match tokio::fs::read(&real_path).await {
            Ok(data) => {
                let start = offset as usize;
                let end = std::cmp::min(start + size as usize, data.len());
                Ok(ReplyData {
                    data: data[start..end].to_vec().into(),
                })
            }
            Err(e) => Err(Errno::from(e.raw_os_error().unwrap_or(libc::EIO))),
        }
    }

    async fn readdir(&self, req: Request, path: &Path, _fh: u64, offset: i64) -> FuseResult<ReplyDirectory<Self::DirEntryStream>> {
        // Check access for this session
        self.check_access(req.uid)?;

        let real_path = self.real_path(path);

        let mut entries = Vec::new();

        // Add . and ..
        if offset == 0 {
            entries.push(DirectoryEntry {
                inode: 1,
                kind: FileType::Directory,
                name: OsString::from("."),
                offset: 1,
            });
        }

        if offset <= 1 {
            entries.push(DirectoryEntry {
                inode: 1,
                kind: FileType::Directory,
                name: OsString::from(".."),
                offset: 2,
            });
        }

        // Read directory entries
        let mut dir_offset = 2;
        if let Ok(mut read_dir) = tokio::fs::read_dir(&real_path).await {
            while let Ok(Some(entry)) = read_dir.next_entry().await {
                dir_offset += 1;

                if dir_offset <= offset {
                    continue;
                }

                let file_type = if entry.file_type().await.map(|t| t.is_dir()).unwrap_or(false) {
                    FileType::Directory
                } else {
                    FileType::RegularFile
                };

                entries.push(DirectoryEntry {
                    inode: 1,
                    kind: file_type,
                    name: entry.file_name(),
                    offset: dir_offset,
                });
            }
        }

        Ok(ReplyDirectory {
            entries: ReaddirStream::new(entries.into_iter()),
        })
    }
}

/// Directory entry stream
pub struct ReaddirStream {
    entries: std::vec::IntoIter<DirectoryEntry>,
}

impl ReaddirStream {
    fn new(iter: std::vec::IntoIter<DirectoryEntry>) -> Self {
        Self { entries: iter }
    }
}

impl futures::Stream for ReaddirStream {
    type Item = FuseResult<DirectoryEntry>;

    fn poll_next(
        mut self: std::pin::Pin<&mut Self>,
        _cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        std::task::Poll::Ready(self.entries.next().map(Ok))
    }
}
