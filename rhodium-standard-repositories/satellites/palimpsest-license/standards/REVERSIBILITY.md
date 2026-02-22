# Reversibility - Palimpsest License Project

**Version:** 0.4.0
**SPDX-License-Identifier: MPL-2.0-or-later
**Last Updated:** 2025-11-28

---

## Overview

This document defines all **reversible operations** in the Palimpsest License project. Following the principle of **safe experimentation**, every operation should be reversible, with clear undo mechanisms and confirmation for risky actions.

**Core Principle:**
_"No destructive defaults. Every action should be undoable, inspectable, and transparent."_

---

## Table of Contents

- [Philosophy](#philosophy)
- [Git Operations](#git-operations)
- [Build System](#build-system)
- [File Operations](#file-operations)
- [Dependency Management](#dependency-management)
- [Configuration Changes](#configuration-changes)
- [License Modifications](#license-modifications)
- [Data Recovery](#data-recovery)
- [Emergency Procedures](#emergency-procedures)

---

## Philosophy

### Reversibility Principles

1. **No Silent Destruction:** Operations that delete or modify data must warn first
2. **Undo Mechanisms:** Every action has a documented undo path
3. **Safe Defaults:** Default behaviour is the safest option
4. **Confirmation Gates:** Risky operations require explicit confirmation
5. **Backup Before Modify:** Automated backups for critical changes
6. **Audit Trail:** All operations logged for inspection
7. **Graceful Degradation:** Partial failures don't corrupt state
8. **Documentation:** Every reversal procedure is documented

### Operations Classification

| Type | Reversibility | Confirmation | Example |
|------|---------------|--------------|---------|
| **Safe** | Automatic | None | `git status`, `just validate` |
| **Modifying** | Manual undo | Warning | `just format`, `npm install` |
| **Risky** | Backup required | Explicit confirmation | `just clean-all`, `git reset --hard` |
| **Destructive** | Not reversible | Multiple confirmations | `git push --force`, `rm -rf` |

---

## Git Operations

### Safe Operations (Always Reversible)

#### Viewing History
```bash
# View commit history
git log
git log --oneline --graph

# View changes
git diff
git diff --staged

# View specific commit
git show <commit-hash>

# REVERSIBILITY: Read-only, no undo needed
```

#### Inspecting State
```bash
# Check status
git status

# List branches
git branch -a

# Show remote info
git remote -v

# REVERSIBILITY: Read-only, no undo needed
```

### Modifying Operations (Reversible with Manual Undo)

#### Staging Changes
```bash
# Stage files
git add <file>

# UNDO:
git reset <file>           # Unstage specific file
git reset                  # Unstage all files
```

#### Committing
```bash
# Create commit
git commit -m "Message"

# UNDO (if not pushed):
git reset --soft HEAD~1    # Keep changes staged
git reset --mixed HEAD~1   # Keep changes unstaged (default)
git reset --hard HEAD~1    # Discard changes (RISKY)

# UNDO (if pushed):
git revert HEAD            # Create new commit that undoes changes
```

#### Branching
```bash
# Create branch
git branch feature-name

# UNDO:
git branch -d feature-name    # Delete branch (safe, prevents unmerged)
git branch -D feature-name    # Force delete (RISKY)

# Switch branch
git checkout branch-name

# UNDO:
git checkout -              # Return to previous branch
```

#### Merging
```bash
# Merge branch
git merge feature-branch

# UNDO (if merge not committed):
git merge --abort

# UNDO (if merge committed):
git reset --hard ORIG_HEAD  # RISKY: Loses merge commit
# OR
git revert -m 1 HEAD        # SAFE: Creates revert commit
```

### Risky Operations (Require Backup)

#### Rebasing
```bash
# Rebase branch
git rebase main

# UNDO (during rebase):
git rebase --abort

# UNDO (after rebase):
git reflog                  # Find previous HEAD
git reset --hard HEAD@{N}   # Restore to before rebase
```

#### Amending Commits
```bash
# Amend last commit
git commit --amend

# UNDO:
git reflog
git reset --soft HEAD@{1}   # Restore previous commit
```

#### Stashing
```bash
# Stash changes
git stash

# UNDO:
git stash pop              # Restore and remove from stash
git stash apply            # Restore and keep in stash

# List stashes
git stash list

# Drop specific stash
git stash drop stash@{N}

# RECOVERY:
# Stashes are kept in reflog for ~90 days
git fsck --unreachable | grep commit
```

### Destructive Operations (Use with Extreme Caution)

#### Hard Reset
```bash
# DESTRUCTIVE: Discards all uncommitted changes
git reset --hard HEAD

# PREVENTION:
git stash              # Save changes first
git branch backup      # Create backup branch

# RECOVERY (limited):
git reflog            # Find lost commits
git fsck --lost-found # Find dangling objects
```

#### Force Push
```bash
# DESTRUCTIVE: Rewrites remote history
git push --force

# SAFER ALTERNATIVE:
git push --force-with-lease  # Fails if remote changed

# PREVENTION:
git branch backup-before-force  # Create backup branch

# RECOVERY:
# If you have access to previous remote state:
git reflog origin/main
git reset --hard origin/main@{N}
```

#### Branch Deletion
```bash
# Delete local branch
git branch -D feature-name

# RECOVERY:
git reflog
git checkout -b feature-name <commit-hash>

# Delete remote branch
git push origin --delete feature-name

# RECOVERY (if done recently):
# Contact repository maintainers
# Check GitHub/GitLab trash/recovery features
```

---

## Build System

### Safe Operations

#### Validation
```bash
# Run validation checks
just validate

# REVERSIBILITY: Read-only inspection, no undo needed
```

#### Testing
```bash
# Run tests
just test

# REVERSIBILITY: No changes to source, no undo needed
```

### Modifying Operations

#### Building
```bash
# Build all components
just build

# UNDO:
just clean           # Remove build artifacts

# FILES MODIFIED:
# - styles/css/*.css
# - dist-newstyle/
# - rescript/lib/
```

#### Formatting
```bash
# Auto-format files
just format

# PREVENTION:
git status           # Check which files will be formatted
git stash           # Backup current state

# UNDO:
git diff            # Review changes
git checkout -- .   # Discard formatting changes
git stash pop       # Restore from backup
```

### Risky Operations

#### Clean Build Artifacts
```bash
# Remove build artifacts
just clean

# UNDO: Not possible, rebuild required
just build

# FILES REMOVED:
# - styles/css/*.css
# - styles/css/*.css.map
# - rescript/lib/
# - TOOLS/validation/haskell/dist-newstyle/
```

#### Deep Clean
```bash
# DESTRUCTIVE: Removes node_modules and build artifacts
just clean-all

# PREVENTION:
# No prevention - this is intentionally destructive

# UNDO: Not possible, reinstall required
just install        # Reinstall dependencies
just build          # Rebuild
```

---

## File Operations

### Safe Operations

#### Reading Files
```bash
# Read file
cat FILE
less FILE
just serve-docs    # View in browser

# REVERSIBILITY: Read-only, no undo needed
```

#### Searching
```bash
# Search content
grep -r "pattern" .
rg "pattern"

# REVERSIBILITY: Read-only, no undo needed
```

### Modifying Operations

#### Editing Files
```bash
# Edit file
vim FILE
nano FILE

# PREVENTION:
cp FILE FILE.backup     # Manual backup

# UNDO:
git checkout -- FILE    # Restore from git
mv FILE.backup FILE     # Restore from backup
```

#### Creating Files
```bash
# Create new file
touch FILE
echo "content" > FILE

# UNDO:
rm FILE                 # Delete file
git clean -f FILE       # Remove untracked file
```

#### Moving Files
```bash
# Move file
mv OLD_PATH NEW_PATH

# UNDO:
mv NEW_PATH OLD_PATH

# If under git:
git mv OLD_PATH NEW_PATH
# UNDO:
git mv NEW_PATH OLD_PATH
```

### Risky Operations

#### Deleting Files
```bash
# Delete file
rm FILE

# PREVENTION:
mv FILE ~/.trash/       # Move to trash instead

# UNDO:
git checkout -- FILE    # Restore from git (if tracked)
mv ~/.trash/FILE FILE   # Restore from trash

# RECOVERY:
# If file was committed to git:
git log --all --full-history -- FILE
git checkout <commit-hash> -- FILE
```

#### Bulk Deletion
```bash
# DESTRUCTIVE: Delete multiple files
rm -rf DIRECTORY/

# PREVENTION:
tar -czf backup-$(date +%Y%m%d).tar.gz DIRECTORY/  # Backup first
git status                                         # Check what's tracked

# UNDO:
git checkout -- DIRECTORY/    # Restore from git (if tracked)
tar -xzf backup-*.tar.gz      # Restore from backup

# RECOVERY:
# Limited - depends on backups and git history
```

---

## Dependency Management

### Safe Operations

#### Inspecting Dependencies
```bash
# List dependencies
npm list
cabal list

# Check for updates
npm outdated
cabal outdated

# REVERSIBILITY: Read-only, no undo needed
```

#### Auditing
```bash
# Security audit
npm audit
just security-audit

# REVERSIBILITY: Read-only, no undo needed
```

### Modifying Operations

#### Installing Dependencies
```bash
# Install dependencies
npm install
cabal install

# PREVENTION:
cp package-lock.json package-lock.json.backup
cp cabal.project.freeze cabal.project.freeze.backup

# UNDO:
rm -rf node_modules/
mv package-lock.json.backup package-lock.json
npm install                # Restore from lock file
```

#### Updating Dependencies
```bash
# Update dependencies
npm update
cabal update

# PREVENTION:
git commit package-lock.json -m "Before dependency update"

# UNDO:
git checkout -- package-lock.json
rm -rf node_modules/
npm install
```

### Risky Operations

#### Removing Dependencies
```bash
# Remove package
npm uninstall <package>

# UNDO:
# Check git history for version
git log -p package.json
npm install <package>@<version>
```

#### Breaking Lock Files
```bash
# RISKY: Regenerate lock file
rm package-lock.json
npm install

# PREVENTION:
git commit package-lock.json -m "Before lock file regeneration"

# UNDO:
git checkout -- package-lock.json
rm -rf node_modules/
npm install
```

---

## Configuration Changes

### Safe Operations

#### Viewing Configuration
```bash
# View git config
git config --list

# View Justfile
cat Justfile

# REVERSIBILITY: Read-only, no undo needed
```

### Modifying Operations

#### Git Configuration
```bash
# Set config
git config user.name "Name"

# UNDO:
git config --unset user.name

# Set local config
git config --local core.hooksPath .git-hooks

# UNDO:
git config --local --unset core.hooksPath
```

#### Environment Variables
```bash
# Set temporary env var
export VARIABLE=value

# UNDO:
unset VARIABLE

# Set permanent (in shell config)
echo 'export VARIABLE=value' >> ~/.bashrc

# UNDO:
# Edit ~/.bashrc and remove line
```

### Risky Operations

#### Modifying Core Config Files
```bash
# Edit Justfile, package.json, etc.
vim Justfile

# PREVENTION:
git diff Justfile          # Review changes before commit
git commit Justfile        # Commit known-good state

# UNDO:
git checkout -- Justfile
```

---

## License Modifications

### Critical: License Text Changes

License text modifications are **high-risk** operations requiring governance approval.

#### Process for License Changes
```bash
# 1. NEVER edit license directly without governance approval
# 2. Create proposal
cp LICENSES/v0.4/palimpsest-v0.4.md proposals/license-change-YYYYMMDD.md
# 3. Submit to Stewardship Council
# 4. Wait for approval (30-90 days)
# 5. If approved, update with version bump

# REVERSIBILITY:
# License changes are tracked in git history
git log LICENSES/v0.4/palimpsest-v0.4.md

# UNDO (only if not released):
git checkout HEAD~1 -- LICENSES/v0.4/palimpsest-v0.4.md

# GOVERNANCE REQUIRED:
# - Minor clarifications: 4/7 Council vote
# - New clauses: 5/7 Council vote
# - Major changes: 6/7 Council vote + 90-day review
```

### Version Management
```bash
# Create new version
cp -r LICENSES/v0.4 LICENSES/v0.5

# UNDO (if not released):
rm -rf LICENSES/v0.5
git checkout -- LICENSES/
```

---

## Data Recovery

### Git Reflog (Lost Commits)
```bash
# View reflog
git reflog

# Recover lost commit
git checkout -b recovery-branch <commit-hash>

# Reflog retention: Default 90 days
git config gc.reflogExpire 180  # Extend to 180 days
```

### Dangling Objects
```bash
# Find unreachable commits
git fsck --unreachable

# Find dangling blobs (deleted files)
git fsck --lost-found

# Recover from dangling commit
git show <dangling-commit-hash>
git checkout <dangling-commit-hash> -- path/to/file
```

### Backup Strategies

#### Automated Backups
```bash
# Create timestamped backup
tar -czf backup-$(date +%Y%m%d-%H%M%S).tar.gz \
  --exclude=node_modules \
  --exclude=dist-newstyle \
  --exclude=.git \
  .

# Restore from backup
tar -xzf backup-YYYYMMDD-HHMMSS.tar.gz
```

#### Git Backup Branch
```bash
# Create backup branch before risky operation
git branch backup-before-rebase-$(date +%Y%m%d)

# Restore from backup branch
git checkout backup-before-rebase-YYYYMMDD
git checkout -b recovery
```

---

## Emergency Procedures

### Accidental Force Push

```bash
# EMERGENCY: Undo force push (within ~24 hours)

# 1. Check reflog for previous state
git reflog origin/main

# 2. Reset to previous state
git reset --hard origin/main@{1}

# 3. Force push the recovery
git push --force-with-lease

# 4. Notify team immediately
# 5. Document incident in CHANGELOG.md
```

### Corrupted Repository

```bash
# EMERGENCY: Repository corruption

# 1. Check integrity
git fsck --full

# 2. Clone fresh copy
git clone <remote-url> repo-recovery

# 3. Copy uncommitted work
cp -r broken-repo/working-files repo-recovery/

# 4. Verify integrity
cd repo-recovery && git fsck --full
```

### Lost Work (No Git Backup)

```bash
# EMERGENCY: Recover from IDE/editor backups

# VSCode backups
~/.config/Code/Backups/

# Vim swap files
ls ~/.vim/swap/

# System trash
~/.local/share/Trash/files/

# File system snapshots (if enabled)
# ZFS: zfs list -t snapshot
# Btrfs: btrfs subvolume list /
```

---

## Reversibility Checklist

Before any risky operation, verify:

- [ ] Do I have a recent git commit?
- [ ] Do I have a backup (if not in git)?
- [ ] Can I describe the undo procedure?
- [ ] Have I tested the undo procedure?
- [ ] Is this operation governance-approved (for license changes)?
- [ ] Have I communicated with team (for shared branches)?
- [ ] Do I have time to fix if something goes wrong?

---

## Best Practices

### 1. Commit Often
```bash
# Small, frequent commits are easier to undo
git commit -m "Small incremental change"
```

### 2. Branch Liberally
```bash
# Experiment on branches, not main
git checkout -b experiment
# If it works: merge
# If it fails: delete branch
```

### 3. Use Stash for WIP
```bash
# Save work in progress before risky operations
git stash save "WIP: feature description"
```

### 4. Tag Stable States
```bash
# Tag known-good states
git tag -a stable-$(date +%Y%m%d) -m "Stable state before refactor"
```

### 5. Document Intent
```bash
# Write clear commit messages
git commit -m "Refactor: Extract validation logic (reversible via git revert)"
```

---

## Tooling Support

### Reversibility Tools

#### Git Hooks
```bash
# Pre-commit hook prevents accidental commits
.git-hooks/pre-commit

# Pre-push hook prevents destructive pushes
.git-hooks/pre-push
```

#### Just Recipes
```bash
# Safe validation (read-only)
just validate

# Safe test (read-only)
just test

# Reversible clean (removes only build artifacts)
just clean

# RISKY: Confirm before use
just clean-all
```

#### Backup Automation
```bash
# Automated backup before risky operations
alias risky='git branch backup-$(date +%Y%m%d-%H%M%S) && echo "Backup created"'
```

---

## Governance and Reversibility

### License Changes are Not Easily Reversible

Once a license version is **released** and **adopted**, reversing it is **legally complex**.

**Process:**
1. Proposals require 30-day minimum review
2. Changes require Council vote (4/7 to 6/7 depending on severity)
3. Released versions are immutable (archived in `LICENSES/vX.Y/`)
4. New versions supersede, but don't replace old versions

**Reversibility:**
- **Before release:** Full reversibility via git
- **After release:** New version required, legal consultation needed
- **Public use:** Cannot revoke, only supersede with new version

---

## Conclusion

**Reversibility is a core value** of the Palimpsest License project. Every operation should be:

1. **Documented** with clear undo procedures
2. **Tested** for reversibility
3. **Backed up** when risky
4. **Governed** when critical (license changes)

Remember: **No destructive defaults. Safe experimentation always.**

---

**Questions or Emergencies?**
- Security incidents: security@palimpsest.license
- Data recovery: info@palimpsest.license
- Governance questions: council@palimpsest.license

**Last Updated:** 2025-11-28
**Version:** 0.4.0
**License:** Palimpsest-0.4 OR MIT
