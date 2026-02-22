# Reversibility

**SPDX-License-Identifier**: CC-BY-SA-4.0
**SPDX-FileCopyrightText**: 2025 Jonathan D.A. Jewell

## Overview

**Reversibility** is a core architectural principle: Every operation should be undoable. This document explains how reversibility is implemented in the Consent-Aware HTTP Standards project.

## Philosophy

> "Without the ability to undo, experimentation becomes risk."

Reversibility enables:
- **Safe experimentation** - Try changes without fear
- **Learning** - Mistakes become learning opportunities
- **Collaboration** - Contributors can explore boldly
- **Trust** - Lower stakes increase participation

## Implementation

### Git Version Control

**All changes are reversible via Git:**

```bash
# Undo last commit (keep changes)
git reset --soft HEAD~1

# Undo last commit (discard changes)
git reset --hard HEAD~1

# Revert a specific commit
git revert <commit-hash>

# Restore a file to previous version
git checkout HEAD~1 -- path/to/file

# View history and restore
git log --oneline
git checkout <commit-hash> -- path/to/file
```

### Branch Protection

**Main branch is protected:**
- All changes via Pull Requests
- Review required before merge
- Failed experiments stay in feature branches
- Easy to abandon without consequence

### Build System Reversibility

**Justfile recipes are non-destructive:**

```bash
# Build artifacts can be cleaned
just clean

# Validation doesn't modify files
just validate

# Tests don't change state
just test
```

### Documentation Reversibility

**Markdown/AsciiDoc is plain text:**
- Full Git history
- Diffs show exactly what changed
- Easy to revert specific sections
- No binary lock-in

### Internet-Draft Versioning

**IETF drafts are versioned:**
- `-00`, `-01`, `-02` versions
- Previous versions preserved
- Can reference earlier designs
- Mistakes documented, not hidden

### Reference Implementation Safety

**Code includes rollback patterns:**

```javascript
// Middleware fails open (safe default)
try {
  enforceAIBDP(req);
} catch (error) {
  console.error('AIBDP error:', error);
  next(); // Allow request through on error
}
```

```python
# Manifest loading with fallback
manifest = load_manifest()
if not manifest:
    return None  # No enforcement if manifest missing
```

### AIBDP Manifest Updates

**Manifests can be updated anytime:**
- Change `status` from `refused` to `allowed`
- Add/remove conditions
- Update `expires` to force re-fetch
- AI systems pick up changes on next check

**Example reversal:**

```json
// Before: Too restrictive
{
  "training": { "status": "refused" }
}

// After: Changed mind
{
  "training": {
    "status": "conditional",
    "conditions": ["Attribution required"]
  }
}
```

### Configuration Reversibility

**Server configs are declarative:**

```nginx
# nginx: Comment out to disable
# return 430;

# Apache: Remove .htaccess rule
# RewriteRule ^ - [R=430,L]

# Caddy: Remove handle block
# handle @ai_bots { respond 430 }
```

### Community Governance Reversibility

**Governance decisions can be revisited:**
- Governance amendments process (GOVERNANCE.adoc)
- Community appeals for major decisions
- Fork rights preserved (ultimate reversibility)

## What's NOT Reversible

### Immutable by Design

1. **Git commit history**
   - Commits are permanent (can hide, but not delete from history)
   - This is a feature: Accountability and audit trail

2. **IETF submission**
   - Once submitted, Internet-Drafts are archived
   - Can withdraw or supersede, but not erase

3. **Public communications**
   - Blog posts, announcements, public discussions
   - Can retract or update, but internet never forgets

4. **Data already collected by AI systems**
   - If AI systems already trained on your content before you added AIBDP
   - Reversibility is *prospective*, not *retroactive*

### Acceptable Irreversibility

These are irreversible for good reason:

- **Security fixes**: Immediate merge, post-hoc notification
- **Code of Conduct enforcement**: Documented outcomes (transparency)
- **Published standards**: RFC permanence (stability)

## Emergency Reversibility

### Project-Level Undo

If the entire project needs to be reversed:

1. **Archive repository** (don't delete - preserve history)
2. **Withdraw Internet-Drafts** formally
3. **Update website** with clear status
4. **Notify community** via all channels
5. **Preserve data** (forks, downloads)

### Personal Reversibility

If you adopted AIBDP and want to undo:

```bash
# Remove AIBDP manifest
rm .well-known/aibdp.json

# Remove HTTP 430 enforcement
# (reverse server config changes)

# Announce change
# (optional but recommended for AI systems)
```

## Reversibility Best Practices

### For Contributors

1. **Use feature branches** - Keep main clean
2. **Commit frequently** - Small, reversible steps
3. **Write clear commit messages** - Makes reverting easier
4. **Test before merge** - Less likely to need reversal

### For Adopters

1. **Start permissive** - Easier to restrict than to loosen
2. **Document rationale** - Future you will thank you
3. **Monitor impact** - Adjust based on data
4. **Communicate changes** - Tell AI systems when policies shift

### For Maintainers

1. **Preserve history** - Don't force-push to main
2. **Document decisions** - CHANGELOG and governance
3. **Enable community input** - Catch issues before they're permanent
4. **Plan for succession** - Reversibility includes project continuity

## Philosophical Grounding

### Reversibility as Care

> "The ability to undo is an expression of care - for yourself, your collaborators, and your future self who might disagree with present-you."

### Reversibility Enables Consent

Just as AIBDP allows content creators to declare and revise boundaries, reversibility allows:
- **Contributors** to experiment without permanent commitment
- **Adopters** to try consent-aware protocols without lock-in
- **Standards** to evolve based on implementation experience

### Reversibility ≠ Instability

Reversibility doesn't mean constantly changing:
- Thoughtful decisions are stable
- Reversibility is **insurance**, not a **plan**
- Git history shows most changes stick

## Examples of Successful Reversibility

### In This Project

1. **Documentation structure** - Reorganized 3 times, Git preserved all versions
2. **License choice** - Evaluated multiple, settled on dual MIT/GPL + Palimpsest option
3. **Manifest format** - Iterated from v0.1 to v0.2 based on feedback
4. **Reference implementations** - Refactored without losing previous working versions

### In Web Standards

1. **HTTP/2 Server Push** - Removed in HTTP/3 after implementation experience
2. **TLS versions** - Deprecated older versions as weaknesses found
3. **HTML tags** - <blink> and <marquee> deprecated but documented

## Monitoring Reversibility

```bash
# Check what's changed recently
git log --oneline --since="2 weeks ago"

# See uncommitted changes
git status
git diff

# Preview impact before reverting
git diff HEAD~5..HEAD

# Dry-run of clean
just clean --dry-run  # (if supported)
```

## Reversibility Checklist

**Before Major Changes:**

- [ ] Create feature branch
- [ ] Commit current state
- [ ] Document rationale in commit message
- [ ] Test thoroughly
- [ ] Get review/feedback
- [ ] Merge with clear description

**If Something Goes Wrong:**

- [ ] Assess impact (how broken is it?)
- [ ] Check Git history (`git log`)
- [ ] Identify good commit (`git log --oneline`)
- [ ] Revert or reset (`git revert` or `git reset`)
- [ ] Test that reversal worked
- [ ] Document what happened (learning)

**Periodic Reviews:**

- [ ] Quarterly: Review major decisions
- [ ] Annual: Assess if governance still serves project
- [ ] Per release: Evaluate if changes worked as intended

## Resources

- **Git documentation**: https://git-scm.com/doc
- **Reversibility in software design**: https://martinfowler.com/
- **IETF process for withdrawing drafts**: https://www.ietf.org/

## Conclusion

Reversibility is not just a technical feature - it's an ethical stance.

> "The right to undo preserves the right to try."

This project models the consent-aware principles it standardizes:
- **Declare boundaries** (AIBDP)
- **Enforce procedurally** (HTTP 430)
- **Allow revision** (Reversibility)
- **Respect autonomy** (Fork rights, community governance)

---

**Questions about reversibility?**
Open a GitHub Discussion or email jonathan@metadatastician.art

**Version**: 1.0
**Last Updated**: 2025-07-20
**Next Review**: 2026-07-20
