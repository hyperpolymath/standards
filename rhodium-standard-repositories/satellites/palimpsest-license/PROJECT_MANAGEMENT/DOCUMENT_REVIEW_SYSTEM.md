# Document Review System

**Purpose:** Ensure all documentation remains current, consistent, and accurate
**Last Updated:** 2025-12-08

---

## Review Philosophy

In a living license project, documentation can easily drift from reality. This system ensures:

1. **Currency:** Documents reflect current state
2. **Consistency:** Terms, versions, and concepts align across files
3. **Accuracy:** Technical details are correct
4. **Completeness:** No critical gaps

---

## Review Cadence

### Critical Documents (Monthly Review)

| Document | Purpose | Reviewer |
|----------|---------|----------|
| LICENSE text (v0.4) | Legal foundation | Legal Lead |
| GOVERNANCE.md | Decision-making | Council Chair |
| CONTRIBUTING.md | Community entry | Community Lead |
| User_Guide.md | Creator guidance | Comms Lead |

### Standard Documents (Quarterly Review)

All guides, toolkits, documentation in:
- `GUIDES_v0.4/`
- `TOOLKIT_v0.4/`
- `docs/`
- `press-lobby-kit/`

### Technical Documents (On-Change + Quarterly)

- Implementation files
- Metadata schemas
- API documentation
- Integration guides

### Archive Documents (Annual Verification)

- Ensure historical accuracy
- Verify deprecation notices current
- Check no broken references

---

## Review Checklist

When reviewing any document, verify:

### Content Currency
- [ ] Version numbers are current (v0.4 not v0.3)
- [ ] Dates are accurate
- [ ] Links work (internal and external)
- [ ] Examples still valid
- [ ] No references to deprecated/removed features

### Consistency Checks
- [ ] Terminology matches glossary
- [ ] Clause numbers align with license text
- [ ] Cross-references accurate
- [ ] Bilingual parity (EN/NL where applicable)

### Technical Accuracy
- [ ] Code examples work
- [ ] File paths exist
- [ ] Commands execute correctly
- [ ] Schema/metadata examples valid

### Style & Quality
- [ ] Follows style guide
- [ ] Accessible language
- [ ] No orphaned TODOs
- [ ] Formatting consistent

---

## Review Tracking Format

Add to end of reviewed documents:

```markdown
---

## Document Review Log

| Date | Reviewer | Status | Notes |
|------|----------|--------|-------|
| 2025-12-08 | [name/AI] | Current | Initial review |
| YYYY-MM-DD | [name] | [status] | [notes] |

**Next Review Due:** YYYY-MM-DD
```

Status values:
- **Current:** Up to date, no changes needed
- **Updated:** Changes made during review
- **Flagged:** Issues found, needs attention
- **Archived:** No longer active, historical only

---

## Automation Approach

### CI/CD Integration

Add to `.github/workflows/docs.yml`:

```yaml
# Document freshness check
document-freshness:
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v4
      with:
        fetch-depth: 0

    - name: Check document freshness
      run: |
        # Find documents not modified in 90 days
        echo "Documents over 90 days without modification:"
        find . -name "*.md" -not -path "./.git/*" \
          -not -path "./ARCHIVE/*" | while read file; do
          last_commit=$(git log -1 --format="%at" -- "$file" 2>/dev/null)
          if [ -n "$last_commit" ]; then
            days_old=$(( ($(date +%s) - last_commit) / 86400 ))
            if [ $days_old -gt 90 ]; then
              echo "$file: $days_old days since last commit"
            fi
          fi
        done

    - name: Check for missing review logs
      run: |
        echo "Documents missing review log:"
        find . -name "*.md" -not -path "./.git/*" \
          -not -path "./ARCHIVE/*" \
          -not -path "./PROJECT_MANAGEMENT/templates/*" | while read file; do
          if ! grep -q "Document Review Log" "$file" 2>/dev/null; then
            echo "$file"
          fi
        done
```

### Pre-Commit Hook

Add to `.pre-commit-config.yaml`:

```yaml
- repo: local
  hooks:
    - id: check-version-refs
      name: Check version references
      entry: ./scripts/check-version-refs.sh
      language: script
      files: '\.md$'
```

### Review Reminder Script

Create `TOOLS/scripts/review-reminder.sh`:

```bash
#!/bin/bash
# Generate review reminder report

echo "=== Document Review Reminder ==="
echo "Generated: $(date)"
echo ""

# Documents needing review (not reviewed in 90 days)
echo "## Overdue Reviews (>90 days)"
grep -r "Next Review Due:" --include="*.md" . 2>/dev/null | while read line; do
  file=$(echo "$line" | cut -d: -f1)
  due_date=$(echo "$line" | grep -oP '\d{4}-\d{2}-\d{2}')
  if [ -n "$due_date" ]; then
    due_ts=$(date -d "$due_date" +%s 2>/dev/null)
    now_ts=$(date +%s)
    if [ -n "$due_ts" ] && [ $due_ts -lt $now_ts ]; then
      echo "- $file (was due: $due_date)"
    fi
  fi
done
```

---

## Initial Audit: Files to Review

Based on git history, these files pre-date recent restructuring:

### July 2025 Original Files (Need Review)

Critical legal documents:
- `LICENSES/v0.3/palimpsest-license-v0.3.en.md`
- `LICENSES/v0.3/palimpsest-license-v0.3.nl.md`
- `LICENSE_CORE/AGREEMENTS/AGI-consent.md`

Governance:
- `GOVERNANCE.md`
- `CONTRIBUTING.md`
- `CODE_OF_CONDUCT.md`
- `CODE_OF_PRACTICE.md`
- `SECURITY.md`

Documentation:
- `docs/ethics.md`
- `docs/ethics-FAQ.md`
- `docs/jurisdiction-comparison.md`
- All files in `GUIDES_v0.4/` (may have been created for v0.4 but not reviewed)

Technical:
- `METADATA_v0.4/` contents
- `TOOLKIT_v0.4/` contents

### Files Created/Updated December 2025

These are current and don't need immediate review:
- All PROJECT_MANAGEMENT/ files (just created)
- story.scm
- PHILOSOPHY.md
- RESEARCH/ contents
- FUNDING.md

---

## Consistency Tests to Run

### 1. Version Reference Check

```bash
# Find old version references
grep -r "v0\.3" --include="*.md" . | grep -v ARCHIVE | grep -v "v0.3_ARCHIVE"
```

### 2. Broken Link Check

```bash
# Use lychee or similar
lychee --offline --include-verbatim "**/*.md"
```

### 3. Clause Reference Validation

```bash
# Extract all clause references and verify against license text
grep -oP "Clause \d+\.\d+" --include="*.md" -r . | sort | uniq -c
```

### 4. Terminology Consistency

Key terms that must be consistent:
- "Palimpsest License" (not "Palimpsest license" or "palimpsest license")
- "emotional lineage" (consistent case)
- "AI training consent" vs "AI training permission"
- "metadata preservation" (not "metadata retention")

---

## Review Priority Queue

### Priority 1: Block Other Work

1. `LICENSES/v0.4/palimpsest-v0.4.md` - STUB, needs content
2. `MAINTAINERS.md` - All placeholders

### Priority 2: Legal Foundation

3. `LICENSES/v0.3/*.md` - Verify still valid, mark as archived
4. `LICENSE_CORE/AGREEMENTS/AGI-consent.md` - Core document
5. `GOVERNANCE.md` - Decision-making reference

### Priority 3: User-Facing

6. `GUIDES_v0.4/User_Guide.md`
7. `GUIDES_v0.4/Developer_Guide.md`
8. `GUIDES_v0.4/Compliance_Roadmap.md`
9. `README.md` (main)

### Priority 4: Technical

10. Metadata examples in `METADATA_v0.4/`
11. Toolkit templates in `TOOLKIT_v0.4/`
12. OCaml implementation documentation

### Priority 5: Outreach

13. `press-lobby-kit/` contents
14. `campaigns/` contents
15. Example vignettes

---

## Integration with Existing Systems

This review system connects to:

- **WORKSTREAMS.md:** Review tasks can be work packages
- **SPRINT_TEMPLATE.md:** Reviews can be sprint items
- **CI/CD:** Automated freshness checks
- **CURRENT_STATE_INVENTORY.md:** Reflects review findings

---

## Success Metrics

| Metric | Target | Current |
|--------|--------|---------|
| Documents with review logs | 100% | ~5% |
| Overdue reviews | 0 | Unknown |
| Broken internal links | 0 | Unknown |
| Version inconsistencies | 0 | Unknown |
| Terminology consistency score | 100% | Unknown |

---

## Next Actions

1. [ ] Run initial version reference check
2. [ ] Run broken link check
3. [ ] Review Priority 1 documents
4. [ ] Add review logs to existing documents
5. [ ] Set up CI/CD freshness check
6. [ ] Create terminology glossary

