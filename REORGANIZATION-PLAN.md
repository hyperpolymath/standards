# Standards Repo Reorganization Plan

## Current Redundancies Identified

### 1. Template Duplication
**Issue:** K9 templates appear in multiple locations:
- `.machine_readable/contractiles/k9/template-*.k9.ncl` (3 copies)
- `0-ai-gatekeeper-protocol/contractiles/k9/template-*.k9.ncl` (3 copies per subdir)
- `0-ai-gatekeeper-protocol/*/contractiles/k9/template-*.k9.ncl` (multiple copies)

**Solution:** Consolidate into single source in self-validating monorepo

### 2. RSR Workflow Duplication
**Issue:** `rsr-antipattern.yml` appears in:
- `.github/workflows/rsr-antipattern.yml`
- `0-ai-gatekeeper-protocol/.github/workflows/rsr-antipattern.yml`
- `0-ai-gatekeeper-protocol/mcp-repo-guardian/.github/workflows/rsr-antipattern.yml`

**Solution:** Move to RSR engine repo, reference via GitHub workflow reuse

### 3. Overlapping Specifications
**Issue:** A2ML templates exist in both:
- `a2ml-templates/` (6 files)
- `rhodium-standard-repositories/templates/` (20+ files with some overlap)

**Solution:** Separate concerns - A2ML templates in a2ml-repo, RSR compliance templates in rsr-engine-repo

## Better Locations Needed

### 1. Contractile Templates
**Current:** Scattered across `.machine_readable/contractiles/` and protocol dirs
**Better:** Centralize in self-validating-repo with clear categorization:
```
self-validating-repo/
  templates/
    contractiles/
      dust/
      intend/
      lust/
      must/
      trust/
    k9/
      kennel/
      yard/
      hunt/
```

### 2. RSR Badges
**Current:** Buried in `rhodium-standard-repositories/badges/`
**Better:** Promote to top-level in rsr-engine-repo:
```
rsr-engine-repo/
  badges/
    rsr-bronze.svg
    rsr-silver.svg
    rsr-gold.svg
    rsr-rhodium.svg
  README.md  # Badge usage guidelines
```

### 3. Machine Readable Specs
**Current:** Mixed in `.machine_readable/6a2/` with other files
**Better:** Organize by standard:
```
standards/
  .machine_readable/
    crg/
      COMPONENT-READINESS-GRADES.a2ml
    trg/
      TOOLCHAIN-READINESS-GRADES.a2ml
    rsr/
      RSR-SPEC.a2ml
```

## Standards Coverage Gaps

### 1. Missing Interoperability Standards
**Gap:** No formal specification for how CRG/TRG/RSR interact
**Add:** `standards/interop/` directory with:
- CRG-TRG mapping specification
- RSR compliance matrix
- Version compatibility rules

### 2. Automation Interface Standards
**Gap:** K9 automation hooks lack formal interface definition
**Add:** `standards/automation/` with:
- K9 contract interface spec
- CI/CD integration patterns
- Automation safety levels

### 3. Template Versioning Standard
**Gap:** No formal template versioning policy
**Add:** `standards/templates/` with:
- Template versioning spec
- Compatibility requirements
- Deprecation policy

### 4. Compliance Testing Standards
**Gap:** RSR certifier lacks formal test specification
**Add:** `rsr-engine-repo/spec/` with:
- Test coverage requirements
- Certification validation rules
- Audit trail format

## Implementation Checklist

### Phase 1: Eliminate Redundancies
- [ ] Consolidate K9 templates into self-validating-repo
- [ ] Remove duplicate RSR workflow files
- [ ] Separate A2ML vs RSR templates
- [ ] Clean up scattered contractile templates

### Phase 2: Relocate Misplaced Items
- [ ] Move RSR badges to rsr-engine-repo/badges/
- [ ] Reorganize machine-readable specs by standard
- [ ] Centralize contractile templates in self-validating-repo
- [ ] Promote RSR workflows to reusable GitHub actions

### Phase 3: Fill Coverage Gaps
- [ ] Create interop/ directory with CRG-TRG-RSR mappings
- [ ] Add automation/ directory with K9 interface specs
- [ ] Develop templates/ directory with versioning standards
- [ ] Formalize compliance testing in rsr-engine-repo

### Phase 4: Documentation Updates
- [ ] Update all README files with new structure
- [ ] Add migration guide for existing projects
- [ ] Create template usage documentation
- [ ] Document version compatibility matrix

## Verification Plan

### Post-Reorganization Checks:
1. **Template Accessibility:** All templates reachable via registry
2. **No Broken References:** `grep -r` for old paths
3. **CI/CD Functionality:** All workflows pass
4. **RSR Compliance:** Run certifier on new structure
5. **Documentation Completeness:** All changes documented

### Validation Commands:
```bash
# Check for remaining duplicates
find . -name "template-*.k9.ncl" | wc -l  # Should be 3 total

# Verify RSR workflow consolidation
find . -name "*rsr-antipattern*" | wc -l  # Should be 1

# Validate template registry
rsr-certifier validate-registry template-registry.json
```

## Risk Mitigation

### Potential Issues & Solutions:
1. **Broken Template References:** Use symlinks during transition
2. **CI/CD Failures:** Test workflows in staging first
3. **Documentation Gaps:** Create redirect pages
4. **Template Version Mismatches:** Implement gradual deprecation

### Rollback Plan:
```bash
# Quick rollback script
git checkout reorganization-start-point
git subtree add --prefix=standards/a2ml a2ml-repo main
# Repeat for other repos
```

## Expected Benefits

1. **Reduced Maintenance:** 40% fewer duplicate files
2. **Clearer Structure:** Logical grouping by function
3. **Better Discoverability:** Centralized template locations
4. **Improved Compliance:** Complete standards coverage
5. **Easier Onboarding:** Consistent documentation structure