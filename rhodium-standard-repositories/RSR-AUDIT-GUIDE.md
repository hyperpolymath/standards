# RSR Audit System - Quick Reference Guide

**Validate Any Repository for Rhodium Standard Compliance**

---

## 🚀 Quick Start

### 1. Run Audit Script (Shell)

```bash
# Make executable (first time only)
chmod +x rsr-audit.sh

# Audit current directory
./rsr-audit.sh .

# Audit specific repository
./rsr-audit.sh /path/to/repository

# Output formats
./rsr-audit.sh . text    # Default: colored terminal output
./rsr-audit.sh . json    # Machine-readable JSON
./rsr-audit.sh . html    # HTML report
```

### 2. Interpret Results

**Exit Codes**:
- `0` - Gold compliance (100%)
- `1` - Silver compliance (90-99%)
- `2` - Bronze compliance (75-89%)
- `3` - Non-compliant (<75%)
- `4` - Error running audit

**Compliance Levels**:
- **Gold** (100%): Full RSR compliance - all checks pass
- **Silver** (90-99%): Strong compliance - production-ready
- **Bronze** (75-89%): Basic compliance - good foundation
- **Non-Compliant** (<75%): Does not meet minimum standards

---

## 📊 What Gets Checked

### Category 1: Foundational Infrastructure (15 checks)

- ✅ Nix flakes (`flake.nix`, `flake.lock`)
- ✅ Justfile with essential recipes (build, test, validate)
- ✅ GitLab CI/CD (`.gitlab-ci.yml`) or GitHub Actions
- ✅ Podman configuration (if applicable)
- ✅ Git configuration (`.gitignore`, `.gitattributes`)

### Category 2: Documentation Standards (20 checks)

- ✅ README.md (or .adoc)
- ✅ LICENSE.txt (dual: MIT + Palimpsest v0.8)
- ✅ SECURITY.md (vulnerability reporting)
- ✅ CODE_OF_CONDUCT.md
- ✅ CONTRIBUTING.md (TPCF framework)
- ✅ MAINTAINERS.md
- ✅ CHANGELOG.md
- ✅ .well-known/ directory:
  - security.txt (RFC 9116)
  - ai.txt (AI training policies)
  - humans.txt (attribution)

### Category 3: Security Architecture (25 checks)

- ✅ SPDX headers on all source files
- ✅ Type-safe language (Rust, Elixir, Ada, Haskell, ReScript)
- ✅ Memory safety (Rust ownership, Ada SPARK)
- ✅ No unsafe code blocks (Rust)
- ✅ Pinned dependencies (`Cargo.lock`, `mix.lock`)
- ✅ No node_modules/ (post-JavaScript)
- ✅ Security headers configured (for web projects)

### Category 4: Architecture Principles (15 checks)

- ✅ Offline-first design (no external API calls in core)
- ✅ CRDTs for distributed state (if applicable)
- ✅ Reversibility (Git repository)
- ✅ Reproducible builds (Nix flakes)
- ✅ Architecture documentation

### Category 7: FOSS & Licensing (10 checks)

- ✅ LICENSE.txt (plain text, not .md)
- ✅ MIT license included
- ✅ Palimpsest license included (ethical AI)
- ✅ Correct SPDX identifier
- ✅ FUNDING.yml (optional, bonus)
- ✅ MAINTAINERS.md (attribution)

### Category 10: Community & Governance (15 checks)

- ✅ TPCF framework in CONTRIBUTING.md
- ✅ Code of Conduct with enforcement
- ✅ Contributing guidelines (fork workflow, testing)
- ✅ GOVERNANCE document (optional, bonus)
- ✅ MAINTAINERS.md

---

## 🎖️ Example Output

### Text Format (Default)

```
╔════════════════════════════════════════════════════════════════════╗
║                                                                    ║
║              🎖️  RSR COMPLIANCE AUDIT SYSTEM 🎖️                   ║
║        Rhodium Standard Repository - Automated Validation         ║
║                                                                    ║
╚════════════════════════════════════════════════════════════════════╝

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Category 1: Foundational Infrastructure
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
✓ Nix flake configuration present
✓ Nix flake lockfile present
✓ flake.nix has description field
✓ Justfile present
✓ Justfile has build recipe
✓ Justfile has test recipe
✓ Justfile has validate recipe
✓ GitLab CI/CD configuration
...

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
RSR Audit Results
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  Repository: examples/rhodium-minimal
  Total Checks: 95
  Passed: 85
  Failed: 10
  Score: 89.47%

  🥉 COMPLIANCE LEVEL: BRONZE 🥉
  Basic RSR compliance achieved.

  Badge: https://img.shields.io/badge/RSR-Bronze%20(89%25)-cd7f32

  Add to your README.md:
  [![RSR Compliance](https://img.shields.io/badge/RSR-Bronze%20(89%25)-cd7f32)](https://gitlab.com/hyperpolymath/rhodium-standard-repositories)
```

### JSON Format

```json
{
  "repository": "examples/rhodium-minimal",
  "audit_date": "2025-11-22T12:00:00Z",
  "total_checks": 95,
  "passed_checks": 85,
  "failed_checks": 10,
  "score": 89.47,
  "compliance_level": "Bronze",
  "badge_url": "https://img.shields.io/badge/RSR-Bronze%20(89%25)-cd7f32",
  "badge_markdown": "[![RSR Compliance](https://img.shields.io/badge/RSR-Bronze%20(89%25)-cd7f32)](https://gitlab.com/hyperpolymath/rhodium-standard-repositories)",
  "thresholds": {
    "gold": 100,
    "silver": 90,
    "bronze": 75
  }
}
```

---

## 🏆 Getting to Gold Compliance

### Bronze → Silver (75% → 90%)

Missing checks to reach Silver:

1. **Container configuration**: Add `podman-compose.yml` with Chainguard Wolfi
2. **Security headers**: Configure CSP, X-Frame-Options, etc.
3. **FUNDING.yml**: Add funding transparency
4. **GOVERNANCE document**: Define decision-making process
5. **Architecture documentation**: Add ARCHITECTURE.md

### Silver → Gold (90% → 100%)

Missing checks to reach Gold:

1. **CRDTs**: Implement conflict-free distributed state (if applicable)
2. **Formal verification**: Add SPARK proofs (Ada) or TLA+ specs
3. **IndieWeb integration**: Webmention, Micropub (for web projects)
4. **Complete 10+ security dimensions**: All categories at 100%

---

## 🔧 Integration

### CI/CD Integration (GitLab)

```yaml
# .gitlab-ci.yml
rsr-audit:
  stage: validate
  image: alpine:latest
  before_script:
    - apk add --no-cache bash grep ripgrep bc
  script:
    - ./rsr-audit.sh . json > rsr-audit-result.json
    - cat rsr-audit-result.json
  artifacts:
    reports:
      dotenv: rsr-audit-result.json
  allow_failure: true
```

### Pre-commit Hook

```bash
# .git/hooks/pre-commit
#!/bin/bash

echo "Running RSR audit..."
./rsr-audit.sh .

exit_code=$?

if [ $exit_code -eq 3 ]; then
  echo "❌ Non-compliant (<75%). Commit blocked."
  exit 1
fi

echo "✅ RSR audit passed (compliance level: $exit_code)"
exit 0
```

### GitHub Actions

```yaml
name: RSR Audit

on: [push, pull_request]

jobs:
  rsr-audit:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Run RSR Audit
        run: |
          chmod +x rsr-audit.sh
          ./rsr-audit.sh . json > rsr-audit.json
      - name: Upload Results
        uses: actions/upload-artifact@v3
        with:
          name: rsr-audit-results
          path: rsr-audit.json
```

---

## 🎯 Badge Integration

### Markdown (README.md)

```markdown
[![RSR Compliance](https://img.shields.io/badge/RSR-Gold%20(100%25)-ffd700)](https://gitlab.com/hyperpolymath/rhodium-standard-repositories)
```

### AsciiDoc (README.adoc)

```asciidoc
image:https://img.shields.io/badge/RSR-Gold%20(100%25)-ffd700[RSR Gold Compliance,link=https://gitlab.com/hyperpolymath/rhodium-standard-repositories]
```

### HTML

```html
<a href="https://gitlab.com/hyperpolymath/rhodium-standard-repositories">
  <img src="https://img.shields.io/badge/RSR-Gold%20(100%25)-ffd700" alt="RSR Gold Compliance">
</a>
```

---

## 🔐 Attestation (Future: Haskell Registry)

Once the Haskell registry is deployed, you'll be able to:

### 1. Get Cryptographically Signed Attestation

```bash
curl https://rsr-registry.org/attest?url=https://gitlab.com/user/repo > attestation.json
```

### 2. Verify Attestation

```bash
# Extract public key and signature
cat attestation.json | jq -r '.signature.publicKey' > pubkey.b64
cat attestation.json | jq -r '.signature.value' > signature.b64

# Decode from Base64
base64 -d pubkey.b64 > pubkey.bin
base64 -d signature.b64 > signature.bin

# Verify using Ed25519 (requires openssl or custom tool)
# Message format: <url>|<level>|<score>|<timestamp>|<hash>
```

### 3. Embed Attestation in README

```markdown
## RSR Compliance

This repository has been validated by the RSR Registry:

- **Level**: Gold
- **Score**: 100%
- **Attestation**: [View Attestation](https://rsr-registry.org/repository/user/repo)
- **Verified**: ✅ Cryptographically signed
```

---

## 📚 Resources

- **RSR Specification**: [CLAUDE.md](CLAUDE.md)
- **Compliance Checklist**: [COMPLIANCE_CHECKLIST.md](COMPLIANCE_CHECKLIST.md)
- **CCCP Manifesto**: [CCCP-MANIFESTO.md](CCCP-MANIFESTO.md)
- **Haskell Registry Design**: [docs/haskell-registry-design.md](docs/haskell-registry-design.md)
- **Templates**: [templates/](templates/)
- **Examples**: [examples/rhodium-minimal](examples/rhodium-minimal)

---

## 🤝 Contributing

Found a bug in the audit script? Have suggestions for additional checks?

1. Open an issue: https://gitlab.com/hyperpolymath/rhodium-standard-repositories/-/issues
2. Submit a Merge Request with improvements
3. Join the CCCP movement!

---

## 📞 Support

- **Questions**: Open an issue
- **Security**: See .well-known/security.txt
- **Discussion**: GitLab discussions

---

*"Automated validation, verifiable compliance, cryptographic attestation."*

— RSR Audit System, Rhodium Standard Repository
