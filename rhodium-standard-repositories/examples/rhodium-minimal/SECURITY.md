# Security Policy

**Rhodium Standard Repository (RSR) - Security Guidelines**

---

## 🔒 Supported Versions

| Version | Supported          | Notes                          |
| ------- | ------------------ | ------------------------------ |
| 0.1.x   | ✅ Yes            | Current stable release         |
| < 0.1   | ❌ No             | Pre-release, not supported     |

---

## 🚨 Reporting a Vulnerability

### Quick Contact

**Preferred Method**: See `.well-known/security.txt` (RFC 9116 compliant)

**Email**: [See .well-known/security.txt for current contact]

**PGP Key**: `.well-known/security-pgp-key.asc` (if available)

### What to Include

When reporting a vulnerability, please include:

1. **Description**: Clear description of the vulnerability
2. **Impact**: What could an attacker do with this?
3. **Reproduction**: Step-by-step instructions to reproduce
4. **Environment**: OS, Rust version, dependencies
5. **Suggested Fix**: If you have ideas (optional)

### Example Report

```markdown
**Summary**: Buffer overflow in input validation

**Impact**: Remote code execution possible

**Reproduction**:
1. Run `rhodium-minimal` with input: [specific input]
2. Observe crash at [location]

**Environment**:
- OS: Ubuntu 24.04
- Rust: 1.75.0
- Binary: release build

**Suggested Fix**: Add bounds checking at line X
```

---

## ⏱️ Response Timeline

We aim for the following response times:

| Timeline     | Action                                |
|------------- |---------------------------------------|
| **24 hours** | Initial acknowledgement of report     |
| **72 hours** | Preliminary assessment of severity    |
| **7 days**   | Detailed investigation and triage     |
| **30 days**  | Patch released (for critical issues)  |
| **90 days**  | Public disclosure (coordinated)       |

**Note**: These are targets, not guarantees. Critical vulnerabilities receive priority.

---

## 🛡️ Security Architecture

### Type Safety (Rust)

This project uses **Rust** for compile-time memory safety guarantees:

- ✅ **No null pointer dereferences**: Rust's `Option<T>` type
- ✅ **No buffer overflows**: Bounds checking enforced
- ✅ **No use-after-free**: Ownership model prevents
- ✅ **No data races**: Borrow checker enforces thread safety

### No Unsafe Code

This minimal example contains **zero `unsafe` blocks**.

Verify with:
```bash
just check-unsafe
```

### No Network Dependencies

This example is **offline-first** with zero network calls:

- ✅ No HTTP requests
- ✅ No DNS lookups
- ✅ No socket connections
- ✅ Works in air-gapped environments

### Supply Chain Security

- ✅ **Zero dependencies**: Minimal attack surface
- ✅ **SPDX headers**: On every source file
- ✅ **Pinned toolchain**: Via Nix flake

---

## 🔐 Security Best Practices

### For Contributors

1. **Never commit secrets**: No API keys, passwords, tokens
2. **SPDX headers required**: On every new file
3. **No unsafe code**: Unless absolutely necessary and documented
4. **Security tests**: Add tests for security-sensitive code

### For Users

1. **Verify checksums**: Check SHA256SUMS before running
2. **Use Nix**: Reproducible builds via `nix build`
3. **Report issues**: See vulnerability reporting above
4. **Keep updated**: Watch for security releases

---

## 🎯 Security Scope

### In Scope

- ✅ Memory safety vulnerabilities
- ✅ Logic errors leading to unexpected behavior
- ✅ Build system security issues
- ✅ Supply chain concerns

### Out of Scope

- ❌ Social engineering attacks
- ❌ Physical access attacks
- ❌ Issues in third-party dependencies (report to upstream)
- ❌ Denial of service via resource exhaustion (expected behavior)

---

## 🏆 Security Acknowledgements

We maintain a security hall of fame for responsible disclosure:

*(None yet - be the first!)*

---

## 📚 Resources

- **RSR Security Standards**: [../../COMPLIANCE_CHECKLIST.md](../../COMPLIANCE_CHECKLIST.md)
- **OWASP Top 10**: https://owasp.org/www-project-top-ten/
- **Rust Security**: https://www.rust-lang.org/policies/security
- **RFC 9116 (security.txt)**: https://www.rfc-editor.org/rfc/rfc9116.html

---

## 🔄 Version History

| Date       | Version | Changes                          |
|------------|---------|----------------------------------|
| 2025-11-22 | 0.1.0   | Initial security policy          |

---

*"Security through simplicity, safety through types."*

— The Rhodium Standard
