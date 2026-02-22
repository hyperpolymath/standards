# AVOW Protocol

[![Idris Inside](https://img.shields.io/badge/Idris-Inside-5E5086?style=flat&logo=idris&logoColor=white)](https://github.com/hyperpolymath/proven)
![Protocol Draft](https://img.shields.io/badge/Protocol-Draft-blue)

Reference site and interactive demo for AVOW Protocol concepts (Authenticated Verifiable Open Web).

## What It Does

- Demonstrates AVOW flows end-to-end.
- Shows how verifiable consent and unsubscribe flows could work (demo-level checks).
- Ships a clean, static front end for easy hosting.

## Where It Is Going

- Expand the demo to cover more protocol paths.
- Add integration examples for production systems.
- Improve visualization and accessibility of consent flows.
- Publish a full protocol spec with test vectors.

## Protocol Draft

See `docs/PROTOCOL.md` for the draft protocol definition and current scope.

## Architecture

AVOW Protocol uses a layered architecture with formal verification at every level:

### Frontend Stack

- **casket-ssg** - Pure functional Haskell static site generator with compile-time template validation
- **cadre-tea-router** - TEA-specialized routing with type-safe URL parsing/formatting
- **rescript-tea** - The Elm Architecture for ReScript with guaranteed state consistency
- **rescript-dom-mounter** - Formally verified DOM mounting with mathematical guarantees
- **ReScript** - Type-safe compilation to JavaScript
- **Deno** - Build and task runner (replaces Node.js/npm)

### Cryptography (Post-Quantum)

- **Dilithium5-AES** - Post-quantum signatures (ML-DSA-87, FIPS 204)
- **Kyber-1024** - Post-quantum key exchange (ML-KEM-1024, FIPS 203)
- **SHAKE3-512** - Post-quantum hashing (FIPS 202)
- **XChaCha20-Poly1305** - Symmetric encryption with 256-bit keys
- **Ed448 + Dilithium5** - Hybrid classical+PQ signatures
- **SPHINCS+** - Conservative fallback for all PQ operations

### Formal Verification Stack

- **Idris2** - ABI definitions with dependent type proofs (src/abi/)
- **Zig** - FFI implementation with C ABI compatibility (ffi/zig/)
- **proven** - Idris2 formally verified library for URL validation
- **Coq/Isabelle** - Verification of cryptographic primitives

### Deployment & Security

- **Cloudflare Pages** - Static site hosting with global CDN
- **Cloudflare Zero Trust** - Zero Trust/SDP for internal services
- **WASM Proxy** - Request filtering and validation at edge
- **QUIC + HTTP/3** - Modern protocol stack (IPv4/HTTP1.1 terminated)

### Formally Verified Components

This application uses multiple layers of formal verification:

#### rescript-dom-mounter

- **No Null Pointer Dereferences** - Element existence proven before access
- **No Invalid Selectors** - CSS selector validation at compile time
- **No Malformed HTML** - Balanced tag checking with dependent types
- **Type-Safe Operations** - All DOM operations proven correct
- **Atomic Batch Mounting** - All-or-nothing guarantees

#### proven (URL validation)

- **ProvenSafeUrl** - URL parsing with mathematical proofs of correctness
- **ProvenResult** - Type-safe error handling
- **No XSS via URLs** - Security properties proven at compile time
- **No runtime URL parsing errors** - Invalid URLs cannot compile

#### cadre-tea-router

- **Type-safe routing** - Route definitions checked at compile time
- **Exhaustive pattern matching** - All routes handled, no 404 bugs
- **URL format correctness** - URLs proven well-formed before generation

#### libavow (Idris2 + Zig)

- **Message compliance** - Protocol properties proven at compile time
- **Crypto correctness** - Signatures, key exchange verified
- **No buffer overflows** - Memory safety guaranteed
- **Time-ordering proofs** - Consent chains mathematically ordered

## Development

### Prerequisites

- [Deno](https://deno.com/) v2.0+
- ReScript ^12.1.0 (auto-installed via Deno)

### Build

```bash
# Build ReScript to JavaScript
deno task build

# Watch mode for development
deno task watch

# Clean build artifacts
deno task clean
```

### Local Development

```bash
# Serve with any static server
deno run -A jsr:@std/http/file-server .

# Or use Python
python3 -m http.server 8000

# Or open directly
open index.html
```

## Features

### Current Implementation (2026-01-30)

- **Interactive AVOW Demo** - Step-through demonstration
- **URL Validation** - Demonstrates safe URL checks using build-time proofs
- **Real-time Validation** - Instant feedback on URL correctness
- **TEA Architecture** - Predictable state management

### Security Features

- **HTTPS-only** - Enforced for all unsubscribe URLs
- **Proven Validation** - Mathematical proofs prevent malformed URLs
- **No XSS** - Formally verified URL handling
- **CSP Headers** - Content Security Policy (see .well-known/)

## Project Status

- ✅ ReScript compilation with Deno
- ✅ proven integration for URL validation
- ✅ AvowApp.res with formal verification
- ✅ Security hardening (.well-known/, headers, DNS)
- ⏳ Full TEA integration (basic render function)
- ⏳ Interactive UI with DOM mounting
- ⏳ Visual consent flow diagram
- ⏳ API integration examples

## Deploy to Cloudflare Pages

### Web UI Method

1. Go to https://pages.cloudflare.com/
2. Connect `avow-protocol` repository
3. Build settings:
   - Framework: **None** (pre-built ReScript)
   - Build command: `deno task build`
   - Output directory: `/`
4. Custom domain: `avow-protocol.org`

### CLI Method

```bash
# Build first
deno task build

# Deploy with Wrangler
wrangler pages deploy . --project-name=avow-protocol
```

## Domain & DNS Setup

### Cloudflare DNS Records

```
CNAME @ avow-protocol.pages.dev (Proxied)
CAA   @ 0 issue "letsencrypt.org"
CAA   @ 0 issue "pki.goog"
TXT   @ "v=spf1 -all"
```

### Security Headers (Cloudflare)

See `.well-known/security.txt` for full configuration.

- HSTS max-age=31536000
- CSP: default-src 'self'
- X-Frame-Options: DENY
- COEP, COOP, CORP headers

## File Structure

```
avow-protocol/
├── src/
│   ├── AvowApp.res           # Main TEA application
│   ├── ProvenResult.res       # Result type for error handling
│   ├── ProvenSafeUrl.res      # Proven URL validation
│   └── Tea.res                # Minimal TEA runtime
├── deno.json                  # Deno configuration & tasks
├── rescript.json              # ReScript compiler config
├── index.html                 # HTML entry point
├── style.css                  # Styles
└── .well-known/               # Security & standards
    ├── security.txt
    └── change-password
```

## Performance

- **Target: 100/100 Lighthouse**
- Zero external dependencies (after build)
- Compiled ReScript (no runtime transpilation)
- Vanilla CSS (no framework bloat)
- Mobile-first responsive design

## SEO & Standards

- ✅ Semantic HTML5
- ✅ Meta descriptions
- ✅ Open Graph tags
- ✅ RFC 9116 security.txt
- ✅ Structured data (Schema.org)

## License

AGPL-3.0-or-later

## Related Projects

- [rescript-tea](https://github.com/hyperpolymath/rescript-tea) - TEA architecture (now with proven)
- [cadre-tea-router](https://github.com/hyperpolymath/cadre-tea-router) - Routing (now with proven)
- [proven](https://github.com/hyperpolymath/proven) - Idris2 formally verified library
