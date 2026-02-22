;; SPDX-License-Identifier: PMPL-1.0-or-later
;; SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
;;
;; AVOW Protocol - Security Requirements
;; Post-quantum cryptography and formal verification standards

(define user-security-requirements
  '(
    ;; Category, Algorithm/Standard, NIST/FIPS Standard, Notes
    (PasswordHashing "Argon2id (512 MiB, 8 iter, 4 lanes)" "—" "Max memory/iterations for GPU/ASIC resistance; aligns with proactive security stance.")
    (GeneralHashing "SHAKE3-512 (512-bit output)" "FIPS 202" "Post-quantum; use for provenance, key derivation, and long-term storage.")
    (PQSignatures "Dilithium5-AES (hybrid)" "ML-DSA-87 (FIPS 204)" "Hybrid with AES-256 for belt-and-suspenders security. SPHINCS+ as conservative backup.")
    (PQKeyExchange "Kyber-1024 + SHAKE256-KDF" "ML-KEM-1024 (FIPS 203)" "Kyber-1024 for KEM, SHAKE256 for key derivation. SPHINCS+ as backup.")
    (ClassicalSigs "Ed448 + Dilithium5 (hybrid)" "—" "Ed448 for classical compatibility; Dilithium5 for PQ. SPHINCS+ as backup. Terminate Ed25519/SHA-1 immediately.")
    (Symmetric "XChaCha20-Poly1305 (256-bit key)" "—" "Larger nonce space; 256-bit keys for quantum margin.")
    (KeyDerivation "HKDF-SHAKE512" "FIPS 202" "Post-quantum KDF; use with all secret key material.")
    (RNG "ChaCha20-DRBG (512-bit seed)" "SP 800-90Ar1" "CSPRNG for deterministic, high-entropy needs.")
    (UserFriendlyHashNames "Base32(SHAKE256(hash)) → Wordlist" "—" "Memorable, deterministic mapping (e.g., \"Gigantic-Giraffe-7\" for drivers).")
    (DatabaseHashing "BLAKE3 (512-bit) + SHAKE3-512" "—" "BLAKE3 for speed, SHAKE3-512 for long-term storage (semantic XML/ARIA tags).")
    (SemanticXMLGraphQL "Virtuoso (VOS) + SPARQL 1.2" "—" "Supports WCAG 2.3 AAA, ARIA, and formal verification for accessibility/compliance.")
    (VMExecution "GraalVM (with formal verification)" "—" "Aligns with preference for introspective, reversible design.")
    (ProtocolStack "QUIC + HTTP/3 + IPv6 (IPv4 disabled)" "—" "Terminate HTTP/1.1, IPv4, and SHA-1 per \"danger zone\" policy.")
    (Accessibility "WCAG 2.3 AAA + ARIA + Semantic XML" "—" "CSS-first, HTML-second; full compliance with accessibility requirements.")
    (Fallback "SPHINCS+" "—" "Conservative PQ backup for all hybrid classical+PQ systems; use if primary PQ algorithm is ever compromised.")
    (FormalVerification "Coq/Isabelle (for crypto primitives)" "—" "Proactive attestation and transparent logic per system design principles.")
    (IdrisVerification "Idris2 (for ABI/protocol proofs)" "—" "Dependent types prove message compliance and verification correctness at compile-time.")
  )
)

;; Implementation Requirements for libavow
(define libavow-crypto-requirements
  '(
    ;; Core cryptographic primitives required for AVOW Protocol
    (SignatureScheme
      (primary "Dilithium5-AES")
      (classical "Ed448")
      (fallback "SPHINCS+")
      (mode "hybrid")
      (purpose "Sign messages and verification proofs"))

    (KeyExchange
      (primary "Kyber-1024")
      (kdf "SHAKE256")
      (purpose "Establish secure channels for message transmission"))

    (Hashing
      (primary "SHAKE3-512")
      (secondary "BLAKE3")
      (purpose "Content hashing, merkle trees, proof generation"))

    (SymmetricEncryption
      (algorithm "XChaCha20-Poly1305")
      (key-size 256)
      (purpose "Encrypt message content and metadata"))

    (ProofSystem
      (language "Idris2")
      (properties ["consent-chain-ordering" "rate-limit-compliance" "unsubscribe-validity"])
      (purpose "Compile-time verification of protocol properties"))

    (FormalVerification
      (tools ["Idris2" "Coq" "Isabelle"])
      (verified-properties ["no-null-dereference" "no-buffer-overflow" "crypto-primitive-correctness"])
      (purpose "Mathematical proofs of implementation correctness"))
  )
)

;; Cloudflare Configuration Requirements
(define cloudflare-security-requirements
  '(
    (ZeroTrust
      (enabled #t)
      (services ["dashboard.internal" "ci.internal" "db.internal"])
      (authentication "Cloudflare Access + SAML"))

    (WASM-Proxy
      (enabled #t)
      (endpoint "wasm.avow-protocol.org")
      (purpose "WASM-based request filtering and validation"))

    (DDoS-Protection
      (enabled #t)
      (mode "automatic")
      (custom-rules ["rate-limit-api" "block-known-bad-actors"]))

    (TLS
      (minimum-version "TLS 1.3")
      (cipher-suites ["TLS_AES_256_GCM_SHA384" "TLS_CHACHA20_POLY1305_SHA256"])
      (http3 #t)
      (quic #t))

    (DNS-Security
      (dnssec #t)
      (caa-records #t)
      (tlsa-records #t)
      (mta-sts #t))

    (Page-Rules
      (force-https #t)
      (hsts-max-age 63072000)
      (include-subdomains #t)
      (preload #t))
  )
)
