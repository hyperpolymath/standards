# Conformance Guide — What Counts as Compliant?

## 📜 HTTP 430

- Returns 430 when AI-specific consent is refused
- Communicates clearly in headers or body
- Avoids fallback to generic 403/451

## 🧭 AIBDP Manifest

- Served at `/.well-known/aibdp.json`
- Declares version, contact, and policy terms
- Machine-readable and interpretable

## 🛠 Optional Enhancements

- Cryptographic binding (COSE/JWS)
- Consent caching
- Manifest diff tracking
- Visual icons for 430-aware participation

## 🪧 Interoperability

- IndieWeb-compatible
- CDN-friendly
- Can integrate into browser-level refusal signaling

---

_Compliance isn’t about rigidity — it’s about architectural honesty._
