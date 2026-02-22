# Consent Layer: Technical Architecture Specification

## Executive Summary

This document provides the technical specification for implementing a Consent Layer across internet infrastructure. It covers metadata standards, protocol extensions, consent registry architecture, cryptographic verification, and platform integration patterns.

**Target audience:** Platform developers, protocol engineers, standards bodies, security researchers.

**Status:** Draft specification v0.9 (pre-release for community review)

---

## 1. Metadata Standards

### 1.1 Core Schema

**Base namespace:** `https://consent-layer.org/ns/`

**Required fields:**

```json
{
  "@context": [
    "https://schema.org",
    "https://consent-layer.org/ns/"
  ],
  "@type": "CreativeWork",
  "identifier": "urn:uuid:a1b2c3d4-e5f6-7890-abcd-ef1234567890",
  "name": "Work Title",
  "author": {
    "@type": "Person",
    "name": "Creator Name",
    "identifier": "https://orcid.org/0000-0001-2345-6789"
  },
  "license": "https://palimpsest.license/v0.4",
  "consent": {
    "@type": "ConsentRequirement",
    "required": ["ai-training", "commercial-adaptation"],
    "contact": "creator@example.com",
    "registry": "https://consent-registry.example.org/works/a1b2c3d4",
    "signature": {
      "algorithm": "Ed25519",
      "publicKey": "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5...",
      "value": "sig-base64-encoded..."
    }
  }
}
```

**Optional fields:**
- `emotionalLineage`: Cultural/narrative context
- `jurisdiction`: Primary legal jurisdiction
- `derivatives`: Policy for derivative works
- `revocable`: Whether consent can be withdrawn
- `expires`: Consent expiry date (ISO 8601)

---

### 1.2 Embedding Locations

#### HTML/Web
```html
<script type="application/ld+json">
{
  "@context": "https://schema.org",
  "@type": "Article",
  "license": "https://palimpsest.license/v0.4",
  "consentRequired": "ai-training"
}
</script>

<meta name="consent:required" content="ai-training">
<meta name="consent:contact" content="creator@example.com">
<meta name="consent:registry" content="https://consent-registry.example.org/work/123">
```

#### Images (EXIF/XMP)
```xml
<rdf:Description rdf:about=""
    xmlns:consent="https://consent-layer.org/ns/"
    xmlns:dc="https://purl.org/dc/elements/1.1/">
  <dc:creator>Jane Doe</dc:creator>
  <dc:rights>Palimpsest License v0.4</dc:rights>
  <consent:required>ai-training</consent:required>
  <consent:contact>jane@example.com</consent:contact>
  <consent:registry>https://consent-registry.example.org/work/456</consent:registry>
  <consent:signature algorithm="Ed25519">sig-base64...</consent:signature>
</rdf:Description>
```

#### PDF (XMP Metadata)
```xml
<x:xmpmeta xmlns:x="adobe:ns:meta/">
  <rdf:RDF xmlns:rdf="https://www.w3.org/1999/02/22-rdf-syntax-ns#">
    <rdf:Description rdf:about=""
        xmlns:consent="https://consent-layer.org/ns/">
      <consent:required>ai-training</consent:required>
      <consent:contact>creator@example.com</consent:contact>
    </rdf:Description>
  </rdf:RDF>
</x:xmpmeta>
```

#### Audio/Video (ID3v2.4 / XMP)
```
Frame: TCOP (Copyright)
Value: Palimpsest License v0.4; Consent required: ai-training; Contact: creator@example.com
```

#### Source Code (SPDX)
```python
# SPDX-License-Identifier: MPL-2.0-or-later
# SPDX-FileCopyrightText: 2025 Jane Doe <jane@example.com>
# Consent-Required: ai-training
# Consent-Contact: jane@example.com
```

---

## 2. Protocol Extensions

### 2.1 HTTP Headers

#### License and Consent Headers
```http
HTTP/1.1 200 OK
Content-Type: text/html; charset=utf-8
X-License: https://palimpsest.license/v0.4
X-Consent-Required: ai-training, commercial-adaptation
X-Consent-Contact: creator@example.com
X-Consent-Registry: https://consent-registry.example.org/work/789
Link: <https://palimpsest.license/v0.4>; rel="license"
```

#### Consent Verification Header (for API requests)
```http
GET /dataset/download HTTP/1.1
X-Consent-Token: Bearer eyJhbGciOiJFZDI1NTE5...
```

Server validates token against consent registry before serving content.

---

### 2.2 DNS TXT Records

**Purpose:** Discover creator's consent policy at domain level.

```dns
_consent.example.com. 86400 IN TXT "v=consent1; policy=https://example.com/.well-known/consent-policy.json"
```

**Policy file (JSON):**
```json
{
  "version": "1.0",
  "defaultLicense": "https://palimpsest.license/v0.4",
  "consentRequired": ["ai-training"],
  "contact": "legal@example.com",
  "registry": "https://consent-registry.example.org/org/example"
}
```

---

### 2.3 Well-Known URI

**Path:** `/.well-known/consent-policy.json`

**Example:**
```json
{
  "@context": "https://consent-layer.org/ns/",
  "domain": "example.com",
  "defaultLicense": "https://palimpsest.license/v0.4",
  "consentRequired": ["ai-training", "commercial-use"],
  "exceptions": [
    {
      "path": "/public-domain/*",
      "consentRequired": []
    }
  ],
  "contact": {
    "email": "consent@example.com",
    "api": "https://api.example.com/consent"
  },
  "registry": "https://consent-registry.example.org"
}
```

---

### 2.4 Email Headers (for shared content)

```email
From: jane@example.com
To: colleague@example.com
Subject: Check out my new article
Content-Type: text/html; charset=utf-8
X-License: https://palimpsest.license/v0.4
X-Consent-Required: ai-training
X-Content-Hash: sha256:a1b2c3d4e5f6...

[Email body with link to article]
```

---

## 3. Consent Registry Architecture

### 3.1 Centralised Registry (MVP)

**Components:**
- **PostgreSQL database:** Store consent records
- **RESTful API:** Query/update consent
- **Web dashboard:** Creator interface
- **Webhook system:** Notify creators of consent requests

**Schema:**
```sql
CREATE TABLE works (
  id UUID PRIMARY KEY,
  title TEXT NOT NULL,
  creator_id UUID REFERENCES creators(id),
  content_hash TEXT NOT NULL,
  license TEXT NOT NULL,
  consent_required TEXT[], -- e.g., {'ai-training', 'commercial'}
  created_at TIMESTAMP DEFAULT NOW()
);

CREATE TABLE consent_requests (
  id UUID PRIMARY KEY,
  work_id UUID REFERENCES works(id),
  requester_name TEXT NOT NULL,
  requester_contact TEXT NOT NULL,
  use_type TEXT NOT NULL, -- e.g., 'ai-training'
  description TEXT,
  terms JSONB, -- e.g., {"compensation": "$100", "attribution": "model card"}
  status TEXT DEFAULT 'pending', -- pending, granted, denied
  created_at TIMESTAMP DEFAULT NOW()
);

CREATE TABLE consent_grants (
  id UUID PRIMARY KEY,
  request_id UUID REFERENCES consent_requests(id),
  granted_at TIMESTAMP DEFAULT NOW(),
  conditions JSONB,
  signature TEXT, -- cryptographic signature from creator
  revocable BOOLEAN DEFAULT TRUE,
  expires_at TIMESTAMP
);
```

---

### 3.2 Federated Registry (Long-Term)

**Architecture:** Federation model (like email, Mastodon).

**Components:**
- **Registry instances:** Multiple providers (consent.cc.org, consent.eff.org, etc.)
- **Discovery protocol:** DNS-based (like MX records for email)
- **Synchronisation:** ActivityPub-style federation

**Discovery:**
```dns
_consent-registry.example.com. 86400 IN TXT "provider=consent.cc.org"
```

**Federation protocol:**
```json
POST https://consent.cc.org/federation/sync
Content-Type: application/json

{
  "type": "ConsentUpdate",
  "workId": "urn:uuid:a1b2c3d4",
  "creator": "jane@example.com",
  "status": "granted",
  "requester": "openai.com",
  "timestamp": "2025-11-22T14:30:00Z",
  "signature": "sig-base64..."
}
```

Receiving registry validates signature, updates local cache.

---

### 3.3 Blockchain-Based Registry (Hybrid)

**Use case:** Immutable audit trail.

**Architecture:**
- **Public index on blockchain:** Work ID, consent status hash
- **Private details off-chain:** Actual terms stored in federated DBs
- **Smart contract:** Enforce consent logic

**Ethereum smart contract example:**
```solidity
contract ConsentRegistry {
    struct Consent {
        address creator;
        bytes32 workHash;
        bytes32 requesterHash;
        uint256 grantedAt;
        bool revocable;
    }

    mapping(bytes32 => Consent) public consents;

    function grantConsent(
        bytes32 workHash,
        bytes32 requesterHash,
        bool revocable
    ) external {
        require(msg.sender == creator, "Only creator can grant");
        consents[keccak256(abi.encodePacked(workHash, requesterHash))] = Consent({
            creator: msg.sender,
            workHash: workHash,
            requesterHash: requesterHash,
            grantedAt: block.timestamp,
            revocable: revocable
        });
    }

    function verifyConsent(
        bytes32 workHash,
        bytes32 requesterHash
    ) external view returns (bool) {
        Consent memory c = consents[keccak256(abi.encodePacked(workHash, requesterHash))];
        return c.grantedAt > 0;
    }
}
```

---

## 4. Consent Registry API Specification

**Base URL:** `https://api.consent-registry.example.org/v1`

**Authentication:** OAuth 2.0 / API keys

---

### 4.1 Register Work

```http
POST /works
Content-Type: application/json
Authorization: Bearer {creator_token}

{
  "title": "My Novel",
  "creator": {
    "name": "Jane Doe",
    "email": "jane@example.com",
    "orcid": "https://orcid.org/0000-0001-2345-6789"
  },
  "contentHash": "sha256:a1b2c3d4e5f6...",
  "license": "https://palimpsest.license/v0.4",
  "consentRequired": ["ai-training"],
  "metadata": {
    "emotionalLineage": "Diaspora narrative about migration",
    "jurisdiction": "NL"
  }
}

Response:
{
  "workId": "urn:uuid:a1b2c3d4-e5f6-7890",
  "registryUrl": "https://consent-registry.example.org/works/a1b2c3d4",
  "createdAt": "2025-11-22T14:30:00Z"
}
```

---

### 4.2 Request Consent

```http
POST /consent/request
Content-Type: application/json

{
  "workId": "urn:uuid:a1b2c3d4-e5f6-7890",
  "requester": {
    "name": "OpenAI",
    "email": "legal@openai.com",
    "publicKey": "ssh-rsa AAAAB3NzaC1..."
  },
  "use": {
    "type": "ai-training",
    "description": "Training GPT-5 language model",
    "scope": "worldwide",
    "duration": "perpetual"
  },
  "terms": {
    "compensation": "$100 USD one-time payment",
    "attribution": "Model card and dataset documentation",
    "revocable": true
  }
}

Response:
{
  "requestId": "req-12345",
  "status": "pending",
  "creatorNotified": true,
  "estimatedResponseTime": "7 days"
}
```

---

### 4.3 Grant/Deny Consent

```http
POST /consent/grant
Content-Type: application/json
Authorization: Bearer {creator_token}

{
  "requestId": "req-12345",
  "decision": "granted",
  "conditions": {
    "compensation": "$500 USD one-time payment",
    "attribution": "Model card, dataset docs, public acknowledgment",
    "revocable": true,
    "auditTrail": "Annual usage reports"
  },
  "signature": "sig-Ed25519-base64..."
}

Response:
{
  "consentId": "consent-67890",
  "grantedAt": "2025-11-22T15:00:00Z",
  "expiresAt": null,
  "revocable": true
}
```

---

### 4.4 Verify Consent

```http
GET /consent/verify?workId=urn:uuid:a1b2c3d4&requester=OpenAI

Response:
{
  "workId": "urn:uuid:a1b2c3d4",
  "requester": "OpenAI",
  "status": "granted",
  "grantedAt": "2025-11-22T15:00:00Z",
  "conditions": {...},
  "proof": {
    "signature": "sig-base64...",
    "publicKey": "ssh-ed25519 AAAAC3NzaC1...",
    "algorithm": "Ed25519"
  }
}
```

---

### 4.5 Revoke Consent

```http
POST /consent/revoke
Content-Type: application/json
Authorization: Bearer {creator_token}

{
  "consentId": "consent-67890",
  "reason": "Terms violated: attribution not provided",
  "effectiveDate": "2025-12-01T00:00:00Z"
}

Response:
{
  "revokedAt": "2025-11-22T16:00:00Z",
  "effectiveDate": "2025-12-01T00:00:00Z",
  "requesterNotified": true
}
```

---

## 5. Cryptographic Verification

### 5.1 Signing Metadata

**Algorithm:** Ed25519 (EdDSA)

**Process:**
1. Creator generates Ed25519 keypair
2. Public key stored in metadata, registry
3. Metadata hash signed with private key
4. Signature embedded in metadata

**Example (Python):**
```python
from cryptography.hazmat.primitives.asymmetric import ed25519
import hashlib
import json

# Generate keypair
private_key = ed25519.Ed25519PrivateKey.generate()
public_key = private_key.public_key()

# Create metadata
metadata = {
    "title": "My Novel",
    "creator": "Jane Doe",
    "license": "https://palimpsest.license/v0.4"
}

# Hash metadata
metadata_hash = hashlib.sha256(json.dumps(metadata, sort_keys=True).encode()).digest()

# Sign hash
signature = private_key.sign(metadata_hash)

# Embed signature
metadata["signature"] = {
    "algorithm": "Ed25519",
    "publicKey": public_key.public_bytes_raw().hex(),
    "value": signature.hex()
}
```

**Verification:**
```python
from cryptography.hazmat.primitives.asymmetric import ed25519

# Extract signature
sig_data = metadata.pop("signature")
public_key_bytes = bytes.fromhex(sig_data["publicKey"])
signature = bytes.fromhex(sig_data["value"])

# Recompute hash
metadata_hash = hashlib.sha256(json.dumps(metadata, sort_keys=True).encode()).digest()

# Verify
public_key = ed25519.Ed25519PublicKey.from_public_bytes(public_key_bytes)
try:
    public_key.verify(signature, metadata_hash)
    print("✓ Signature valid")
except:
    print("✗ Signature invalid")
```

---

### 5.2 Content Hashing

**Algorithm:** SHA-256

**Purpose:** Uniquely identify work, detect tampering.

**Process:**
```python
import hashlib

with open("my_novel.txt", "rb") as f:
    content = f.read()
    content_hash = hashlib.sha256(content).hexdigest()
    print(f"sha256:{content_hash}")
```

**Use in Registry:**
- Work registered with content hash
- Later verification: Recompute hash, compare
- If hashes don't match → content modified

---

### 5.3 Consent Receipt (Verifiable Credential)

**Format:** W3C Verifiable Credentials

```json
{
  "@context": [
    "https://www.w3.org/2018/credentials/v1",
    "https://consent-layer.org/ns/credentials/v1"
  ],
  "type": ["VerifiableCredential", "ConsentReceipt"],
  "issuer": "https://consent-registry.example.org",
  "issuanceDate": "2025-11-22T15:00:00Z",
  "credentialSubject": {
    "id": "did:example:jane123",
    "consentGiven": {
      "workId": "urn:uuid:a1b2c3d4",
      "requester": "OpenAI",
      "useType": "ai-training",
      "conditions": {...},
      "grantedAt": "2025-11-22T15:00:00Z"
    }
  },
  "proof": {
    "type": "Ed25519Signature2020",
    "created": "2025-11-22T15:00:00Z",
    "proofPurpose": "assertionMethod",
    "verificationMethod": "https://consent-registry.example.org/keys/1",
    "proofValue": "z58DAdFfa9SkqZMVPxAQpic7ndSayn1PzZs6ZjWp1CktyGesjuTSwRdoWhAfGFCF5bppETSTojQCrfFPP2oumHKtz"
  }
}
```

---

## 6. Platform Integration Patterns

### 6.1 Upload Flow (Adding Consent Metadata)

**User uploads work to platform (e.g., GitHub, DeviantArt):**

1. **Platform UI:**
   ```html
   <form>
     <input type="file" name="work">
     <label>License:</label>
     <select name="license">
       <option value="cc-by-4.0">CC BY 4.0</option>
       <option value="palimpsest-0.4">Palimpsest v0.4</option>
     </select>
     <label>Consent required for:</label>
     <input type="checkbox" name="consent[]" value="ai-training"> AI Training
     <input type="checkbox" name="consent[]" value="commercial"> Commercial Use
     <input type="email" name="consent_contact" placeholder="Your email">
   </form>
   ```

2. **Platform backend:**
   - Embeds metadata in file (EXIF, XMP, header comments)
   - Registers work in consent registry (optional)
   - Stores consent preferences in platform database

3. **Display:**
   - Show license badge on work page
   - Show "Consent required for: AI Training" notice
   - Link to consent request form

---

### 6.2 Download Flow (Preserving Metadata)

**User downloads work:**

1. **Platform ensures metadata travels:**
   - Images: EXIF/XMP preserved
   - PDFs: XMP preserved
   - Code: License headers intact

2. **HTTP headers included:**
   ```http
   HTTP/1.1 200 OK
   Content-Type: image/jpeg
   X-License: Palimpsest-0.4
   X-Consent-Required: ai-training
   X-Consent-Contact: creator@example.com
   ```

3. **README or sidecar file:**
   ```
   LICENSE.txt (alongside downloaded file):

   This work is licensed under Palimpsest v0.4
   Consent required for: AI Training
   Contact: creator@example.com
   Registry: https://consent-registry.example.org/work/123
   ```

---

### 6.3 Sharing Flow (Metadata Propagation)

**User shares work (e.g., retweet, embed):**

1. **Platform generates share card with metadata:**
   ```html
   <!-- Twitter Card -->
   <meta name="twitter:card" content="summary_large_image">
   <meta name="twitter:title" content="Artwork Title">
   <meta name="twitter:description" content="By Jane Doe">
   <meta name="twitter:image" content="https://example.com/image.jpg">
   <meta name="license" content="Palimpsest-0.4">
   <meta name="consent:required" content="ai-training">
   ```

2. **Embed code includes metadata:**
   ```html
   <iframe src="https://example.com/embed/work123" data-license="Palimpsest-0.4" data-consent-required="ai-training"></iframe>
   ```

3. **API responses include metadata:**
   ```json
   GET /api/works/123

   {
     "id": 123,
     "title": "My Artwork",
     "creator": "Jane Doe",
     "license": "https://palimpsest.license/v0.4",
     "consentRequired": ["ai-training"],
     "consentContact": "jane@example.com"
   }
   ```

---

### 6.4 API Access (Consent Verification)

**AI company scrapes platform:**

1. **Scraper checks metadata:**
   ```python
   import requests

   response = requests.get("https://example.com/work/123")
   license = response.headers.get("X-License")
   consent_required = response.headers.get("X-Consent-Required")

   if consent_required:
       print(f"Consent required for: {consent_required}")
       # Query consent registry or skip work
   ```

2. **Platform API enforces consent:**
   ```http
   GET /api/dataset/download
   Authorization: Bearer {api_key}

   Response (if no consent):
   HTTP/1.1 403 Forbidden
   Content-Type: application/json

   {
     "error": "Consent required for AI training",
     "consentRequired": ["ai-training"],
     "requestConsentAt": "https://consent-registry.example.org/request?work=123"
   }
   ```

3. **With consent token:**
   ```http
   GET /api/dataset/download
   Authorization: Bearer {api_key}
   X-Consent-Token: {consent_receipt_jwt}

   Response:
   HTTP/1.1 200 OK
   {dataset_content}
   ```

---

## 7. Security Considerations

### 7.1 Preventing Metadata Stripping

**Challenge:** Malicious actors remove metadata to avoid consent requirements.

**Mitigations:**
1. **Cryptographic binding:** Content hash in signed metadata (if file modified, signature invalid)
2. **Watermarking:** Embed invisible consent marker in content itself (steganography)
3. **Platform policies:** ToS prohibits metadata removal (violation → account suspension)
4. **Legal recourse:** Metadata stripping is license breach (DMCA 1202 in US, similar laws in EU)

---

### 7.2 Preventing Forgery

**Challenge:** Attacker claims to have consent when they don't.

**Mitigations:**
1. **Cryptographic signatures:** Consent receipts signed by registry (verifiable via public key)
2. **Timestamping:** Blockchain or RFC 3161 timestamps prove when consent granted
3. **Audit trails:** Consent registry logs all requests/grants (investigators can verify)

---

### 7.3 Privacy Preservation

**Challenge:** Public consent records reveal what AI companies are training on.

**Mitigations:**
1. **Selective disclosure:** Only work ID + consent status public; terms private (shared only with requester)
2. **Encrypted fields:** Requester identity encrypted, decryptable only by creator
3. **Zero-knowledge proofs:** Prove "consent exists" without revealing details

---

### 7.4 DDoS Protection

**Challenge:** Consent registry becomes single point of failure.

**Mitigations:**
1. **Rate limiting:** Max 100 requests/minute per IP
2. **CDN caching:** Cache consent status for 1 hour
3. **Federation:** Multiple registry providers (no single point of failure)
4. **Fallback:** If registry down, platforms cache last-known consent status

---

## 8. Interoperability

### 8.1 With Existing Licenses

**Palimpsest + Creative Commons:**
- Palimpsest is more restrictive (consent required)
- CC BY metadata preserved, Palimpsest metadata added
- Both licenses apply (most restrictive terms govern)

**Palimpsest + GPL:**
- Software code: GPL applies
- Documentation/creative assets: Palimpsest applies
- Dual licensing clarified in LICENSE file

---

### 8.2 With SPDX

**SPDX identifier:** `PALIMPSEST-0.4`

**In SPDX file:**
```spdx
SPDXVersion: SPDX-2.3
DataLicense: CC0-1.0
SPDXID: SPDXRef-DOCUMENT

PackageName: my-novel
SPDXID: SPDXRef-Package
PackageVersion: 1.0
PackageSupplier: Person: Jane Doe (jane@example.com)
PackageLicenseDeclared: PALIMPSEST-0.4
PackageLicenseConcluded: PALIMPSEST-0.4
FilesAnalyzed: true

FileName: ./my-novel.txt
SPDXID: SPDXRef-File1
LicenseConcluded: PALIMPSEST-0.4
LicenseInfoInFile: PALIMPSEST-0.4
```

---

### 8.3 With Dublin Core

```xml
<rdf:Description rdf:about="https://example.com/work/123">
  <dc:title>My Novel</dc:title>
  <dc:creator>Jane Doe</dc:creator>
  <dc:date>2025-11-22</dc:date>
  <dc:rights>Palimpsest License v0.4</dc:rights>
  <dcterms:license rdf:resource="https://palimpsest.license/v0.4"/>
  <consent:required xmlns:consent="https://consent-layer.org/ns/">ai-training</consent:required>
</rdf:Description>
```

---

## 9. Implementation Roadmap

### Phase 1: Metadata Standards (Months 1-6)
- Finalise JSON-LD schema
- Publish embedding guidelines (HTML, images, PDFs, code)
- Create validation tools (CLI, web service)
- Submit to W3C Community Group

### Phase 2: Consent Registry MVP (Months 7-12)
- Build centralised registry (PostgreSQL + REST API)
- Web dashboard for creators
- API documentation + client libraries (JS, Python, Rust)
- Beta testing with 100 creators

### Phase 3: Protocol Extensions (Months 13-18)
- HTTP header support (deploy in Nginx/Apache modules)
- DNS TXT record support (publish guidance)
- Well-known URI support (reference implementation)
- Submit IETF Internet-Draft

### Phase 4: Platform Integration (Months 19-24)
- Partner with 3 platforms (pilot integrations)
- Case studies + best practices documentation
- Developer SDKs for easy integration
- Public launch

---

## 10. Reference Implementations

**Metadata Validator:**
- **URL:** https://github.com/consent-layer/metadata-validator
- **Language:** TypeScript
- **Usage:** `npx consent-validate file.jpg`

**Consent Registry:**
- **URL:** https://github.com/consent-layer/registry
- **Language:** Rust (Actix-web framework)
- **Deployment:** Docker + Kubernetes

**Client Libraries:**
- **JavaScript:** https://npmjs.com/package/consent-layer
- **Python:** https://pypi.org/project/consent-layer
- **Rust:** https://crates.io/crates/consent-layer

---

## 11. Next Steps

**For Developers:**
1. Review this spec, provide feedback
2. Implement in your platform/tool
3. Join W3C Community Group
4. Contribute to reference implementations

**For Standards Bodies:**
1. Adopt metadata schema in relevant standards
2. Support protocol extension proposals (IETF, W3C)
3. Integrate with existing specs (SPDX, Dublin Core, Schema.org)

**Contact:**
- Spec questions: spec@consent-layer.org
- Implementation help: dev@consent-layer.org
- Standards liaisons: standards@consent-layer.org

---

**Document Version:** 0.9 (Draft)
**Date:** November 2025
**Authors:** Consent Layer Technical Working Group
**License:** CC0 1.0 (public domain—freely implementable)
