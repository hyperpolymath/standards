# Palimpsest License v0.4 — Compliance Roadmap

## A Step-by-Step Guide to Full Compliance

This roadmap guides creators, platforms, and organisations through achieving and maintaining full compliance with the Palimpsest License v0.4.

---

## Table of Contents

1. [Compliance Levels](#compliance-levels)
2. [Phase 1: Basic Compliance (Essential)](#phase-1-basic-compliance-essential)
3. [Phase 2: Standard Compliance (Recommended)](#phase-2-standard-compliance-recommended)
4. [Phase 3: Advanced Compliance (Future-Proof)](#phase-3-advanced-compliance-future-proof)
5. [Platform Compliance](#platform-compliance)
6. [AI System Compliance](#ai-system-compliance)
7. [Ongoing Maintenance](#ongoing-maintenance)
8. [Compliance Verification](#compliance-verification)
9. [Common Compliance Failures](#common-compliance-failures)
10. [Remediation Procedures](#remediation-procedures)

---

## Compliance Levels

The Palimpsest License has three compliance levels:

### Level 1: Basic (Minimum Legal Compliance)
✅ Legally valid license application
✅ Meets minimum attribution requirements
✅ Prevents most licence violations
⏱ Time to achieve: 1-2 hours

### Level 2: Standard (Recommended Best Practice)
✅ Everything in Level 1
✅ Full metadata implementation
✅ Cultural context preservation
✅ AI consent tracking
⏱ Time to achieve: 1-2 days

### Level 3: Advanced (Future-Proof & Traceable)
✅ Everything in Level 2
✅ Cryptographic traceability
✅ Blockchain registration
✅ Ambient attribution
✅ Automated compliance monitoring
⏱ Time to achieve: 1-2 weeks

---

## Phase 1: Basic Compliance (Essential)

**Goal**: Meet minimum legal requirements for Palimpsest License application.

**Who needs this**: Everyone applying the Palimpsest License.

### Step 1.1: Add LICENSE File

**Action**: Create a `LICENSE` or `LICENSE.txt` file in your project root.

**Content**:
```
Palimpsest License v0.4

Copyright [Year] [Your Legal Name or Organisation]

This work is licensed under the Palimpsest License version 0.4.

The full license text is available at:
https://github.com/yourorg/palimpsest-license/blob/main/LICENSES/v0.4/palimpsest-v0.4.md

For permissions beyond this license, contact: [your email]
```

**Verification**:
- [ ] LICENSE file exists in project root
- [ ] File contains "Palimpsest License v0.4"
- [ ] Copyright year and holder are correct
- [ ] Contact information is provided

---

### Step 1.2: Add SPDX Identifiers

**Action**: Add license identifiers to all source files.

**Python example**:
```python
# SPDX-License-Identifier: MPL-2.0-or-later
# Copyright 2025 Your Name
```

**JavaScript example**:
```javascript
// SPDX-License-Identifier: MPL-2.0-or-later
// Copyright 2025 Your Name
```

**Markdown/Documentation**:
```markdown
---
license: Palimpsest-0.4
copyright: 2025 Your Name
---
```

**Verification**:
- [ ] All source code files have SPDX headers
- [ ] Documentation files have license metadata
- [ ] Binary files have embedded or sidecar metadata

**Automated check**:
```bash
# Check Python files
grep -r "SPDX-License-Identifier: Palimpsest-0.4" --include="*.py" src/

# Check for missing headers
find src/ -name "*.py" -exec grep -L "SPDX-License-Identifier" {} \;
```

---

### Step 1.3: Minimum Attribution Notice

**Action**: Create a `NOTICE` or `ATTRIBUTION.md` file.

**Content**:
```markdown
# Attribution

This work is created by [Your Name] and licensed under the Palimpsest License v0.4.

## Author
- **Name**: [Your Legal Name or Pseudonym]
- **Contact**: [Email or website]
- **Year**: [Creation year]

## License
Palimpsest License v0.4
Full text: [link to license]

## Attribution Requirements
When using this work, you must provide:
1. Creator's name: [Your Name]
2. Work title: [Work Title]
3. License: Palimpsest License v0.4
4. Link to license: [URL]

Example attribution:
"[Work Title]" by [Your Name], licensed under Palimpsest License v0.4
```

**Verification**:
- [ ] Attribution file exists
- [ ] File specifies creator name
- [ ] File specifies license version
- [ ] Example attribution provided

---

### Step 1.4: README Integration

**Action**: Add license badge and notice to README.

**Markdown**:
```markdown
# Your Project Name

[![License: Palimpsest-0.4](https://img.shields.io/badge/License-Palimpsest%200.4-blue.svg)](https://palimpsest.license/v0.4)

[Project description]

## License

This project is licensed under the **Palimpsest License v0.4** — see the [LICENSE](LICENSE) file for details.

The Palimpsest License protects this work's cultural and emotional context, and requires explicit consent for interpretive AI use.

For questions or permissions: [your email]
```

**Verification**:
- [ ] README includes license badge
- [ ] README links to LICENSE file
- [ ] README mentions key license restrictions
- [ ] Contact method provided

---

### Phase 1 Checklist Summary

✅ **Essential Files Created**:
- [ ] `LICENSE` — Full license text
- [ ] `NOTICE` or `ATTRIBUTION.md` — Attribution requirements
- [ ] `README.md` — License badge and notice

✅ **All Source Files**:
- [ ] Have SPDX identifier headers
- [ ] Have copyright notice

✅ **Verification**:
- [ ] Run automated compliance check
- [ ] Manually verify license text is correct
- [ ] Test that attribution instructions are clear

**Estimated time**: 1-2 hours

**Result**: Your work is now legally licensed under Palimpsest v0.4 with minimum viable compliance.

---

## Phase 2: Standard Compliance (Recommended)

**Goal**: Full metadata implementation and cultural context preservation.

**Who needs this**: Anyone serious about protecting cultural/emotional context, or distributing through platforms.

### Step 2.1: Create Machine-Readable Metadata

**Action**: Create `.palimpsest.json` in project root.

**Template**:
```json
{
  "@context": "https://schema.org/",
  "@type": "CreativeWork",
  "@id": "urn:uuid:[generate-uuid]",
  "name": "Your Work Title",
  "description": "Brief description of the work",
  "author": {
    "@type": "Person",
    "name": "Your Name",
    "email": "your@email.com",
    "url": "https://yourwebsite.com"
  },
  "dateCreated": "2025-01-15",
  "dateModified": "2025-01-15",
  "license": {
    "@type": "CreativeWork",
    "name": "Palimpsest License v0.4",
    "url": "https://palimpsest.license/v0.4",
    "identifier": "Palimpsest-0.4"
  },
  "palimpsestVersion": "0.4",
  "emotionalLineage": {
    "culturalOrigins": "Describe cultural roots, traditions, or community connections",
    "emotionalIntent": "What you hope people feel or understand from this work",
    "historicalContext": "When and why this was created",
    "symbolicElements": [
      {
        "symbol": "Specific metaphor/image",
        "meaning": "Its cultural or emotional significance"
      }
    ]
  },
  "permissions": {
    "nonInterpretiveAI": true,
    "interpretiveAI": false,
    "interpretiveAIContact": "ai-requests@example.com",
    "commercialUse": true,
    "derivativeWorks": true,
    "shareAlike": false
  }
}
```

**How to generate UUID**:
```bash
# Linux/Mac
uuidgen

# Python
python3 -c "import uuid; print(f'urn:uuid:{uuid.uuid4()}')"

# Node.js
node -e "console.log('urn:uuid:' + require('crypto').randomUUID())"
```

**Verification**:
- [ ] `.palimpsest.json` exists
- [ ] JSON is valid (check with `python -m json.tool .palimpsest.json`)
- [ ] All required fields present
- [ ] UUID is unique
- [ ] Permissions accurately reflect your wishes

---

### Step 2.2: Document Cultural and Emotional Context

**Action**: Create `PALIMPSEST_CONTEXT.md` with detailed context.

**Template**:
```markdown
# Cultural and Emotional Context

## Work Information
- **Title**: [Work title]
- **Creator**: [Your name]
- **Created**: [Date or period]
- **Version**: [If applicable]

## Cultural Background

### Origins
[Describe the cultural traditions, communities, or heritage this work draws from]

Examples:
- "This work draws from Scottish Gaelic oral storytelling traditions"
- "Inspired by diaspora experiences of displacement and resilience"
- "Rooted in the collective memory of the [community] community"

### Historical Context
[What was happening when you created this? What inspired it?]

Examples:
- "Written during the 2020 pandemic as a meditation on isolation"
- "Created in response to [political event]"
- "Part of an ongoing exploration of [theme]"

## Emotional Intent

### What I Hope You Feel
[Describe the emotional experience you intend for audiences]

Examples:
- "A sense of quiet grief mixed with stubborn hope"
- "The weight of intergenerational trauma alongside moments of joy"
- "Disorientation that gradually transforms into understanding"

### What This Work Means to Me
[Personal significance — optional but encouraged]

## Symbolic Elements

### [Symbol/Metaphor 1]
**Appears**: [Where in the work]
**Represents**: [Cultural or emotional significance]

### [Symbol/Metaphor 2]
**Appears**: [Where in the work]
**Represents**: [Cultural or emotional significance]

## Appropriate Use

### Respectful Engagement
This work should be used in ways that:
- Honour the cultural context described above
- Preserve the emotional weight and complexity
- Credit not just me, but the broader traditions I draw from

### Discouraged Uses
Please avoid:
- Removing cultural context from derivatives
- Using in ways that trivialise the subject matter
- Exploiting the emotional content for purely commercial gain without proper attribution

## Permissions Beyond License

### AI Use
- **Non-Interpretive AI** (translation, accessibility): ✅ Permitted
- **Interpretive AI** (training, generation): ❌ Requires consent
- **Contact for AI permissions**: [email]

### Commercial Derivatives
- Allowed with proper attribution
- Revenue sharing appreciated but not required
- Contact me if you're creating significant commercial derivatives

## Community Acknowledgements
[If applicable: acknowledge communities, elders, or collective sources]

Example:
"While I am the legal author, this work stands on the shoulders of [community/tradition].
I acknowledge their collective wisdom and ongoing cultural stewardship."

---

**Questions or clarifications**: [your email]
**Last updated**: [date]
```

**Verification**:
- [ ] Context document exists
- [ ] Cultural origins described (if applicable)
- [ ] Emotional intent articulated
- [ ] Symbolic elements explained
- [ ] Appropriate use guidelines provided

---

### Step 2.3: Package Manager Integration

**Action**: Add Palimpsest metadata to package files.

**npm (package.json)**:
```json
{
  "license": "Palimpsest-0.4",
  "palimpsest": {
    "version": "0.4",
    "metadataFile": ".palimpsest.json",
    "contextFile": "PALIMPSEST_CONTEXT.md"
  },
  "files": [
    "dist",
    "LICENSE",
    ".palimpsest.json",
    "PALIMPSEST_CONTEXT.md"
  ]
}
```

**Python (pyproject.toml)**:
```toml
[project]
license = {text = "Palimpsest-0.4"}

[tool.palimpsest]
version = "0.4"
metadata-file = ".palimpsest.json"
context-file = "PALIMPSEST_CONTEXT.md"
```

**Rust (Cargo.toml)**:
```toml
[package]
license = "Palimpsest-0.4"

[package.metadata.palimpsest]
version = "0.4"
metadata-file = ".palimpsest.json"
```

**Verification**:
- [ ] Package manifest includes license identifier
- [ ] Metadata files included in distribution
- [ ] Files not accidentally excluded by `.gitignore` or `.npmignore`

---

### Step 2.4: Validate Metadata

**Action**: Use validation tools to ensure metadata correctness.

**JSON Schema validation**:
```bash
# Install validator
npm install -g ajv-cli

# Validate metadata
ajv validate -s palimpsest-schema.json -d .palimpsest.json
```

**Python validation script**:
```python
#!/usr/bin/env python3
import json
from pathlib import Path

def validate_palimpsest_metadata(metadata_path='.palimpsest.json'):
    """Basic validation of Palimpsest metadata."""
    required_fields = ['@type', 'name', 'author', 'license', 'permissions']

    if not Path(metadata_path).exists():
        print(f"❌ {metadata_path} not found")
        return False

    try:
        with open(metadata_path) as f:
            metadata = json.load(f)
    except json.JSONDecodeError as e:
        print(f"❌ Invalid JSON: {e}")
        return False

    errors = []

    # Check required fields
    for field in required_fields:
        if field not in metadata:
            errors.append(f"Missing required field: {field}")

    # Check license identifier
    if metadata.get('license', {}).get('identifier') != 'Palimpsest-0.4':
        errors.append("License identifier must be 'Palimpsest-0.4'")

    # Check permissions structure
    perms = metadata.get('permissions', {})
    if 'interpretiveAI' not in perms or 'nonInterpretiveAI' not in perms:
        errors.append("Permissions must specify interpretiveAI and nonInterpretiveAI")

    if errors:
        print("❌ Validation failed:")
        for error in errors:
            print(f"  - {error}")
        return False

    print("✅ Metadata validation passed")
    return True

if __name__ == '__main__':
    import sys
    success = validate_palimpsest_metadata()
    sys.exit(0 if success else 1)
```

**Verification**:
- [ ] Metadata passes JSON syntax check
- [ ] All required fields present
- [ ] License identifier correct
- [ ] Permissions specified

---

### Phase 2 Checklist Summary

✅ **Advanced Files Created**:
- [ ] `.palimpsest.json` — Machine-readable metadata
- [ ] `PALIMPSEST_CONTEXT.md` — Detailed cultural/emotional context

✅ **Package Integration**:
- [ ] Package manifest includes license
- [ ] Metadata files included in distribution
- [ ] Package description mentions cultural protection

✅ **Validation**:
- [ ] Metadata passes automated validation
- [ ] Emotional lineage is thoughtfully documented
- [ ] Permissions clearly specified

**Estimated time**: 1-2 days (including thoughtful context documentation)

**Result**: Full metadata compliance with cultural context preservation.

---

## Phase 3: Advanced Compliance (Future-Proof)

**Goal**: Cryptographic traceability, blockchain registration, and automated monitoring.

**Who needs this**: High-value works, institutions, works with significant cultural importance, or anyone wanting maximum future-proof protection.

### Step 3.1: Cryptographic Content Hashing

**Action**: Generate and store cryptographic hashes of your work.

**Generate SHA-256 hash**:
```bash
# Linux/Mac
sha256sum your-work.txt

# Or using Python
python3 << EOF
import hashlib
with open('your-work.txt', 'rb') as f:
    print(hashlib.sha256(f.read()).hexdigest())
EOF
```

**Add to metadata**:
```json
{
  "traceability": {
    "contentHash": {
      "algorithm": "sha256",
      "value": "a3d4f5e6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4",
      "timestamp": "2025-01-15T12:00:00Z",
      "files": [
        {
          "path": "main-work.md",
          "hash": "abc123..."
        },
        {
          "path": "supplementary.md",
          "hash": "def456..."
        }
      ]
    }
  }
}
```

**Verification**:
- [ ] Content hashes generated
- [ ] Hashes added to metadata
- [ ] Timestamp recorded
- [ ] Individual file hashes (for multi-file works)

---

### Step 3.2: Digital Signatures

**Action**: Cryptographically sign your metadata.

**Generate Ed25519 key pair** (do once, store securely):
```python
from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PrivateKey
from cryptography.hazmat.primitives import serialization

# Generate key pair
private_key = Ed25519PrivateKey.generate()
public_key = private_key.public_key()

# Save private key (KEEP THIS SECRET!)
with open('palimpsest_private_key.pem', 'wb') as f:
    f.write(private_key.private_bytes(
        encoding=serialization.Encoding.PEM,
        format=serialization.PrivateFormat.PKCS8,
        encryption_algorithm=serialization.NoEncryption()
    ))

# Save public key (include in metadata)
with open('palimpsest_public_key.pem', 'wb') as f:
    f.write(public_key.public_bytes(
        encoding=serialization.Encoding.PEM,
        format=serialization.PublicFormat.SubjectPublicKeyInfo
    ))

print("✅ Key pair generated")
print("⚠️  Store private key securely! Do not commit to git!")
```

**Sign metadata**:
```python
import json
import base64
from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PrivateKey
from cryptography.hazmat.primitives import serialization

# Load private key
with open('palimpsest_private_key.pem', 'rb') as f:
    private_key = serialization.load_pem_private_key(f.read(), password=None)

# Load metadata
with open('.palimpsest.json') as f:
    metadata = json.load(f)

# Sign metadata (canonical JSON)
metadata_bytes = json.dumps(metadata, sort_keys=True).encode('utf-8')
signature = private_key.sign(metadata_bytes)

# Add signature to metadata
metadata['cryptographicSignature'] = {
    'algorithm': 'ed25519',
    'signature': base64.b64encode(signature).decode('utf-8'),
    'publicKey': open('palimpsest_public_key.pem').read(),
    'signedAt': '2025-01-15T12:00:00Z'
}

# Save signed metadata
with open('.palimpsest.json', 'w') as f:
    json.dump(metadata, f, indent=2)

print("✅ Metadata signed")
```

**Verification**:
- [ ] Key pair generated and stored securely
- [ ] Metadata is signed
- [ ] Public key included in metadata
- [ ] Signature timestamp recorded
- [ ] Private key NOT in version control (add to `.gitignore`)

---

### Step 3.3: Blockchain Registration

**Action**: Register your work on a blockchain for immutable timestamping.

**Option A: Ethereum (via smart contract)**

```javascript
// register-on-ethereum.js
const { ethers } = require('ethers');
const fs = require('fs');

async function registerWork() {
  const provider = new ethers.JsonRpcProvider(process.env.ETH_RPC_URL);
  const wallet = new ethers.Wallet(process.env.PRIVATE_KEY, provider);

  // Palimpsest Registry contract (hypothetical)
  const contractAddress = '0x...';  // Official registry address
  const contractABI = [...];  // Registry ABI

  const contract = new ethers.Contract(contractAddress, contractABI, wallet);

  // Load metadata
  const metadata = JSON.parse(fs.readFileSync('.palimpsest.json', 'utf8'));
  const contentHash = metadata.traceability.contentHash.value;
  const metadataURI = `ipfs://${await uploadToIPFS(metadata)}`;

  // Register on blockchain
  const tx = await contract.registerWork(
    contentHash,
    metadataURI,
    metadata.author.name
  );

  const receipt = await tx.wait();
  console.log(`✅ Registered on Ethereum: ${receipt.hash}`);

  // Update metadata with blockchain info
  metadata.traceability.blockchainRegistry = {
    network: 'ethereum',
    transactionHash: receipt.hash,
    blockNumber: receipt.blockNumber,
    contractAddress: contractAddress,
    timestamp: new Date().toISOString()
  };

  fs.writeFileSync('.palimpsest.json', JSON.stringify(metadata, null, 2));
}

registerWork().catch(console.error);
```

**Option B: Simple Timestamp (Bitcoin/Ethereum)**

Use OpenTimestamps (free, no cost):
```bash
# Install OpenTimestamps
pip install opentimestamps-client

# Timestamp your content hash
ots stamp content-hash.txt

# This creates content-hash.txt.ots
# Upload this proof file alongside your work
```

**Option C: Arweave (Permanent Storage)**

```bash
# Install Arweave CLI
npm install -g arweave-deploy

# Deploy metadata to Arweave
arweave deploy .palimpsest.json --wallet-file wallet.json

# Returns: https://arweave.net/[transaction-id]
```

**Add to metadata**:
```json
{
  "traceability": {
    "blockchainRegistry": {
      "network": "ethereum",
      "transactionHash": "0x...",
      "blockNumber": 12345678,
      "explorerUrl": "https://etherscan.io/tx/0x..."
    },
    "arweaveId": "arweave-tx-id",
    "arweaveUrl": "https://arweave.net/[id]",
    "opentimestamp": {
      "proofFile": "content-hash.txt.ots",
      "attestationUrl": "https://opentimestamps.org/info?hash=[hash]"
    }
  }
}
```

**Verification**:
- [ ] Work registered on at least one blockchain
- [ ] Transaction hash recorded in metadata
- [ ] Proof files stored (if using OpenTimestamps)
- [ ] Blockchain registration is publicly verifiable

---

### Step 3.4: IPFS/Distributed Storage

**Action**: Store metadata on IPFS for censorship-resistant access.

**Upload to IPFS**:
```bash
# Using ipfs-http-client
npm install ipfs-http-client

node << EOF
const { create } = require('ipfs-http-client');
const fs = require('fs');

async function uploadToIPFS() {
  const client = create({ url: 'https://ipfs.infura.io:5001/api/v0' });
  const metadata = fs.readFileSync('.palimpsest.json');

  const { cid } = await client.add(metadata);
  console.log(\`✅ Uploaded to IPFS: \${cid}\`);
  console.log(\`Access at: https://ipfs.io/ipfs/\${cid}\`);

  // Update metadata file with IPFS hash
  const metadataObj = JSON.parse(metadata);
  metadataObj.traceability = metadataObj.traceability || {};
  metadataObj.traceability.ipfsHash = cid.toString();

  fs.writeFileSync('.palimpsest.json', JSON.stringify(metadataObj, null, 2));
}

uploadToIPFS().catch(console.error);
EOF
```

**Verification**:
- [ ] Metadata uploaded to IPFS
- [ ] IPFS CID recorded in metadata
- [ ] Metadata accessible via IPFS gateways
- [ ] Optional: Pin with Pinata or similar service

---

### Step 3.5: Ambient Attribution Implementation

**Action**: Embed attribution within the work itself, not just external metadata.

**For Images** (steganography or visible watermark):
```python
from PIL import Image, ImageDraw, ImageFont

def add_ambient_attribution(image_path, author, title):
    img = Image.open(image_path)
    draw = ImageDraw.Draw(img)
    font = ImageFont.truetype('arial.ttf', 12)

    attribution = f"© {author} | {title} | Palimpsest-0.4"

    # Add semi-transparent text at bottom
    text_bbox = draw.textbbox((0, 0), attribution, font=font)
    text_width = text_bbox[2] - text_bbox[0]

    x = img.width - text_width - 10
    y = img.height - 25

    draw.text((x, y), attribution, fill=(255, 255, 255, 200), font=font)

    return img
```

**For Audio** (embed in metadata + subtle audio watermark):
```python
from mutagen.mp3 import MP3
from mutagen.id3 import ID3, TXXX, COMM

def add_ambient_attribution_audio(audio_path, metadata):
    audio = MP3(audio_path, ID3=ID3)

    # Add detailed ID3 tags
    audio.tags.add(TXXX(encoding=3, desc='LICENSE', text='Palimpsest-0.4'))
    audio.tags.add(TXXX(encoding=3, desc='CULTURAL_CONTEXT',
                        text=metadata['emotionalLineage']['culturalOrigins']))
    audio.tags.add(COMM(encoding=3, lang='eng', desc='Emotional Intent',
                        text=metadata['emotionalLineage']['emotionalIntent']))

    audio.save()
```

**For Text/Documents** (metadata + footer):
```markdown
---

*This work is licensed under the Palimpsest License v0.4*
*Created by [Author] | [Cultural Context]*
*For context and attribution: see .palimpsest.json*
```

**For Web Content** (meta tags + visible credit):
```html
<head>
  <meta name="license" content="Palimpsest-0.4" />
  <meta name="author" content="Your Name" />
  <meta name="palimpsest:emotionalLineage" content="Cultural context description" />
  <link rel="license" href="https://palimpsest.license/v0.4" />
</head>

<footer>
  <p>
    © 2025 Your Name |
    Licensed under <a href="https://palimpsest.license/v0.4">Palimpsest License v0.4</a> |
    <a href=".palimpsest.json">Full metadata</a>
  </p>
</footer>
```

**Verification**:
- [ ] Attribution embedded in work itself
- [ ] Attribution survives basic transformations
- [ ] Cultural context preserved in embedded form
- [ ] Attribution is aesthetically appropriate (not intrusive)

---

### Step 3.6: Automated Compliance Monitoring

**Action**: Set up continuous integration to check compliance.

**GitHub Actions workflow**:
```yaml
# .github/workflows/palimpsest-compliance.yml
name: Palimpsest Compliance Check

on: [push, pull_request]

jobs:
  compliance:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Check LICENSE file
        run: |
          test -f LICENSE || (echo "❌ LICENSE file missing" && exit 1)
          grep -q "Palimpsest License v0.4" LICENSE || \
            (echo "❌ LICENSE file doesn't contain Palimpsest v0.4" && exit 1)

      - name: Validate metadata
        run: |
          test -f .palimpsest.json || (echo "❌ .palimpsest.json missing" && exit 1)
          python3 -m json.tool .palimpsest.json > /dev/null || \
            (echo "❌ .palimpsest.json is invalid JSON" && exit 1)

      - name: Check SPDX headers
        run: |
          missing=$(find src -name "*.py" -exec grep -L "SPDX-License-Identifier: Palimpsest-0.4" {} \;)
          if [ -n "$missing" ]; then
            echo "❌ Files missing SPDX headers:"
            echo "$missing"
            exit 1
          fi

      - name: Validate metadata schema
        run: |
          pip install jsonschema
          python3 scripts/validate-palimpsest-metadata.py

      - name: Check content hashes
        run: |
          python3 scripts/verify-content-hashes.py

      - name: Report compliance
        run: echo "✅ Palimpsest compliance check passed"
```

**Verification**:
- [ ] CI pipeline includes compliance checks
- [ ] Checks run on every commit
- [ ] Failures block merging
- [ ] Compliance status visible in repository

---

### Phase 3 Checklist Summary

✅ **Cryptographic Protection**:
- [ ] Content hashes generated and recorded
- [ ] Metadata digitally signed
- [ ] Public key published

✅ **Blockchain Registration**:
- [ ] Registered on blockchain (Ethereum/Bitcoin/Arweave)
- [ ] Transaction hash recorded
- [ ] Registration publicly verifiable

✅ **Distributed Storage**:
- [ ] Metadata on IPFS
- [ ] IPFS hash recorded
- [ ] Optionally pinned for permanence

✅ **Ambient Attribution**:
- [ ] Attribution embedded in works
- [ ] Survives format transformations
- [ ] Aesthetically integrated

✅ **Automation**:
- [ ] CI pipeline validates compliance
- [ ] Automated hash verification
- [ ] Continuous monitoring

**Estimated time**: 1-2 weeks (including blockchain registration and automation setup)

**Result**: Maximum future-proof protection with cryptographic traceability and automated monitoring.

---

## Platform Compliance

### For Content Platforms (YouTube, Medium, etc.)

**Step 1**: Add to description/bio:
```
Licensed under Palimpsest License v0.4
© 2025 [Your Name]
Cultural Context: [Brief description]
AI Training: Prohibited without consent
Full license: [link]
```

**Step 2**: Include in video/audio:
- Opening or closing credit mentioning license
- Verbal acknowledgment in introduction
- Visual overlay with license information

**Step 3**: Metadata:
- Set copyright holder correctly
- Add license info to platform-specific fields
- Link to full metadata (GitHub, personal site)

---

### For Code Repositories (GitHub, GitLab)

**Step 1**: Repository settings:
- License: Choose "Other" or "Proprietary" (until SPDX recognises Palimpsest)
- Add license badge to README
- Enable branch protection to require compliance checks

**Step 2**: Files:
- All files from Phase 1 & 2
- CI/CD compliance validation
- Pre-commit hooks for SPDX headers

**Step 3**: Documentation:
- Contributing guide mentions license
- Pull request template includes compliance checklist
- Issue templates reference license requirements

---

### For Package Registries (npm, PyPI, crates.io)

**Step 1**: Package manifest compliance (see Phase 2.3)

**Step 2**: Distribution:
- Include LICENSE in package
- Include .palimpsest.json
- Include context documentation
- Ensure metadata not stripped during build

**Step 3**: Registry listing:
- Set license field correctly
- Add detailed description mentioning cultural protection
- Link to full documentation

---

## AI System Compliance

### For AI Developers Using Palimpsest-Licensed Training Data

**Step 1: Consent Verification**
- [ ] Check `.palimpsest.json` for `permissions.interpretiveAI`
- [ ] If `false`, contact creator for consent
- [ ] Obtain written consent before including in dataset
- [ ] Document consent in training data manifest

**Step 2: Attribution in Outputs**
- [ ] Include source attribution in generated content
- [ ] Preserve cultural context in metadata
- [ ] Provide way to trace generated content back to sources

**Step 3: Metadata Preservation**
- [ ] Store full Palimpsest metadata for each work
- [ ] Link generated outputs to source works
- [ ] Provide API to query source attributions

---

### For AI Users Creating Content

**Step 1: Disclosure**
- [ ] Disclose AI involvement in your own metadata
- [ ] Credit AI tool and any training sources (if known)
- [ ] Maintain human creative control

**Step 2: Attribution**
- [ ] Follow attribution requirements of any Palimpsest works used as prompts
- [ ] Include cultural context if AI was trained on contextual works

---

## Ongoing Maintenance

### Annual Review

**Every 12 months**:
- [ ] Review and update `.palimpsest.json` if work has changed
- [ ] Update copyright year if applicable
- [ ] Check for license version updates
- [ ] Verify blockchain registrations still accessible
- [ ] Refresh IPFS pins
- [ ] Review and update cultural context if understanding has evolved

---

### When Making Changes

**Before each release/version**:
- [ ] Update `dateModified` in metadata
- [ ] Regenerate content hashes
- [ ] Re-sign metadata (if using digital signatures)
- [ ] Update blockchain registration (or create new one)
- [ ] Run compliance validation

---

### License Version Migrations

**If Palimpsest releases v0.5 or v1.0**:
- [ ] Review changelog for breaking changes
- [ ] Assess if migration is beneficial
- [ ] Update LICENSE file
- [ ] Update SPDX identifiers
- [ ] Update `.palimpsest.json` version field
- [ ] Notify users/downstream consumers
- [ ] Consider dual-licensing during transition

---

## Compliance Verification

### Self-Assessment Checklist

**Level 1: Basic (Minimum Viable)**
- [ ] LICENSE file exists and is correct
- [ ] SPDX headers in all source files
- [ ] README mentions license
- [ ] Attribution requirements documented
- [ ] Contact information provided

**Level 2: Standard (Recommended)**
- [ ] All Level 1 requirements
- [ ] `.palimpsest.json` metadata complete and valid
- [ ] Cultural/emotional context documented
- [ ] Permissions clearly specified (AI use, commercial, etc.)
- [ ] Package manifests include license

**Level 3: Advanced (Future-Proof)**
- [ ] All Level 2 requirements
- [ ] Content cryptographically hashed
- [ ] Metadata digitally signed
- [ ] Blockchain registered
- [ ] IPFS distributed
- [ ] Ambient attribution implemented
- [ ] Automated compliance monitoring

---

### Third-Party Audit

**Request audit from**:
- Palimpsest Stewardship Council
- IP lawyers familiar with cultural licensing
- Digital rights organisations
- Community peer review

**Audit covers**:
- Legal validity of license application
- Completeness of metadata
- Effectiveness of cultural context documentation
- Technical implementation quality
- Blockchain/cryptographic verification

---

## Common Compliance Failures

### Failure 1: Metadata Stripped During Distribution

**Problem**: Build process removes `.palimpsest.json`

**Detection**:
```bash
# Check if metadata included in build
npm pack --dry-run | grep palimpsest
```

**Fix**:
```json
// package.json
{
  "files": [
    "dist",
    "LICENSE",
    ".palimpsest.json",  // ← Explicitly include
    "PALIMPSEST_CONTEXT.md"
  ]
}
```

---

### Failure 2: Missing SPDX Headers

**Problem**: Some files lack license identifiers

**Detection**:
```bash
find src -name "*.py" -exec grep -L "SPDX-License-Identifier" {} \;
```

**Fix**: Add pre-commit hook:
```bash
#!/bin/bash
# .git/hooks/pre-commit

files=$(git diff --cached --name-only --diff-filter=ACM | grep '\.py$')
for file in $files; do
  if ! grep -q "SPDX-License-Identifier: Palimpsest-0.4" "$file"; then
    echo "❌ $file missing SPDX header"
    exit 1
  fi
done
```

---

### Failure 3: Invalid Metadata JSON

**Problem**: `.palimpsest.json` has syntax errors

**Detection**:
```bash
python3 -m json.tool .palimpsest.json
```

**Fix**: Use a JSON validator in your editor (VS Code, etc.) or CI pipeline

---

### Failure 4: Ambiguous AI Permissions

**Problem**: Metadata doesn't clearly specify AI permissions

**Detection**: Check metadata has:
```json
{
  "permissions": {
    "nonInterpretiveAI": true,
    "interpretiveAI": false
  }
}
```

**Fix**: Explicitly set both fields, even if one seems "obvious"

---

### Failure 5: Lost Cultural Context

**Problem**: Derivatives remove or simplify cultural context

**Detection**: Review derivative works for:
- [ ] Attribution includes cultural origins
- [ ] Emotional intent preserved
- [ ] Symbolic elements explained

**Fix**: Add explicit guidelines in `PALIMPSEST_CONTEXT.md`:
```markdown
## Non-Negotiable Elements

When creating derivatives, you MUST preserve:
1. [Specific cultural reference]
2. [Specific emotional context]
3. [Specific symbolic element]
```

---

## Remediation Procedures

### If You Discover Non-Compliance

**Step 1: Assess severity**
- **Critical**: No LICENSE file, completely wrong license
- **Major**: Missing metadata, incorrect SPDX identifiers
- **Minor**: Typos, incomplete context documentation

**Step 2: Remediate**
- Fix the issue immediately
- Commit with message: "fix: Palimpsest license compliance - [issue]"
- Tag release if necessary: `v1.0.1-compliance-fix`

**Step 3: Notify**
- If publicly distributed: issue announcement
- If on package registry: publish patched version
- If downstream users: contact major users

**Step 4: Prevent recurrence**
- Add to CI/CD validation
- Update checklists
- Add pre-commit hooks

---

### If Someone Else Is Non-Compliant With Your Work

**See**: User Guide section on "Enforcement and Dispute Resolution"

**Quick reference**:
1. Document the violation
2. Contact violator (friendly first)
3. Request specific remediation
4. Set reasonable deadline (7-14 days)
5. Escalate to formal notice if needed
6. Legal action as last resort

---

## Conclusion

Palimpsest License compliance is a journey, not a destination. Start with Phase 1 (Basic) to get legally compliant, progress to Phase 2 (Standard) for full metadata and cultural protection, and advance to Phase 3 (Advanced) for maximum future-proof protection.

**Remember**: The license exists to protect meaning, not just content. Take time to thoughtfully document your work's cultural and emotional context — that's where Palimpsest's real protection lies.

---

**Document Version**: 1.0
**Last Updated**: 2025-11-22
**License**: This guide is licensed under CC BY-SA 4.0
**Maintainer**: Palimpsest Stewardship Council
