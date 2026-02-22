# Palimpsest License v0.4  Developer Guide

## Technical Integration and Implementation

This guide provides developers, platform engineers, and technical teams with practical guidance for integrating and implementing Palimpsest License compliance in software projects, platforms, and digital systems.

---

## Table of Contents

1. [Overview for Developers](#overview-for-developers)
2. [License Identifier and SPDX](#license-identifier-and-spdx)
3. [Metadata Formats and Schemas](#metadata-formats-and-schemas)
4. [Integration Patterns](#integration-patterns)
5. [Platform-Specific Implementation](#platform-specific-implementation)
6. [API and Automation](#api-and-automation)
7. [Cryptographic Traceability](#cryptographic-traceability)
8. [AI System Integration](#ai-system-integration)
9. [Validation and Compliance Checking](#validation-and-compliance-checking)
10. [Performance Considerations](#performance-considerations)
11. [Testing and Quality Assurance](#testing-and-quality-assurance)
12. [Common Implementation Patterns](#common-implementation-patterns)
13. [Troubleshooting](#troubleshooting)

---

## Overview for Developers

### What Makes Palimpsest Different?

Unlike purely technical licenses (MIT, Apache) or creative licenses (CC), the Palimpsest License requires:

1. **Metadata preservation**: Cultural and emotional context must survive transformations
2. **AI consent tracking**: Systems must distinguish between permitted and prohibited AI uses
3. **Cryptographic attribution**: Future-proof traceability mechanisms
4. **Ambient attribution support**: Credits embedded within works, not just external metadata
5. **Emotional lineage preservation**: Context must accompany content

### Core Technical Requirements

**Minimum viable implementation:**
- License text inclusion (`LICENSE` file)
- SPDX identifier in headers
- Basic attribution metadata (JSON-LD or equivalent)

**Production-ready implementation:**
- Machine-readable metadata schema
- Cryptographic hashing for content verification
- AI usage consent flags
- Metadata preservation in transformations
- Validation and compliance checking

**Advanced implementation:**
- Blockchain/IPFS registration
- Quantum-resistant cryptography
- Ambient attribution rendering
- Automated compliance monitoring
- DAO governance integration

---

## License Identifier and SPDX

### Official Identifier

**SPDX Short Identifier**: `Palimpsest-0.4`

**SPDX License Expression**: `Palimpsest-0.4`

**License URL**: `https://palimpsest.license/v0.4` (when deployed)

### File Headers

**Python:**
```python
# SPDX-License-Identifier: MPL-2.0-or-later
# Copyright 2025 Your Name
# This file is part of [Project Name], licensed under Palimpsest v0.4
```

**JavaScript/TypeScript:**
```javascript
// SPDX-License-Identifier: MPL-2.0-or-later
// Copyright 2025 Your Name
// Licensed under the Palimpsest License v0.4
```

**Rust:**
```rust
// SPDX-License-Identifier: MPL-2.0-or-later
// Copyright 2025 Your Name
```

**HTML/XML:**
```html
<!-- SPDX-License-Identifier: Palimpsest-0.4 -->
<!-- Copyright 2025 Your Name -->
```

**Markdown:**
```markdown
---
license: Palimpsest-0.4
copyright: 2025 Your Name
---
```

### Package Manager Integration

**package.json (npm):**
```json
{
  "name": "your-package",
  "version": "1.0.0",
  "license": "Palimpsest-0.4",
  "author": "Your Name <email@example.com>",
  "palimpsest": {
    "version": "0.4",
    "metadata": ".palimpsest.json",
    "emotionalLineage": "Brief cultural/emotional context"
  }
}
```

**Cargo.toml (Rust):**
```toml
[package]
name = "your-package"
version = "0.1.0"
license = "Palimpsest-0.4"
authors = ["Your Name <email@example.com>"]

[package.metadata.palimpsest]
version = "0.4"
metadata-file = ".palimpsest.json"
```

**pyproject.toml (Python):**
```toml
[project]
name = "your-package"
version = "0.1.0"
license = {text = "Palimpsest-0.4"}
authors = [{name = "Your Name", email = "email@example.com"}]

[tool.palimpsest]
version = "0.4"
metadata = ".palimpsest.json"
emotional-lineage = "Brief cultural/emotional context"
```

---

## Metadata Formats and Schemas

### Core Metadata Schema (JSON-LD)

**`.palimpsest.json`**  Canonical machine-readable metadata:

```json
{
  "@context": {
    "@vocab": "https://schema.org/",
    "palimpsest": "https://palimpsest.license/schema/v0.4#"
  },
  "@type": "CreativeWork",
  "@id": "urn:uuid:550e8400-e29b-41d4-a716-446655440000",
  "name": "Work Title",
  "author": {
    "@type": "Person",
    "name": "Creator Name",
    "email": "creator@example.com",
    "url": "https://creator-website.com"
  },
  "dateCreated": "2025-01-15T12:00:00Z",
  "dateModified": "2025-01-20T14:30:00Z",
  "license": {
    "@type": "CreativeWork",
    "name": "Palimpsest License v0.4",
    "url": "https://palimpsest.license/v0.4",
    "identifier": "Palimpsest-0.4"
  },
  "palimpsest:emotionalLineage": {
    "@type": "palimpsest:EmotionalContext",
    "culturalOrigins": "Description of cultural context and roots",
    "emotionalIntent": "What the creator hopes to convey",
    "historicalContext": "When and why this was created",
    "symbolicElements": [
      {
        "symbol": "Specific metaphor or image",
        "meaning": "Cultural or emotional significance"
      }
    ]
  },
  "palimpsest:permissions": {
    "@type": "palimpsest:PermissionSet",
    "nonInterpretiveAI": true,
    "interpretiveAI": false,
    "interpretiveAIContact": "ai-requests@example.com",
    "commercialUse": true,
    "derivativeWorks": true,
    "shareAlike": false
  },
  "palimpsest:traceability": {
    "@type": "palimpsest:TraceabilityData",
    "contentHash": {
      "algorithm": "sha256",
      "value": "a3d4f5e6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4"
    },
    "cryptographicSignature": {
      "algorithm": "ed25519",
      "publicKey": "...",
      "signature": "..."
    },
    "blockchainRegistry": {
      "network": "ethereum",
      "contractAddress": "0x...",
      "tokenId": "12345",
      "transactionHash": "0x..."
    },
    "ipfsHash": "QmYwAPJzv5CZsnA625s3Xf2nemtYgPpHdWEz79ojWnPbdG",
    "arweaveId": "arweave-tx-id"
  },
  "palimpsest:attribution": {
    "@type": "palimpsest:AttributionRequirements",
    "minimumAttribution": "Creator Name, Work Title, Palimpsest-0.4",
    "preferredAttribution": "Full text of preferred attribution",
    "ambientAttributionGuidelines": "How to embed credits within the work itself"
  }
}
```

### Minimal Metadata (For Simple Projects)

```json
{
  "@context": "https://schema.org/",
  "@type": "CreativeWork",
  "name": "Work Title",
  "author": "Creator Name",
  "license": "Palimpsest-0.4",
  "dateCreated": "2025-01-15",
  "palimpsestVersion": "0.4",
  "emotionalContext": "Brief description",
  "aiPermissions": {
    "nonInterpretive": true,
    "interpretive": false
  }
}
```

### Alternative Formats

**YAML (`.palimpsest.yml`):**
```yaml
---
context: https://schema.org/
type: CreativeWork
id: urn:uuid:550e8400-e29b-41d4-a716-446655440000
name: Work Title
author:
  type: Person
  name: Creator Name
  email: creator@example.com
license:
  name: Palimpsest License v0.4
  identifier: Palimpsest-0.4
  url: https://palimpsest.license/v0.4
emotionalLineage:
  culturalOrigins: Description of cultural context
  emotionalIntent: What the creator hopes to convey
permissions:
  nonInterpretiveAI: true
  interpretiveAI: false
  commercialUse: true
traceability:
  contentHash:
    algorithm: sha256
    value: a3d4f5e6b7c8...
```

**RDF/Turtle (`.palimpsest.ttl`):**
```turtle
@prefix schema: <https://schema.org/> .
@prefix palimpsest: <https://palimpsest.license/schema/v0.4#> .

<urn:uuid:550e8400-e29b-41d4-a716-446655440000>
  a schema:CreativeWork ;
  schema:name "Work Title" ;
  schema:author [
    a schema:Person ;
    schema:name "Creator Name" ;
    schema:email "creator@example.com"
  ] ;
  schema:license <https://palimpsest.license/v0.4> ;
  palimpsest:emotionalLineage [
    a palimpsest:EmotionalContext ;
    palimpsest:culturalOrigins "Description" ;
    palimpsest:emotionalIntent "Intent"
  ] .
```

---

## Integration Patterns

### Pattern 1: Embedded Metadata (Single File)

For standalone files (images, audio, documents):

**Example: JPEG with EXIF/XMP metadata:**
```xml
<!-- XMP metadata embedded in JPEG -->
<rdf:RDF xmlns:rdf="https://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns:palimpsest="https://palimpsest.license/schema/v0.4#">
  <rdf:Description>
    <palimpsest:license>Palimpsest-0.4</palimpsest:license>
    <palimpsest:author>Creator Name</palimpsest:author>
    <palimpsest:emotionalContext>Cultural context description</palimpsest:emotionalContext>
  </rdf:Description>
</rdf:RDF>
```

**Example: MP3 with ID3v2 tags:**
```python
from mutagen.id3 import ID3, TXXX, COMM

audio = ID3("song.mp3")
audio.add(TXXX(encoding=3, desc='LICENSE', text='Palimpsest-0.4'))
audio.add(TXXX(encoding=3, desc='EMOTIONAL_LINEAGE', text='Cultural context'))
audio.add(COMM(encoding=3, lang='eng', desc='Palimpsest Metadata',
               text='See .palimpsest.json for full metadata'))
audio.save()
```

### Pattern 2: Sidecar Metadata

For projects with multiple files:

```
project/
   LICENSE                      # License text
   .palimpsest.json            # Machine-readable metadata
   PALIMPSEST_ATTRIBUTION.md   # Human-readable context
   src/
      main.py
      utils.py
   assets/
       image.png
       image.png.palimpsest.json  # Per-file metadata
```

### Pattern 3: Database-Backed Metadata

For platforms managing many works:

**PostgreSQL schema:**
```sql
CREATE TABLE palimpsest_works (
    id UUID PRIMARY KEY,
    title TEXT NOT NULL,
    author TEXT NOT NULL,
    license_version TEXT DEFAULT '0.4',
    created_at TIMESTAMP WITH TIME ZONE,
    modified_at TIMESTAMP WITH TIME ZONE,
    emotional_lineage JSONB,
    permissions JSONB,
    content_hash TEXT,
    metadata JSONB
);

CREATE INDEX idx_license ON palimpsest_works(license_version);
CREATE INDEX idx_content_hash ON palimpsest_works(content_hash);
CREATE INDEX idx_permissions ON palimpsest_works USING gin(permissions);
```

**Example query:**
```sql
-- Find works that allow interpretive AI
SELECT id, title, author FROM palimpsest_works
WHERE permissions->>'interpretiveAI' = 'true';

-- Find works with specific cultural context
SELECT id, title FROM palimpsest_works
WHERE emotional_lineage->>'culturalOrigins' ILIKE '%diaspora%';
```

### Pattern 4: API-First Metadata

For web services and APIs:

**REST API response:**
```json
{
  "work": {
    "id": "550e8400-e29b-41d4-a716-446655440000",
    "title": "Work Title",
    "content": "...",
    "metadata": {
      "license": {
        "identifier": "Palimpsest-0.4",
        "url": "https://palimpsest.license/v0.4",
        "text": "https://api.example.com/licenses/palimpsest-0.4.txt"
      },
      "author": {
        "name": "Creator Name",
        "url": "https://creator-website.com"
      },
      "emotionalLineage": {
        "culturalOrigins": "...",
        "emotionalIntent": "..."
      },
      "permissions": {
        "nonInterpretiveAI": true,
        "interpretiveAI": false,
        "aiConsentUrl": "https://api.example.com/works/550e8400/ai-consent-request"
      }
    }
  }
}
```

---

## Platform-Specific Implementation

### GitHub / GitLab

**Repository structure:**
```
repo/
   LICENSE                    # Palimpsest license text
   .palimpsest.json          # Metadata
   README.md                 # Include badge and attribution
   PALIMPSEST_ATTRIBUTION.md # Human-readable context
   .github/
       workflows/
           palimpsest-validation.yml  # CI validation
```

**GitHub Actions workflow:**
```yaml
name: Palimpsest Compliance Check

on: [push, pull_request]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Validate Palimpsest metadata
        run: |
          # Check LICENSE file exists
          test -f LICENSE || (echo "LICENSE file missing" && exit 1)

          # Validate JSON metadata
          if [ -f .palimpsest.json ]; then
            python -m json.tool .palimpsest.json > /dev/null
          fi

          # Check SPDX headers (example for Python files)
          ! grep -r --include="*.py" -L "SPDX-License-Identifier: Palimpsest-0.4" src/
```

### NPM Package

**package.json with full metadata:**
```json
{
  "name": "@yourorg/package-name",
  "version": "1.0.0",
  "description": "Package description",
  "license": "Palimpsest-0.4",
  "author": {
    "name": "Your Name",
    "email": "you@example.com",
    "url": "https://yourwebsite.com"
  },
  "palimpsest": {
    "version": "0.4",
    "metadataFile": ".palimpsest.json",
    "emotionalLineage": "Cultural/emotional context",
    "permissions": {
      "nonInterpretiveAI": true,
      "interpretiveAI": false,
      "aiContactEmail": "ai-requests@example.com"
    }
  },
  "files": [
    "dist",
    "LICENSE",
    ".palimpsest.json",
    "PALIMPSEST_ATTRIBUTION.md"
  ]
}
```

**Post-install script to preserve attribution:**
```javascript
// scripts/postinstall.js
const fs = require('fs');
const path = require('path');

const metadataPath = path.join(__dirname, '..', '.palimpsest.json');
if (fs.existsSync(metadataPath)) {
  console.log('\n=== Palimpsest License Notice ===');
  const metadata = JSON.parse(fs.readFileSync(metadataPath, 'utf8'));
  console.log(`This package is licensed under Palimpsest License v0.4`);
  console.log(`Author: ${metadata.author.name}`);
  if (metadata['palimpsest:emotionalLineage']) {
    console.log(`Cultural Context: ${metadata['palimpsest:emotionalLineage'].culturalOrigins}`);
  }
  console.log('================================\n');
}
```

### Python Package (PyPI)

**setup.py or pyproject.toml:**
```python
# setup.py
setup(
    name='your-package',
    version='0.1.0',
    license='Palimpsest-0.4',
    author='Your Name',
    author_email='you@example.com',
    description='Package description',
    long_description=open('README.md').read(),
    long_description_content_type='text/markdown',
    package_data={
        '': ['LICENSE', '.palimpsest.json', 'PALIMPSEST_ATTRIBUTION.md']
    },
    classifiers=[
        'License :: Other/Proprietary License',  # Until SPDX recognises Palimpsest
    ]
)
```

**Runtime metadata access:**
```python
# your_package/__init__.py
import json
from pathlib import Path

def get_palimpsest_metadata():
    """Load Palimpsest metadata for this package."""
    metadata_path = Path(__file__).parent / '.palimpsest.json'
    if metadata_path.exists():
        return json.loads(metadata_path.read_text())
    return None

def print_attribution():
    """Display Palimpsest attribution notice."""
    metadata = get_palimpsest_metadata()
    if metadata:
        print(f"Licensed under Palimpsest v{metadata['palimpsest:version']}")
        print(f"Author: {metadata['author']['name']}")
        if 'palimpsest:emotionalLineage' in metadata:
            context = metadata['palimpsest:emotionalLineage']
            print(f"Cultural Context: {context.get('culturalOrigins', 'N/A')}")
```

### Docker Containers

**Dockerfile with metadata:**
```dockerfile
FROM node:18-alpine

LABEL org.opencontainers.image.licenses="Palimpsest-0.4"
LABEL org.opencontainers.image.authors="Your Name <you@example.com>"
LABEL org.palimpsest.version="0.4"
LABEL org.palimpsest.emotional-lineage="Cultural context description"

WORKDIR /app

COPY LICENSE .palimpsest.json PALIMPSEST_ATTRIBUTION.md ./
COPY package*.json ./
RUN npm ci --only=production

COPY . .

CMD ["node", "index.js"]
```

**Runtime attribution display:**
```javascript
// Display attribution on container startup
console.log('='.repeat(50));
console.log('Palimpsest Licensed Software');
console.log('License: Palimpsest-0.4');
console.log('See /app/PALIMPSEST_ATTRIBUTION.md for full context');
console.log('='.repeat(50));
```

### WordPress Plugin

**Plugin header:**
```php
<?php
/**
 * Plugin Name: Your Plugin Name
 * Description: Plugin description
 * Version: 1.0.0
 * Author: Your Name
 * License: Palimpsest-0.4
 * License URI: https://palimpsest.license/v0.4
 * Palimpsest Emotional Context: Brief cultural context
 */

// Load Palimpsest metadata
function yourplugin_get_metadata() {
    $metadata_file = plugin_dir_path(__FILE__) . '.palimpsest.json';
    if (file_exists($metadata_file)) {
        return json_decode(file_get_contents($metadata_file), true);
    }
    return null;
}

// Display attribution in admin
add_action('admin_footer', function() {
    $metadata = yourplugin_get_metadata();
    if ($metadata) {
        echo '<div style="margin-top: 20px; padding: 10px; background: #f0f0f0;">';
        echo '<strong>Palimpsest License Notice:</strong> ';
        echo 'This plugin is licensed under Palimpsest v0.4. ';
        echo 'Cultural Context: ' . esc_html($metadata['palimpsest:emotionalLineage']['culturalOrigins']);
        echo '</div>';
    }
});
```

---

## API and Automation

### Metadata Validation API

**Node.js validation library:**
```javascript
// palimpsest-validator.js
const Ajv = require('ajv');
const ajv = new Ajv();

const palimpsestSchema = {
  type: 'object',
  required: ['@context', '@type', 'name', 'author', 'license'],
  properties: {
    '@context': { type: ['string', 'object'] },
    '@type': { const: 'CreativeWork' },
    'name': { type: 'string', minLength: 1 },
    'author': {
      type: 'object',
      required: ['name'],
      properties: {
        name: { type: 'string' },
        email: { type: 'string', format: 'email' }
      }
    },
    'license': {
      type: 'object',
      properties: {
        identifier: { const: 'Palimpsest-0.4' }
      }
    },
    'palimpsest:emotionalLineage': {
      type: 'object',
      properties: {
        culturalOrigins: { type: 'string' },
        emotionalIntent: { type: 'string' }
      }
    },
    'palimpsest:permissions': {
      type: 'object',
      required: ['nonInterpretiveAI', 'interpretiveAI'],
      properties: {
        nonInterpretiveAI: { type: 'boolean' },
        interpretiveAI: { type: 'boolean' }
      }
    }
  }
};

const validate = ajv.compile(palimpsestSchema);

function validatePalimpsestMetadata(metadata) {
  const valid = validate(metadata);
  if (!valid) {
    return {
      valid: false,
      errors: validate.errors
    };
  }
  return { valid: true };
}

module.exports = { validatePalimpsestMetadata };
```

### CLI Tool

**palimpsest-cli:**
```bash
#!/usr/bin/env node
const fs = require('fs');
const { validatePalimpsestMetadata } = require('./palimpsest-validator');

const command = process.argv[2];

switch (command) {
  case 'init':
    // Create template .palimpsest.json
    const template = {
      '@context': 'https://schema.org/',
      '@type': 'CreativeWork',
      'name': 'Your Work Title',
      'author': { 'name': 'Your Name' },
      'license': { 'identifier': 'Palimpsest-0.4' }
    };
    fs.writeFileSync('.palimpsest.json', JSON.stringify(template, null, 2));
    console.log('Created .palimpsest.json template');
    break;

  case 'validate':
    const metadata = JSON.parse(fs.readFileSync('.palimpsest.json', 'utf8'));
    const result = validatePalimpsestMetadata(metadata);
    if (result.valid) {
      console.log(' Metadata is valid');
    } else {
      console.error(' Validation errors:', result.errors);
      process.exit(1);
    }
    break;

  case 'hash':
    // Generate content hash
    const crypto = require('crypto');
    const content = fs.readFileSync(process.argv[3]);
    const hash = crypto.createHash('sha256').update(content).digest('hex');
    console.log(`SHA-256: ${hash}`);
    break;

  default:
    console.log('Usage: palimpsest-cli [init|validate|hash <file>]');
}
```

---

## Cryptographic Traceability

### Content Hashing

**Generate SHA-256 hash:**
```python
import hashlib

def hash_content(file_path):
    """Generate SHA-256 hash of file content."""
    sha256 = hashlib.sha256()
    with open(file_path, 'rb') as f:
        for chunk in iter(lambda: f.read(4096), b''):
            sha256.update(chunk)
    return sha256.hexdigest()

# Usage
content_hash = hash_content('work.txt')
print(f"Content hash: {content_hash}")
```

**Include in metadata:**
```json
{
  "palimpsest:traceability": {
    "contentHash": {
      "algorithm": "sha256",
      "value": "a3d4f5e6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4",
      "timestamp": "2025-01-15T12:00:00Z"
    }
  }
}
```

### Digital Signatures

**Sign metadata with Ed25519:**
```python
from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PrivateKey
from cryptography.hazmat.primitives import serialization
import json
import base64

# Generate key pair (do once, store securely)
private_key = Ed25519PrivateKey.generate()
public_key = private_key.public_key()

# Sign metadata
metadata = json.dumps({...}, sort_keys=True).encode('utf-8')
signature = private_key.sign(metadata)

# Add to metadata
signed_metadata = {
    ...metadata_dict,
    "palimpsest:cryptographicSignature": {
        "algorithm": "ed25519",
        "publicKey": base64.b64encode(
            public_key.public_bytes(
                encoding=serialization.Encoding.Raw,
                format=serialization.PublicFormat.Raw
            )
        ).decode('utf-8'),
        "signature": base64.b64encode(signature).decode('utf-8')
    }
}
```

### Blockchain Registration

**Ethereum smart contract interaction:**
```javascript
// Using ethers.js
const { ethers } = require('ethers');

async function registerWork(contentHash, metadataURI) {
  const provider = new ethers.JsonRpcProvider(process.env.ETH_RPC_URL);
  const wallet = new ethers.Wallet(process.env.PRIVATE_KEY, provider);

  const contractABI = [...]; // Palimpsest Registry ABI
  const contractAddress = '0x...'; // Palimpsest Registry address
  const contract = new ethers.Contract(contractAddress, contractABI, wallet);

  const tx = await contract.registerWork(
    contentHash,
    metadataURI,
    { gasLimit: 200000 }
  );

  const receipt = await tx.wait();
  console.log(`Registered on blockchain: ${receipt.hash}`);
  return receipt.hash;
}
```

**IPFS upload:**
```javascript
const { create } = require('ipfs-http-client');

async function uploadToIPFS(metadata) {
  const client = create({ url: 'https://ipfs.infura.io:5001/api/v0' });
  const { cid } = await client.add(JSON.stringify(metadata));
  console.log(`IPFS CID: ${cid}`);
  return cid.toString();
}
```

---

## AI System Integration

### Consent Checking Middleware

**Express.js middleware:**
```javascript
const checkPalimpsestAIConsent = async (req, res, next) => {
  const workId = req.params.workId;
  const aiPurpose = req.body.aiPurpose; // 'interpretive' or 'non-interpretive'

  // Fetch work metadata
  const work = await db.query('SELECT metadata FROM works WHERE id = $1', [workId]);
  const permissions = work.metadata['palimpsest:permissions'];

  if (aiPurpose === 'interpretive' && !permissions.interpretiveAI) {
    return res.status(403).json({
      error: 'Interpretive AI use not permitted',
      license: 'Palimpsest-0.4',
      contactForConsent: permissions.interpretiveAIContact,
      message: 'This work requires explicit consent for interpretive AI use. ' +
               'Please contact the creator at the provided email.'
    });
  }

  if (aiPurpose === 'non-interpretive' && !permissions.nonInterpretiveAI) {
    return res.status(403).json({
      error: 'Non-interpretive AI use not permitted',
      license: 'Palimpsest-0.4'
    });
  }

  next();
};

// Usage
app.post('/api/ai/process/:workId', checkPalimpsestAIConsent, async (req, res) => {
  // Process work with AI
});
```

### Training Dataset Filter

**Filter dataset for AI training:**
```python
import json
from pathlib import Path

def filter_training_dataset(dataset_dir, ai_type='interpretive'):
    """
    Filter dataset to only include works that allow specified AI use.

    Args:
        dataset_dir: Path to dataset directory
        ai_type: 'interpretive' or 'non-interpretive'

    Returns:
        List of permitted work paths
    """
    permitted_works = []

    for metadata_file in Path(dataset_dir).glob('**/.palimpsest.json'):
        with open(metadata_file) as f:
            metadata = json.load(f)

        permissions = metadata.get('palimpsest:permissions', {})

        if ai_type == 'interpretive':
            if permissions.get('interpretiveAI', False):
                work_file = metadata_file.parent / metadata.get('workFile', 'work.txt')
                permitted_works.append(work_file)
        elif ai_type == 'non-interpretive':
            if permissions.get('nonInterpretiveAI', True):  # Default true
                work_file = metadata_file.parent / metadata.get('workFile', 'work.txt')
                permitted_works.append(work_file)

    return permitted_works

# Usage
permitted = filter_training_dataset('/path/to/dataset', ai_type='interpretive')
print(f"Found {len(permitted)} works that permit interpretive AI")
```

### AI-Generated Attribution

**Append attribution to AI outputs:**
```python
def generate_with_attribution(prompt, source_work_metadata):
    """Generate AI content and include required attribution."""

    # Generate content (example with OpenAI)
    response = openai.ChatCompletion.create(
        model="gpt-4",
        messages=[{"role": "user", "content": prompt}]
    )

    generated_content = response.choices[0].message.content

    # Append attribution
    author = source_work_metadata['author']['name']
    title = source_work_metadata['name']
    cultural_context = source_work_metadata.get('palimpsest:emotionalLineage', {}).get('culturalOrigins', '')

    attribution = f"""

---
AI-Generated Content Attribution:
Source Material: "{title}" by {author}
Licensed under: Palimpsest License v0.4
Cultural Context: {cultural_context}
Generated using: {response.model}
Generated on: {datetime.now().isoformat()}

This derivative work honours the emotional lineage and cultural context
of the original source material.
"""

    return generated_content + attribution
```

---

## Validation and Compliance Checking

### Automated Compliance Scanner

**Scan repository for compliance:**
```python
#!/usr/bin/env python3
import os
import json
from pathlib import Path

class PalimpsestComplianceChecker:
    def __init__(self, repo_path):
        self.repo_path = Path(repo_path)
        self.issues = []

    def check(self):
        """Run all compliance checks."""
        self.check_license_file()
        self.check_metadata_file()
        self.check_spdx_headers()
        self.check_metadata_validity()
        return len(self.issues) == 0

    def check_license_file(self):
        """Ensure LICENSE file exists."""
        license_file = self.repo_path / 'LICENSE'
        if not license_file.exists():
            self.issues.append('ERROR: LICENSE file not found')
        else:
            content = license_file.read_text()
            if 'Palimpsest' not in content:
                self.issues.append('WARNING: LICENSE file may not contain Palimpsest text')

    def check_metadata_file(self):
        """Ensure .palimpsest.json exists."""
        metadata_file = self.repo_path / '.palimpsest.json'
        if not metadata_file.exists():
            self.issues.append('ERROR: .palimpsest.json metadata file not found')

    def check_spdx_headers(self):
        """Check for SPDX headers in source files."""
        source_files = list(self.repo_path.glob('**/*.py'))
        source_files += list(self.repo_path.glob('**/*.js'))
        source_files += list(self.repo_path.glob('**/*.ts'))

        missing_headers = []
        for source_file in source_files:
            if 'node_modules' in str(source_file) or '.git' in str(source_file):
                continue

            content = source_file.read_text()
            if 'SPDX-License-Identifier: Palimpsest-0.4' not in content:
                missing_headers.append(source_file.relative_to(self.repo_path))

        if missing_headers:
            self.issues.append(f'WARNING: {len(missing_headers)} files missing SPDX headers')
            for file in missing_headers[:5]:  # Show first 5
                self.issues.append(f'  - {file}')

    def check_metadata_validity(self):
        """Validate metadata JSON structure."""
        metadata_file = self.repo_path / '.palimpsest.json'
        if not metadata_file.exists():
            return

        try:
            metadata = json.loads(metadata_file.read_text())

            # Check required fields
            required = ['@type', 'name', 'author', 'license']
            for field in required:
                if field not in metadata:
                    self.issues.append(f'ERROR: Metadata missing required field: {field}')

            # Check permissions exist
            if 'palimpsest:permissions' not in metadata:
                self.issues.append('WARNING: Metadata missing palimpsest:permissions')

        except json.JSONDecodeError as e:
            self.issues.append(f'ERROR: Metadata JSON is invalid: {e}')

    def report(self):
        """Print compliance report."""
        if not self.issues:
            print(' All Palimpsest compliance checks passed')
            return True
        else:
            print(f' Found {len(self.issues)} compliance issues:\n')
            for issue in self.issues:
                print(f'  {issue}')
            return False

# Usage
if __name__ == '__main__':
    checker = PalimpsestComplianceChecker('.')
    checker.check()
    success = checker.report()
    exit(0 if success else 1)
```

---

## Performance Considerations

### Metadata Caching

**Cache parsed metadata:**
```javascript
const LRU = require('lru-cache');

const metadataCache = new LRU({
  max: 500,
  ttl: 1000 * 60 * 60 // 1 hour
});

async function getPalimpsestMetadata(workId) {
  // Check cache first
  const cached = metadataCache.get(workId);
  if (cached) return cached;

  // Fetch from database
  const metadata = await db.query(
    'SELECT metadata FROM works WHERE id = $1',
    [workId]
  );

  // Cache and return
  metadataCache.set(workId, metadata);
  return metadata;
}
```

### Lazy Loading

**Load metadata only when needed:**
```python
class PalimpsestWork:
    def __init__(self, work_path):
        self.work_path = work_path
        self._metadata = None

    @property
    def metadata(self):
        """Lazy load metadata on first access."""
        if self._metadata is None:
            metadata_path = self.work_path.parent / '.palimpsest.json'
            if metadata_path.exists():
                with open(metadata_path) as f:
                    self._metadata = json.load(f)
            else:
                self._metadata = {}
        return self._metadata

    @property
    def allows_interpretive_ai(self):
        """Quick check without loading full metadata."""
        return self.metadata.get('palimpsest:permissions', {}).get('interpretiveAI', False)
```

---

## Testing and Quality Assurance

### Unit Tests

**Jest tests for metadata validation:**
```javascript
// palimpsest.test.js
const { validatePalimpsestMetadata } = require('./palimpsest-validator');

describe('Palimpsest Metadata Validation', () => {
  test('validates minimal valid metadata', () => {
    const metadata = {
      '@context': 'https://schema.org/',
      '@type': 'CreativeWork',
      'name': 'Test Work',
      'author': { 'name': 'Test Author' },
      'license': { 'identifier': 'Palimpsest-0.4' },
      'palimpsest:permissions': {
        'nonInterpretiveAI': true,
        'interpretiveAI': false
      }
    };

    const result = validatePalimpsestMetadata(metadata);
    expect(result.valid).toBe(true);
  });

  test('rejects metadata without author', () => {
    const metadata = {
      '@context': 'https://schema.org/',
      '@type': 'CreativeWork',
      'name': 'Test Work',
      'license': { 'identifier': 'Palimpsest-0.4' }
    };

    const result = validatePalimpsestMetadata(metadata);
    expect(result.valid).toBe(false);
    expect(result.errors).toBeDefined();
  });

  test('validates emotional lineage when present', () => {
    const metadata = {
      '@context': 'https://schema.org/',
      '@type': 'CreativeWork',
      'name': 'Test Work',
      'author': { 'name': 'Test Author' },
      'license': { 'identifier': 'Palimpsest-0.4' },
      'palimpsest:emotionalLineage': {
        'culturalOrigins': 'Test cultural context',
        'emotionalIntent': 'Test intent'
      },
      'palimpsest:permissions': {
        'nonInterpretiveAI': true,
        'interpretiveAI': false
      }
    };

    const result = validatePalimpsestMetadata(metadata);
    expect(result.valid).toBe(true);
  });
});
```

### Integration Tests

**Test metadata preservation through transformations:**
```python
import pytest
import json
from your_app import transform_work, get_work_metadata

def test_metadata_preserved_after_transformation():
    """Ensure metadata survives format conversion."""
    original_work = 'test_work.md'
    original_metadata = get_work_metadata(original_work)

    # Transform to different format
    transformed_work = transform_work(original_work, output_format='html')
    transformed_metadata = get_work_metadata(transformed_work)

    # Check key metadata preserved
    assert original_metadata['author'] == transformed_metadata['author']
    assert original_metadata['license'] == transformed_metadata['license']
    assert original_metadata['palimpsest:emotionalLineage'] == \
           transformed_metadata['palimpsest:emotionalLineage']
```

---

## Common Implementation Patterns

### Pattern: Attribution Watermark

**Add visible attribution to images:**
```python
from PIL import Image, ImageDraw, ImageFont

def add_palimpsest_watermark(image_path, metadata):
    """Add visible attribution watermark to image."""
    img = Image.open(image_path)
    draw = ImageDraw.Draw(img)

    author = metadata['author']['name']
    license_text = f" {author} | Palimpsest-0.4"

    # Add semi-transparent attribution at bottom
    font = ImageFont.truetype("arial.ttf", 16)
    text_bbox = draw.textbbox((0, 0), license_text, font=font)
    text_width = text_bbox[2] - text_bbox[0]
    text_height = text_bbox[3] - text_bbox[1]

    x = img.width - text_width - 10
    y = img.height - text_height - 10

    # Semi-transparent background
    draw.rectangle([x-5, y-5, x+text_width+5, y+text_height+5],
                   fill=(0, 0, 0, 128))
    draw.text((x, y), license_text, fill=(255, 255, 255), font=font)

    return img
```

### Pattern: Metadata Inheritance

**Derivative works inherit parent metadata:**
```javascript
function createDerivativeMetadata(parentMetadata, derivativeInfo) {
  return {
    ...parentMetadata,
    '@id': derivativeInfo.newId,
    'name': derivativeInfo.newTitle,
    'dateCreated': new Date().toISOString(),
    'palimpsest:derivativeOf': {
      '@id': parentMetadata['@id'],
      'name': parentMetadata.name,
      'author': parentMetadata.author
    },
    'palimpsest:derivativeAuthor': {
      'name': derivativeInfo.derivativeAuthor
    },
    // Preserve original emotional lineage
    'palimpsest:emotionalLineage': {
      ...parentMetadata['palimpsest:emotionalLineage'],
      'derivativeContext': derivativeInfo.howDerivativeHonoursOriginal
    }
  };
}
```

---

## Troubleshooting

### Common Issues

**Issue: Metadata not persisting through transformations**
```bash
# Diagnosis
 Metadata stripped during PDF conversion

# Solution
# Ensure converter preserves XMP metadata
pdftk input.pdf update_info_utf8 metadata.txt output output.pdf

# Or use a metadata-aware converter
exiftool -TagsFromFile source.jpg "-all:all>all:all" converted.png
```

**Issue: SPDX identifier not recognised**
```bash
# Diagnosis
 Some tools don't recognise "Palimpsest-0.4"

# Solution
# Use dual licensing temporarily
# SPDX-License-Identifier: MPL-2.0-or-later

# Or add to SPDX license list request
# (See https://github.com/spdx/license-list-XML)
```

**Issue: Large metadata file**
```bash
# Diagnosis
 .palimpsest.json is 500KB due to embedded signatures

# Solution
# Store large data externally, reference by URL
{
  "palimpsest:cryptographicSignature": {
    "url": "https://example.com/signatures/work-id.sig"
  },
  "palimpsest:blockchainRegistry": {
    "url": "https://etherscan.io/tx/0x..."
  }
}
```

---

## Next Steps

### Further Reading
- **User Guide**: `/GUIDES_v0.4/User_Guide.md`  For non-technical users
- **Compliance Roadmap**: `/GUIDES_v0.4/Compliance_Roadmap.md`  Ensure full compliance
- **Integration Guide**: `/docs/integration-guide.md`  Platform-specific details
- **Ethics Documentation**: `/docs/ethics.md`  Understand the philosophy

### Tools and Libraries
- **palimpsest-validator** (npm): Metadata validation
- **palimpsest-cli** (pip): Command-line tools
- **palimpsest-python** (PyPI): Python integration library
- **palimpsest-js** (npm): JavaScript utilities

### Community
- **GitHub Discussions**: [link to discussions]
- **Technical Mailing List**: [link to mailing list]
- **Stack Overflow Tag**: `palimpsest-license`

---

**Document Version**: 1.0
**Last Updated**: 2025-11-22
**License**: This guide is licensed under CC BY-SA 4.0
**Maintainer**: Palimpsest Stewardship Council
