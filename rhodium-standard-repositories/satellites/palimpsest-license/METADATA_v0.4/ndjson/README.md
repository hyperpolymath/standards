# NDJSON Metadata for Palimpsest License

This directory contains newline-delimited JSON (NDJSON) schemas and examples for Palimpsest License metadata.

## Overview

NDJSON (Newline Delimited JSON) is a format for streaming and processing large amounts of structured data. Each line is a valid JSON object, making it ideal for:
- Log-style metadata tracking
- Streaming data processing
- Append-only audit trails
- Big data analysis
- Real-time monitoring

## Files

### schema.json
Complete JSON Schema (Draft 7) defining the structure for all Palimpsest metadata types:
- **license** - License metadata
- **work** - Original creative works
- **derivative** - Derivative works and remixes
- **lineage** - Chain-of-custody tracking
- **usage** - Usage logs and analytics
- **violation** - Violation reports and resolutions

### examples.ndjson
Real-world examples demonstrating:
- Original protest song with emotional lineage
- Compliant derivative work with proper attribution
- Lineage chain tracking with quantum-proof signatures
- Usage logs (both compliant and non-compliant)
- Violation reports and their resolutions
- AI-generated derivatives with synthetic lineage tags

## Schema Structure

### Record Types

#### 1. License Metadata
Basic license information:
```json
{
  "id": "palimpsest-v0.4",
  "type": "license",
  "version": {"major": 0, "minor": 4, "patch": 0, "status": "in-development"},
  "title": {"en": "Palimpsest License", "nl": "Palimpsest Licentie"},
  "timestamp": "2025-11-22T00:00:00Z"
}
```

#### 2. Work Metadata
Creative works with emotional lineage and AI consent:
```json
{
  "id": "work-example-001",
  "type": "work",
  "title": {"en": "Example Work"},
  "creator": {
    "name": "Creator Name",
    "uri": "https://example.org/creator",
    "orcid": "0000-0001-2345-6789"
  },
  "emotionalLineage": {
    "context": "Cultural and emotional context",
    "themes": ["theme1", "theme2"],
    "sensitivities": ["trauma", "cultural-heritage"],
    "symbolicElements": [
      {
        "symbol": "example-symbol",
        "meaning": "What it represents",
        "culturalOrigin": "Where it comes from"
      }
    ]
  },
  "aiConsent": {
    "training": false,
    "generation": true,
    "conditions": ["Specific conditions"]
  }
}
```

#### 3. Derivative Metadata
Derivatives with lineage statements and reciprocity:
```json
{
  "id": "derivative-example-001",
  "type": "derivative",
  "originalWork": {
    "uri": "https://example.org/original",
    "creator": "Original Creator"
  },
  "lineageStatement": {
    "attribution": {
      "type": "symbolic",
      "content": "How attribution is provided",
      "preservesContext": true
    },
    "transformations": [
      {
        "type": "remix",
        "description": "What was changed",
        "preservesIntegrity": true
      }
    ],
    "reciprocity": {
      "offered": true,
      "type": "percentage-share",
      "details": "5% revenue sharing"
    }
  },
  "syntheticLineageTag": {
    "aiSystem": "AI System Name",
    "model": "model-version",
    "quantumProofHash": "hash-value"
  }
}
```

#### 4. Lineage Chain
Complete chain-of-custody with quantum-proof signatures:
```json
{
  "type": "lineage",
  "rootWork": {
    "uri": "https://example.org/root-work",
    "creator": "Original Creator"
  },
  "chain": [
    {
      "uri": "https://example.org/work-1",
      "creator": "Creator 1",
      "timestamp": "2024-01-01T00:00:00Z",
      "transformationType": "original"
    },
    {
      "uri": "https://example.org/work-2",
      "creator": "Creator 2",
      "timestamp": "2024-06-01T00:00:00Z",
      "transformationType": "remix"
    }
  ],
  "quantumProof": {
    "algorithm": "SPHINCS+",
    "signature": "quantum-resistant-signature",
    "publicKey": "public-key"
  }
}
```

#### 5. Usage Logs
Track how works are being used:
```json
{
  "type": "usage",
  "workUri": "https://example.org/work",
  "usageType": "remix",
  "user": {
    "identifier": "user-id",
    "type": "individual"
  },
  "timestamp": "2025-11-22T12:00:00Z",
  "compliant": true,
  "details": {
    "attributionProvided": true,
    "contextPreserved": true
  }
}
```

#### 6. Violation Reports
Document and track violations:
```json
{
  "type": "violation",
  "workUri": "https://example.org/work",
  "violationType": "unauthorised-ai-training",
  "reportedBy": {
    "name": "Reporter Name",
    "email": "reporter@example.org",
    "relationship": "creator"
  },
  "timestamp": "2025-11-22T12:00:00Z",
  "description": "Description of violation",
  "evidence": [
    {
      "type": "url",
      "content": "https://evidence.example"
    }
  ],
  "status": "under-review"
}
```

## Usage Examples

### Streaming Processing

#### Python
```python
import json

# Read and process NDJSON
with open('examples.ndjson', 'r') as f:
    for line in f:
        record = json.loads(line)
        if record['type'] == 'violation':
            print(f"Violation: {record['violationType']}")

# Write NDJSON
with open('output.ndjson', 'w') as f:
    for record in records:
        f.write(json.dumps(record) + '\n')
```

#### Node.js
```javascript
const fs = require('fs');
const readline = require('readline');

// Read and process NDJSON
const rl = readline.createInterface({
  input: fs.createReadStream('examples.ndjson')
});

rl.on('line', (line) => {
  const record = JSON.parse(line);
  if (record.type === 'work') {
    console.log(`Work: ${record.title.en}`);
  }
});

// Write NDJSON
const records = [...];
const output = fs.createWriteStream('output.ndjson');
records.forEach(record => {
  output.write(JSON.stringify(record) + '\n');
});
```

#### Bash (jq)
```bash
# Extract all violations
cat examples.ndjson | jq -c 'select(.type == "violation")'

# Count records by type
cat examples.ndjson | jq -r '.type' | sort | uniq -c

# Filter works with AI consent for training
cat examples.ndjson | jq -c 'select(.type == "work" and .aiConsent.training == true)'

# Extract lineage chains
cat examples.ndjson | jq -c 'select(.type == "lineage")'
```

### Database Integration

#### PostgreSQL (with JSONB)
```sql
-- Create table for NDJSON records
CREATE TABLE palimpsest_metadata (
  id SERIAL PRIMARY KEY,
  record_type VARCHAR(50),
  data JSONB,
  timestamp TIMESTAMP DEFAULT NOW()
);

-- Create indexes for common queries
CREATE INDEX idx_record_type ON palimpsest_metadata(record_type);
CREATE INDEX idx_work_uri ON palimpsest_metadata USING GIN ((data->'workUri'));
CREATE INDEX idx_creator ON palimpsest_metadata USING GIN ((data->'creator'));

-- Insert NDJSON records
COPY palimpsest_metadata(record_type, data)
FROM '/path/to/examples.ndjson'
WITH (FORMAT text);

-- Query examples
SELECT data->'title'->>'en' AS title
FROM palimpsest_metadata
WHERE record_type = 'work'
  AND data->'aiConsent'->>'training' = 'false';
```

#### MongoDB
```javascript
// Import NDJSON
mongoimport --db palimpsest --collection metadata --file examples.ndjson

// Query violations
db.metadata.find({ type: "violation", status: "under-review" })

// Aggregate usage by type
db.metadata.aggregate([
  { $match: { type: "usage" } },
  { $group: { _id: "$usageType", count: { $sum: 1 } } }
])
```

### Apache Kafka / Stream Processing

```python
from kafka import KafkaProducer, KafkaConsumer
import json

# Producer
producer = KafkaProducer(
    bootstrap_servers=['localhost:9092'],
    value_serializer=lambda v: json.dumps(v).encode('utf-8')
)

with open('examples.ndjson', 'r') as f:
    for line in f:
        record = json.loads(line)
        producer.send('palimpsest-metadata', record)

# Consumer
consumer = KafkaConsumer(
    'palimpsest-metadata',
    bootstrap_servers=['localhost:9092'],
    value_deserializer=lambda m: json.loads(m.decode('utf-8'))
)

for message in consumer:
    record = message.value
    if record['type'] == 'violation':
        # Trigger alert
        send_alert(record)
```

## Validation

### Using ajv (Node.js)
```javascript
const Ajv = require('ajv');
const fs = require('fs');

const ajv = new Ajv();
const schema = JSON.parse(fs.readFileSync('schema.json', 'utf8'));
const validate = ajv.compile(schema);

// Validate each line
const readline = require('readline');
const rl = readline.createInterface({
  input: fs.createReadStream('examples.ndjson')
});

rl.on('line', (line) => {
  const record = JSON.parse(line);
  const valid = validate(record);
  if (!valid) {
    console.error('Validation errors:', validate.errors);
  }
});
```

### Using Python jsonschema
```python
import json
from jsonschema import validate, ValidationError

with open('schema.json', 'r') as f:
    schema = json.load(f)

with open('examples.ndjson', 'r') as f:
    for i, line in enumerate(f, 1):
        try:
            record = json.loads(line)
            validate(instance=record, schema=schema)
            print(f"Line {i}: Valid")
        except ValidationError as e:
            print(f"Line {i}: Invalid - {e.message}")
```

## Analytics and Reporting

### Generate Reports
```python
import json
from collections import Counter, defaultdict

def analyse_metadata(filename):
    type_counts = Counter()
    violations_by_type = Counter()
    ai_consent_stats = defaultdict(int)

    with open(filename, 'r') as f:
        for line in f:
            record = json.loads(line)
            type_counts[record['type']] += 1

            if record['type'] == 'violation':
                violations_by_type[record['violationType']] += 1

            if record['type'] == 'work':
                ai_consent = record.get('aiConsent', {})
                if ai_consent.get('training'):
                    ai_consent_stats['training_allowed'] += 1
                if ai_consent.get('generation'):
                    ai_consent_stats['generation_allowed'] += 1

    print("Record Type Distribution:", dict(type_counts))
    print("Violations by Type:", dict(violations_by_type))
    print("AI Consent Statistics:", dict(ai_consent_stats))

analyse_metadata('examples.ndjson')
```

## Best Practices

### 1. Append-Only Operations
NDJSON files should be append-only for audit trail purposes:
```python
def append_record(filename, record):
    with open(filename, 'a') as f:
        f.write(json.dumps(record) + '\n')
```

### 2. Timestamping
Always include ISO 8601 timestamps:
```python
from datetime import datetime

record = {
    "timestamp": datetime.utcnow().isoformat() + 'Z',
    # ... other fields
}
```

### 3. Unique Identifiers
Use consistent ID schemes:
- `palimpsest-v{version}` for licenses
- `work-{category}-{number}` for works
- `derivative-{type}-{number}` for derivatives
- `violation-report-{number}` for violations

### 4. Compression
For large files, use gzip:
```bash
gzip examples.ndjson
# Creates examples.ndjson.gz

# Read compressed files
zcat examples.ndjson.gz | jq -c 'select(.type == "work")'
```

## Integration with Other Systems

### Blockchain/DLT
NDJSON records can be hashed and stored on blockchain for immutability:
```python
import hashlib
import json

def hash_record(record):
    record_json = json.dumps(record, sort_keys=True)
    return hashlib.sha256(record_json.encode()).hexdigest()
```

### IPFS
Store NDJSON files on IPFS for decentralised access:
```bash
ipfs add examples.ndjson
# Returns IPFS hash: QmXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
```

### DAO Integration
Use NDJSON for DAO governance tracking:
```javascript
// Record DAO vote on license amendment
const vote_record = {
  "type": "governance",
  "proposal": "amendment-v0.5",
  "vote": {
    "for": 5,
    "against": 1,
    "abstain": 1
  },
  "timestamp": new Date().toISOString(),
  "outcome": "approved"
};
```

## Performance Considerations

### Large Files
For files with millions of records:
1. Use streaming parsers (don't load entire file into memory)
2. Implement index files for random access
3. Consider partitioning by date or type
4. Use compression (gzip, zstd)

### Querying
For frequent queries:
1. Import into a database (PostgreSQL, MongoDB)
2. Build indexes on commonly queried fields
3. Use caching for frequently accessed records
4. Consider materialised views for aggregations

## Standards Conformance

These schemas conform to:
- **JSON Schema Draft 7** - Schema validation
- **ISO 8601** - Date and time formats
- **RFC 3986** - URI formats
- **ORCID** - Researcher identification
- **DOI** - Digital object identifiers

## Licence

These schemas and examples are licensed under the Palimpsest License v0.4.

## Further Reading

- [NDJSON Specification](https://ndjson.org/)
- [JSON Schema](https://json-schema.org/)
- [JSON Lines](https://jsonlines.org/)
- [Streaming JSON Processing](https://en.wikipedia.org/wiki/JSON_streaming)
