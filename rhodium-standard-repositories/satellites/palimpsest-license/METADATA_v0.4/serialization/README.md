# Protocol Buffers for Palimpsest License Metadata

This directory contains Protocol Buffers (protobuf) schema definitions for efficient serialisation and deserialisation of Palimpsest License metadata.

## Overview

**Protocol Buffers** (protobuf) is a language-neutral, platform-neutral extensible mechanism for serialising structured data. It was chosen for the Palimpsest License metadata infrastructure because it:

- Provides efficient binary serialisation
- Supports schema evolution (forward/backward compatibility)
- Generates code for multiple languages
- Offers strong typing with validation
- Integrates well with gRPC for distributed systems
- Supports versioned data structures ideal for license evolution

## Why Protocol Buffers?

We chose Protocol Buffers over FlatBuffers and Cap'n Proto because:

1. **Maturity**: Industry-standard with extensive tooling and support
2. **Language Support**: Official code generators for 20+ languages
3. **Schema Evolution**: Built-in support for versioning and compatibility
4. **Ecosystem**: Integration with gRPC, BigQuery, Kafka, etc.
5. **Documentation**: Comprehensive docs and large community
6. **Validation**: Strong typing catches errors at compile time

## Files

### palimpsest.proto
Complete Protocol Buffers v3 schema defining:
- **License**: Version, governance, protection features, jurisdiction
- **Work**: Creative works with emotional lineage and AI consent
- **Derivative**: Derivative works with lineage statements and attribution
- **Lineage**: Chain-of-custody tracking with quantum-proof signatures
- **Usage**: Usage tracking and analytics
- **Violation**: Violation reports and dispute resolution
- **Common Types**: Multilingual text, versions, quantum proofs, blockchain records

## Installation

### Prerequisites

Install the Protocol Buffers compiler (`protoc`):

#### macOS
```bash
brew install protobuf
```

#### Ubuntu/Debian
```bash
apt-get install -y protobuf-compiler
```

#### From source
```bash
wget https://github.com/protocolbuffers/protobuf/releases/download/v25.1/protoc-25.1-linux-x86_64.zip
unzip protoc-25.1-linux-x86_64.zip -d /usr/local
```

### Language-Specific Plugins

#### Go
```bash
go install google.golang.org/protobuf/cmd/protoc-gen-go@latest
```

#### Python
```bash
pip install protobuf
```

#### JavaScript/TypeScript
```bash
npm install -g protoc-gen-ts
npm install google-protobuf
```

#### Java
No additional installation needed (built into protoc)

#### C++
No additional installation needed (built into protoc)

#### Rust
```bash
cargo install protobuf-codegen
```

## Code Generation

### Go

```bash
protoc --go_out=./gen/go \
       --go_opt=paths=source_relative \
       palimpsest.proto
```

Usage:
```go
package main

import (
    "fmt"
    "google.golang.org/protobuf/proto"
    pb "github.com/palimpsest-license/metadata/gen/go/v1"
)

func main() {
    work := &pb.Work{
        WorkId:       "work-example-001",
        CanonicalUri: "https://example.org/work",
        Title: &pb.MultilingualText{
            Translations: map[string]string{
                "en": "Example Work",
                "nl": "Voorbeeldwerk",
            },
        },
        Creator: &pb.Person{
            Name:  "Creator Name",
            Email: "creator@example.org",
            Orcid: "0000-0001-2345-6789",
        },
        AiConsent: &pb.Work_AIConsent{
            TrainingAllowed:   false,
            GenerationAllowed: true,
            Conditions:        []string{"Attribution required"},
        },
    }

    // Serialise to binary
    data, err := proto.Marshal(work)
    if err != nil {
        panic(err)
    }

    // Deserialise from binary
    newWork := &pb.Work{}
    err = proto.Unmarshal(data, newWork)
    if err != nil {
        panic(err)
    }

    fmt.Printf("Work ID: %s\n", newWork.WorkId)
}
```

### Python

```bash
protoc --python_out=./gen/python \
       palimpsest.proto
```

Usage:
```python
from google.protobuf import timestamp_pb2
import palimpsest_pb2

# Create a work
work = palimpsest_pb2.Work()
work.work_id = "work-example-001"
work.canonical_uri = "https://example.org/work"
work.title.translations["en"] = "Example Work"
work.title.translations["nl"] = "Voorbeeldwerk"

work.creator.name = "Creator Name"
work.creator.email = "creator@example.org"
work.creator.orcid = "0000-0001-2345-6789"

work.ai_consent.training_allowed = False
work.ai_consent.generation_allowed = True
work.ai_consent.conditions.append("Attribution required")

# Serialise to binary
data = work.SerializeToString()

# Deserialise from binary
new_work = palimpsest_pb2.Work()
new_work.ParseFromString(data)

print(f"Work ID: {new_work.work_id}")
```

### JavaScript/TypeScript

```bash
protoc --js_out=import_style=commonjs,binary:./gen/js \
       --ts_out=./gen/ts \
       palimpsest.proto
```

Usage (Node.js):
```javascript
const { Work, Person, MultilingualText } = require('./gen/js/palimpsest_pb');

// Create a work
const work = new Work();
work.setWorkId('work-example-001');
work.setCanonicalUri('https://example.org/work');

const title = new MultilingualText();
title.getTranslationsMap().set('en', 'Example Work');
title.getTranslationsMap().set('nl', 'Voorbeeldwerk');
work.setTitle(title);

const creator = new Person();
creator.setName('Creator Name');
creator.setEmail('creator@example.org');
creator.setOrcid('0000-0001-2345-6789');
work.setCreator(creator);

// Serialise to binary
const bytes = work.serializeBinary();

// Deserialise from binary
const newWork = Work.deserializeBinary(bytes);
console.log(`Work ID: ${newWork.getWorkId()}`);
```

### Java

```bash
protoc --java_out=./gen/java \
       palimpsest.proto
```

Usage:
```java
import org.palimpsest.license.metadata.v1.Work;
import org.palimpsest.license.metadata.v1.Person;
import org.palimpsest.license.metadata.v1.MultilingualText;

public class Example {
    public static void main(String[] args) {
        Work work = Work.newBuilder()
            .setWorkId("work-example-001")
            .setCanonicalUri("https://example.org/work")
            .setTitle(MultilingualText.newBuilder()
                .putTranslations("en", "Example Work")
                .putTranslations("nl", "Voorbeeldwerk"))
            .setCreator(Person.newBuilder()
                .setName("Creator Name")
                .setEmail("creator@example.org")
                .setOrcid("0000-0001-2345-6789"))
            .setAiConsent(Work.AIConsent.newBuilder()
                .setTrainingAllowed(false)
                .setGenerationAllowed(true)
                .addConditions("Attribution required"))
            .build();

        // Serialise to binary
        byte[] bytes = work.toByteArray();

        // Deserialise from binary
        Work newWork = Work.parseFrom(bytes);
        System.out.println("Work ID: " + newWork.getWorkId());
    }
}
```

### Rust

```bash
protoc --rust_out=./gen/rust \
       palimpsest.proto
```

Usage:
```rust
use protobuf::Message;
mod palimpsest;
use palimpsest::Work;

fn main() {
    let mut work = Work::new();
    work.set_work_id("work-example-001".to_string());
    work.set_canonical_uri("https://example.org/work".to_string());

    let mut title = palimpsest::MultilingualText::new();
    title.mut_translations().insert("en".to_string(), "Example Work".to_string());
    title.mut_translations().insert("nl".to_string(), "Voorbeeldwerk".to_string());
    work.set_title(title);

    // Serialise to binary
    let bytes = work.write_to_bytes().unwrap();

    // Deserialise from binary
    let new_work = Work::parse_from_bytes(&bytes).unwrap();
    println!("Work ID: {}", new_work.get_work_id());
}
```

## JSON Encoding

Protocol Buffers can be serialised to JSON for human-readable formats:

### Go
```go
import "google.golang.org/protobuf/encoding/protojson"

jsonBytes, err := protojson.Marshal(work)
```

### Python
```python
from google.protobuf import json_format

json_str = json_format.MessageToJson(work)
```

### JavaScript
```javascript
const json = work.toObject();
```

## gRPC Integration

Protocol Buffers integrate seamlessly with gRPC for building distributed metadata services:

```protobuf
service PalimpsestMetadataService {
  rpc RegisterWork(Work) returns (WorkRegistrationResponse);
  rpc RegisterDerivative(Derivative) returns (DerivativeRegistrationResponse);
  rpc TrackLineage(Lineage) returns (LineageTrackingResponse);
  rpc ReportViolation(Violation) returns (ViolationReportResponse);
  rpc QueryWorks(WorkQuery) returns (stream Work);
  rpc VerifyLineage(LineageVerificationRequest) returns (LineageVerificationResponse);
}
```

## Schema Evolution

Protocol Buffers support schema evolution through field numbering:

### Adding Fields
```protobuf
message Work {
  string work_id = 1;
  // ... existing fields ...

  // New field - safe to add
  string new_field = 22;  // Use next available number
}
```

### Deprecating Fields
```protobuf
message Work {
  reserved 20, 21;  // Reserve deprecated field numbers
  reserved "deprecated_field_name";

  string work_id = 1;
  // ... other fields ...
}
```

### Best Practices
1. Never reuse field numbers
2. Use `reserved` for deprecated fields
3. Add new fields with new numbers
4. Don't change field types (create new field instead)
5. Don't change field names (only affects code generation)

## Validation

### Buf Schema Registry

Install [buf](https://buf.build/):
```bash
curl -sSL "https://github.com/bufbuild/buf/releases/latest/download/buf-$(uname -s)-$(uname -m)" \
  -o /usr/local/bin/buf
chmod +x /usr/local/bin/buf
```

Create `buf.yaml`:
```yaml
version: v1
breaking:
  use:
    - FILE
lint:
  use:
    - DEFAULT
```

Validate schema:
```bash
buf lint palimpsest.proto
buf breaking --against '.git#branch=main'
```

### Manual Validation

```bash
# Check syntax
protoc --descriptor_set_out=/dev/null palimpsest.proto

# Check for breaking changes
protoc --descriptor_set_out=old.pb palimpsest.proto.old
protoc --descriptor_set_out=new.pb palimpsest.proto
buf breaking new.pb --against old.pb
```

## Performance Benchmarks

Protocol Buffers vs JSON (approximate):

| Operation | Protobuf | JSON | Improvement |
|-----------|----------|------|-------------|
| Serialisation | 50ns | 500ns | 10x faster |
| Deserialisation | 80ns | 800ns | 10x faster |
| Size | 100 bytes | 300 bytes | 3x smaller |
| Memory | Low | Medium | 2-3x less |

## Use Cases

### 1. Blockchain Integration
Store metadata hashes on-chain, full metadata off-chain:
```go
work := &pb.Work{ /* ... */ }
data, _ := proto.Marshal(work)
hash := sha256.Sum256(data)
// Store hash on blockchain
// Store data on IPFS
```

### 2. Distributed Metadata Service
```go
// gRPC server
type Server struct {
    pb.UnimplementedPalimpsestMetadataServiceServer
}

func (s *Server) RegisterWork(ctx context.Context, work *pb.Work) (*pb.WorkRegistrationResponse, error) {
    // Validate and store work
    return &pb.WorkRegistrationResponse{
        Success: true,
        WorkId:  work.WorkId,
    }, nil
}
```

### 3. Analytics Pipeline
```python
# Apache Beam / Dataflow pipeline
import apache_beam as beam

class ProcessWork(beam.DoFn):
    def process(self, serialised_work):
        work = palimpsest_pb2.Work()
        work.ParseFromString(serialised_work)

        if work.ai_consent.training_allowed:
            yield ('training_allowed', 1)
        else:
            yield ('training_disallowed', 1)

# Pipeline
(pipeline
 | beam.io.ReadFromAvro('works.avro')
 | beam.ParDo(ProcessWork())
 | beam.GroupByKey()
 | beam.io.WriteToText('analytics'))
```

### 4. Microservices Communication
Services communicate using protobuf over gRPC for type safety and performance.

## Storage Formats

### Binary Files
```bash
# Write work to file
protoc --encode=palimpsest.metadata.v1.Work palimpsest.proto < work.txt > work.pb

# Read work from file
protoc --decode=palimpsest.metadata.v1.Work palimpsest.proto < work.pb
```

### Databases

#### PostgreSQL with Binary Column
```sql
CREATE TABLE works (
    work_id VARCHAR PRIMARY KEY,
    metadata BYTEA NOT NULL
);

-- Insert
INSERT INTO works (work_id, metadata) VALUES ('work-001', $1);

-- Query
SELECT metadata FROM works WHERE work_id = 'work-001';
```

#### MongoDB
```javascript
db.works.insertOne({
    work_id: "work-001",
    metadata: Binary(protobufData)
});
```

## Testing

### Unit Tests (Go)
```go
func TestWorkSerialisation(t *testing.T) {
    work := &pb.Work{
        WorkId: "test-001",
        Title: &pb.MultilingualText{
            Translations: map[string]string{"en": "Test"},
        },
    }

    data, err := proto.Marshal(work)
    assert.NoError(t, err)

    newWork := &pb.Work{}
    err = proto.Unmarshal(data, newWork)
    assert.NoError(t, err)
    assert.Equal(t, "test-001", newWork.WorkId)
}
```

### Integration Tests
Test round-trip serialisation and compatibility across language implementations.

## Documentation Generation

Generate HTML documentation from protobuf:
```bash
protoc --doc_out=./docs --doc_opt=html,index.html palimpsest.proto
```

## Licence

This Protocol Buffers schema is licensed under the Palimpsest License v0.4.

## Further Reading

- [Protocol Buffers Documentation](https://protobuf.dev/)
- [Protocol Buffers Language Guide](https://protobuf.dev/programming-guides/proto3/)
- [gRPC](https://grpc.io/)
- [Buf Schema Registry](https://buf.build/)
- [Protocol Buffers Best Practices](https://protobuf.dev/programming-guides/api/)
