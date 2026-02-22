# Palimpsest License Metadata Infrastructure v0.4

This directory contains comprehensive metadata infrastructure for the Palimpsest License, enabling interoperability, machine-readability, and integration across diverse systems and platforms.

## Overview

The Palimpsest License metadata infrastructure provides multiple representations and serialisation formats to support:

- **Library and archival systems** (Dublin Core)
- **Streaming data processing** (NDJSON)
- **High-performance distributed systems** (Protocol Buffers)
- **Semantic web and linked data** (VoID RDF)

> **Note (2025-12-09):** Elixir/Ecto schemas have been archived to `ARCHIVE/elixir-experiment/`.
> The project now uses OCaml as the primary implementation language.

All formats use **British English** throughout and provide bilingual support (English/Dutch) where appropriate.

## Directory Structure

```
METADATA_v0.4/
├── dublin-core/          # Dublin Core metadata (XML, JSON, RDF)
├── ndjson/               # Newline-delimited JSON schemas and examples
├── serialization/        # Protocol Buffers for binary serialisation
├── void-rdf/             # VoID RDF vocabulary and semantic web integration
└── README.md             # This file
```

## Formats

### 1. Dublin Core (`dublin-core/`)

**Purpose:** Library cataloguing, repository integration, metadata harvesting

**Files:**
- `palimpsest-v0.4-dc.xml` - Dublin Core XML with full DCTERMS
- `palimpsest-v0.4-dc.json` - JSON-LD representation
- `palimpsest-v0.4-dc.rdf` - RDF/XML with FOAF and custom vocabulary

**Use cases:**
- Digital repositories (DSpace, Fedora, EPrints)
- Library catalogues (MARC, BIBFRAME)
- OAI-PMH metadata harvesting
- Citation management systems

**Standards:**
- Dublin Core Metadata Element Set v1.1 (ISO 15836-1:2017)
- DCMI Metadata Terms
- RDF 1.1
- JSON-LD 1.1

### 2. NDJSON (`ndjson/`)

**Purpose:** Streaming data, log-style tracking, big data analytics

**Files:**
- `schema.json` - JSON Schema (Draft 7) for all record types
- `examples.ndjson` - Real-world examples with emotional lineage
- `README.md` - Comprehensive documentation

**Record types:**
- License metadata
- Works (with emotional lineage and AI consent)
- Derivatives (with lineage statements)
- Lineage chains
- Usage logs
- Violation reports

**Use cases:**
- Apache Kafka streaming
- Elasticsearch/Kibana logging
- BigQuery/Athena analytics
- Real-time monitoring
- Append-only audit trails

### 3. Ecto Schemas (`ecto/`)

**Purpose:** Elixir/Phoenix web applications, PostgreSQL integration

**Files:**
- `license.ex` - License metadata schema
- `work.ex` - Creative work schema with emotional lineage
- `derivative.ex` - Derivative work schema with lineage statements
- `lineage.ex` - Lineage chain tracking with quantum proofs
- `violation.ex` - Violation reports and dispute resolution
- `README.md` - Installation, migrations, and usage examples

**Features:**
- Strong typing with changesets
- Validation (required fields, formats, business logic)
- Embedded schemas for complex data
- Helper functions for common operations
- Migration scripts for PostgreSQL

**Use cases:**
- Phoenix web applications
- Elixir microservices
- PostgreSQL with JSONB
- GraphQL APIs
- LiveView applications

### 4. Protocol Buffers (`serialization/`)

**Purpose:** High-performance serialisation, distributed systems, gRPC

**Files:**
- `palimpsest.proto` - Complete Protocol Buffers v3 schema
- `README.md` - Code generation, integration, and examples

**Features:**
- Binary serialisation (10x faster than JSON)
- Schema evolution (forward/backward compatibility)
- Code generation for 20+ languages
- gRPC integration
- Strong typing

**Supported languages:**
- Go, Python, JavaScript/TypeScript
- Java, C++, C#, Rust
- Ruby, PHP, Kotlin, Swift

**Use cases:**
- gRPC microservices
- Blockchain integration
- High-throughput data pipelines
- Mobile applications
- Cross-language communication

### 5. VoID RDF (`void-rdf/`)

**Purpose:** Semantic web, linked open data, SPARQL queries

**Files:**
- `palimpsest-void.ttl` - VoID dataset description
- `palimpsest-vocab.ttl` - RDF/OWL vocabulary
- `README.md` - Triple stores, SPARQL, and linked data

**Vocabulary:**
- Classes: Work, Derivative, Lineage, Violation, EmotionalLineage, AIConsent
- Properties: emotionalLineage, aiConsent, derivedFrom, quantumProof
- Linksets to ORCID, DOI, Creative Commons

**Use cases:**
- Triple stores (Jena Fuseki, Virtuoso, GraphDB)
- SPARQL endpoints
- Linked Open Data publication
- Knowledge graphs
- Semantic search

## Integration Patterns

### Web Applications

```
User Request
    ↓
Phoenix/Rails/Django Controller
    ↓
Ecto/ActiveRecord/Django ORM ← [Ecto Schemas]
    ↓
PostgreSQL/MySQL Database
    ↓
JSON API Response ← [Dublin Core JSON]
```

### Streaming Data Pipeline

```
Creative Works
    ↓
Producer (Kafka/Kinesis) ← [NDJSON]
    ↓
Stream Processing (Beam/Flink)
    ↓
Triple Store (Virtuoso) ← [VoID RDF]
    ↓
Analytics/SPARQL Queries
```

### Microservices Architecture

```
Service A (Go) ←→ [Protocol Buffers] ←→ Service B (Python)
    ↓                                         ↓
Database A                              Database B
```

### Blockchain Integration

```
Creative Work
    ↓
Serialise ← [Protocol Buffers]
    ↓
Hash (SHA-256/Quantum-resistant)
    ↓
Store on Blockchain (hash only)
    ↓
Store full metadata on IPFS ← [NDJSON/Dublin Core]
```

## Cross-Format Mapping

| Concept | Dublin Core | NDJSON | Ecto | Protobuf | RDF |
|---------|-------------|--------|------|----------|-----|
| Title | dc:title | title.en | title_en | title.translations["en"] | dcterms:title |
| Creator | dc:creator | creator.name | creator.name | creator.name | dcterms:creator |
| License | dcterms:license | license_id | license_id | license_id | dcterms:license |
| AI Consent | palimpsest:aiConsent | ai_consent | ai_consent | ai_consent | palimpsest:aiConsent |
| Lineage | palimpsest:lineage | emotional_lineage | emotional_lineage | emotional_lineage | palimpsest:emotionalLineage |

## Data Flow Examples

### 1. Work Registration

```
1. User submits work via web form
2. Phoenix validates with Ecto schema
3. Store in PostgreSQL
4. Publish to Kafka as NDJSON
5. Generate Dublin Core for repository
6. Serialise with Protocol Buffers for blockchain
7. Generate RDF for triple store
8. SPARQL endpoint exposes linked data
```

### 2. Violation Report

```
1. Creator reports violation via API
2. Validate with JSON Schema (NDJSON)
3. Store in database (Ecto)
4. Notify via gRPC (Protocol Buffers)
5. Update RDF graph (VoID)
6. Export to archive (Dublin Core)
```

### 3. Lineage Tracking

```
1. Derivative created
2. Compute quantum-proof hash (Protocol Buffers)
3. Record lineage chain (Ecto)
4. Publish event (NDJSON streaming)
5. Update knowledge graph (RDF)
6. Generate citation (Dublin Core)
```

## Development Workflow

### Adding a New Field

When adding a new field to the metadata:

1. **Update JSON Schema** (`ndjson/schema.json`)
   - Add field definition with type and validation
   - Update examples in `examples.ndjson`

2. **Update Ecto Schema** (`ecto/*.ex`)
   - Add field to schema
   - Update changeset validation
   - Create database migration

3. **Update Protocol Buffers** (`serialization/palimpsest.proto`)
   - Add field with new field number
   - Regenerate code for all languages
   - Update documentation

4. **Update RDF Vocabulary** (`void-rdf/palimpsest-vocab.ttl`)
   - Add property definition
   - Include domain/range
   - Add multilingual labels

5. **Update Dublin Core** (`dublin-core/palimpsest-v0.4-dc.*`)
   - Add to appropriate DC/DCTERMS element
   - Or add as custom extension
   - Update all three formats (XML, JSON, RDF)

6. **Test Cross-Format Compatibility**
   - Ensure same data can round-trip across formats
   - Validate with schema validators
   - Test serialisation/deserialisation

## Validation Tools

### JSON Schema
```bash
npm install -g ajv-cli
ajv validate -s ndjson/schema.json -d ndjson/examples.ndjson
```

### Protocol Buffers
```bash
protoc --descriptor_set_out=/dev/null serialization/palimpsest.proto
```

### RDF/OWL
```bash
riot --validate void-rdf/palimpsest-vocab.ttl
```

### Dublin Core
```bash
xmllint --noout --schema https://dublincore.org/schemas/xmls/qdc/2008/02/11/dc.xsd \
        dublin-core/palimpsest-v0.4-dc.xml
```

## Performance Characteristics

| Format | Serialisation | Deserialisation | Size | Human Readable |
|--------|--------------|-----------------|------|----------------|
| Protocol Buffers | Fastest | Fastest | Smallest | No |
| NDJSON | Fast | Fast | Medium | Yes |
| Dublin Core JSON | Medium | Medium | Medium | Yes |
| Dublin Core XML | Slow | Slow | Large | Yes |
| RDF Turtle | Slow | Medium | Large | Yes |

**Recommendations:**
- **Performance-critical**: Protocol Buffers
- **Streaming/logging**: NDJSON
- **Human editing**: Dublin Core JSON or RDF Turtle
- **Library systems**: Dublin Core XML
- **Semantic web**: RDF (any serialisation)

## Versioning and Evolution

All metadata formats support evolution:

- **Protocol Buffers**: Field numbering, reserved fields
- **JSON Schema**: `$schema` versioning
- **RDF**: `owl:versionIRI`, `owl:versionInfo`
- **Ecto**: Database migrations
- **Dublin Core**: `dcterms:hasVersion`, `dcterms:replaces`

## Accessibility

All metadata formats prioritise accessibility:
- UTF-8 encoding throughout
- Multilingual support (language tags)
- Screen reader friendly (structured data)
- Plain language documentation
- Multiple representation formats

## Security Considerations

### Quantum-Proof Cryptography
- SPHINCS+ signatures in Protocol Buffers
- Merkle tree verification in RDF
- Future-proof hashing algorithms

### Privacy
- Anonymisation options in violation reports
- GDPR-compliant data structures
- Configurable data retention

### Validation
- Schema validation prevents malformed data
- Type safety in Ecto and Protocol Buffers
- SHACL shapes for RDF

## Licence

All metadata formats and schemas are licensed under the Palimpsest License v0.4.

## Contributing

When contributing to the metadata infrastructure:

1. **Maintain format parity** - updates to one format should be reflected in all
2. **Follow British English** - use British spelling throughout
3. **Test thoroughly** - validate against schemas
4. **Document changes** - update relevant READMEs
5. **Respect governance** - major changes require Council review

## Support

For questions or issues:
- Consult format-specific READMEs in each subdirectory
- Review examples and usage patterns
- Open an issue on the project repository
- Contact the Palimpsest Stewardship Council

## Further Reading

### Standards
- [Dublin Core Metadata Initiative](https://www.dublincore.org/)
- [JSON Schema](https://json-schema.org/)
- [Protocol Buffers](https://protobuf.dev/)
- [RDF Primer](https://www.w3.org/TR/rdf11-primer/)
- [VoID Specification](https://www.w3.org/TR/void/)

### Implementations
- [Ecto Documentation](https://hexdocs.pm/ecto/)
- [Protocol Buffers Language Guide](https://protobuf.dev/programming-guides/proto3/)
- [Apache Jena](https://jena.apache.org/)
- [rdflib (Python)](https://rdflib.readthedocs.io/)

### Best Practices
- [Linked Data Best Practices](https://www.w3.org/TR/ld-bp/)
- [API Design with Protocol Buffers](https://protobuf.dev/programming-guides/api/)
- [JSON-LD Best Practices](https://www.w3.org/TR/json-ld11-api/)
