# Dublin Core Metadata for Palimpsest License v0.4

This directory contains Dublin Core metadata representations of the Palimpsest License in multiple formats.

## Overview

[Dublin Core](https://www.dublincore.org/) is an internationally recognised metadata standard for describing digital resources. It provides a simple yet comprehensive vocabulary for resource description, making it ideal for license documentation and discovery.

## Files

### palimpsest-v0.4-dc.xml
**Format:** XML with Dublin Core namespace
**Schema:** Dublin Core Metadata Element Set v1.1 + Dublin Core Terms
**Use cases:**
- Library catalogue systems
- Digital repository integration
- XML-based metadata harvesting
- OAI-PMH (Open Archives Initiative Protocol for Metadata Harvesting)

### palimpsest-v0.4-dc.json
**Format:** JSON-LD with Dublin Core context
**Schema:** Dublin Core with JSON-LD context
**Use cases:**
- Modern web applications
- RESTful API responses
- JavaScript/Node.js integration
- NoSQL database storage
- Linked data applications

### palimpsest-v0.4-dc.rdf
**Format:** RDF/XML
**Schema:** Dublin Core RDF vocabulary + FOAF + custom Palimpsest vocabulary
**Use cases:**
- Semantic web applications
- Triple stores and SPARQL queries
- Linked Open Data publication
- Knowledge graph integration
- Ontology-based systems

## Dublin Core Elements Used

### Core Elements (DC)
- **dc:title** - License name in multiple languages
- **dc:creator** - Palimpsest Stewardship Council
- **dc:subject** - Keywords and topics
- **dc:description** - Full description in English and Dutch
- **dc:publisher** - Publishing organisation
- **dc:contributor** - Council members and stakeholders
- **dc:date** - Publication date
- **dc:type** - Resource type (License, Legal Document)
- **dc:format** - Available formats
- **dc:identifier** - Unique identifiers (URI, DOI)
- **dc:source** - Previous version
- **dc:language** - Supported languages (en, nl)
- **dc:relation** - Related resources
- **dc:coverage** - Geographic/temporal scope
- **dc:rights** - Rights statement

### Dublin Core Terms Extensions (DCTERMS)
- **dcterms:abstract** - Short abstract
- **dcterms:license** - License URI
- **dcterms:modified** - Last modification date
- **dcterms:created** - Creation date
- **dcterms:isVersionOf** - Version relationship
- **dcterms:hasVersion** - Version number
- **dcterms:replaces** - Superseded version
- **dcterms:conformsTo** - Standards compliance
- **dcterms:spatial** - Geographic coverage
- **dcterms:temporal** - Temporal coverage
- **dcterms:audience** - Target audiences
- **dcterms:provenance** - Development history
- **dcterms:rightsHolder** - Rights holder information
- **dcterms:bibliographicCitation** - Citation format
- **dcterms:alternative** - Alternative names
- **dcterms:tableOfContents** - Structure outline
- **dcterms:accessRights** - Access level
- **dcterms:accrualMethod** - Update methodology
- **dcterms:accrualPeriodicity** - Update frequency
- **dcterms:accrualPolicy** - Update governance

## Custom Palimpsest Extensions

The metadata includes custom extensions in the `palimpsest:` namespace:

- **palimpsest:version** - Semantic version information
- **palimpsest:governance** - Governance structure and voting thresholds
- **palimpsest:protection** - Protection features (emotional lineage, quantum-proof traceability, etc.)
- **palimpsest:jurisdiction** - Legal jurisdiction and enforcement details

## Usage Examples

### XML Integration

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE document [
  <!ENTITY palimpsest-metadata SYSTEM "palimpsest-v0.4-dc.xml">
]>
<document>
  <metadata>
    &palimpsest-metadata;
  </metadata>
</document>
```

### JSON-LD in Web Applications

```javascript
// Node.js / Browser
const metadata = require('./palimpsest-v0.4-dc.json');

// Embed in HTML
<script type="application/ld+json">
  // Include contents of palimpsest-v0.4-dc.json
</script>
```

### RDF Query (SPARQL)

```sparql
PREFIX dc: <https://purl.org/dc/elements/1.1/>
PREFIX dcterms: <https://purl.org/dc/terms/>
PREFIX palimpsest: <https://palimpsest.license/vocab#>

SELECT ?title ?version ?protection
WHERE {
  <https://palimpsest.license/v0.4> dc:title ?title .
  <https://palimpsest.license/v0.4> palimpsest:version ?version .
  <https://palimpsest.license/v0.4> palimpsest:protection ?protection .
}
```

## Validation

### XML Validation
```bash
xmllint --noout --schema https://dublincore.org/schemas/xmls/qdc/2008/02/11/dc.xsd palimpsest-v0.4-dc.xml
```

### JSON-LD Validation
```bash
# Using jsonld command-line tool
jsonld format palimpsest-v0.4-dc.json

# Or validate online at:
# https://json-ld.org/playground/
```

### RDF Validation
```bash
# Using rapper (part of Raptor RDF library)
rapper -i rdfxml -o ntriples palimpsest-v0.4-dc.rdf > /dev/null
```

## Integration with Other Systems

### Repository Integration
These metadata files can be integrated with:
- **DSpace** - Digital repository platform
- **Fedora Commons** - Flexible repository architecture
- **EPrints** - Repository software for research outputs
- **Zenodo** - Research data repository

### Harvesting Protocols
- **OAI-PMH** - Open Archives Initiative Protocol for Metadata Harvesting
- **ResourceSync** - Web-based resource synchronisation
- **SWORD** - Simple Web-service Offering Repository Deposit

### Cataloguing Systems
- **Library Catalogues** - MARC, BIBFRAME integration
- **Content Management Systems** - WordPress, Drupal metadata plugins
- **Digital Asset Management** - DAM system integration

## Conversion Between Formats

### XML to JSON
```bash
# Using xmlstarlet and jq
xmlstarlet sel -t -m "//metadata" -c . palimpsest-v0.4-dc.xml | xml2json | jq .
```

### JSON to RDF
```javascript
// Using jsonld.js
const jsonld = require('jsonld');
const doc = require('./palimpsest-v0.4-dc.json');

jsonld.toRDF(doc, {format: 'application/n-quads'}, (err, nquads) => {
  console.log(nquads);
});
```

### RDF to Turtle
```bash
rapper -i rdfxml -o turtle palimpsest-v0.4-dc.rdf > palimpsest-v0.4-dc.ttl
```

## Standards Conformance

These metadata files conform to:
- **Dublin Core Metadata Element Set, Version 1.1** (ISO 15836-1:2017)
- **DCMI Metadata Terms** (DCMI Recommendation)
- **RDF 1.1** (W3C Recommendation)
- **JSON-LD 1.1** (W3C Recommendation)

## Accessibility Considerations

All metadata files:
- Use UTF-8 encoding
- Include language tags for multilingual content
- Provide both English and Dutch descriptions
- Follow semantic web best practices
- Are machine-readable and human-readable

## Maintenance

When updating the license:
1. Update all three formats (XML, JSON, RDF) simultaneously
2. Increment version numbers consistently
3. Update modification dates
4. Validate against schemas
5. Test integration with consuming systems
6. Document changes in CHANGELOG.md

## Further Reading

- [Dublin Core Metadata Initiative](https://www.dublincore.org/)
- [Dublin Core Metadata Element Set](https://www.dublincore.org/specifications/dublin-core/dces/)
- [DCMI Metadata Terms](https://www.dublincore.org/specifications/dublin-core/dcmi-terms/)
- [Dublin Core Application Profiles](https://www.dublincore.org/specifications/dublin-core/profile-guidelines/)
- [JSON-LD 1.1 Specification](https://www.w3.org/TR/json-ld11/)
- [RDF 1.1 Primer](https://www.w3.org/TR/rdf11-primer/)

## Licence

These metadata files are licensed under the Palimpsest License v0.4.

## Contact

For questions or issues regarding the metadata, please:
- Open an issue on the project repository
- Contact the Palimpsest Stewardship Council
- Consult the GOVERNANCE.md file for formal processes
