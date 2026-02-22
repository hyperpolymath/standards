# VoID RDF Vocabulary for Palimpsest License

This directory contains Vocabulary of Interlinked Datasets (VoID) descriptions and the Palimpsest License RDF/OWL vocabulary for semantic web integration.

## Overview

**VoID** (Vocabulary of Interlinked Datasets) is an RDF vocabulary for expressing metadata about RDF datasets. It enables:
- Dataset discovery and cataloguing
- Linking to external datasets
- SPARQL endpoint descriptions
- Statistical summaries
- Semantic web integration

The **Palimpsest Vocabulary** is an RDF/OWL ontology defining classes and properties specific to the Palimpsest License, enabling rich semantic descriptions of creative works, emotional lineage, and AI consent.

## Files

### palimpsest-void.ttl
VoID description of the Palimpsest License metadata dataset, including:
- Dataset metadata and statistics
- Partitions for works, derivatives, lineages, violations
- Linksets to external datasets (ORCID, DOI, Creative Commons)
- SPARQL endpoint description
- Access methods and data dumps

### palimpsest-vocab.ttl
RDF/OWL vocabulary defining Palimpsest-specific concepts:
- **Classes**: Work, Derivative, Lineage, Violation, EmotionalLineage, AIConsent, etc.
- **Properties**: emotionalLineage, aiConsent, derivedFrom, quantumProof, etc.
- **Individuals**: Sensitivity types (trauma, cultural-heritage, sacred, etc.)

## Usage

### Loading into a Triple Store

#### Apache Jena Fuseki
```bash
# Start Fuseki
fuseki-server --file=palimpsest-void.ttl --file=palimpsest-vocab.ttl /palimpsest

# Or use fuseki-cli
s-put http://localhost:3030/palimpsest/data default palimpsest-void.ttl
s-put http://localhost:3030/palimpsest/data default palimpsest-vocab.ttl
```

#### OpenLink Virtuoso
```sql
DB.DBA.RDF_LOAD_RDFXML_MT(
    file_to_string_output('palimpsest-void.ttl'),
    '',
    'https://palimpsest.license/dataset'
);

DB.DBA.RDF_LOAD_RDFXML_MT(
    file_to_string_output('palimpsest-vocab.ttl'),
    '',
    'https://palimpsest.license/vocab'
);
```

#### GraphDB
```bash
# Import via GUI or curl
curl -X POST \
  http://localhost:7200/repositories/palimpsest/statements \
  -H "Content-Type: text/turtle" \
  --data-binary @palimpsest-void.ttl
```

### SPARQL Queries

#### Find all works with AI training disallowed
```sparql
PREFIX palimpsest: <https://palimpsest.license/vocab#>

SELECT ?work ?title ?creator
WHERE {
  ?work a palimpsest:Work ;
        dcterms:title ?title ;
        dcterms:creator ?creator ;
        palimpsest:aiConsent ?consent .

  ?consent palimpsest:trainingAllowed false .
}
```

#### Find works with trauma sensitivity
```sparql
PREFIX palimpsest: <https://palimpsest.license/vocab#>

SELECT ?work ?title ?context
WHERE {
  ?work a palimpsest:Work ;
        dcterms:title ?title ;
        palimpsest:emotionalLineage ?lineage .

  ?lineage palimpsest:emotionalContext ?context ;
           palimpsest:sensitivity "trauma" .
}
```

#### Trace lineage chain
```sparql
PREFIX palimpsest: <https://palimpsest.license/vocab#>

SELECT ?work ?creator ?generation ?parent
WHERE {
  ?lineage a palimpsest:Lineage ;
           palimpsest:rootWork <https://palimpsest.license/works/original-001> .

  ?work palimpsest:lineageChain ?lineage ;
        dcterms:creator ?creator ;
        palimpsest:generationNumber ?generation ;
        palimpsest:derivedFrom ?parent .
}
ORDER BY ?generation
```

#### Find all violations of a specific type
```sparql
PREFIX palimpsest: <https://palimpsest.license/vocab#>

SELECT ?violation ?work ?description ?status
WHERE {
  ?violation a palimpsest:Violation ;
             palimpsest:violationType "unauthorised-ai-training" ;
             palimpsest:violates ?work ;
             dcterms:description ?description ;
             palimpsest:violationStatus ?status .
}
```

#### Dataset statistics
```sparql
PREFIX void: <https://rdfs.org/ns/void#>
PREFIX palimpsest: <https://palimpsest.license/vocab#>

SELECT ?partition ?class ?entities
WHERE {
  <https://palimpsest.license/dataset> void:subset ?partition .

  ?partition void:class ?class ;
             void:entities ?entities .
}
```

### Python with rdflib

```python
from rdflib import Graph, Namespace, RDF, RDFS, Literal
from rdflib.namespace import DCTERMS, FOAF

# Load vocabularies
g = Graph()
g.parse("palimpsest-void.ttl", format="turtle")
g.parse("palimpsest-vocab.ttl", format="turtle")

# Define namespace
PALIMPSEST = Namespace("https://palimpsest.license/vocab#")
g.bind("palimpsest", PALIMPSEST)

# Add a work
work_uri = "https://palimpsest.license/works/example-001"
g.add((work_uri, RDF.type, PALIMPSEST.Work))
g.add((work_uri, DCTERMS.title, Literal("Example Work", lang="en")))
g.add((work_uri, DCTERMS.creator, Literal("Creator Name")))

# Add emotional lineage
lineage_uri = f"{work_uri}/lineage"
g.add((work_uri, PALIMPSEST.emotionalLineage, lineage_uri))
g.add((lineage_uri, RDF.type, PALIMPSEST.EmotionalLineage))
g.add((lineage_uri, PALIMPSEST.emotionalContext,
       Literal("Cultural context description")))
g.add((lineage_uri, PALIMPSEST.theme, Literal("displacement")))
g.add((lineage_uri, PALIMPSEST.sensitivity, Literal("trauma")))

# Add AI consent
consent_uri = f"{work_uri}/ai-consent"
g.add((work_uri, PALIMPSEST.aiConsent, consent_uri))
g.add((consent_uri, RDF.type, PALIMPSEST.AIConsent))
g.add((consent_uri, PALIMPSEST.trainingAllowed, Literal(False)))
g.add((consent_uri, PALIMPSEST.generationAllowed, Literal(True)))

# Query
query = """
PREFIX palimpsest: <https://palimpsest.license/vocab#>

SELECT ?work ?title
WHERE {
    ?work a palimpsest:Work ;
          dcterms:title ?title .
}
"""

for row in g.query(query):
    print(f"Work: {row.work}, Title: {row.title}")

# Serialise to file
g.serialize("output.ttl", format="turtle")
```

### JavaScript with rdflib.js

```javascript
const $rdf = require('rdflib');

// Create store and load data
const store = $rdf.graph();
const fetcher = new $rdf.Fetcher(store);

fetcher.load('palimpsest-void.ttl').then(() => {
    return fetcher.load('palimpsest-vocab.ttl');
}).then(() => {
    // Define namespaces
    const PALIMPSEST = $rdf.Namespace('https://palimpsest.license/vocab#');
    const DCTERMS = $rdf.Namespace('https://purl.org/dc/terms/');

    // Query works
    const works = store.match(null, $rdf.sym('https://www.w3.org/1999/02/22-rdf-syntax-ns#type'), PALIMPSEST('Work'));

    works.forEach(statement => {
        const work = statement.subject;
        const title = store.any(work, DCTERMS('title'));
        console.log(`Work: ${work.value}, Title: ${title.value}`);
    });
});
```

### Java with Apache Jena

```java
import org.apache.jena.rdf.model.*;
import org.apache.jena.vocabulary.*;
import org.apache.jena.query.*;

public class PalimpsestExample {
    public static void main(String[] args) {
        // Create model and load data
        Model model = ModelFactory.createDefaultModel();
        model.read("palimpsest-void.ttl", "TURTLE");
        model.read("palimpsest-vocab.ttl", "TURTLE");

        // Define namespace
        String palimpsestNS = "https://palimpsest.license/vocab#";

        // Query
        String queryString =
            "PREFIX palimpsest: <https://palimpsest.license/vocab#> " +
            "PREFIX dcterms: <https://purl.org/dc/terms/> " +
            "SELECT ?work ?title " +
            "WHERE { " +
            "  ?work a palimpsest:Work ; " +
            "        dcterms:title ?title . " +
            "}";

        Query query = QueryFactory.create(queryString);
        try (QueryExecution qexec = QueryExecutionFactory.create(query, model)) {
            ResultSet results = qexec.execSelect();
            while (results.hasNext()) {
                QuerySolution soln = results.nextSolution();
                Resource work = soln.getResource("work");
                Literal title = soln.getLiteral("title");
                System.out.println("Work: " + work + ", Title: " + title);
            }
        }
    }
}
```

## Linked Open Data Publication

### Content Negotiation

Serve different formats based on Accept header:

```nginx
location /works/ {
    if ($http_accept ~* "text/turtle") {
        rewrite ^/works/(.*)$ /data/works/$1.ttl last;
    }
    if ($http_accept ~* "application/rdf\+xml") {
        rewrite ^/works/(.*)$ /data/works/$1.rdf last;
    }
    if ($http_accept ~* "application/ld\+json") {
        rewrite ^/works/(.*)$ /data/works/$1.jsonld last;
    }
    # Default to HTML
    rewrite ^/works/(.*)$ /html/works/$1.html last;
}
```

### Dereferenceable URIs

All Palimpsest URIs should be dereferenceable:
- `https://palimpsest.license/works/example-001` → RDF metadata
- `https://palimpsest.license/vocab#Work` → Ontology definition

### 5-Star Linked Data

Palimpsest RDF achieves 5-star Linked Open Data:
1. ✓ Available on web with open licence
2. ✓ Machine-readable structured data (RDF)
3. ✓ Non-proprietary format (Turtle, RDF/XML)
4. ✓ Uses URIs to identify things
5. ✓ Links to other data (ORCID, DOI, Creative Commons)

## VoID Maintenance

Update statistics regularly:

```python
from rdflib import Graph, Namespace, Literal
from rdflib.namespace import VOID

g = Graph()
g.parse("palimpsest-void.ttl", format="turtle")

# Update statistics
dataset = "https://palimpsest.license/dataset"
PALIMPSEST = Namespace("https://palimpsest.license/vocab#")

# Count triples
total_triples = len(g)

# Count entities by type
works_count = len(list(g.subjects(RDF.type, PALIMPSEST.Work)))
derivatives_count = len(list(g.subjects(RDF.type, PALIMPSEST.Derivative)))
lineages_count = len(list(g.subjects(RDF.type, PALIMPSEST.Lineage)))
violations_count = len(list(g.subjects(RDF.type, PALIMPSEST.Violation)))

# Update VoID file
# (Implementation would update the counts in palimpsest-void.ttl)
```

## Integration with Data Catalogues

### CKAN
```python
import ckanapi

ckan = ckanapi.RemoteCKAN('https://data.example.org')

dataset = {
    'name': 'palimpsest-license-metadata',
    'title': 'Palimpsest License Metadata',
    'notes': 'RDF dataset of creative works under Palimpsest License',
    'url': 'https://palimpsest.license/dataset',
    'resources': [
        {
            'name': 'VoID Description',
            'url': 'https://palimpsest.license/void',
            'format': 'RDF/Turtle'
        },
        {
            'name': 'SPARQL Endpoint',
            'url': 'https://sparql.palimpsest.license/query',
            'format': 'api/sparql'
        }
    ]
}

ckan.action.package_create(**dataset)
```

### DataHub
```json
{
  "@context": "https://schema.org/",
  "@type": "Dataset",
  "name": "palimpsest-license-metadata",
  "description": "RDF dataset of creative works under Palimpsest License",
  "url": "https://palimpsest.license/dataset",
  "distribution": [
    {
      "@type": "DataDownload",
      "encodingFormat": "text/turtle",
      "contentUrl": "https://palimpsest.license/data/dump.ttl.gz"
    },
    {
      "@type": "DataDownload",
      "encodingFormat": "application/rdf+xml",
      "contentUrl": "https://palimpsest.license/data/dump.rdf.gz"
    }
  ]
}
```

## Validation

### SHACL Validation
```turtle
@prefix sh: <https://www.w3.org/ns/shacl#> .
@prefix palimpsest: <https://palimpsest.license/vocab#> .

palimpsest:WorkShape
    a sh:NodeShape ;
    sh:targetClass palimpsest:Work ;
    sh:property [
        sh:path dcterms:title ;
        sh:minCount 1 ;
        sh:datatype xsd:string ;
    ] ;
    sh:property [
        sh:path palimpsest:aiConsent ;
        sh:minCount 1 ;
        sh:class palimpsest:AIConsent ;
    ] .
```

### OWL Reasoning

Enable OWL reasoning in triple store for:
- Inference of implicit relationships
- Consistency checking
- Classification

## Performance Optimisation

### Indexing
```sql
-- Virtuoso indexes
CREATE INDEX works_creator ON DB.DBA.RDF_QUAD (P, O)
WHERE P = 'https://purl.org/dc/terms/creator';

CREATE INDEX works_ai_consent ON DB.DBA.RDF_QUAD (P, O)
WHERE P = 'https://palimpsest.license/vocab#aiConsent';
```

### Caching
Implement HTTP caching headers:
```http
Cache-Control: public, max-age=3600
ETag: "abc123"
Last-Modified: Tue, 22 Nov 2025 12:00:00 GMT
```

## Licence

This VoID description and vocabulary are licensed under the Palimpsest License v0.4.

## Further Reading

- [VoID Specification](https://www.w3.org/TR/void/)
- [OWL 2 Web Ontology Language](https://www.w3.org/TR/owl2-overview/)
- [SPARQL 1.1 Query Language](https://www.w3.org/TR/sparql11-query/)
- [Linked Data Patterns](https://patterns.dataincubator.org/)
- [5-Star Open Data](https://5stardata.info/)
- [Best Practices for Publishing Linked Data](https://www.w3.org/TR/ld-bp/)
