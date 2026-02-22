# Palimpsest License REST API

REST API endpoints for Palimpsest License validation and metadata management.

## Base URL

```
https://api.palimpsestlicense.org/v1
```

## Authentication

All API requests require an API key passed in the `X-API-Key` header:

```bash
curl -H "X-API-Key: your_api_key_here" \
  https://api.palimpsestlicense.org/v1/licenses/0.4
```

## Endpoints

### Get License Information

Get information about a specific version of the Palimpsest License.

```http
GET /licenses/{version}
```

**Parameters:**
- `version` (path, required): License version (e.g., "0.4")
- `format` (query, optional): Response format (json, markdown, html, text)

**Example Request:**

```bash
curl https://api.palimpsestlicense.org/v1/licenses/0.4
```

**Example Response:**

```json
{
  "version": "0.4",
  "name": "Palimpsest License v0.4",
  "identifier": "Palimpsest-0.4",
  "url": "https://palimpsestlicense.org/v0.4",
  "summary": "A future-proof, layered licensing framework for creative work",
  "features": [
    "Emotional lineage protection",
    "AGI consent requirements",
    "Metadata preservation",
    "Quantum-proof traceability"
  ]
}
```

### Validate Metadata

Validate JSON-LD metadata for Palimpsest License compliance.

```http
POST /validate/metadata
```

**Request Body:**

```json
{
  "@context": "https://schema.org",
  "@type": "CreativeWork",
  "name": "My Work",
  "license": "https://palimpsestlicense.org/v0.4",
  "author": {
    "@type": "Person",
    "name": "Jane Doe"
  }
}
```

**Example Response:**

```json
{
  "valid": true,
  "errors": [],
  "warnings": [],
  "metadata": { ... }
}
```

### Validate URL

Check if a URL contains valid Palimpsest License metadata.

```http
POST /validate/url
```

**Request Body:**

```json
{
  "url": "https://example.com",
  "checkHtml": true,
  "checkJsonLd": true
}
```

**Example Response:**

```json
{
  "url": "https://example.com",
  "valid": true,
  "metadataFound": true,
  "metadata": { ... },
  "sources": ["html-meta", "jsonld"],
  "errors": []
}
```

### Generate Metadata

Generate Palimpsest License metadata in various formats.

```http
POST /metadata/generate
```

**Request Body:**

```json
{
  "workTitle": "My Creative Work",
  "authorName": "Jane Doe",
  "authorUrl": "https://example.com/jane",
  "emotionalLineage": "A reflection on diaspora and belonging",
  "version": "0.4",
  "agiConsentRequired": true,
  "format": "all"
}
```

**Example Response:**

```json
{
  "jsonld": "{\n  \"@context\": \"https://schema.org\",\n  ...\n}",
  "htmlMeta": "<meta name=\"license\" content=\"...\">...",
  "httpHeaders": {
    "X-License": "Palimpsest-0.4",
    "X-License-Url": "https://palimpsestlicense.org/v0.4",
    ...
  }
}
```

### Check Compliance

Comprehensive compliance check for Palimpsest License.

```http
POST /compliance/check
```

**Request Body:**

```json
{
  "metadata": { ... },
  "strictMode": false
}
```

**Example Response:**

```json
{
  "compliant": true,
  "checks": {
    "metadataPresent": true,
    "requiredFieldsPresent": true,
    "emotionalLineageDeclared": true,
    "agiConsentSpecified": true
  },
  "recommendations": [
    "Consider adding more detailed emotional lineage description"
  ],
  "errors": []
}
```

## Error Responses

All endpoints return errors in the following format:

```json
{
  "error": "ValidationError",
  "message": "Missing required field: author.name",
  "code": "MISSING_REQUIRED_FIELD",
  "details": {
    "field": "author.name"
  }
}
```

### Error Codes

- `INVALID_VERSION` - Invalid license version
- `MISSING_REQUIRED_FIELD` - Required field missing in metadata
- `INVALID_METADATA` - Metadata format is invalid
- `INVALID_URL` - Invalid URL provided
- `VALIDATION_FAILED` - Validation failed
- `UNAUTHORIZED` - Missing or invalid API key
- `RATE_LIMIT_EXCEEDED` - Too many requests

## Rate Limiting

API requests are rate-limited to:
- 100 requests per minute (authenticated)
- 10 requests per minute (unauthenticated)

Rate limit headers:
```
X-RateLimit-Limit: 100
X-RateLimit-Remaining: 95
X-RateLimit-Reset: 1234567890
```

## Examples

### Node.js

```javascript
const axios = require('axios');

async function validateMetadata(metadata) {
  try {
    const response = await axios.post(
      'https://api.palimpsestlicense.org/v1/validate/metadata',
      metadata,
      {
        headers: {
          'X-API-Key': process.env.PALIMPSEST_API_KEY,
          'Content-Type': 'application/json'
        }
      }
    );
    return response.data;
  } catch (error) {
    console.error('Validation error:', error.response.data);
  }
}
```

### Python

```python
import requests

def validate_url(url):
    response = requests.post(
        'https://api.palimpsestlicense.org/v1/validate/url',
        json={'url': url},
        headers={'X-API-Key': os.getenv('PALIMPSEST_API_KEY')}
    )
    return response.json()
```

### cURL

```bash
# Validate metadata
curl -X POST \
  https://api.palimpsestlicense.org/v1/validate/metadata \
  -H "X-API-Key: your_api_key" \
  -H "Content-Type: application/json" \
  -d '{
    "@type": "CreativeWork",
    "license": "https://palimpsestlicense.org/v0.4",
    "author": {"@type": "Person", "name": "Jane Doe"}
  }'
```

## Webhooks

Register webhooks to receive notifications about:
- License updates
- Validation failures
- Compliance changes

See [Webhooks Documentation](./webhooks.md) for details.

## SDKs

Official SDKs available:
- JavaScript/TypeScript: `@palimpsest/api-client`
- Python: `palimpsest-api`
- PHP: `palimpsest/api-client`
- Ruby: `palimpsest-api`

## Support

- API Status: https://status.palimpsestlicense.org
- Documentation: https://docs.palimpsestlicense.org/api
- Email: api@palimpsestlicense.org
