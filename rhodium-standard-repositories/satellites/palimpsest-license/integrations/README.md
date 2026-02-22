# Palimpsest License - Integration Scripts

Comprehensive integration scripts and tools for implementing the Palimpsest License across different platforms, frameworks, and technologies.

## Overview

This directory contains production-ready integrations for:
- **Server-side** frameworks (Node.js, Python, PHP, Ruby)
- **Client-side** libraries and browser extensions
- **Platform-specific** tools (GitHub Actions, GitLab CI, WordPress, SSGs)
- **API specifications** (REST, GraphQL, OpenAPI)

## Quick Start

### Server-side Integration

#### Node.js/Express

```bash
npm install @palimpsest/license-middleware
```

```javascript
import { palimpsestMiddleware } from '@palimpsest/license-middleware';

app.use(palimpsestMiddleware({
  workTitle: 'My Work',
  authorName: 'Jane Doe',
  emotionalLineage: 'A reflection on diaspora and belonging'
}));
```

[Full Documentation](./server/nodejs/README.md)

#### Python/Flask

```bash
pip install palimpsest-license
```

```python
from palimpsest import PalimpsestFlask, PalimpsestConfig

config = PalimpsestConfig(
    work_title="My Work",
    author_name="Jane Doe"
)
palimpsest = PalimpsestFlask(app, config)
```

[Full Documentation](./server/python/README.md)

#### PHP

```bash
composer require palimpsest/license-php
```

```php
use Palimpsest\PalimpsestConfig;
use Palimpsest\PalimpsestLicense;

$config = new PalimpsestConfig([
    'workTitle' => 'My Work',
    'authorName' => 'Jane Doe'
]);
$license = new PalimpsestLicense($config);
$license->injectHeaders();
```

[Full Documentation](./server/php/)

#### Ruby/Rails

```bash
gem install palimpsest-license
```

```ruby
# config/application.rb
config.middleware.use Palimpsest::Middleware, {
  work_title: 'My Work',
  author_name: 'Jane Doe'
}
```

[Full Documentation](./server/ruby/)

### Client-side Integration

#### JavaScript Library

```html
<script src="https://unpkg.com/@palimpsest/license-client@latest/dist/palimpsest.js"></script>
<script>
  const verification = Palimpsest.verifyLicense();
  const badge = Palimpsest.createLicenseBadge({ theme: 'light' });
  document.body.appendChild(badge);
</script>
```

[Full Documentation](./client/javascript/README.md)

#### Web Components

```html
<script src="palimpsest-badge.js"></script>
<palimpsest-badge version="0.4" theme="light" language="en"></palimpsest-badge>
```

[Full Documentation](./client/web-components/)

#### Browser Extension

Install the Palimpsest License Verifier extension to automatically detect and verify license metadata on any webpage.

[Full Documentation](./client/browser-extension/)

### Platform-specific Integration

#### GitHub Actions

```yaml
# .github/workflows/license-check.yml
name: License Validation
on: [push, pull_request]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: palimpsest-license/palimpsest-license/integrations/platforms/github-actions@main
```

[Full Documentation](./platforms/github-actions/README.md)

#### GitLab CI

```yaml
# .gitlab-ci.yml
include:
  - local: 'integrations/platforms/gitlab-ci/.gitlab-ci-palimpsest.yml'

palimpsest:validate:
  extends: .palimpsest_validation
```

[Full Documentation](./platforms/gitlab-ci/)

#### WordPress

Install the Palimpsest License plugin from the WordPress plugin directory or upload manually.

[Full Documentation](./platforms/wordpress/)

#### Static Site Generators

**Hugo:**
```html
{{ partial "palimpsest-meta.html" . }}
{{ partial "palimpsest-jsonld.html" . }}
```

**Jekyll:**
```liquid
{% include palimpsest-meta.html %}
{% include palimpsest-jsonld.html %}
```

**Eleventy:**
```javascript
const palimpsestPlugin = require('./palimpsest-plugin.js');
eleventyConfig.addPlugin(palimpsestPlugin, { /* config */ });
```

[Full Documentation](./platforms/static-site-generators/)

## API Specifications

### REST API

```bash
curl https://api.palimpsestlicense.org/v1/validate/metadata \
  -H "X-API-Key: your_key" \
  -H "Content-Type: application/json" \
  -d '{"@type": "CreativeWork", "license": "...", "author": {...}}'
```

[Full Documentation](./api/rest/endpoints.md)

### GraphQL

```graphql
query {
  validateMetadata(metadata: {
    license: "https://palimpsestlicense.org/v0.4"
    author: { type: "Person", name: "Jane Doe" }
  }) {
    valid
    errors
  }
}
```

[Full Documentation](./api/graphql/)

### OpenAPI/Swagger

Interactive API documentation available at:
- Swagger UI: https://api.palimpsestlicense.org/docs
- OpenAPI Spec: [palimpsest-license-api.yaml](./api/openapi/palimpsest-license-api.yaml)

## Directory Structure

```
integrations/
├── server/                       # Server-side integrations
│   ├── nodejs/                   # Node.js/Express middleware
│   ├── python/                   # Python/Flask integration
│   ├── php/                      # PHP library
│   ├── ruby/                     # Ruby/Rails integration
│   └── nginx-apache/             # Web server configs
├── client/                       # Client-side integrations
│   ├── javascript/               # JavaScript library
│   ├── typescript/               # TypeScript definitions
│   ├── web-components/           # Custom HTML elements
│   └── browser-extension/        # Browser extension
├── platforms/                    # Platform-specific tools
│   ├── github-actions/           # GitHub Actions workflow
│   ├── gitlab-ci/                # GitLab CI template
│   ├── wordpress/                # WordPress plugin
│   └── static-site-generators/   # SSG plugins (Hugo, Jekyll, Eleventy)
└── api/                          # API specifications
    ├── rest/                     # REST API documentation
    ├── graphql/                  # GraphQL schema
    └── openapi/                  # OpenAPI specification
```

## Features

### All Integrations Include

✅ **Automatic Metadata Injection**
- HTTP headers with license information
- HTML meta tags for SEO
- JSON-LD structured data
- Machine-readable formats

✅ **Bilingual Support**
- English and Dutch translations
- Language-aware widgets
- Localised error messages

✅ **Customisation**
- Configurable license versions
- Custom metadata fields
- Theme support (light/dark)
- Flexible formatting options

✅ **Validation**
- Metadata compliance checking
- Required field verification
- Format validation
- Error reporting

✅ **Compliance**
- Clause 2.3: Metadata preservation
- Clause 1.2: AGI consent declaration
- Emotional lineage protection
- Quantum-proof traceability

## Usage Examples

### Complete Integration Example

```javascript
// Server-side (Express)
import express from 'express';
import { palimpsestMiddleware } from '@palimpsest/license-middleware';

const app = express();

app.use(palimpsestMiddleware({
  workTitle: 'Voices of the Diaspora',
  authorName: 'Amara Okafor',
  emotionalLineage: 'Stories of migration and belonging',
  version: '0.4',
  language: 'en'
}));

app.get('/', (req, res) => {
  res.send(`
    <!DOCTYPE html>
    <html>
      <head>
        <title>My Work</title>
        <!-- License metadata automatically injected -->
      </head>
      <body>
        <h1>My Creative Work</h1>

        <!-- Client-side verification -->
        <div id="license-badge"></div>

        <script src="palimpsest.js"></script>
        <script>
          const badge = Palimpsest.createLicenseBadge({ theme: 'light' });
          document.getElementById('license-badge').appendChild(badge);
        </script>
      </body>
      <!-- JSON-LD automatically injected -->
    </html>
  `);
});

app.listen(3000);
```

### Multi-platform Workflow

```yaml
# GitHub Actions
name: Full Validation
on: [push]

jobs:
  validate-license:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      # Validate license metadata
      - uses: palimpsest-license/palimpsest-license/integrations/platforms/github-actions@main
        with:
          check-html: true
          fail-on-missing: true

      # Deploy with license intact
      - run: npm run build
      - run: npm run deploy
```

## Contributing

We welcome contributions! Please see:
- [Contributing Guidelines](../CONTRIBUTING.md)
- [Code of Practice](../CODE_OF_PRACTICE.md)
- [Governance](../GOVERNANCE.md)

### Adding New Integrations

1. Create directory: `integrations/{category}/{platform}/`
2. Include comprehensive README
3. Add examples and tests
4. Update this main README
5. Submit pull request

## Testing

Each integration includes its own test suite:

```bash
# Node.js
cd server/nodejs && npm test

# Python
cd server/python && pytest

# PHP
cd server/php && composer test

# Ruby
cd server/ruby && rake test
```

## Support

- **Documentation:** https://palimpsestlicense.org
- **Issues:** https://github.com/palimpsest-license/palimpsest-license/issues
- **Email:** hello@palimpsestlicense.org
- **API Support:** api@palimpsestlicense.org

## Licence

All integration scripts are licensed under the Palimpsest License v0.4.

## Related Resources

- [User Guide](../GUIDES_v0.4/User_Guide.md)
- [Developer Guide](../GUIDES_v0.4/Developer_Guide.md)
- [Compliance Roadmap](../GUIDES_v0.4/Compliance_Roadmap.md)
- [Red Flag Index](../GUIDES_v0.4/Red_Flag_Index.md)
- [Audit Template](../TOOLKIT_v0.4/Audit_Template.md)

---

**Version:** 0.4.0
**Last Updated:** 2025-11-22
**Maintained by:** Palimpsest License Contributors
