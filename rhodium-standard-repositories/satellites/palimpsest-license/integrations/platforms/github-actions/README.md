# Palimpsest License - GitHub Actions

GitHub Action for validating Palimpsest License metadata in your repository.

## Usage

### Basic Validation

```yaml
name: License Validation
on: [push, pull_request]

jobs:
  validate-license:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Validate Palimpsest License
        uses: palimpsest-license/palimpsest-license/integrations/platforms/github-actions@main
```

### Advanced Configuration

```yaml
name: License Validation
on: [push, pull_request]

jobs:
  validate-license:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Validate Palimpsest License
        uses: palimpsest-license/palimpsest-license/integrations/platforms/github-actions@main
        with:
          # Path to license file
          license-file: 'LICENSE.md'

          # Path to metadata file
          metadata-file: 'metadata/palimpsest.jsonld'

          # Check HTML files for license metadata
          check-html: 'true'

          # Glob pattern for HTML files
          html-pattern: '**/*.html'

          # Fail workflow if metadata is missing
          fail-on-missing: 'false'

          # Generate license badge
          generate-badge: 'true'
```

### Use Outputs in Other Steps

```yaml
- name: Validate Palimpsest License
  id: license
  uses: palimpsest-license/palimpsest-license/integrations/platforms/github-actions@main

- name: Check validation result
  if: steps.license.outputs.license-valid == 'true'
  run: echo "License is valid!"

- name: Report metadata count
  run: echo "Found metadata in ${{ steps.license.outputs.metadata-found }} files"
```

## Inputs

| Input | Description | Required | Default |
|-------|-------------|----------|---------|
| `license-file` | Path to license file | No | `LICENSE.md` |
| `metadata-file` | Path to metadata file (JSON-LD) | No | `metadata/palimpsest.jsonld` |
| `check-html` | Check HTML files for license metadata | No | `true` |
| `html-pattern` | Glob pattern for HTML files | No | `**/*.html` |
| `fail-on-missing` | Fail workflow if metadata is missing | No | `false` |
| `generate-badge` | Generate license badge in output | No | `true` |

## Outputs

| Output | Description |
|--------|-------------|
| `license-valid` | Whether the license metadata is valid (`true`/`false`) |
| `metadata-found` | Number of files with license metadata |
| `errors` | Validation errors (if any) |

## Examples

### Validate on Pull Request

```yaml
name: PR License Check
on:
  pull_request:
    types: [opened, synchronize]

jobs:
  check-license:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Validate License
        uses: palimpsest-license/palimpsest-license/integrations/platforms/github-actions@main
        with:
          fail-on-missing: 'true'

      - name: Comment on PR
        if: failure()
        uses: actions/github-script@v7
        with:
          script: |
            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: '❌ License validation failed. Please ensure Palimpsest License metadata is present and valid.'
            })
```

### Generate Badge and Update README

```yaml
name: Update License Badge
on:
  push:
    branches: [main]

jobs:
  update-badge:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Validate and Generate Badge
        id: license
        uses: palimpsest-license/palimpsest-license/integrations/platforms/github-actions@main
        with:
          generate-badge: 'true'
```

## Requirements

- GitHub Actions runner with bash
- Optional: `jq` for JSON validation (usually pre-installed)

## Contributing

See [CONTRIBUTING.md](../../../../CONTRIBUTING.md) for guidelines.

## Licence

This action is licensed under the Palimpsest License v0.4.
