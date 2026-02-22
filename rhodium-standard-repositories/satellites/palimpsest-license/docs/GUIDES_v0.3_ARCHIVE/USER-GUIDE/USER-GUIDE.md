# Developer Guide

## 1. Embedding Tags
\\\python
from palimpsest import add_license_tag
add_license_tag("output.json", "Palimpsest-0.3", creator="Jane Doe")
\\\

## 2. License Validation
\\\julia
using LicenseParser
validate("path/to/asset.xml", "Palimpsest-0.3")
\\\
"@ | Out-File -FilePath ".\docs\GUIDES\DEVELOPER-GUIDE\DEVELOPER-GUIDE.md" -Encoding UTF8

@"
# User Guide

## 1. Complying with Palimpsest
- Add \syntheticLineageTag\ to AI outputs.
- Disclose training in model cards.

## 2. AGPL Compliance
- Share modifications publicly.
- Use \TOOLS/LICENSE-PARSER\ to validate.
