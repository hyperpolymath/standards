# Palimpsest Validator Test Suite

## Overview

The Palimpsest Validator test suite provides comprehensive coverage of the validation tools for the Palimpsest License project. The test suite is designed to achieve 80%+ code coverage across all validator modules.

## Test Structure

### Unit Tests

#### 1. **LicenseSpec.hs**
Tests for license document validation:
- Version extraction (standard format, keyword format, edge cases)
- License header detection
- Language detection (English/Dutch)
- Clause number parsing (integer, decimal, malformed)
- Clause extraction from markdown
- Governing law extraction
- Clause pattern detection
- License structure parsing
- Duplicate clause number detection
- Property-based tests using QuickCheck
- Edge cases and malformed input handling
- Common pitfall detection (Clause 1.2, 2.3)

**Coverage Areas:**
- `extractVersion`
- `hasLicenseHeader`
- `detectLanguage`
- `parseClauseNumber`
- `parseLicenseStructure`
- `extractGoverningLaw`
- `isClausePattern`
- `findDuplicates`

#### 2. **MetadataSpec.hs**
Tests for metadata schema validation:
- File type detection (JSON-LD, XML)
- Context extraction from JSON-LD
- License ID extraction
- Metadata version extraction
- JSON-LD structure validation
- SPDX license validation
- Dublin Core validation
- Malformed metadata handling
- Palimpsest-specific validation
- Edge cases (empty objects, null values, nested structures)

**Coverage Areas:**
- `isJSONLDFile`, `isXMLFile`
- `extractContext`
- `extractLicenseId`
- `extractMetadataVersion`
- `validateJSONLDStructure`
- `validateSPDXLicense`
- `validateDublinCore`

#### 3. **BilingualSpec.hs**
Tests for bilingual consistency validation:
- Title matching (case-insensitive, formatting differences)
- Bilingual map parsing (markdown tables)
- Default bilingual mappings validation
- Title normalization
- Bilingual mapping lookup
- Property-based tests (reflexivity, symmetry)
- Edge cases (empty titles, special characters, long titles)
- Version consistency checks
- Clause alignment verification

**Coverage Areas:**
- `titleMatches`
- `parseTableRow`
- `parseBilingualMap`
- `defaultBilingualMap`
- `checkStructuralParity`
- `checkClauseAlignment`

#### 4. **ReferenceSpec.hs** (New)
Tests for cross-reference validation:
- Clause reference extraction
- File reference extraction
- URL reference extraction
- Internal link extraction
- Heading to anchor conversion
- Anchor existence checking
- URL format validation
- Reference type discrimination
- Edge cases (malformed links, nested brackets)
- Complex reference scenarios

**Coverage Areas:**
- `extractClauseRefs`
- `extractFileRefs`
- `extractURLRefs`
- `extractInternalLinks`
- `headingToAnchor`
- `checkAnchorExists`
- `isValidURL`
- `validateReference`

#### 5. **UtilsSpec.hs** (New)
Tests for utility functions:
- Text normalization (lowercase, trimming, whitespace handling)
- Clause number extraction
- Clause reference parsing
- File type detection (markdown, JSON-LD, XML)
- Relative path calculation
- Property-based tests
- Edge cases (unicode, empty strings, long text)
- Special character handling

**Coverage Areas:**
- `normaliseText`
- `extractClauseNumber`
- `parseClauseReference`
- `isMarkdownFile`
- `isJSONLDFile`
- `isXMLFile`
- `relativePath`

### Integration Tests

#### 6. **Integration/PipelineSpec.hs** (New)
End-to-end validation pipeline tests:
- Complete license validation workflow
- Metadata extraction and parsing pipeline
- Bilingual consistency workflow
- Reference validation workflow
- Multi-component validation
- Error handling and recovery
- Real-world scenarios
- Cross-validation scenarios
- Performance and scalability tests

**Coverage Areas:**
- Full validation pipeline
- Component integration
- Error handling
- Edge cases at system level

## Property-Based Testing

The test suite uses QuickCheck for property-based testing to verify:
- Functions never crash on arbitrary input
- Invariants hold across random inputs
- Consistency properties (e.g., symmetry, reflexivity)
- Idempotence where applicable

## Running Tests

### Basic Test Run
```bash
cd TOOLS/validation/haskell
cabal test
```

### Test with Coverage
```bash
just test-coverage
```

or

```bash
cd TOOLS/validation/haskell
cabal test --enable-coverage
```

### Generate HTML Coverage Report
```bash
cd TOOLS/validation/haskell
cabal test --enable-coverage
hpc markup dist-newstyle/build/.../palimpsest-validator-test.tix --destdir=coverage-report
open coverage-report/hpc_index.html
```

## Coverage Goals

**Target:** 80%+ coverage across all modules

### Expected Coverage by Module:

| Module | Target Coverage | Priority |
|--------|----------------|----------|
| License | 85%+ | High |
| Metadata | 80%+ | High |
| Bilingual | 80%+ | High |
| Reference | 75%+ | Medium |
| Utils | 90%+ | High |
| Types | 60%+ | Low |

## Test Organization

```
test/
├── Spec.hs                              # Main test runner
├── Palimpsest/
│   └── Validator/
│       ├── LicenseSpec.hs              # License validation tests
│       ├── MetadataSpec.hs             # Metadata validation tests
│       ├── BilingualSpec.hs            # Bilingual consistency tests
│       ├── ReferenceSpec.hs            # Reference validation tests (NEW)
│       └── UtilsSpec.hs                # Utility function tests (NEW)
└── Integration/
    └── PipelineSpec.hs                 # Integration tests (NEW)
```

## Test Statistics

**Total Test Cases:** 150+

Breakdown:
- LicenseSpec: 35+ tests
- MetadataSpec: 30+ tests
- BilingualSpec: 35+ tests
- ReferenceSpec: 30+ tests
- UtilsSpec: 25+ tests
- Integration: 15+ tests

## Continuous Integration

Tests should be run:
1. Before every commit
2. In CI/CD pipeline
3. Before creating pull requests
4. After dependency updates

## Adding New Tests

When adding new tests:
1. Follow existing naming conventions
2. Use descriptive test names
3. Include both positive and negative test cases
4. Add property-based tests for pure functions
5. Document complex test scenarios
6. Update this file with new coverage areas

## Known Limitations

1. Some I/O operations are difficult to test in pure unit tests
2. Property-based tests may occasionally find edge cases that need investigation
3. Coverage metrics may not capture all logical branches

## Future Improvements

- [ ] Add mutation testing
- [ ] Implement fuzzing for parser functions
- [ ] Add performance benchmarks
- [ ] Create test fixtures for realistic license documents
- [ ] Add snapshot testing for output formatting
- [ ] Implement contract testing for API boundaries
