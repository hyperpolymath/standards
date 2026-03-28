# LOL i18n Service — ABI/FFI/API V-Triple Documentation

## Overview

The LOL (Language of Languages) i18n service follows the **Hyperpolymath V-Triple Standard**:

- **ABI (Application Binary Interface)** defined in **Idris2** with formal proofs
- **FFI (Foreign Function Interface)** implemented in **Zig** for C compatibility
- **API** exposed in **V-lang** for idiomatic high-level access

LOL is always called as a service, never embedded. Consumers initialise with
`lol_init()`, perform translation lookups and plural form selection across
1500+ languages, then clean up with `lol_free()`.

## Architecture

```
┌─────────────────────────────────────────────────┐
│  ABI Definitions (Idris2)                       │
│  src/abi/                                       │
│  - Types.idr    (Result, Locale, PluralCategory,│
│                  LanguageInfo, TranslationResult)│
│  - Layout.idr   (Memory layout proofs for each  │
│                  C struct that crosses the FFI)  │
│  - Foreign.idr  (FFI function declarations with │
│                  safe wrappers)                  │
└─────────────────┬───────────────────────────────┘
                  │
                  │ generates
                  ▼
┌─────────────────────────────────────────────────┐
│  C Headers (auto-generated)                     │
│  generated/abi/lol.h                            │
│  - lol_result_t, lol_plural_category_t          │
│  - lol_locale_t, lol_translation_result_t       │
│  - lol_language_info_t, lol_plural_rule_t       │
│  - All function prototypes                      │
└─────────────────┬───────────────────────────────┘
                  │
                  │ implemented by
                  ▼
┌─────────────────────────────────────────────────┐
│  FFI Implementation (Zig)                       │
│  ffi/zig/src/main.zig                           │
│  - BCP 47 locale parsing                        │
│  - CLDR plural rule engine                      │
│  - Corpus data directory access                 │
│  - Translation lookup with fallback chains      │
│  - Thread-safe error handling                   │
└─────────────────┬───────────────────────────────┘
                  │
                  │ linked by (-llol)
                  ▼
┌─────────────────────────────────────────────────┐
│  V-lang API (idiomatic wrapper)                 │
│  api/v-lol/src/                                 │
│  - lol.v     (Service struct, public methods)   │
│  - ffi.v     (Raw C bindings)                   │
│  - types.v   (V types: Locale, PluralCategory,  │
│               TranslationResult, LanguageInfo)  │
└─────────────────────────────────────────────────┘
```

## Directory Structure

```
lol/
├── src/
│   └── abi/                         # ABI definitions (Idris2)
│       ├── Types.idr                # Core types with formal proofs
│       ├── Layout.idr               # Memory layout verification
│       └── Foreign.idr              # FFI function declarations
├── ffi/
│   └── zig/                         # FFI implementation (Zig)
│       ├── build.zig                # Build configuration
│       ├── src/main.zig             # Implementation
│       └── test/integration_test.zig# Integration tests
├── generated/
│   └── abi/
│       └── lol.h                    # Auto-generated C header
├── api/
│   ├── v-lol/                       # V-lang API wrapper
│   │   ├── v.mod                    # V module definition
│   │   └── src/
│   │       ├── lol.v                # Public API (Service struct)
│   │       ├── ffi.v                # Raw C bindings
│   │       └── types.v              # V types and enums
│   └── v-gateway/                   # Existing triple API gateway
│       └── src/                     # REST + gRPC + GraphQL
└── ABI-FFI-README.md                # This file
```

## Key Types

| Type | Idris2 | C | V |
|------|--------|---|---|
| Result codes | `Result` | `lol_result_t` | `LolError` |
| Plural category | `PluralCategory` | `lol_plural_category_t` | `PluralCategory` |
| Locale | `Locale` | `lol_locale_t` | `Locale` |
| Translation | `TranslationResult` | `lol_translation_result_t` | `TranslationResult` |
| Language info | `LanguageInfo` | `lol_language_info_t` | `LanguageInfo` |
| Plural rule | `PluralRule` | `lol_plural_rule_t` | `PluralRule` |

## Formal Proofs (Idris2 ABI)

The Idris2 ABI layer provides the following compile-time guarantees:

1. **Result code round-trip**: `resultFromInt (resultToInt r) = Just r` for all `r`
2. **Plural category round-trip**: `pluralFromInt (pluralToInt p) = Just p` for all `p`
3. **Result code injectivity**: distinct results map to distinct integer codes
4. **Handle non-null**: the `Handle` type cannot wrap a null pointer (enforced by `So`)
5. **Struct alignment**: all C struct layouts are proven to have correctly aligned fields
6. **Plural form count bounds**: `PluralRule.formCount` is proven to be in range 1-6

No `believe_me`, `assert_total`, or other escape hatches are used.

## Building

### Zig FFI

```bash
cd ffi/zig
zig build              # Builds liblol.so and liblol.a
zig build test         # Runs unit tests
zig build test-integration  # Runs integration tests
```

### V-lang API

```bash
cd api/v-lol
v -cflags "-L../../ffi/zig/zig-out/lib" src/   # Build with liblol
```

## Usage (V-lang)

```v
import lol

fn main() {
    // Open the service with a corpus data directory
    mut svc := lol.open('/path/to/corpus') or { panic(err) }
    defer svc.close()

    // Translate a key
    result := svc.translate('en-US', 'app.greeting') or {
        println('Translation not found, using default')
        lol.TranslationResult{ text: 'Hello!', resolved_locale: 'en' }
    }
    println(result.text)

    // Select plural form
    cat := svc.select_plural('ar', 5)
    println('Arabic plural for 5: ${cat}')  // "few"

    // Get language metadata
    info := svc.get_language('eng') or { panic(err) }
    println('${info.name} (${info.native_name}): ${info.verse_count} verses')

    // Get plural rule
    rule := svc.get_plural_rule('ru') or { panic(err) }
    println('Russian has ${rule.form_count} plural forms')
}
```

## License

SPDX-License-Identifier: PMPL-1.0-or-later

Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
