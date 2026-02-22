# Palimpsest OCaml Library

OCaml implementation of Palimpsest License metadata tools. Supports both native compilation and JavaScript output via Melange.

## Structure

```
ocaml/
├── lib/                 # Core library (native OCaml)
│   ├── types.ml        # Type definitions
│   ├── parser.ml       # JSON parsing/serialisation
│   ├── compliance.ml   # Compliance checking
│   └── palimpsest.ml   # Main module
├── browser/            # Melange build target (OCaml → JS)
│   └── palimpsest_browser.ml
├── test/               # Test suite
│   └── test_palimpsest.ml
└── dune-project        # Build configuration
```

## Building

### Native (server-side, CLI tools)

```bash
cd ocaml
opam install . --deps-only
dune build
dune test
```

### JavaScript (browser, via Melange)

```bash
cd ocaml
opam install melange
dune build @melange
# Output in _build/default/browser/output/
```

## Dependencies

- **ocaml** >= 4.14.0
- **dune** >= 3.0
- **yojson** - JSON parsing (native)
- **melange** >= 3.0 - JavaScript compilation
- **alcotest** - Testing (dev)

## Usage

### Native OCaml

```ocaml
open Palimpsest

(* Parse metadata from JSON *)
let metadata = parse {|{"workTitle": "My Work", "licenseUri": "..."}|}

(* Check compliance *)
let result = check_compliance metadata [Training; Interpretive]

(* Generate report *)
let report = generate_report result
```

### From JavaScript (after Melange build)

```javascript
import * as Palimpsest from './palimpsest_browser.js';

const metadata = Palimpsest.create_metadata("My Work", "https://...");
const isAIAllowed = Palimpsest.is_training_permitted(metadata);
```

## Note on Dual Targets

The native library uses `yojson` for JSON handling. For Melange/browser builds, the browser module provides a JS-friendly interface. Full Melange support may require `melange-json` for pure-JS JSON handling in future versions.

## License

MIT - Palimpsest Stewardship Council
