# AVOW Protocol Website Architecture

## Multi-Layer Stack (All Layers Working Together)

### Build-Time (Static Generation)
- **Casket-SSG (Haskell)** - Generates static HTML from Markdown
- Runs once during deployment
- Pure functional, predictable output

### Run-Time (Client-Side)
- **ReScript** - Type-safe JavaScript compilation
- **ReScript-TEA** - The Elm Architecture for predictable state management
- **Cadre-TEA-Router** - Type-safe client-side routing

### Verification Layers
- **k9-svc** - Self-Validating Components
  - Validates form inputs
  - Proves unsubscribe links work before sending
  - Runtime invariant checking

- **a2ml** - Attested Markup Language
  - Typed, verifiable consent proofs
  - Cryptographic timestamping
  - Content with mathematical guarantees

- **proven (Idris2)** - Formally Verified Operations
  - SafeString - String operations that can't crash
  - SafeUrl - URL parsing with proofs
  - SafeHtml - Safe HTML generation
  - All operations mathematically proven correct

## Architecture Flow

```
User Request
    ↓
Casket (Haskell) → Static HTML
    ↓
Browser Loads Page
    ↓
ReScript-TEA Initializes
    ↓
User Interaction (click demo)
    ↓
TEA Update (pure state transition)
    ↓
k9-svc Validates (proves correctness)
    ↓
a2ml Generates Proof (cryptographic)
    ↓
proven Ensures Safety (Idris2 proofs)
    ↓
DOM Updated (ReScript)
```

## Why This Stack?

### Casket-SSG (Haskell)
- ✅ Keeps Haskell for what it does best: pure transformations
- ✅ Perfect for static site generation
- ✅ Not destroyed or replaced - enhanced!

### ReScript + TEA
- ✅ Type safety for client-side code
- ✅ Predictable state management
- ✅ Model-Update-View pattern
- ✅ Compile-time guarantees

### k9-svc
- ✅ Components prove their own correctness
- ✅ Runtime validation with proofs
- ✅ Used where relevant (form validation, link checking)

### a2ml
- ✅ Typed markup for consent proofs
- ✅ Verifiable content
- ✅ Used where relevant (cryptographic proofs)

### proven (Idris2)
- ✅ Mathematical proofs of correctness
- ✅ Operations that cannot crash
- ✅ Used for super-robust critical paths
- ✅ SafeUrl, SafeString, SafeHtml modules

## Integration Points

### Current (Vanilla JS)
```javascript
// Can crash, no guarantees
document.querySelector('.stat-card').style.opacity = '1'
```

### With ReScript-TEA
```rescript
// Type-safe, compile-time checked
let update = (model, msg) =>
  switch msg {
  | ShowStats => {...model, statsVisible: true}
  }
```

### With k9-svc
```rescript
// Component proves it maintains invariants
(k9-svc-check unsubscribe-link
  (response-code 200)
  (response-time < 200ms)
  (proof validated))
```

### With a2ml
```rescript
// Typed, verifiable proof
(proof consent
  (action subscribe)
  (timestamp verified)
  (type explicit))
```

### With proven
```idris
-- Operations that cannot crash (Idris2)
parseUrl : String -> Either ProofOfError (Url, ProofOfValid)
```

## No Layers Destroyed

- ✅ **Haskell (Casket)** - Still generates static HTML
- ✅ **ReScript** - Adds type safety on top
- ✅ **TEA** - Adds state management on top
- ✅ **k9-svc** - Adds validation where relevant
- ✅ **a2ml** - Adds typed proofs where relevant
- ✅ **proven** - Adds formal verification for critical operations

Each layer complements the others. Nothing is destroyed!
