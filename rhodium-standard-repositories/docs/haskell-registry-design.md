# RSR Haskell Registry & Validation Service

**Comprehensive Design for Automated RSR Compliance Validation, Badge Generation, and Attestation**

---

## 📋 Table of Contents

1. [Overview](#overview)
2. [Architecture](#architecture)
3. [Haskell Implementation](#haskell-implementation)
4. [API Specification](#api-specification)
5. [Attestation Format](#attestation-format)
6. [Badge Generation](#badge-generation)
7. [Database Schema](#database-schema)
8. [Deployment](#deployment)

---

## Overview

### Purpose

The RSR Haskell Registry is a **pure functional validation service** that:

1. **Validates** repositories against RSR compliance criteria
2. **Generates** compliance badges with cryptographic attestation
3. **Maintains** a public registry of RSR-compliant projects
4. **Provides** REST API for automated validation
5. **Issues** signed attestations using cryptographic signatures

### Why Haskell?

- **Pure Functional**: No side effects in validation logic
- **Type Safety**: Impossible states are unrepresentable
- **Property-Based Testing**: QuickCheck for validation correctness
- **Formal Verification**: Liquid Haskell for critical paths
- **Performance**: Compiled, efficient validation
- **Servant**: Type-safe REST APIs

---

## Architecture

### High-Level Design

```
┌─────────────────────────────────────────────────────────────────────┐
│                        RSR Haskell Registry                         │
│                                                                     │
│  ┌──────────────────┐      ┌──────────────────┐                    │
│  │   Web Interface  │      │   REST API       │                    │
│  │   (Elm GUI)      │◄────►│   (Servant)      │                    │
│  └──────────────────┘      └────────┬─────────┘                    │
│                                     │                               │
│                          ┌──────────▼──────────┐                    │
│                          │  Validation Engine  │                    │
│                          │  (Pure Functions)   │                    │
│                          └──────────┬──────────┘                    │
│                                     │                               │
│                ┌────────────────────┼────────────────────┐          │
│                │                    │                    │          │
│      ┌─────────▼────────┐  ┌───────▼────────┐  ┌───────▼────────┐ │
│      │ Git Repo Cloner  │  │ File Analyzer  │  │ Attestation    │ │
│      │ (libgit2)        │  │ (Parsec)       │  │ Generator      │ │
│      └──────────────────┘  └────────────────┘  └───────┬────────┘ │
│                                                         │          │
│                                                ┌────────▼────────┐ │
│                                                │ Signing Service │ │
│                                                │ (Ed25519)       │ │
│                                                └────────┬────────┘ │
│                                                         │          │
│                                     ┌───────────────────▼─────────┐│
│                                     │   PostgreSQL Database        ││
│                                     │   - Repositories              ││
│                                     │   - Attestations              ││
│                                     │   - Audit Logs                ││
│                                     └──────────────────────────────┘│
└─────────────────────────────────────────────────────────────────────┘
```

### Components

1. **Validation Engine**: Pure Haskell functions checking compliance
2. **REST API**: Servant-based type-safe endpoints
3. **Git Integration**: Clone and analyze repositories
4. **Attestation Service**: Cryptographic signatures (Ed25519)
5. **Database**: PostgreSQL with Persistent/Esqueleto
6. **Badge Generator**: Dynamic SVG generation
7. **Web UI**: Elm frontend for human interaction

---

## Haskell Implementation

### Project Structure

```
rsr-registry/
├── app/
│   └── Main.hs                    # Entry point
├── src/
│   ├── RSR/
│   │   ├── Types.hs               # Core types
│   │   ├── Validation/
│   │   │   ├── Core.hs            # Validation engine
│   │   │   ├── Category1.hs      # Infrastructure checks
│   │   │   ├── Category2.hs      # Documentation checks
│   │   │   ├── Category3.hs      # Security checks
│   │   │   ├── Category4.hs      # Architecture checks
│   │   │   ├── Category7.hs      # Licensing checks
│   │   │   └── Category10.hs     # Community checks
│   │   ├── API.hs                 # Servant API definition
│   │   ├── Database.hs            # Persistent models
│   │   ├── Attestation.hs        # Cryptographic signatures
│   │   ├── Badge.hs               # SVG badge generation
│   │   └── Git.hs                 # Repository cloning
│   └── Main.hs
├── test/
│   ├── Spec.hs
│   ├── RSR/
│   │   ├── ValidationSpec.hs     # Property-based tests
│   │   └── AttestationSpec.hs    # Crypto tests
├── package.yaml                   # Hpack configuration
├── stack.yaml                     # Stack resolver
└── rsr-registry.cabal             # Generated by Hpack
```

### Core Types (`src/RSR/Types.hs`)

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module RSR.Types where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Compliance levels
data ComplianceLevel
  = Gold      -- 100%
  | Silver    -- 90-99%
  | Bronze    -- 75-89%
  | NonCompliant  -- <75%
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Individual check result
data CheckResult = CheckResult
  { checkId :: Text
  , checkDescription :: Text
  , checkCategory :: Int
  , checkPassed :: Bool
  , checkReason :: Maybe Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Complete audit result
data AuditResult = AuditResult
  { auditRepositoryUrl :: Text
  , auditTimestamp :: UTCTime
  , auditTotalChecks :: Int
  , auditPassedChecks :: Int
  , auditFailedChecks :: Int
  , auditScore :: Double
  , auditLevel :: ComplianceLevel
  , auditChecks :: [CheckResult]
  , auditBadgeUrl :: Text
  , auditAttestationUrl :: Maybe Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Repository information
data Repository = Repository
  { repoUrl :: Text
  , repoName :: Text
  , repoOwner :: Text
  , repoDescription :: Maybe Text
  , repoLanguages :: [Text]
  , repoStars :: Maybe Int
  , repoLastAuditDate :: Maybe UTCTime
  , repoComplianceLevel :: Maybe ComplianceLevel
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Attestation (cryptographic proof of compliance)
data Attestation = Attestation
  { attestationRepositoryUrl :: Text
  , attestationLevel :: ComplianceLevel
  , attestationScore :: Double
  , attestationTimestamp :: UTCTime
  , attestationSignature :: Text  -- Ed25519 signature
  , attestationPublicKey :: Text  -- Ed25519 public key
  , attestationAuditHash :: Text  -- SHA-256 of audit result
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)
```

### Validation Engine (`src/RSR/Validation/Core.hs`)

```haskell
{-# LANGUAGE OverloadedStrings #-}

module RSR.Validation.Core
  ( validateRepository
  , calculateScore
  , determineComplianceLevel
  ) where

import RSR.Types
import qualified RSR.Validation.Category1 as Cat1
import qualified RSR.Validation.Category2 as Cat2
import qualified RSR.Validation.Category3 as Cat3
import qualified RSR.Validation.Category4 as Cat4
import qualified RSR.Validation.Category7 as Cat7
import qualified RSR.Validation.Category10 as Cat10

import Data.Text (Text)
import Data.Time (getCurrentTime)
import System.FilePath (FilePath)

-- | Main validation function (pure)
validateRepository :: FilePath -> IO AuditResult
validateRepository repoPath = do
  timestamp <- getCurrentTime

  -- Run all category validations
  cat1Results <- Cat1.validateInfrastructure repoPath
  cat2Results <- Cat2.validateDocumentation repoPath
  cat3Results <- Cat3.validateSecurity repoPath
  cat4Results <- Cat4.validateArchitecture repoPath
  cat7Results <- Cat7.validateLicensing repoPath
  cat10Results <- Cat10.validateCommunity repoPath

  let allChecks = concat
        [ cat1Results
        , cat2Results
        , cat3Results
        , cat4Results
        , cat7Results
        , cat10Results
        ]

  let totalChecks = length allChecks
  let passedChecks = length (filter checkPassed allChecks)
  let failedChecks = totalChecks - passedChecks

  let score = calculateScore totalChecks passedChecks
  let level = determineComplianceLevel score

  let badgeUrl = generateBadgeUrl level score

  return $ AuditResult
    { auditRepositoryUrl = "file://" <> pack repoPath
    , auditTimestamp = timestamp
    , auditTotalChecks = totalChecks
    , auditPassedChecks = passedChecks
    , auditFailedChecks = failedChecks
    , auditScore = score
    , auditLevel = level
    , auditChecks = allChecks
    , auditBadgeUrl = badgeUrl
    , auditAttestationUrl = Nothing  -- Generated separately
    }

-- | Calculate compliance score (pure)
calculateScore :: Int -> Int -> Double
calculateScore total passed
  | total == 0 = 0.0
  | otherwise  = (fromIntegral passed / fromIntegral total) * 100.0

-- | Determine compliance level (pure)
determineComplianceLevel :: Double -> ComplianceLevel
determineComplianceLevel score
  | score >= 100.0 = Gold
  | score >= 90.0  = Silver
  | score >= 75.0  = Bronze
  | otherwise      = NonCompliant

-- | Generate badge URL (pure)
generateBadgeUrl :: ComplianceLevel -> Double -> Text
generateBadgeUrl level score =
  let levelText = case level of
        Gold -> "Gold"
        Silver -> "Silver"
        Bronze -> "Bronze"
        NonCompliant -> "Non--Compliant"
      color = case level of
        Gold -> "ffd700"
        Silver -> "c0c0c0"
        Bronze -> "cd7f32"
        NonCompliant -> "red"
      scoreText = pack $ printf "%.0f" score
  in "https://img.shields.io/badge/RSR-" <> levelText
     <> "%20(" <> scoreText <> "%25)-" <> color
```

### Category Validation Example (`src/RSR/Validation/Category2.hs`)

```haskell
{-# LANGUAGE OverloadedStrings #-}

module RSR.Validation.Category2
  ( validateDocumentation
  ) where

import RSR.Types
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Data.Text (Text, pack, unpack, isInfixOf)
import qualified Data.Text.IO as TIO

validateDocumentation :: FilePath -> IO [CheckResult]
validateDocumentation repoPath = sequence
  [ checkFileExists repoPath "README.md" 2 "README.md present"
  , checkFileExists repoPath "LICENSE.txt" 2 "LICENSE.txt present (must be .txt)"
  , checkFileExists repoPath "SECURITY.md" 2 "SECURITY.md present"
  , checkFileExists repoPath "CODE_OF_CONDUCT.md" 2 "CODE_OF_CONDUCT.md present"
  , checkFileExists repoPath "CONTRIBUTING.md" 2 "CONTRIBUTING.md present"
  , checkFileExists repoPath "MAINTAINERS.md" 2 "MAINTAINERS.md present"
  , checkFileExists repoPath "CHANGELOG.md" 2 "CHANGELOG.md present"
  , checkFileExists repoPath ".well-known/security.txt" 2 ".well-known/security.txt (RFC 9116)"
  , checkFileExists repoPath ".well-known/ai.txt" 2 ".well-known/ai.txt (AI policies)"
  , checkFileExists repoPath ".well-known/humans.txt" 2 ".well-known/humans.txt (attribution)"
  , checkFileContains repoPath "LICENSE.txt" "SPDX-License-Identifier" 2 "LICENSE.txt has SPDX identifier"
  , checkFileContains repoPath "LICENSE.txt" "MIT" 2 "LICENSE.txt includes MIT"
  , checkFileContains repoPath "LICENSE.txt" "Palimpsest" 2 "LICENSE.txt includes Palimpsest"
  , checkFileContains repoPath "README.md" "Installation" 2 "README has Installation section"
  , checkFileContains repoPath "CONTRIBUTING.md" "TPCF" 2 "CONTRIBUTING mentions TPCF"
  ]

-- | Helper: Check if file exists
checkFileExists :: FilePath -> FilePath -> Int -> Text -> IO CheckResult
checkFileExists repoPath file category description = do
  exists <- doesFileExist (repoPath </> file)
  return $ CheckResult
    { checkId = pack file
    , checkDescription = description
    , checkCategory = category
    , checkPassed = exists
    , checkReason = if exists then Nothing else Just "File not found"
    }

-- | Helper: Check if file contains pattern
checkFileContains :: FilePath -> FilePath -> Text -> Int -> Text -> IO CheckResult
checkFileContains repoPath file pattern category description = do
  let fullPath = repoPath </> file
  exists <- doesFileExist fullPath
  if not exists
    then return $ CheckResult
           { checkId = pack file <> ":" <> pattern
           , checkDescription = description
           , checkCategory = category
           , checkPassed = False
           , checkReason = Just "File not found"
           }
    else do
      content <- TIO.readFile fullPath
      let found = pattern `isInfixOf` content
      return $ CheckResult
        { checkId = pack file <> ":" <> pattern
        , checkDescription = description
        , checkCategory = category
        , checkPassed = found
        , checkReason = if found then Nothing else Just "Pattern not found"
        }
```

### Attestation Service (`src/RSR/Attestation.hs`)

```haskell
{-# LANGUAGE OverloadedStrings #-}

module RSR.Attestation
  ( generateAttestation
  , verifyAttestation
  , Ed25519KeyPair(..)
  , generateKeyPair
  ) where

import RSR.Types
import Crypto.Error (CryptoFailable(..))
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Aeson (encode)
import Crypto.Hash (hash, Digest, SHA256)
import Data.Time (getCurrentTime)

-- | Ed25519 key pair for signing
data Ed25519KeyPair = Ed25519KeyPair
  { keyPairPublic :: Ed25519.PublicKey
  , keyPairSecret :: Ed25519.SecretKey
  }

-- | Generate new Ed25519 key pair
generateKeyPair :: IO Ed25519KeyPair
generateKeyPair = do
  secretKey <- Ed25519.generateSecretKey
  let publicKey = Ed25519.toPublic secretKey
  return $ Ed25519KeyPair publicKey secretKey

-- | Generate cryptographically signed attestation
generateAttestation
  :: Ed25519KeyPair
  -> AuditResult
  -> IO Attestation
generateAttestation keyPair auditResult = do
  timestamp <- getCurrentTime

  -- Hash the audit result
  let auditJson = encode auditResult
  let auditHash = hash auditJson :: Digest SHA256
  let auditHashText = pack $ show auditHash

  -- Create message to sign
  let message = encodeUtf8 $
        auditRepositoryUrl auditResult <> "|"
        <> pack (show $ auditLevel auditResult) <> "|"
        <> pack (show $ auditScore auditResult) <> "|"
        <> pack (show timestamp) <> "|"
        <> auditHashText

  -- Sign the message
  let signature = Ed25519.sign (keyPairSecret keyPair) (keyPairPublic keyPair) message

  -- Encode to Base64
  let signatureB64 = decodeUtf8 $ B64.encode $ Ed25519.unSignature signature
  let publicKeyB64 = decodeUtf8 $ B64.encode $ Ed25519.unPublicKey $ keyPairPublic keyPair

  return $ Attestation
    { attestationRepositoryUrl = auditRepositoryUrl auditResult
    , attestationLevel = auditLevel auditResult
    , attestationScore = auditScore auditResult
    , attestationTimestamp = timestamp
    , attestationSignature = signatureB64
    , attestationPublicKey = publicKeyB64
    , attestationAuditHash = auditHashText
    }

-- | Verify attestation signature
verifyAttestation :: Attestation -> Bool
verifyAttestation att =
  case (decodePublicKey $ attestationPublicKey att, decodeSignature $ attestationSignature att) of
    (CryptoPassed pubKey, CryptoPassed sig) ->
      let message = encodeUtf8 $
            attestationRepositoryUrl att <> "|"
            <> pack (show $ attestationLevel att) <> "|"
            <> pack (show $ attestationScore att) <> "|"
            <> pack (show $ attestationTimestamp att) <> "|"
            <> attestationAuditHash att
      in Ed25519.verify pubKey message sig
    _ -> False

-- Helper functions
decodePublicKey :: Text -> CryptoFailable Ed25519.PublicKey
decodePublicKey b64 =
  case B64.decode (encodeUtf8 b64) of
    Left _ -> CryptoFailed CryptoError_PublicKeySizeInvalid
    Right bs -> Ed25519.publicKey bs

decodeSignature :: Text -> CryptoFailable Ed25519.Signature
decodeSignature b64 =
  case B64.decode (encodeUtf8 b64) of
    Left _ -> CryptoFailed CryptoError_SecretKeyStructureInvalid
    Right bs -> Ed25519.signature bs
```

---

## API Specification

### REST API (`src/RSR/API.hs`)

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module RSR.API where

import RSR.Types
import Servant
import Data.Text (Text)

-- | API type specification
type RSRAPI =
       -- Validate repository by URL
       "validate" :> QueryParam "url" Text :> Get '[JSON] AuditResult

       -- Get attestation for repository
  :<|> "attest" :> QueryParam "url" Text :> Get '[JSON] Attestation

       -- Get badge (SVG)
  :<|> "badge" :> Capture "level" ComplianceLevel :> Capture "score" Int :> Get '[SVG] RawSVG

       -- List all compliant repositories
  :<|> "repositories" :> QueryParam "level" ComplianceLevel :> Get '[JSON] [Repository]

       -- Get repository details
  :<|> "repository" :> Capture "owner" Text :> Capture "name" Text :> Get '[JSON] Repository

-- | API server implementation
server :: Server RSRAPI
server = validateHandler
    :<|> attestHandler
    :<|> badgeHandler
    :<|> listRepositoriesHandler
    :<|> getRepositoryHandler

-- Handlers
validateHandler :: Maybe Text -> Handler AuditResult
validateHandler = undefined  -- Implementation

attestHandler :: Maybe Text -> Handler Attestation
attestHandler = undefined  -- Implementation

badgeHandler :: ComplianceLevel -> Int -> Handler RawSVG
badgeHandler = undefined  -- SVG generation

listRepositoriesHandler :: Maybe ComplianceLevel -> Handler [Repository]
listRepositoriesHandler = undefined  -- Database query

getRepositoryHandler :: Text -> Text -> Handler Repository
getRepositoryHandler = undefined  -- Database query
```

### API Endpoints

| Method | Endpoint | Description | Example |
|--------|----------|-------------|---------|
| `GET` | `/validate?url=<repo-url>` | Validate repository, return audit result | `/validate?url=https://gitlab.com/user/repo` |
| `GET` | `/attest?url=<repo-url>` | Generate signed attestation | `/attest?url=https://gitlab.com/user/repo` |
| `GET` | `/badge/:level/:score` | Get SVG badge | `/badge/Gold/100` |
| `GET` | `/repositories?level=Gold` | List repositories by level | `/repositories?level=Silver` |
| `GET` | `/repository/:owner/:name` | Get repository details | `/repository/hyperpolymath/rhodium-standard-repositories` |

---

## Attestation Format

### JSON-LD Attestation

```json
{
  "@context": "https://rsr-registry.org/context/v1",
  "@type": "RSRAttestation",
  "id": "urn:rsr:attestation:2025-11-22:gitlab.com:hyperpolymath:rhodium-standard-repositories",
  "repository": {
    "@type": "Repository",
    "url": "https://gitlab.com/hyperpolymath/rhodium-standard-repositories",
    "owner": "hyperpolymath",
    "name": "rhodium-standard-repositories"
  },
  "compliance": {
    "@type": "ComplianceResult",
    "level": "Gold",
    "score": 100.0,
    "totalChecks": 100,
    "passedChecks": 100,
    "failedChecks": 0
  },
  "audit": {
    "@type": "AuditMetadata",
    "timestamp": "2025-11-22T12:00:00Z",
    "auditHash": "sha256:1234567890abcdef...",
    "auditVersion": "1.0.0"
  },
  "signature": {
    "@type": "Ed25519Signature",
    "value": "base64-encoded-signature",
    "publicKey": "base64-encoded-public-key",
    "algorithm": "Ed25519"
  },
  "verifiableCredential": {
    "@context": ["https://www.w3.org/2018/credentials/v1"],
    "@type": "VerifiableCredential",
    "issuer": "did:web:rsr-registry.org",
    "issuanceDate": "2025-11-22T12:00:00Z",
    "credentialSubject": {
      "id": "https://gitlab.com/hyperpolymath/rhodium-standard-repositories",
      "rsrCompliance": "Gold"
    },
    "proof": {
      "type": "Ed25519Signature2020",
      "created": "2025-11-22T12:00:00Z",
      "proofPurpose": "assertionMethod",
      "verificationMethod": "did:web:rsr-registry.org#key-1",
      "jws": "base64url-encoded-signature"
    }
  }
}
```

### Verification Process

1. **Download attestation** from `/attest?url=<repo-url>`
2. **Extract public key** from `signature.publicKey` field
3. **Reconstruct message**: `<repo-url>|<level>|<score>|<timestamp>|<audit-hash>`
4. **Decode signature** from Base64
5. **Verify** using Ed25519 signature verification
6. **Check timestamp** (attestation valid for 90 days)

---

## Badge Generation

### Dynamic SVG Badge (`src/RSR/Badge.hs`)

```haskell
module RSR.Badge (generateBadgeSVG) where

import RSR.Types
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

generateBadgeSVG :: ComplianceLevel -> Double -> Text
generateBadgeSVG level score =
  let (levelText, color) = case level of
        Gold -> ("GOLD", "#FFD700")
        Silver -> ("SILVER", "#C0C0C0")
        Bronze -> ("BRONZE", "#CD7F32")
        NonCompliant -> ("NON-COMPLIANT", "#F44336")

      scoreText = T.pack $ printf "%.0f%%" score

  in T.unlines
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    , "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"200\" height=\"20\">"
    , "  <linearGradient id=\"b\" x2=\"0\" y2=\"100%\">"
    , "    <stop offset=\"0\" stop-color=\"#bbb\" stop-opacity=\".1\"/>"
    , "    <stop offset=\"1\" stop-opacity=\".1\"/>"
    , "  </linearGradient>"
    , "  <rect rx=\"3\" width=\"200\" height=\"20\" fill=\"#555\"/>"
    , "  <rect rx=\"3\" x=\"60\" width=\"140\" height=\"20\" fill=\"" <> color <> "\"/>"
    , "  <text x=\"10\" y=\"15\" fill=\"#fff\" font-family=\"DejaVu Sans,Verdana,sans-serif\" font-size=\"11\">"
    , "    RSR"
    , "  </text>"
    , "  <text x=\"70\" y=\"15\" fill=\"#fff\" font-family=\"DejaVu Sans,Verdana,sans-serif\" font-size=\"11\">"
    , "    " <> levelText <> " (" <> scoreText <> ")"
    , "  </text>"
    , "</svg>"
    ]
```

### Badge Embedding

**Markdown**:
```markdown
[![RSR Compliance](https://rsr-registry.org/badge/Gold/100)](https://rsr-registry.org/repository/hyperpolymath/rhodium-standard-repositories)
```

**HTML**:
```html
<a href="https://rsr-registry.org/repository/hyperpolymath/rhodium-standard-repositories">
  <img src="https://rsr-registry.org/badge/Gold/100" alt="RSR Gold Compliance (100%)">
</a>
```

**AsciiDoc**:
```asciidoc
image:https://rsr-registry.org/badge/Gold/100[RSR Gold Compliance, link=https://rsr-registry.org/repository/hyperpolymath/rhodium-standard-repositories]
```

---

## Database Schema

### PostgreSQL Schema

```sql
-- Repositories table
CREATE TABLE repositories (
  id SERIAL PRIMARY KEY,
  url TEXT NOT NULL UNIQUE,
  owner TEXT NOT NULL,
  name TEXT NOT NULL,
  description TEXT,
  languages TEXT[],
  stars INTEGER,
  created_at TIMESTAMP NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMP NOT NULL DEFAULT NOW(),
  last_audit_date TIMESTAMP,
  compliance_level TEXT CHECK (compliance_level IN ('Gold', 'Silver', 'Bronze', 'Non-Compliant')),
  compliance_score DOUBLE PRECISION,
  INDEX idx_repositories_owner_name (owner, name),
  INDEX idx_repositories_compliance_level (compliance_level)
);

-- Audit results table
CREATE TABLE audit_results (
  id SERIAL PRIMARY KEY,
  repository_id INTEGER NOT NULL REFERENCES repositories(id),
  audit_date TIMESTAMP NOT NULL DEFAULT NOW(),
  total_checks INTEGER NOT NULL,
  passed_checks INTEGER NOT NULL,
  failed_checks INTEGER NOT NULL,
  score DOUBLE PRECISION NOT NULL,
  compliance_level TEXT NOT NULL CHECK (compliance_level IN ('Gold', 'Silver', 'Bronze', 'Non-Compliant')),
  audit_data JSONB NOT NULL,  -- Full CheckResult array
  INDEX idx_audit_results_repository (repository_id),
  INDEX idx_audit_results_date (audit_date DESC)
);

-- Attestations table
CREATE TABLE attestations (
  id SERIAL PRIMARY KEY,
  repository_id INTEGER NOT NULL REFERENCES repositories(id),
  audit_result_id INTEGER NOT NULL REFERENCES audit_results(id),
  created_at TIMESTAMP NOT NULL DEFAULT NOW(),
  expires_at TIMESTAMP NOT NULL,  -- 90 days validity
  signature TEXT NOT NULL,
  public_key TEXT NOT NULL,
  audit_hash TEXT NOT NULL,
  attestation_data JSONB NOT NULL,  -- Full Attestation JSON
  UNIQUE (repository_id, audit_result_id),
  INDEX idx_attestations_repository (repository_id),
  INDEX idx_attestations_expires (expires_at)
);

-- Public keys table (registry signing keys)
CREATE TABLE signing_keys (
  id SERIAL PRIMARY KEY,
  public_key TEXT NOT NULL UNIQUE,
  created_at TIMESTAMP NOT NULL DEFAULT NOW(),
  revoked_at TIMESTAMP,
  is_active BOOLEAN NOT NULL DEFAULT TRUE
);
```

---

## Deployment

### Docker / Podman Deployment

**Dockerfile** (using Chainguard Wolfi base):

```dockerfile
# Build stage
FROM haskell:9.4 AS builder

WORKDIR /opt/rsr-registry

# Install dependencies
COPY stack.yaml package.yaml stack.yaml.lock ./
RUN stack build --only-dependencies

# Build application
COPY . .
RUN stack build --copy-bins --local-bin-path /opt/rsr-registry/bin

# Runtime stage (Chainguard Wolfi)
FROM cgr.dev/chainguard/wolfi-base:latest

# Install runtime dependencies
RUN apk add --no-cache gmp libffi libpq

# Copy binary
COPY --from=builder /opt/rsr-registry/bin/rsr-registry /usr/local/bin/

# Non-root user
USER 1000:1000

EXPOSE 8080

CMD ["/usr/local/bin/rsr-registry"]
```

**podman-compose.yml**:

```yaml
version: '3.8'

services:
  rsr-registry:
    build: .
    image: rsr-registry:latest
    ports:
      - "8080:8080"
    environment:
      DATABASE_URL: postgresql://rsr:password@postgres:5432/rsr_registry
      SIGNING_KEY_PATH: /secrets/signing_key.pem
    volumes:
      - ./secrets:/secrets:ro
    depends_on:
      - postgres

  postgres:
    image: cgr.dev/chainguard/postgres:latest
    environment:
      POSTGRES_USER: rsr
      POSTGRES_PASSWORD: password
      POSTGRES_DB: rsr_registry
    volumes:
      - postgres_data:/var/lib/postgresql/data

volumes:
  postgres_data:
```

### Nix Flake Deployment

```nix
{
  description = "RSR Registry - Haskell validation service";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        haskellPackages = pkgs.haskellPackages;

        rsr-registry = haskellPackages.callCabal2nix "rsr-registry" ./. {};

      in {
        packages.default = rsr-registry;

        devShells.default = haskellPackages.shellFor {
          packages = p: [ rsr-registry ];
          buildInputs = with haskellPackages; [
            cabal-install
            haskell-language-server
            ghcid
            hlint
            ormolu
          ];
        };
      }
    );
}
```

---

## Usage Examples

### CLI (using rsr-audit.sh)

```bash
# Audit local repository
./rsr-audit.sh /path/to/repo

# Audit with JSON output
./rsr-audit.sh /path/to/repo --format json > audit.json

# Audit with HTML report
./rsr-audit.sh /path/to/repo --format html > report.html
```

### API (using Haskell registry)

```bash
# Validate repository
curl https://rsr-registry.org/validate?url=https://gitlab.com/hyperpolymath/rhodium-standard-repositories

# Get attestation
curl https://rsr-registry.org/attest?url=https://gitlab.com/hyperpolymath/rhodium-standard-repositories

# Get badge
curl https://rsr-registry.org/badge/Gold/100

# List Gold-level repositories
curl https://rsr-registry.org/repositories?level=Gold
```

### Verification

```bash
# Download attestation
curl https://rsr-registry.org/attest?url=<repo-url> > attestation.json

# Verify signature (using openssl or custom tool)
# Extract signature, public key, reconstruct message, verify
```

---

## Next Steps

1. **Implement Haskell validation engine** (Category1-10 modules)
2. **Set up PostgreSQL database** with schema
3. **Deploy registry service** (Podman + Nix)
4. **Generate signing key pair** (Ed25519)
5. **Create Elm web UI** for human interaction
6. **Integrate with GitLab API** for automatic validation on push
7. **Publish registry at rsr-registry.org**

---

*"Pure functional validation, cryptographic attestation, verifiable compliance."*

— RSR Haskell Registry, Type-Safe Correctness
