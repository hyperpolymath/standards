-- SPDX-License-Identifier: MPL-2.0
-- (PMPL-1.0-or-later preferred; MPL-2.0 required for Hackage OSI-approved policy)
--
-- Data.A2ML.Types — Core data types for A2ML (Attested Markup Language) documents.
--
-- Defines the abstract syntax tree (AST) for A2ML, including documents,
-- blocks, inline elements, directives, and attestation metadata.

module Data.A2ML.Types
  ( -- * Document
    Document (..)

    -- * Block-level elements
  , Block (..)

    -- * Inline elements
  , Inline (..)

    -- * Directives
  , Directive (..)
  , DirectiveName (..)

    -- * Attestation metadata
  , Attestation (..)
  , TrustLevel (..)
  , Manifest (..)
  , Reference (..)
  ) where

import           Data.Map.Strict (Map)
import           Data.Text       (Text)

-- | An A2ML document consists of an optional manifest and a sequence of blocks.
data Document = Document
  { documentManifest :: Maybe Manifest
    -- ^ Optional manifest with attestation metadata.
  , documentBlocks   :: [Block]
    -- ^ Top-level blocks in the document.
  } deriving (Show, Eq)

-- | Block-level elements in an A2ML document.
data Block
  = Heading Int [Inline]
    -- ^ Heading with level (1-5) and inline content.
  | Paragraph [Inline]
    -- ^ A paragraph of inline content.
  | BulletList [[Inline]]
    -- ^ A bullet list where each item is a sequence of inline elements.
  | DirectiveBlock Directive
    -- ^ A directive block (@name(attrs): ... @end).
  | BlankLine
    -- ^ An explicit blank line separator.
  deriving (Show, Eq)

-- | Inline elements within paragraphs and headings.
data Inline
  = PlainText Text
    -- ^ Literal text with no formatting.
  | Bold [Inline]
    -- ^ Bold text (**content**).
  | Italic [Inline]
    -- ^ Italic text (*content*).
  | Link Text Text
    -- ^ Hyperlink with display text and URL.
  | InlineRef Text
    -- ^ An inline reference (@ref(id)).
  | InlineCode Text
    -- ^ Inline code (`code`).
  deriving (Show, Eq)

-- | A directive block delimited by @name(attrs): ... @end.
data Directive = Directive
  { directiveName       :: DirectiveName
    -- ^ The directive type (abstract, refs, attestation, etc.).
  , directiveAttributes :: Map Text Text
    -- ^ Key-value attributes from the directive header.
  , directiveBody       :: Text
    -- ^ Raw body text of the directive.
  } deriving (Show, Eq)

-- | Known directive names in A2ML.
data DirectiveName
  = DirAbstract
    -- ^ @abstract: — Document abstract / summary.
  | DirRefs
    -- ^ @refs: — Reference list.
  | DirAttestation
    -- ^ @attestation: — Cryptographic attestation block.
  | DirMeta
    -- ^ @meta: — Metadata block.
  | DirCustom Text
    -- ^ Any other directive name not in the known set.
  deriving (Show, Eq, Ord)

-- | A cryptographic attestation attached to content.
data Attestation = Attestation
  { attestationSigner    :: Text
    -- ^ Identity of the signer.
  , attestationAlgorithm :: Text
    -- ^ Signing algorithm (e.g., "ed25519", "sha256").
  , attestationSignature :: Text
    -- ^ The signature value (hex or base64 encoded).
  , attestationTimestamp  :: Maybe Text
    -- ^ Optional ISO-8601 timestamp of the attestation.
  } deriving (Show, Eq)

-- | Trust level for attested content.
data TrustLevel
  = Unsigned
    -- ^ No attestation present.
  | SelfAttested
    -- ^ Signed by the document author.
  | ThirdPartyAttested
    -- ^ Signed by a third-party verifier.
  | MultiAttested
    -- ^ Signed by multiple parties.
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Document-level manifest with provenance and attestation metadata.
data Manifest = Manifest
  { manifestTitle        :: Maybe Text
    -- ^ Optional document title.
  , manifestAuthor       :: Maybe Text
    -- ^ Optional document author.
  , manifestVersion      :: Maybe Text
    -- ^ Optional document version string.
  , manifestLicense      :: Maybe Text
    -- ^ SPDX license identifier.
  , manifestTrustLevel   :: TrustLevel
    -- ^ Overall trust level of the document.
  , manifestAttestations :: [Attestation]
    -- ^ Attestations attached to the manifest.
  } deriving (Show, Eq)

-- | A bibliographic reference in an @refs: block.
data Reference = Reference
  { referenceId   :: Text
    -- ^ Reference identifier (e.g., "1", "rfc9110").
  , referenceText :: Text
    -- ^ Display text / citation.
  } deriving (Show, Eq)
