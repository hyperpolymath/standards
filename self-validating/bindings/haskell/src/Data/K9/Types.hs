-- SPDX-License-Identifier: MPL-2.0
-- (PMPL-1.0-or-later preferred; MPL-2.0 required for Hackage OSI-approved policy)
--
-- Data.K9.Types — Core data types for K9 (Self-Validating Components).
--
-- Defines the abstract syntax tree for K9 component specifications, including
-- pedigree metadata, security levels, target platforms, recipes, and contracts.

module Data.K9.Types
  ( -- * Component
    Component (..)

    -- * Pedigree metadata
  , Pedigree (..)

    -- * Security
  , SecurityLevel (..)
  , SecurityPolicy (..)

    -- * Target platform
  , Target (..)

    -- * Recipes
  , Recipe (..)
  , Recipes (..)

    -- * Validation
  , Validation (..)

    -- * Contract
  , Contract (..)
  , ContractClause (..)
  ) where

import           Data.Map.Strict (Map)
import           Data.Text       (Text)

-- | A K9 self-validating component. This is the top-level AST node.
data Component = Component
  { componentPedigree   :: Pedigree
    -- ^ Identity and provenance metadata.
  , componentSecurity   :: SecurityPolicy
    -- ^ Security level and permission flags.
  , componentTarget     :: Maybe Target
    -- ^ Optional target platform constraints.
  , componentRecipes    :: Maybe Recipes
    -- ^ Optional build/deploy/validate recipes.
  , componentValidation :: Maybe Validation
    -- ^ Optional self-validation block (checksum, pedigree version).
  , componentContent    :: Map Text Text
    -- ^ Additional content key-value pairs.
  , componentTags       :: [Text]
    -- ^ Tags for categorisation.
  } deriving (Show, Eq)

-- | Pedigree: identity and provenance metadata for a K9 component.
data Pedigree = Pedigree
  { pedigreeName        :: Text
    -- ^ Component name (e.g., "hello-k9").
  , pedigreeVersion     :: Text
    -- ^ Semantic version string.
  , pedigreeDescription :: Text
    -- ^ Human-readable description.
  , pedigreeAuthor      :: Maybe Text
    -- ^ Author identity.
  , pedigreeLicense     :: Maybe Text
    -- ^ SPDX license identifier.
  } deriving (Show, Eq)

-- | K9 security levels forming a trust hierarchy.
--
-- * 'Kennel' — Pure data, no execution, safe anywhere.
-- * 'Yard'   — Controlled execution, limited permissions.
-- * 'Hunt'   — Full execution with explicit authorisation required.
data SecurityLevel
  = Kennel
    -- ^ Pure data only. No code execution. Safe to open anywhere.
  | Yard
    -- ^ Controlled execution with limited permissions.
  | Hunt
    -- ^ Full execution. Requires explicit authorisation.
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Security policy combining the level with specific permission flags.
data SecurityPolicy = SecurityPolicy
  { securityLevel           :: SecurityLevel
    -- ^ The trust level.
  , securityAllowNetwork    :: Bool
    -- ^ Whether the component may access the network.
  , securityAllowFsWrite    :: Bool
    -- ^ Whether the component may write to the filesystem.
  , securityAllowSubprocess :: Bool
    -- ^ Whether the component may spawn subprocesses.
  } deriving (Show, Eq)

-- | Target platform constraints.
data Target = Target
  { targetOS             :: Maybe Text
    -- ^ Target operating system (e.g., "Linux", "Darwin").
  , targetIsEdge         :: Bool
    -- ^ Whether this targets edge/embedded environments.
  , targetRequiresPodman :: Bool
    -- ^ Whether Podman container runtime is required.
  , targetMemory         :: Maybe Text
    -- ^ Memory constraint (e.g., "512M", "2G").
  } deriving (Show, Eq)

-- | Named recipes for lifecycle operations.
data Recipe = Recipe
  { recipeName    :: Text
    -- ^ Recipe identifier (e.g., "install", "validate").
  , recipeCommand :: Text
    -- ^ Shell command to execute.
  } deriving (Show, Eq)

-- | Collection of standard lifecycle recipes.
data Recipes = Recipes
  { recipeInstall  :: Maybe Text
    -- ^ Installation command.
  , recipeValidate :: Maybe Text
    -- ^ Validation / typecheck command.
  , recipeDeploy   :: Maybe Text
    -- ^ Deployment command.
  , recipeMigrate  :: Maybe Text
    -- ^ Migration command.
  , recipeCustom   :: Map Text Text
    -- ^ Additional named recipes.
  } deriving (Show, Eq)

-- | Self-validation block.
data Validation = Validation
  { validationChecksum       :: Text
    -- ^ SHA-256 (or other) checksum of the component.
  , validationPedigreeVersion :: Text
    -- ^ Version of the pedigree schema used.
  , validationHuntAuthorized :: Bool
    -- ^ Whether Hunt-level execution has been explicitly authorised.
  } deriving (Show, Eq)

-- | A contract attached to a K9 component (from the contractile system).
data Contract = Contract
  { contractName    :: Text
    -- ^ Contract identifier.
  , contractClauses :: [ContractClause]
    -- ^ Individual clauses in the contract.
  } deriving (Show, Eq)

-- | A single clause within a K9 contract.
data ContractClause = ContractClause
  { clauseType       :: Text
    -- ^ Clause type: "must", "trust", "dust", "intend", "k9".
  , clausePredicate  :: Text
    -- ^ The predicate or assertion text.
  , clauseVerified   :: Bool
    -- ^ Whether this clause has been verified.
  } deriving (Show, Eq)
