-- | Configuration for sanctify-php
-- SPDX-License-Identifier: AGPL-3.0-or-later
module Sanctify.Config
    ( -- * Configuration types
      Config(..)
    , AnalysisConfig(..)
    , TransformConfig(..)
    , OutputConfig(..)
    , WordPressConfig(..)

      -- * Loading configuration
    , loadConfig
    , defaultConfig
    , mergeConfigs

      -- * Confidence levels
    , ConfidenceLevel(..)
    , AutoFixPolicy(..)
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)

-- | Confidence level for auto-fixes
data ConfidenceLevel
    = ConfidenceCertain   -- ^ 100% safe, no edge cases
    | ConfidenceLikely    -- ^ Very likely safe, rare edge cases
    | ConfidencePossible  -- ^ May be safe, review recommended
    | ConfidenceAdvisory  -- ^ Just report, don't auto-fix
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Policy for automatic fixes
data AutoFixPolicy
    = FixAll              -- ^ Apply all fixes (risky)
    | FixCertainOnly      -- ^ Only Certain confidence
    | FixLikelyAndAbove   -- ^ Certain + Likely
    | FixNone             -- ^ Report only, no changes
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Main configuration
data Config = Config
    { configAnalysis   :: AnalysisConfig
    , configTransform  :: TransformConfig
    , configOutput     :: OutputConfig
    , configWordPress  :: Maybe WordPressConfig
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Analysis settings
data AnalysisConfig = AnalysisConfig
    { analysisCheckSecurity     :: Bool    -- ^ Run security checks
    , analysisCheckTypes        :: Bool    -- ^ Run type inference
    , analysisCheckWordPress    :: Bool    -- ^ Run WordPress-specific checks
    , analysisIgnorePatterns    :: [Text]  -- ^ File patterns to ignore
    , analysisMinSeverity       :: Text    -- ^ Minimum severity to report
    , analysisTaintTracking     :: Bool    -- ^ Enable taint tracking
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Transformation settings
data TransformConfig = TransformConfig
    { transformAutoFix          :: AutoFixPolicy
    , transformAddStrictTypes   :: Bool    -- ^ Add declare(strict_types=1)
    , transformAddTypeHints     :: Bool    -- ^ Infer and add type hints
    , transformAddEscaping      :: Bool    -- ^ Add WordPress escaping
    , transformAddSanitization  :: Bool    -- ^ Add input sanitization
    , transformAddNonces        :: Bool    -- ^ Add nonce verification
    , transformAddAbspath       :: Bool    -- ^ Add ABSPATH check
    , transformAddExitRedirect  :: Bool    -- ^ Add exit after redirect
    , transformPreserveComments :: Bool    -- ^ Keep existing comments
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Output settings
data OutputConfig = OutputConfig
    { outputFormat        :: OutputFormat
    , outputReportFormat  :: ReportFormat
    , outputDirectory     :: Maybe FilePath
    , outputInPlace       :: Bool       -- ^ Modify files in place
    , outputBackupSuffix  :: Maybe Text -- ^ Backup suffix (e.g., ".bak")
    , outputDiffOnly      :: Bool       -- ^ Show diff, don't modify
    , outputVerbose       :: Bool       -- ^ Verbose output
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Output format for transformed code
data OutputFormat
    = FormatPretty      -- ^ Formatted PHP
    | FormatMinimal     -- ^ Minimal whitespace
    | FormatPreserve    -- ^ Try to preserve original formatting
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Report format
data ReportFormat
    = ReportJson        -- ^ JSON for tools
    | ReportSarif       -- ^ SARIF for IDE integration
    | ReportHtml        -- ^ HTML for humans
    | ReportText        -- ^ Plain text
    | ReportMarkdown    -- ^ Markdown for docs
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | WordPress-specific settings
data WordPressConfig = WordPressConfig
    { wpPluginSlug      :: Text         -- ^ Plugin slug for prefixing
    , wpTextDomain      :: Text         -- ^ i18n text domain
    , wpMinVersion      :: Text         -- ^ Minimum WP version
    , wpEnforcePrefix   :: Bool         -- ^ Require function prefixes
    , wpEnforceNonces   :: Bool         -- ^ Require nonce verification
    , wpEnforceCaps     :: Bool         -- ^ Require capability checks
    , wpStrictEscaping  :: Bool         -- ^ All output must be escaped
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Default configuration
defaultConfig :: Config
defaultConfig = Config
    { configAnalysis = AnalysisConfig
        { analysisCheckSecurity = True
        , analysisCheckTypes = True
        , analysisCheckWordPress = False  -- Auto-detected
        , analysisIgnorePatterns = ["vendor/*", "node_modules/*", "*.min.php"]
        , analysisMinSeverity = "low"
        , analysisTaintTracking = True
        }
    , configTransform = TransformConfig
        { transformAutoFix = FixCertainOnly
        , transformAddStrictTypes = True
        , transformAddTypeHints = True
        , transformAddEscaping = True
        , transformAddSanitization = True
        , transformAddNonces = False      -- Needs manual review
        , transformAddAbspath = True
        , transformAddExitRedirect = True
        , transformPreserveComments = True
        }
    , configOutput = OutputConfig
        { outputFormat = FormatPretty
        , outputReportFormat = ReportJson
        , outputDirectory = Nothing
        , outputInPlace = False
        , outputBackupSuffix = Just ".sanctify.bak"
        , outputDiffOnly = True
        , outputVerbose = False
        }
    , configWordPress = Nothing
    }

-- | Default WordPress configuration
defaultWordPressConfig :: Text -> WordPressConfig
defaultWordPressConfig slug = WordPressConfig
    { wpPluginSlug = slug
    , wpTextDomain = slug
    , wpMinVersion = "6.0"
    , wpEnforcePrefix = True
    , wpEnforceNonces = True
    , wpEnforceCaps = True
    , wpStrictEscaping = True
    }

-- | Load configuration from file
loadConfig :: FilePath -> IO (Either String Config)
loadConfig path = do
    -- For now, just return default config
    -- TODO: Parse YAML/JSON config file
    pure $ Right defaultConfig

-- | Merge two configs (second overrides first)
mergeConfigs :: Config -> Config -> Config
mergeConfigs base override = Config
    { configAnalysis = mergeAnalysis (configAnalysis base) (configAnalysis override)
    , configTransform = mergeTransform (configTransform base) (configTransform override)
    , configOutput = mergeOutput (configOutput base) (configOutput override)
    , configWordPress = configWordPress override
    }
  where
    mergeAnalysis _ o = o  -- Simple override for now
    mergeTransform _ o = o
    mergeOutput _ o = o
