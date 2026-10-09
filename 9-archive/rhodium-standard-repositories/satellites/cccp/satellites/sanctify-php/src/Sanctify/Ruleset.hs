-- | Ruleset management for sanctify-php
-- Provides configurable rule collections for static analysis
-- SPDX-License-Identifier: MPL-2.0
module Sanctify.Ruleset
    ( -- * Core types
      Ruleset(..)
    , RuleConfig(..)
    , RuleId(..)
    , RuleSeverity(..)
    , RuleCategory(..)

      -- * Ruleset operations
    , createRuleset
    , mergeRulesets
    , enableRule
    , disableRule
    , setRuleSeverity
    , isRuleEnabled
    , getRuleConfig

      -- * Predefined rulesets
    , strictRuleset
    , securityRuleset
    , wordpressRuleset
    , minimalRuleset
    , defaultRuleset

      -- * Rule definitions
    , allRules
    , rulesByCategory
    , getRuleInfo
    , RuleInfo(..)

      -- * Serialization
    , loadRuleset
    , saveRuleset
    , parseRulesetYaml
    , getPredefinedRuleset
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Aeson (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Value, encode, eitherDecodeStrict')
import qualified Data.ByteString.Lazy as BL
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)
import Control.Monad (forM)

-- | Unique identifier for a rule
newtype RuleId = RuleId { unRuleId :: Text }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
    deriving newtype (ToJSONKey, FromJSONKey)

-- | Severity level for rules
data RuleSeverity
    = SeverityOff       -- ^ Rule is disabled
    | SeverityInfo      -- ^ Informational only
    | SeverityWarning   -- ^ Warning, may not be an issue
    | SeverityError     -- ^ Error, should be fixed
    | SeverityCritical  -- ^ Critical security issue
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Categories of rules
data RuleCategory
    = CategorySecurity      -- ^ Security vulnerabilities
    | CategoryDeadCode      -- ^ Dead/unreachable code
    | CategoryTypes         -- ^ Type-related issues
    | CategoryWordPress     -- ^ WordPress-specific
    | CategoryPerformance   -- ^ Performance issues
    | CategoryStyle         -- ^ Code style
    | CategoryMaintenance   -- ^ Maintainability
    deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)
    deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

-- | Configuration for a single rule
data RuleConfig = RuleConfig
    { rcEnabled   :: Bool           -- ^ Is the rule active?
    , rcSeverity  :: RuleSeverity   -- ^ Override severity
    , rcOptions   :: Map Text Value -- ^ Rule-specific options
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Default rule configuration
defaultRuleConfig :: RuleConfig
defaultRuleConfig = RuleConfig
    { rcEnabled = True
    , rcSeverity = SeverityWarning
    , rcOptions = Map.empty
    }

-- | Information about a rule
data RuleInfo = RuleInfo
    { riId          :: RuleId
    , riName        :: Text
    , riDescription :: Text
    , riCategory    :: RuleCategory
    , riDefault     :: RuleSeverity
    , riAutoFixable :: Bool
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | A complete ruleset
data Ruleset = Ruleset
    { rsName        :: Text                   -- ^ Ruleset name
    , rsDescription :: Text                   -- ^ Description
    , rsExtends     :: Maybe Text             -- ^ Parent ruleset name
    , rsRules       :: Map RuleId RuleConfig  -- ^ Rule configurations
    , rsCategories  :: Map RuleCategory Bool  -- ^ Enable/disable categories
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | All available rules in the system
allRules :: [RuleInfo]
allRules =
    -- Security rules
    [ RuleInfo (RuleId "SEC001") "sql-injection"
        "Detect potential SQL injection vulnerabilities"
        CategorySecurity SeverityCritical True
    , RuleInfo (RuleId "SEC002") "xss"
        "Detect potential cross-site scripting (XSS) vulnerabilities"
        CategorySecurity SeverityCritical True
    , RuleInfo (RuleId "SEC003") "command-injection"
        "Detect potential command injection via shell execution"
        CategorySecurity SeverityCritical False
    , RuleInfo (RuleId "SEC004") "path-traversal"
        "Detect potential path traversal in file operations"
        CategorySecurity SeverityError True
    , RuleInfo (RuleId "SEC005") "unsafe-deserialization"
        "Detect unsafe use of unserialize()"
        CategorySecurity SeverityError True
    , RuleInfo (RuleId "SEC006") "weak-crypto"
        "Detect use of weak cryptographic functions (md5, sha1)"
        CategorySecurity SeverityWarning True
    , RuleInfo (RuleId "SEC007") "hardcoded-secrets"
        "Detect hardcoded passwords, API keys, and secrets"
        CategorySecurity SeverityError False
    , RuleInfo (RuleId "SEC008") "insecure-random"
        "Detect use of insecure random number generators"
        CategorySecurity SeverityWarning True
    , RuleInfo (RuleId "SEC009") "eval-usage"
        "Detect use of eval() and similar dynamic code execution"
        CategorySecurity SeverityCritical False
    , RuleInfo (RuleId "SEC010") "missing-strict-types"
        "Detect missing declare(strict_types=1)"
        CategorySecurity SeverityInfo True

    -- Dead code rules
    , RuleInfo (RuleId "DEAD001") "unused-variable"
        "Detect variables that are assigned but never used"
        CategoryDeadCode SeverityWarning False
    , RuleInfo (RuleId "DEAD002") "unreachable-code"
        "Detect code after return, throw, break, or continue"
        CategoryDeadCode SeverityWarning False
    , RuleInfo (RuleId "DEAD003") "unused-parameter"
        "Detect function parameters that are never used"
        CategoryDeadCode SeverityInfo False
    , RuleInfo (RuleId "DEAD004") "unused-import"
        "Detect use statements that are never referenced"
        CategoryDeadCode SeverityWarning False

    -- Type rules
    , RuleInfo (RuleId "TYPE001") "missing-param-type"
        "Detect function parameters without type hints"
        CategoryTypes SeverityInfo True
    , RuleInfo (RuleId "TYPE002") "missing-return-type"
        "Detect functions without return type declarations"
        CategoryTypes SeverityInfo True
    , RuleInfo (RuleId "TYPE003") "missing-property-type"
        "Detect class properties without type declarations"
        CategoryTypes SeverityInfo True
    , RuleInfo (RuleId "TYPE004") "type-coercion-risk"
        "Detect potential type coercion issues"
        CategoryTypes SeverityWarning False

    -- WordPress rules
    , RuleInfo (RuleId "WP001") "missing-escaping"
        "Detect unescaped output in WordPress context"
        CategoryWordPress SeverityError True
    , RuleInfo (RuleId "WP002") "missing-sanitization"
        "Detect unsanitized input in WordPress context"
        CategoryWordPress SeverityError True
    , RuleInfo (RuleId "WP003") "missing-nonce"
        "Detect form handlers without nonce verification"
        CategoryWordPress SeverityError True
    , RuleInfo (RuleId "WP004") "missing-capability-check"
        "Detect privileged operations without capability checks"
        CategoryWordPress SeverityError False
    , RuleInfo (RuleId "WP005") "direct-db-query"
        "Detect direct database queries without prepare()"
        CategoryWordPress SeverityError True
    , RuleInfo (RuleId "WP006") "missing-text-domain"
        "Detect translatable strings without text domain"
        CategoryWordPress SeverityWarning True
    , RuleInfo (RuleId "WP007") "unprefixed-function"
        "Detect global functions without plugin prefix"
        CategoryWordPress SeverityWarning False
    , RuleInfo (RuleId "WP008") "deprecated-function"
        "Detect use of deprecated WordPress functions"
        CategoryWordPress SeverityWarning True

    -- Performance rules
    , RuleInfo (RuleId "PERF001") "n-plus-one-query"
        "Detect potential N+1 query patterns"
        CategoryPerformance SeverityWarning False
    , RuleInfo (RuleId "PERF002") "large-array-in-loop"
        "Detect large array operations inside loops"
        CategoryPerformance SeverityInfo False

    -- Style rules
    , RuleInfo (RuleId "STYLE001") "mixed-tabs-spaces"
        "Detect mixed indentation styles"
        CategoryStyle SeverityInfo False
    , RuleInfo (RuleId "STYLE002") "long-function"
        "Detect functions exceeding line limit"
        CategoryStyle SeverityInfo False

    -- Maintenance rules
    , RuleInfo (RuleId "MAINT001") "todo-comment"
        "Detect TODO/FIXME comments"
        CategoryMaintenance SeverityInfo False
    , RuleInfo (RuleId "MAINT002") "complex-condition"
        "Detect overly complex conditional expressions"
        CategoryMaintenance SeverityInfo False
    ]

-- | Get rules by category
rulesByCategory :: RuleCategory -> [RuleInfo]
rulesByCategory cat = filter ((== cat) . riCategory) allRules

-- | Get information about a specific rule
getRuleInfo :: RuleId -> Maybe RuleInfo
getRuleInfo rid = lookup rid [(riId r, r) | r <- allRules]

-- | Create a new ruleset from a list of enabled rules
createRuleset :: Text -> Text -> [RuleId] -> Ruleset
createRuleset name desc ruleIds = Ruleset
    { rsName = name
    , rsDescription = desc
    , rsExtends = Nothing
    , rsRules = Map.fromList
        [ (rid, defaultRuleConfig { rcEnabled = rid `elem` ruleIds })
        | RuleInfo{riId = rid} <- allRules
        ]
    , rsCategories = Map.fromList [(cat, True) | cat <- [minBound..maxBound]]
    }

-- | Merge two rulesets (second overrides first)
mergeRulesets :: Ruleset -> Ruleset -> Ruleset
mergeRulesets base override = Ruleset
    { rsName = rsName override
    , rsDescription = rsDescription override
    , rsExtends = rsExtends override
    , rsRules = Map.unionWith mergeRuleConfig (rsRules base) (rsRules override)
    , rsCategories = Map.union (rsCategories override) (rsCategories base)
    }
  where
    mergeRuleConfig _ new = new

-- | Enable a specific rule
enableRule :: RuleId -> Ruleset -> Ruleset
enableRule rid rs = rs
    { rsRules = Map.alter enable rid (rsRules rs) }
  where
    enable Nothing = Just defaultRuleConfig
    enable (Just rc) = Just rc { rcEnabled = True }

-- | Disable a specific rule
disableRule :: RuleId -> Ruleset -> Ruleset
disableRule rid rs = rs
    { rsRules = Map.alter disable rid (rsRules rs) }
  where
    disable Nothing = Just defaultRuleConfig { rcEnabled = False }
    disable (Just rc) = Just rc { rcEnabled = False }

-- | Set severity for a specific rule
setRuleSeverity :: RuleId -> RuleSeverity -> Ruleset -> Ruleset
setRuleSeverity rid sev rs = rs
    { rsRules = Map.alter setSev rid (rsRules rs) }
  where
    setSev Nothing = Just defaultRuleConfig { rcSeverity = sev }
    setSev (Just rc) = Just rc { rcSeverity = sev }

-- | Check if a rule is enabled
isRuleEnabled :: RuleId -> Ruleset -> Bool
isRuleEnabled rid rs =
    case Map.lookup rid (rsRules rs) of
        Nothing -> True  -- Default enabled
        Just rc -> rcEnabled rc && categoryEnabled
  where
    categoryEnabled = case getRuleInfo rid of
        Nothing -> True
        Just info -> fromMaybe True $ Map.lookup (riCategory info) (rsCategories rs)

-- | Get configuration for a rule
getRuleConfig :: RuleId -> Ruleset -> RuleConfig
getRuleConfig rid rs = fromMaybe defaultRuleConfig $ Map.lookup rid (rsRules rs)

-- ============================================================================
-- Predefined Rulesets
-- ============================================================================

-- | Strict ruleset - all rules enabled with high severity
strictRuleset :: Ruleset
strictRuleset = Ruleset
    { rsName = "strict"
    , rsDescription = "All rules enabled with elevated severity"
    , rsExtends = Nothing
    , rsRules = Map.fromList
        [ (riId info, RuleConfig True (elevate (riDefault info)) Map.empty)
        | info <- allRules
        ]
    , rsCategories = Map.fromList [(cat, True) | cat <- [minBound..maxBound]]
    }
  where
    elevate SeverityInfo = SeverityWarning
    elevate SeverityWarning = SeverityError
    elevate other = other

-- | Security-focused ruleset
securityRuleset :: Ruleset
securityRuleset = Ruleset
    { rsName = "security"
    , rsDescription = "Security-focused rules only"
    , rsExtends = Nothing
    , rsRules = Map.fromList
        [ (riId info, RuleConfig (riCategory info == CategorySecurity) (riDefault info) Map.empty)
        | info <- allRules
        ]
    , rsCategories = Map.fromList
        [ (CategorySecurity, True)
        , (CategoryDeadCode, False)
        , (CategoryTypes, False)
        , (CategoryWordPress, False)
        , (CategoryPerformance, False)
        , (CategoryStyle, False)
        , (CategoryMaintenance, False)
        ]
    }

-- | WordPress-specific ruleset
wordpressRuleset :: Ruleset
wordpressRuleset = Ruleset
    { rsName = "wordpress"
    , rsDescription = "WordPress plugin/theme development rules"
    , rsExtends = Nothing
    , rsRules = Map.fromList
        [ (riId info, RuleConfig enabled (riDefault info) Map.empty)
        | info <- allRules
        , let enabled = riCategory info `elem`
                [CategorySecurity, CategoryWordPress, CategoryDeadCode]
        ]
    , rsCategories = Map.fromList
        [ (CategorySecurity, True)
        , (CategoryDeadCode, True)
        , (CategoryTypes, True)
        , (CategoryWordPress, True)
        , (CategoryPerformance, True)
        , (CategoryStyle, False)
        , (CategoryMaintenance, False)
        ]
    }

-- | Minimal ruleset - only critical security issues
minimalRuleset :: Ruleset
minimalRuleset = Ruleset
    { rsName = "minimal"
    , rsDescription = "Only critical security issues"
    , rsExtends = Nothing
    , rsRules = Map.fromList
        [ (riId info, RuleConfig (riDefault info >= SeverityError) (riDefault info) Map.empty)
        | info <- allRules
        ]
    , rsCategories = Map.fromList
        [ (CategorySecurity, True)
        , (CategoryDeadCode, False)
        , (CategoryTypes, False)
        , (CategoryWordPress, False)
        , (CategoryPerformance, False)
        , (CategoryStyle, False)
        , (CategoryMaintenance, False)
        ]
    }

-- | Default ruleset - balanced settings
defaultRuleset :: Ruleset
defaultRuleset = Ruleset
    { rsName = "default"
    , rsDescription = "Balanced default settings"
    , rsExtends = Nothing
    , rsRules = Map.fromList
        [ (riId info, RuleConfig True (riDefault info) Map.empty)
        | info <- allRules
        ]
    , rsCategories = Map.fromList
        [ (CategorySecurity, True)
        , (CategoryDeadCode, True)
        , (CategoryTypes, True)
        , (CategoryWordPress, False)  -- Auto-detected
        , (CategoryPerformance, False)
        , (CategoryStyle, False)
        , (CategoryMaintenance, False)
        ]
    }

-- ============================================================================
-- Serialization
-- ============================================================================

-- | Load a ruleset from a JSON/YAML file
loadRuleset :: FilePath -> IO (Either String Ruleset)
loadRuleset path = do
    content <- TIO.readFile path
    pure $ parseRulesetYaml content

-- | Save a ruleset to a file
saveRuleset :: FilePath -> Ruleset -> IO ()
saveRuleset path rs = BL.writeFile path (encode rs)

-- | Parse a ruleset from YAML/JSON text
parseRulesetYaml :: Text -> Either String Ruleset
parseRulesetYaml content =
    case eitherDecodeStrict' (TE.encodeUtf8 content) of
        Left err -> Left $ "Failed to parse ruleset: " ++ err
        Right rs -> Right rs

-- | Get a predefined ruleset by name
getPredefinedRuleset :: Text -> Maybe Ruleset
getPredefinedRuleset name = case T.toLower name of
    "strict"    -> Just strictRuleset
    "security"  -> Just securityRuleset
    "wordpress" -> Just wordpressRuleset
    "minimal"   -> Just minimalRuleset
    "default"   -> Just defaultRuleset
    _           -> Nothing
