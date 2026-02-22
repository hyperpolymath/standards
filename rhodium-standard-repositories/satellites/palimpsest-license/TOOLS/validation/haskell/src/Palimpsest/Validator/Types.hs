{-
SPDX-License-Identifier: MIT
SPDX-FileCopyrightText: 2024-2025 Palimpsest Stewardship Council
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Core types for the Palimpsest License validation system
module Palimpsest.Validator.Types
  ( ValidationResult(..)
  , ValidationError(..)
  , ValidationWarning(..)
  , Severity(..)
  , ClauseReference(..)
  , LicenseVersion(..)
  , Language(..)
  , MetadataFormat(..)
  , ValidationConfig(..)
  , ValidatorM
  , runValidator
  , validationError
  , validationWarning
  , validationSuccess
  ) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Severity levels for validation issues
data Severity
  = Critical  -- ^ License is invalid
  | Error     -- ^ Serious issue, should be fixed
  | Warning   -- ^ Minor issue, recommended to fix
  | Info      -- ^ Informational notice
  deriving (Show, Eq, Ord, Generic)

-- | Validation errors with detailed context
data ValidationError = ValidationError
  { errorSeverity :: Severity
  , errorLocation :: Text  -- ^ File path or clause reference
  , errorMessage  :: Text
  , errorDetails  :: Maybe Text
  } deriving (Show, Eq, Generic)

-- | Validation warnings (non-fatal issues)
data ValidationWarning = ValidationWarning
  { warningSeverity :: Severity
  , warningLocation :: Text
  , warningMessage  :: Text
  } deriving (Show, Eq, Generic)

-- | Overall validation result
data ValidationResult = ValidationResult
  { validationPassed :: Bool
  , validationErrors :: [ValidationError]
  , validationWarnings :: [ValidationWarning]
  , validationSummary :: Text
  } deriving (Show, Eq, Generic)

-- | Clause reference for cross-validation
data ClauseReference = ClauseReference
  { clauseNumber :: Text
  , clauseTitle  :: Text
  , clauseFile   :: FilePath
  } deriving (Show, Eq, Generic)

-- | License version identifier
data LicenseVersion
  = V0_3
  | V0_4
  | Custom Text
  deriving (Show, Eq, Generic)

-- | Supported languages for bilingual validation
data Language
  = English
  | Dutch
  | OtherLanguage Text
  deriving (Show, Eq, Generic)

-- | Metadata format types
data MetadataFormat
  = JSONLD
  | XML
  | DublinCore
  | NDJSON
  deriving (Show, Eq, Generic)

-- | Validation configuration
data ValidationConfig = ValidationConfig
  { configRootPath        :: FilePath
  , configStrictMode      :: Bool  -- ^ Fail on warnings in strict mode
  , configCheckBilingual  :: Bool  -- ^ Enable bilingual consistency checks
  , configCheckReferences :: Bool  -- ^ Enable cross-reference validation
  , configVerbose         :: Bool
  } deriving (Show, Eq, Generic)

-- | Validator monad stack
type ValidatorM = ReaderT ValidationConfig (WriterT [ValidationWarning] (ExceptT ValidationError IO))

-- | Run a validator with configuration
runValidator :: ValidationConfig -> ValidatorM a -> IO (Either ValidationError (a, [ValidationWarning]))
runValidator config action = runExceptT $ runWriterT $ runReaderT action config

-- | Report a validation error
validationError :: Severity -> Text -> Text -> Maybe Text -> ValidatorM a
validationError sev loc msg details = throwError $ ValidationError sev loc msg details

-- | Report a validation warning
validationWarning :: Severity -> Text -> Text -> ValidatorM ()
validationWarning sev loc msg = tell [ValidationWarning sev loc msg]

-- | Create a successful validation result
validationSuccess :: Text -> ValidationResult
validationSuccess summary = ValidationResult
  { validationPassed = True
  , validationErrors = []
  , validationWarnings = []
  , validationSummary = summary
  }
