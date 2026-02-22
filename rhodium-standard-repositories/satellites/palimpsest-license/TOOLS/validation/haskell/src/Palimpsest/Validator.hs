{-
SPDX-License-Identifier: MIT
SPDX-FileCopyrightText: 2024-2025 Palimpsest Stewardship Council
-}

{-# LANGUAGE OverloadedStrings #-}

-- | Main validator module for the Palimpsest License
module Palimpsest.Validator
  ( validateProject
  , validateLicense
  , validateMetadata
  , validateBilingual
  , validateAllReferences
  , module Palimpsest.Validator.Types
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))
import Palimpsest.Validator.Types
import Palimpsest.Validator.License
import Palimpsest.Validator.Metadata
import Palimpsest.Validator.Bilingual
import Palimpsest.Validator.Reference
import Palimpsest.Validator.Utils

-- | Validate entire project structure
validateProject :: ValidatorM ValidationResult
validateProject = do
  config <- ask
  liftIO $ putStrLn "=== Palimpsest License Validation Suite ==="
  liftIO $ putStrLn ""

  let root = configRootPath config

  -- Validate v0.4 license (primary)
  liftIO $ putStrLn "Validating v0.4 license..."
  v04Result <- validateLicenseVersion root "v0.4"

  -- Validate v0.3 licenses if they exist
  liftIO $ putStrLn "\nValidating v0.3 licenses..."
  v03Result <- validateLicenseVersion root "v0.3"

  -- Validate metadata
  liftIO $ putStrLn "\nValidating metadata..."
  metadataResult <- validateProjectMetadata root

  -- Validate bilingual consistency (if enabled)
  when (configCheckBilingual config) $ do
    liftIO $ putStrLn "\nValidating bilingual consistency..."
    validateBilingualPairs root

  -- Validate references (if enabled)
  when (configCheckReferences config) $ do
    liftIO $ putStrLn "\nValidating cross-references..."
    validateProjectReferences root

  liftIO $ putStrLn "\n=== Validation Complete ==="

  pure $ validationSuccess "Project validation completed"

-- | Validate a specific license version
validateLicenseVersion :: FilePath -> String -> ValidatorM ()
validateLicenseVersion root version = do
  let licensePath = root </> "LICENSES" </> version </> ("palimpsest-" ++ version ++ ".md")
  exists <- liftIO $ readFileText licensePath
  case exists of
    Just _ -> do
      _ <- validateLicenseFile licensePath
      liftIO $ putStrLn $ "  ✓ " ++ version ++ " license validated"
    Nothing ->
      liftIO $ putStrLn $ "  - " ++ version ++ " license not found (skipping)"

-- | Validate project metadata files
validateProjectMetadata :: FilePath -> ValidatorM ()
validateProjectMetadata root = do
  -- Check METADATA_v0.4
  let metadataDir = root </> "METADATA_v0.4"
  metadataFiles <- liftIO $ findFiles metadataDir (\f -> isJSONLDFile f || isXMLFile f)

  if null metadataFiles
    then liftIO $ putStrLn "  - No metadata files found"
    else do
      liftIO $ putStrLn $ "  Found " ++ show (length metadataFiles) ++ " metadata file(s)"
      mapM_ (\f -> do
        _ <- validateMetadataFile f
        liftIO $ putStrLn $ "  ✓ Validated: " ++ f
        ) metadataFiles

  -- Check metadata/ directory
  let metadataDir2 = root </> "metadata"
  metadataFiles2 <- liftIO $ findFiles metadataDir2 isJSONLDFile

  if not (null metadataFiles2)
    then mapM_ (\f -> do
      _ <- validateMetadataFile f
      liftIO $ putStrLn $ "  ✓ Validated: " ++ f
      ) metadataFiles2
    else pure ()

-- | Validate bilingual document pairs
validateBilingualPairs :: FilePath -> ValidatorM ()
validateBilingualPairs root = do
  let v03EnPath = root </> "LICENSES/v0.3/palimpsest-license-v0.3.en.md"
      v03NlPath = root </> "LICENSES/v0.3/palimpsest-license-v0.3.nl.md"
      mapPath = root </> "docs/bilingual-map.md"

  -- Check if v0.3 bilingual pair exists
  enExists <- liftIO $ readFileText v03EnPath
  nlExists <- liftIO $ readFileText v03NlPath

  case (enExists, nlExists) of
    (Just _, Just _) -> do
      validateBilingualConsistency v03EnPath v03NlPath mapPath
      liftIO $ putStrLn "  ✓ v0.3 bilingual consistency validated"
    _ ->
      liftIO $ putStrLn "  - v0.3 bilingual pair not found (skipping)"

-- | Validate cross-references throughout project
validateProjectReferences :: FilePath -> ValidatorM ()
validateProjectReferences root = do
  -- Find all markdown files
  mdFiles <- liftIO $ findFiles root isMarkdownFile

  -- Validate references in each file
  let importantDocs = filter isImportantDoc mdFiles

  if null importantDocs
    then liftIO $ putStrLn "  - No documents to validate"
    else do
      liftIO $ putStrLn $ "  Checking " ++ show (length importantDocs) ++ " document(s)"
      validateClauseReferences importantDocs
      liftIO $ putStrLn "  ✓ Cross-references validated"

-- | Check if document is important for validation
isImportantDoc :: FilePath -> Bool
isImportantDoc path =
  any (`elem` path)
    [ "LICENSES/"
    , "GUIDES_"
    , "TOOLKIT_"
    , "LICENSE_CORE/"
    , "GOVERNANCE.md"
    , "CONTRIBUTING.md"
    ]

-- | Validate a single license file
validateLicense :: FilePath -> ValidatorM LicenseStructure
validateLicense = validateLicenseFile

-- | Validate a single metadata file
validateMetadata :: FilePath -> ValidatorM MetadataStructure
validateMetadata = validateMetadataFile

-- | Validate bilingual consistency
validateBilingual :: FilePath -> FilePath -> FilePath -> ValidatorM ()
validateBilingual = validateBilingualConsistency

-- | Validate all references in multiple files
validateAllReferences :: [FilePath] -> ValidatorM ()
validateAllReferences = validateClauseReferences
