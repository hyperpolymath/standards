{-
SPDX-License-Identifier: MIT
SPDX-FileCopyrightText: 2024-2025 Palimpsest Stewardship Council
-}

{-# LANGUAGE OverloadedStrings #-}

-- | Bilingual consistency checker for Dutch ↔ English license texts
module Palimpsest.Validator.Bilingual
  ( validateBilingualConsistency
  , checkClauseAlignment
  , checkStructuralParity
  , BilingualMapping(..)
  , loadBilingualMap
  ) where

import Control.Monad (when, unless, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (mapMaybe, isNothing, fromMaybe)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Palimpsest.Validator.Types
import Palimpsest.Validator.Utils
import Palimpsest.Validator.License (LicenseStructure(..), Clause(..), validateLicenseFile)

-- | Bilingual mapping between clause numbers and titles
data BilingualMapping = BilingualMapping
  { mappingClauseNumber :: Text
  , mappingEnglishTitle :: Text
  , mappingDutchTitle   :: Text
  } deriving (Show, Eq)

-- | Expected bilingual mappings (from bilingual-map.md)
defaultBilingualMap :: [BilingualMapping]
defaultBilingualMap =
  [ BilingualMapping "1" "Definitions" "Definities"
  , BilingualMapping "2" "Permissions" "Toestemmingen"
  , BilingualMapping "3" "Conditions" "Voorwaarden"
  , BilingualMapping "4" "Attribution Options" "Toeschrijvingsmogelijkheden"
  , BilingualMapping "5" "Symbolic Integrity" "Symbolische Integriteit"
  , BilingualMapping "6" "Emotional Lineage" "Emotionele Afstamming"
  , BilingualMapping "7" "Synthetic Realities" "Synthetische Realiteiten"
  , BilingualMapping "8" "Accessibility Ethos" "Toegankelijkheidsethos"
  , BilingualMapping "9" "Dispute Resolution" "Geschillenbeslechting"
  , BilingualMapping "10" "Governing Law" "Toepasselijk Recht"
  , BilingualMapping "11" "Enforcement Venue" "Handhavingsforum"
  , BilingualMapping "12" "Compatibility" "Compatibiliteit"
  , BilingualMapping "13" "Amendments" "Wijzigingen"
  , BilingualMapping "14" "Termination" "Beëindiging"
  , BilingualMapping "15" "Severability" "Scheidbaarheid"
  , BilingualMapping "16" "Entire Agreement" "Volledige Overeenstemming"
  , BilingualMapping "17" "No Waiver" "Geen Verwerking"
  , BilingualMapping "18" "Notices" "Mededelingen"
  , BilingualMapping "19" "Language" "Taal"
  , BilingualMapping "20" "Cultural Context" "Culturele Context"
  ]

-- | Load bilingual mapping from file or use defaults
loadBilingualMap :: FilePath -> IO [BilingualMapping]
loadBilingualMap mapFile = do
  content <- readFileText mapFile
  case content of
    Just text -> pure $ parseBilingualMap text
    Nothing -> do
      putStrLn $ "Warning: Could not load " ++ mapFile ++ ", using defaults"
      pure defaultBilingualMap

-- | Parse bilingual map from markdown table
parseBilingualMap :: Text -> [BilingualMapping]
parseBilingualMap text =
  let textLines = T.lines text
      tableLines = filter isTableRow textLines
      rows = mapMaybe parseTableRow tableLines
  in rows
  where
    isTableRow line = T.isPrefixOf "|" line && not (T.isPrefixOf "|---" line)

-- | Parse a single table row into BilingualMapping
parseTableRow :: Text -> Maybe BilingualMapping
parseTableRow line =
  let cells = map T.strip $ T.splitOn "|" line
      -- Table format: | Clause No. | English | Dutch |
  in case cells of
       (_ : num : eng : dut : _) ->
         if T.null num || num == "Clause No."
           then Nothing
           else Just $ BilingualMapping num eng dut
       _ -> Nothing

-- | Validate bilingual consistency between English and Dutch versions
validateBilingualConsistency :: FilePath -> FilePath -> FilePath -> ValidatorM ()
validateBilingualConsistency englishPath dutchPath mapPath = do
  config <- ask
  when (configVerbose config) $
    liftIO $ putStrLn "Validating bilingual consistency..."

  -- Load bilingual mapping
  mapping <- liftIO $ loadBilingualMap mapPath

  -- Parse both license files
  englishLicense <- validateLicenseFile englishPath
  dutchLicense <- validateLicenseFile dutchPath

  -- Check structural parity
  checkStructuralParity englishPath dutchPath englishLicense dutchLicense

  -- Check clause alignment
  checkClauseAlignment englishPath dutchPath mapping
                       (licenseClauses englishLicense)
                       (licenseClauses dutchLicense)

  -- Check version consistency
  checkVersionConsistency englishPath dutchPath
                         (licenseVersion englishLicense)
                         (licenseVersion dutchLicense)

-- | Check structural parity between languages
checkStructuralParity :: FilePath -> FilePath -> LicenseStructure -> LicenseStructure -> ValidatorM ()
checkStructuralParity enPath nlPath enLicense nlLicense = do
  let enClauseCount = length $ licenseClauses enLicense
      nlClauseCount = length $ licenseClauses nlLicense

  -- Check clause count matches
  when (enClauseCount /= nlClauseCount) $
    validationError Error "bilingual-consistency"
      (T.pack $ "Clause count mismatch: English has " ++ show enClauseCount ++
                " clauses, Dutch has " ++ show nlClauseCount)
      (Just "Both versions must have the same number of clauses")

  -- Check both have headers
  let enHasHeader = licenseHasHeader enLicense
      nlHasHeader = licenseHasHeader nlLicense

  when (enHasHeader /= nlHasHeader) $
    validationWarning Warning "bilingual-consistency"
      "Header presence mismatch between English and Dutch versions"

  -- Check governing law is present in both
  let enHasGovLaw = isNothing (licenseGoverningLaw enLicense)
      nlHasGovLaw = isNothing (licenseGoverningLaw nlLicense)

  when (enHasGovLaw && not nlHasGovLaw) $
    validationWarning Warning (T.pack enPath)
      "Governing law present in Dutch but not English version"

  when (nlHasGovLaw && not enHasGovLaw) $
    validationWarning Warning (T.pack nlPath)
      "Governing law present in English but not Dutch version"

-- | Check clause alignment against bilingual mapping
checkClauseAlignment :: FilePath -> FilePath -> [BilingualMapping] -> [Clause] -> [Clause] -> ValidatorM ()
checkClauseAlignment enPath nlPath mapping enClauses nlClauses = do
  -- Create maps for quick lookup
  let enMap = Map.fromList $ map (\c -> (clauseNum c, clauseHeading c)) enClauses
      nlMap = Map.fromList $ map (\c -> (clauseNum c, clauseHeading c)) nlClauses

  -- Check each mapping entry
  forM_ mapping $ \m -> do
    let num = mappingClauseNumber m
        expectedEn = mappingEnglishTitle m
        expectedNl = mappingDutchTitle m

    -- Check English clause
    case Map.lookup num enMap of
      Nothing ->
        validationWarning Warning (T.pack enPath)
          ("Missing clause " <> num <> " (expected: " <> expectedEn <> ")")
      Just actualEn ->
        when (not $ titleMatches expectedEn actualEn) $
          validationWarning Warning (T.pack enPath)
            ("Clause " <> num <> " title mismatch. Expected: '" <> expectedEn <>
             "', found: '" <> actualEn <> "'")

    -- Check Dutch clause
    case Map.lookup num nlMap of
      Nothing ->
        validationWarning Warning (T.pack nlPath)
          ("Missing clause " <> num <> " (expected: " <> expectedNl <> ")")
      Just actualNl ->
        when (not $ titleMatches expectedNl actualNl) $
          validationWarning Warning (T.pack nlPath)
            ("Clause " <> num <> " title mismatch. Expected: '" <> expectedNl <>
             "', found: '" <> actualNl <> "'")

  -- Check for clauses not in mapping
  let mappedNums = map mappingClauseNumber mapping
      enNums = Map.keys enMap
      nlNums = Map.keys nlMap
      unmappedEn = filter (`notElem` mappedNums) enNums
      unmappedNl = filter (`notElem` mappedNums) nlNums

  unless (null unmappedEn) $
    validationWarning Info (T.pack enPath)
      ("Clauses not in bilingual map: " <> T.intercalate ", " unmappedEn)

  unless (null unmappedNl) $
    validationWarning Info (T.pack nlPath)
      ("Clauses not in bilingual map: " <> T.intercalate ", " unmappedNl)

-- | Check if title matches (allowing for minor formatting differences)
titleMatches :: Text -> Text -> Bool
titleMatches expected actual =
  normalise expected == normalise actual ||
  normalise expected `T.isInfixOf` normalise actual ||
  normalise actual `T.isInfixOf` normalise expected
  where
    normalise = T.toLower . T.strip . T.filter (/= '—') . T.filter (/= ':')

-- | Check version consistency between languages
checkVersionConsistency :: FilePath -> FilePath -> Text -> Text -> ValidatorM ()
checkVersionConsistency enPath nlPath enVersion nlVersion = do
  when (enVersion /= nlVersion && enVersion /= "unknown" && nlVersion /= "unknown") $
    validationError Error "bilingual-consistency"
      (T.pack $ "Version mismatch: English is " ++ T.unpack enVersion ++
                ", Dutch is " ++ T.unpack nlVersion)
      (Just "Both language versions must have the same version number")
