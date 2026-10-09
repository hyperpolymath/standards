{-
SPDX-License-Identifier: MIT
SPDX-FileCopyrightText: 2024-2025 Palimpsest Stewardship Council
-}

{-# LANGUAGE OverloadedStrings #-}

-- | License text format validator
-- Validates markdown structure, clause numbering, and format compliance
module Palimpsest.Validator.License
  ( validateLicenseFile
  , validateClauseStructure
  , validateClauseNumbering
  , checkRequiredSections
  , LicenseStructure(..)
  , Clause(..)
  ) where

import Control.Monad (when, unless, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (isNothing, mapMaybe, catMaybes)
import Data.List (sort, nub)
import Palimpsest.Validator.Types
import Palimpsest.Validator.Utils
import Text.Regex.TDFA ((=~))

-- | Representation of a license clause
data Clause = Clause
  { clauseNum     :: Text
  , clauseHeading :: Text
  , clauseContent :: Text
  , clauseLine    :: Int
  } deriving (Show, Eq)

-- | License document structure
data LicenseStructure = LicenseStructure
  { licenseVersion    :: Text
  , licenseClauses    :: [Clause]
  , licenseHasHeader  :: Bool
  , licenseLanguage   :: Maybe Language
  , licenseGoverningLaw :: Maybe Text
  } deriving (Show, Eq)

-- | Validate a complete license file
validateLicenseFile :: FilePath -> ValidatorM LicenseStructure
validateLicenseFile path = do
  config <- ask
  content <- liftIO $ readFileText path

  case content of
    Nothing -> validationError Critical (T.pack path) "License file not found or unreadable" Nothing
    Just text -> do
      when (configVerbose config) $
        liftIO $ putStrLn $ "Validating license file: " ++ path

      -- Check file is not empty
      when (T.null $ T.strip text) $
        validationError Critical (T.pack path) "License file is empty" Nothing

      -- Parse structure
      let structure = parseLicenseStructure text

      -- Validate structure
      validateStructure path structure

      -- Validate clause numbering
      validateClauseNumbering path (licenseClauses structure)

      -- Check required sections
      checkRequiredSections path structure

      -- Check for common pitfalls
      checkCommonPitfalls path text

      pure structure

-- | Parse license text into structured format
parseLicenseStructure :: Text -> LicenseStructure
parseLicenseStructure text =
  let textLines = zip [1..] (T.lines text)
      clauses = extractClauses textLines
      version = extractVersion text
      hasHeader = hasLicenseHeader text
      lang = detectLanguage text
      govLaw = extractGoverningLaw text
  in LicenseStructure
       { licenseVersion = version
       , licenseClauses = clauses
       , licenseHasHeader = hasHeader
       , licenseLanguage = lang
       , licenseGoverningLaw = govLaw
       }

-- | Extract clauses from numbered lines
extractClauses :: [(Int, Text)] -> [Clause]
extractClauses lines' =
  let isClauseHeading line = T.isPrefixOf "##" line || isClausePattern line
      clauseStarts = filter (\(_, line) -> isClauseHeading line) lines'
  in mapMaybe (parseClause lines') clauseStarts

-- | Check if line matches clause pattern
isClausePattern :: Text -> Bool
isClausePattern line =
  let pattern = "^[Cc]lause[[:space:]]+[0-9]+" :: String
  in T.unpack line =~ pattern

-- | Parse a single clause
parseClause :: [(Int, Text)] -> (Int, Text) -> Maybe Clause
parseClause allLines (lineNum, heading) =
  let num = extractClauseNum heading
      title = T.strip $ T.dropWhile (/= '—') heading
      content = extractClauseContent allLines lineNum
  in case num of
       Just n -> Just $ Clause n title content lineNum
       Nothing -> Nothing

-- | Extract clause number from heading
extractClauseNum :: Text -> Maybe Text
extractClauseNum text =
  let pattern = "[Cc]lause[[:space:]]+([0-9]+\\.?[0-9]*)" :: String
      matches = T.unpack text =~ pattern :: [[String]]
  in case matches of
       ((_ : n : _) : _) -> Just (T.pack n)
       _ -> -- Try markdown heading format "## 1. Title"
         let mdPattern = "##[[:space:]]+([0-9]+\\.?[0-9]*)" :: String
             mdMatches = T.unpack text =~ mdPattern :: [[String]]
         in case mdMatches of
              ((_ : n : _) : _) -> Just (T.pack n)
              _ -> Nothing

-- | Extract clause content (all lines until next clause)
extractClauseContent :: [(Int, Text)] -> Int -> Text
extractClauseContent allLines startLine =
  let relevantLines = dropWhile (\(n, _) -> n <= startLine) allLines
      contentLines = takeWhile (\(_, line) -> not (isClausePattern line || T.isPrefixOf "##" line)) relevantLines
  in T.unlines $ map snd contentLines

-- | Extract version from text
extractVersion :: Text -> Text
extractVersion text =
  let pattern = "[Vv]ersion[[:space:]]+([0-9]+\\.[0-9]+)" :: String
      matches = T.unpack text =~ pattern :: [[String]]
  in case matches of
       ((_ : v : _) : _) -> T.pack v
       _ -> -- Try "v0.3" format
         let altPattern = "v([0-9]+\\.[0-9]+)" :: String
             altMatches = T.unpack text =~ altPattern :: [[String]]
         in case altMatches of
              ((_ : v : _) : _) -> T.pack v
              _ -> "unknown"

-- | Check if document has proper header
hasLicenseHeader :: Text -> Bool
hasLicenseHeader text =
  let firstLines = take 10 $ T.lines text
      hasTitle = any (\line -> T.isPrefixOf "# " line && T.isInfixOf "Palimpsest" line) firstLines
  in hasTitle

-- | Detect document language
detectLanguage :: Text -> Maybe Language
detectLanguage text
  | T.isInfixOf "Governing Law" text || T.isInfixOf "Definitions" text = Just English
  | T.isInfixOf "Toepasselijk Recht" text || T.isInfixOf "Definities" text = Just Dutch
  | otherwise = Nothing

-- | Extract governing law from document
extractGoverningLaw :: Text -> Maybe Text
extractGoverningLaw text =
  let pattern = "Governing Law[[:space:]]*:[[:space:]]*(.+)" :: String
      matches = T.unpack text =~ pattern :: [[String]]
  in case matches of
       ((_ : law : _) : _) -> Just (T.strip $ T.pack law)
       _ -> Nothing

-- | Validate overall structure
validateStructure :: FilePath -> LicenseStructure -> ValidatorM ()
validateStructure path structure = do
  -- Check for header
  unless (licenseHasHeader structure) $
    validationWarning Warning (T.pack path) "License file missing proper header with title"

  -- Check version is present
  when (licenseVersion structure == "unknown") $
    validationWarning Warning (T.pack path) "Could not detect license version"

  -- Check for clauses
  when (null $ licenseClauses structure) $
    validationError Error (T.pack path) "No clauses found in license document" Nothing

  -- Check language is detected
  when (isNothing $ licenseLanguage structure) $
    validationWarning Info (T.pack path) "Could not detect document language"

  -- Check governing law is present
  when (isNothing $ licenseGoverningLaw structure) $
    validationWarning Warning (T.pack path) "Governing law not specified"

-- | Validate clause numbering is sequential and correct
validateClauseNumbering :: FilePath -> [Clause] -> ValidatorM ()
validateClauseNumbering path clauses = do
  let numbers = map clauseNum clauses
      parsed = mapMaybe parseClauseNumber numbers

  -- Check for duplicate clause numbers
  let duplicates = findDuplicates numbers
  unless (null duplicates) $
    validationError Error (T.pack path)
      ("Duplicate clause numbers found: " <> T.intercalate ", " duplicates)
      Nothing

  -- Check sequential numbering
  let sorted = sort parsed
      expected = [1..length parsed]
  when (parsed /= sorted) $
    validationWarning Warning (T.pack path) "Clause numbers are not in sequential order"

  -- Check for gaps in numbering
  forM_ (zip expected parsed) $ \(exp, actual) ->
    when (exp /= actual) $
      validationWarning Warning (T.pack path)
        ("Clause numbering gap: expected " <> T.pack (show exp) <> ", found " <> T.pack (show actual))

-- | Parse clause number to integer
parseClauseNumber :: Text -> Maybe Int
parseClauseNumber text =
  case T.decimal text of
    Right (n, _) -> Just n
    Left _ -> Nothing

-- | Find duplicate elements in list
findDuplicates :: Eq a => [a] -> [a]
findDuplicates xs = xs \\ nub xs
  where
    (\\) :: Eq a => [a] -> [a] -> [a]
    (\\) = foldr (\x acc -> if x `elem` acc then acc else x : filter (/= x) acc) []

-- | Check for required sections in license
checkRequiredSections :: FilePath -> LicenseStructure -> ValidatorM ()
checkRequiredSections path structure = do
  let clauseTitles = map (T.toLower . clauseHeading) (licenseClauses structure)
      requiredSections =
        [ ("definitions", "Definitions")
        , ("attribution", "Attribution")
        , ("governing law", "Governing Law")
        , ("termination", "Termination")
        ]

  forM_ requiredSections $ \(keyword, sectionName) ->
    unless (any (T.isInfixOf keyword) clauseTitles) $
      validationWarning Warning (T.pack path)
        ("Recommended section missing: " <> sectionName)

-- | Check for common pitfalls mentioned in EXPLAINME_ROOT.md
checkCommonPitfalls :: FilePath -> Text -> ValidatorM ()
checkCommonPitfalls path text = do
  -- Check for metadata preservation clause
  unless (T.isInfixOf "metadata" (T.toLower text) || T.isInfixOf "Clause 2.3" text) $
    validationWarning Warning (T.pack path)
      "No explicit metadata preservation clause found (potential breach of Clause 2.3)"

  -- Check for NI systems consent clause
  unless (T.isInfixOf "Non-Interpretive" text || T.isInfixOf "NI systems" text || T.isInfixOf "Clause 1.2" text) $
    validationWarning Warning (T.pack path)
      "No explicit Non-Interpretive (NI) systems clause found (potential breach of Clause 1.2)"

  -- Check for emotional lineage protection
  unless (T.isInfixOf "emotional" (T.toLower text) || T.isInfixOf "lineage" (T.toLower text)) $
    validationWarning Info (T.pack path)
      "No explicit emotional lineage protection found"
