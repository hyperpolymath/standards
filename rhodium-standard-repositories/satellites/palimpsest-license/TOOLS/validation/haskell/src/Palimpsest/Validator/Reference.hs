{-
SPDX-License-Identifier: MIT
SPDX-FileCopyrightText: 2024-2025 Palimpsest Stewardship Council
-}

{-# LANGUAGE OverloadedStrings #-}

-- | Cross-reference validator for license documents
module Palimpsest.Validator.Reference
  ( validateReferences
  , findBrokenLinks
  , validateClauseReferences
  , ReferenceType(..)
  , Reference(..)
  ) where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (mapMaybe, catMaybes)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), takeDirectory, normalise)
import Text.Regex.TDFA ((=~))
import Palimpsest.Validator.Types
import Palimpsest.Validator.Utils

-- | Type of reference
data ReferenceType
  = ClauseReference    -- ^ Reference to another clause (e.g., "see Clause 2.3")
  | FileReference      -- ^ Reference to another file (e.g., "see docs/guide.md")
  | URLReference       -- ^ External URL reference
  | InternalLink       -- ^ Markdown internal link
  deriving (Show, Eq)

-- | Parsed reference
data Reference = Reference
  { refType     :: ReferenceType
  , refTarget   :: Text
  , refSource   :: FilePath
  , refLine     :: Int
  , refContext  :: Text
  } deriving (Show, Eq)

-- | Validate all references in a file
validateReferences :: FilePath -> ValidatorM [Reference]
validateReferences path = do
  config <- ask
  when (configVerbose config) $
    liftIO $ putStrLn $ "Validating references in: " ++ path

  content <- liftIO $ readFileText path
  case content of
    Nothing -> do
      validationWarning Warning (T.pack path) "Could not read file for reference validation"
      pure []
    Just text -> do
      let refs = extractReferences path text
      -- Validate each reference
      forM_ refs validateReference
      pure refs

-- | Extract all references from text
extractReferences :: FilePath -> Text -> [Reference]
extractReferences sourcePath text =
  let numberedLines = zip [1..] (T.lines text)
      clauseRefs = concatMap (extractClauseRefs sourcePath) numberedLines
      fileRefs = concatMap (extractFileRefs sourcePath) numberedLines
      urlRefs = concatMap (extractURLRefs sourcePath) numberedLines
      internalRefs = concatMap (extractInternalLinks sourcePath) numberedLines
  in clauseRefs ++ fileRefs ++ urlRefs ++ internalRefs

-- | Extract clause references from a line
extractClauseRefs :: FilePath -> (Int, Text) -> [Reference]
extractClauseRefs path (lineNum, line) =
  let pattern = "[Cc]lause[[:space:]]+([0-9]+\\.?[0-9]*)" :: String
      matches = T.unpack line =~ pattern :: [[String]]
  in map (\(match:clause:_) -> Reference
            { refType = ClauseReference
            , refTarget = T.pack clause
            , refSource = path
            , refLine = lineNum
            , refContext = T.pack match
            }) matches

-- | Extract file references from a line (markdown links)
extractFileRefs :: FilePath -> (Int, Text) -> [Reference]
extractFileRefs path (lineNum, line) =
  let pattern = "\\[([^]]+)\\]\\(([^)]+\\.md[^)]*)\\)" :: String
      matches = T.unpack line =~ pattern :: [[String]]
  in mapMaybe (\parts -> case parts of
         (_:linkText:target:_) ->
           if not (T.isPrefixOf "http" (T.pack target))
             then Just $ Reference
                    { refType = FileReference
                    , refTarget = T.pack target
                    , refSource = path
                    , refLine = lineNum
                    , refContext = T.pack linkText
                    }
             else Nothing
         _ -> Nothing
      ) matches

-- | Extract URL references from a line
extractURLRefs :: FilePath -> (Int, Text) -> [Reference]
extractURLRefs path (lineNum, line) =
  let pattern = "\\[([^]]+)\\]\\((https?://[^)]+)\\)" :: String
      matches = T.unpack line =~ pattern :: [[String]]
  in map (\(_:linkText:url:_) -> Reference
            { refType = URLReference
            , refTarget = T.pack url
            , refSource = path
            , refLine = lineNum
            , refContext = T.pack linkText
            }) matches

-- | Extract internal markdown links
extractInternalLinks :: FilePath -> (Int, Text) -> [Reference]
extractInternalLinks path (lineNum, line) =
  let pattern = "\\[([^]]+)\\]\\(#([^)]+)\\)" :: String
      matches = T.unpack line =~ pattern :: [[String]]
  in map (\(_:linkText:anchor:_) -> Reference
            { refType = InternalLink
            , refTarget = T.pack anchor
            , refSource = path
            , refLine = lineNum
            , refContext = T.pack linkText
            }) matches

-- | Validate a single reference
validateReference :: Reference -> ValidatorM ()
validateReference ref = do
  config <- ask
  case refType ref of
    ClauseReference -> validateClauseRef ref
    FileReference -> validateFileRef config ref
    URLReference -> validateURLRef ref
    InternalLink -> validateInternalRef ref

-- | Validate clause reference
validateClauseRef :: Reference -> ValidatorM ()
validateClauseRef ref = do
  -- Note: We can't fully validate clause references without parsing the target file
  -- This is a placeholder for more sophisticated validation
  when (T.null $ refTarget ref) $
    validationWarning Warning (T.pack $ refSource ref)
      ("Empty clause reference at line " <> T.pack (show $ refLine ref))

-- | Validate file reference
validateFileRef :: ValidationConfig -> Reference -> ValidatorM ()
validateFileRef config ref = do
  let sourceDir = takeDirectory $ refSource ref
      targetPath = if T.isPrefixOf "/" (refTarget ref)
                     then configRootPath config </> T.unpack (T.drop 1 $ refTarget ref)
                     else sourceDir </> T.unpack (refTarget ref)
      normalisedTarget = normalise targetPath

  exists <- liftIO $ doesFileExist normalisedTarget
  unless exists $
    validationWarning Warning (T.pack $ refSource ref)
      ("Broken file reference at line " <> T.pack (show $ refLine ref) <>
       ": " <> refTarget ref <> " (resolved to: " <> T.pack normalisedTarget <> ")")

-- | Validate URL reference
validateURLRef :: Reference -> ValidatorM ()
validateURLRef ref = do
  -- For now, just check basic URL format
  -- Could be extended to actually check if URLs are accessible
  when (not $ isValidURL $ refTarget ref) $
    validationWarning Warning (T.pack $ refSource ref)
      ("Possibly malformed URL at line " <> T.pack (show $ refLine ref) <>
       ": " <> refTarget ref)

-- | Check if URL has valid format
isValidURL :: Text -> Bool
isValidURL url =
  T.isPrefixOf "http://" url || T.isPrefixOf "https://" url

-- | Validate internal link/anchor
validateInternalRef :: Reference -> ValidatorM ()
validateInternalRef ref = do
  -- Read source file and check if anchor exists
  content <- liftIO $ readFileText (refSource ref)
  case content of
    Nothing -> pure ()
    Just text -> do
      let anchorExists = checkAnchorExists (refTarget ref) text
      unless anchorExists $
        validationWarning Warning (T.pack $ refSource ref)
          ("Broken internal link at line " <> T.pack (show $ refLine ref) <>
           ": #" <> refTarget ref)

-- | Check if anchor exists in document
checkAnchorExists :: Text -> Text -> Bool
checkAnchorExists anchor text =
  let -- Convert heading to anchor format: "## My Heading" -> "my-heading"
      headings = filter (T.isPrefixOf "#") (T.lines text)
      anchors = map headingToAnchor headings
  in anchor `elem` anchors

-- | Convert heading to anchor format
headingToAnchor :: Text -> Text
headingToAnchor heading =
  let withoutHashes = T.dropWhile (== '#') heading
      trimmed = T.strip withoutHashes
      lowered = T.toLower trimmed
      -- Replace spaces with hyphens, remove special chars
      hyphenated = T.map (\c -> if c == ' ' then '-' else c) lowered
      cleaned = T.filter (\c -> c == '-' || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')) hyphenated
  in cleaned

-- | Find all broken links in a file
findBrokenLinks :: FilePath -> ValidatorM [Reference]
findBrokenLinks path = do
  refs <- validateReferences path
  -- Return only the references that failed validation
  -- (In a real implementation, we'd track which refs failed)
  pure refs

-- | Validate clause references across documents
validateClauseReferences :: [FilePath] -> ValidatorM ()
validateClauseReferences paths = do
  config <- ask
  when (configVerbose config) $
    liftIO $ putStrLn "Validating cross-document clause references..."

  -- Extract all references from all files
  allRefs <- concat <$> mapM validateReferences paths

  -- Filter to clause references only
  let clauseRefs = filter (\r -> refType r == ClauseReference) allRefs

  when (configVerbose config) $
    liftIO $ putStrLn $ "Found " ++ show (length clauseRefs) ++ " clause references"

  -- Could extend this to verify clause numbers exist in referenced documents
  pure ()
