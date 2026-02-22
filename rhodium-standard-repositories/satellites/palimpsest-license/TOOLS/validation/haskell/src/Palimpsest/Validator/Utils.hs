{-
SPDX-License-Identifier: MIT
SPDX-FileCopyrightText: 2024-2025 Palimpsest Stewardship Council
-}

{-# LANGUAGE OverloadedStrings #-}

-- | Utility functions for validation
module Palimpsest.Validator.Utils
  ( readFileText
  , normaliseText
  , extractClauseNumber
  , isMarkdownFile
  , isJSONLDFile
  , isXMLFile
  , parseClauseReference
  , findFiles
  , relativePath
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, listDirectory, doesDirectoryExist)
import System.FilePath ((</>), takeExtension, makeRelative)
import Text.Regex.TDFA ((=~))
import Data.List (isSuffixOf)

-- | Read file contents as Text
readFileText :: FilePath -> IO (Maybe Text)
readFileText path = do
  exists <- doesFileExist path
  if exists
    then Just <$> TIO.readFile path
    else pure Nothing

-- | Normalise text for comparison (trim, lowercase)
normaliseText :: Text -> Text
normaliseText = T.toLower . T.strip

-- | Extract clause number from text (e.g., "Clause 1.2" -> "1.2")
extractClauseNumber :: Text -> Maybe Text
extractClauseNumber text =
  let pattern = "Clause[[:space:]]+([0-9]+\\.?[0-9]*)" :: String
      matches = T.unpack text =~ pattern :: [[String]]
  in case matches of
       ((_ : num : _) : _) -> Just (T.pack num)
       _ -> Nothing

-- | Check if file is a Markdown file
isMarkdownFile :: FilePath -> Bool
isMarkdownFile path = takeExtension path `elem` [".md", ".markdown"]

-- | Check if file is a JSON-LD file
isJSONLDFile :: FilePath -> Bool
isJSONLDFile path = takeExtension path `elem` [".jsonld", ".json"]

-- | Check if file is an XML file
isXMLFile :: FilePath -> Bool
isXMLFile path = takeExtension path `elem` [".xml"]

-- | Parse clause reference from text
parseClauseReference :: Text -> Maybe (Text, Maybe Text)
parseClauseReference text =
  let pattern = "Clause[[:space:]]+([0-9]+\\.?[0-9]*)([[:space:]]+(.+))?" :: String
      matches = T.unpack text =~ pattern :: [[String]]
  in case matches of
       ((_ : num : _ : title : _) : _) ->
         Just (T.pack num, if null title then Nothing else Just (T.pack title))
       ((_ : num : _) : _) ->
         Just (T.pack num, Nothing)
       _ -> Nothing

-- | Find files matching a predicate in directory
findFiles :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
findFiles dir predicate = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      contents <- listDirectory dir
      let fullPaths = map (dir </>) contents
      matching <- filterM (\path -> do
        isFile <- doesFileExist path
        pure (isFile && predicate path)) fullPaths
      subdirs <- filterM doesDirectoryExist fullPaths
      nested <- concat <$> mapM (\subdir -> findFiles subdir predicate) subdirs
      pure (matching ++ nested)

-- | Get relative path from base directory
relativePath :: FilePath -> FilePath -> FilePath
relativePath = makeRelative

-- Helper function
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = pure []
filterM p (x:xs) = do
  b <- p x
  if b
    then (x:) <$> filterM p xs
    else filterM p xs
