-- SPDX-License-Identifier: MPL-2.0
-- (PMPL-1.0-or-later preferred; MPL-2.0 required for Hackage OSI-approved policy)
--
-- Data.K9.Parser — Parser for K9 self-validating component specifications.
--
-- Parses both .k9 (YAML-like) and .k9.ncl (Nickel) variants into the typed
-- AST defined in Data.K9.Types. The parser is line-oriented and extracts:
--   - Magic number (K9!)
--   - Pedigree metadata (name, version, description, author)
--   - Security level (Kennel/Yard/Hunt) with permission flags
--   - Target platform constraints
--   - Recipes and validation blocks

module Data.K9.Parser
  ( -- * Parsing
    parseK9
  , parseK9File
  , parseK9Nickel

    -- * Errors
  , ParseError (..)

    -- * Format detection
  , K9Format (..)
  , detectFormat
  ) where

import           Data.K9.Types

import qualified Data.Map.Strict as Map
import           Data.Text       (Text)
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO

-- | K9 file format variants.
data K9Format
  = K9Yaml
    -- ^ YAML-like .k9 format (starts with K9! magic number).
  | K9Nickel
    -- ^ Nickel .k9.ncl format (starts with let/import).
  deriving (Show, Eq)

-- | Errors that can occur during K9 parsing.
data ParseError
  = MissingMagicNumber
    -- ^ The .k9 file does not start with the K9! magic number.
  | MissingPedigree Text
    -- ^ A required pedigree field is missing.
  | InvalidSecurityLevel Text
    -- ^ An unrecognised security level string was encountered.
  | MalformedYaml Int Text
    -- ^ Malformed YAML-like syntax at the given line.
  | NickelNotSupported
    -- ^ Nickel parsing is not yet implemented (placeholder).
  | EmptyInput
    -- ^ The input was empty.
  deriving (Show, Eq)

-- | Detect the format of a K9 file from its content.
detectFormat :: Text -> K9Format
detectFormat input =
  let firstLine = T.strip (head (T.lines input))
  in  if firstLine == "K9!" then K9Yaml else K9Nickel

-- | Parse a K9 component from text (auto-detects format).
--
-- For .k9 files, expects the K9! magic number on the first line.
-- For .k9.ncl files, currently returns 'NickelNotSupported'.
--
-- ==== Examples
--
-- >>> parseK9 "K9!\n---\nmetadata:\n  name: test\n  version: 1.0.0\n  description: A test\n"
-- Right (Component { ... })
parseK9 :: Text -> Either ParseError Component
parseK9 input
  | T.null (T.strip input) = Left EmptyInput
  | otherwise =
      case detectFormat input of
        K9Yaml   -> parseK9Yaml input
        K9Nickel -> parseK9Nickel input

-- | Parse a K9 component from a file path (auto-detects format).
parseK9File :: FilePath -> IO (Either ParseError Component)
parseK9File path = parseK9 <$> TIO.readFile path

-- | Parse a K9 Nickel (.k9.ncl) file.
--
-- Currently a placeholder that returns 'NickelNotSupported'. Full Nickel
-- parsing requires a Nickel evaluator binding.
parseK9Nickel :: Text -> Either ParseError Component
parseK9Nickel _ = Left NickelNotSupported

-- ---------------------------------------------------------------------------
-- Internal: YAML-like K9 parser
-- ---------------------------------------------------------------------------

-- | Parse the YAML-like .k9 format.
parseK9Yaml :: Text -> Either ParseError Component
parseK9Yaml input = do
  let ls = T.lines input
  case ls of
    [] -> Left EmptyInput
    (first : rest)
      | T.strip first /= "K9!" -> Left MissingMagicNumber
      | otherwise -> do
          let kvs = parseYamlLines rest
          buildComponent kvs

-- | Simple key-value extraction from YAML-like lines.
-- Supports single-level nesting via indentation.
parseYamlLines :: [Text] -> Map.Map Text Text
parseYamlLines = go "" Map.empty
  where
    go _ acc [] = acc
    go section acc (l:ls)
      | T.null (T.strip l) || T.isPrefixOf "#" (T.strip l) || T.strip l == "---" =
          go section acc ls
      | not (T.isPrefixOf " " l) && T.isSuffixOf ":" (T.strip l) =
          -- New section header (e.g., "metadata:")
          let sectionName = T.stripEnd (T.dropEnd 1 (T.strip l))
          in  go sectionName acc ls
      | T.isPrefixOf "  " l =
          -- Indented key-value under current section
          let stripped = T.strip l
          in  case T.breakOn ": " stripped of
                (key, val)
                  | not (T.null val) ->
                      let fullKey = if T.null section
                                    then key
                                    else section <> "." <> key
                          value = T.strip (T.drop 2 val)
                          -- Strip surrounding quotes if present
                          cleanVal = stripQuotes value
                      in  go section (Map.insert fullKey cleanVal acc) ls
                _ ->
                  -- Possibly a list item (- item) or multiline value
                  go section acc ls
      | otherwise =
          -- Top-level key-value
          case T.breakOn ": " (T.strip l) of
            (key, val) | not (T.null val) ->
              go section (Map.insert key (stripQuotes (T.strip (T.drop 2 val))) acc) ls
            _ -> go section acc ls

-- | Strip surrounding double or single quotes from a text value.
stripQuotes :: Text -> Text
stripQuotes t
  | T.length t >= 2 && T.head t == '"' && T.last t == '"'   = T.init (T.tail t)
  | T.length t >= 2 && T.head t == '\'' && T.last t == '\'' = T.init (T.tail t)
  | otherwise = t

-- | Build a Component from extracted key-value pairs.
buildComponent :: Map.Map Text Text -> Either ParseError Component
buildComponent kvs = do
  name <- requireField "metadata.name" kvs
  version <- requireField "metadata.version" kvs
  description <- requireField "metadata.description" kvs
  let pedigree = Pedigree
        { pedigreeName        = name
        , pedigreeVersion     = version
        , pedigreeDescription = description
        , pedigreeAuthor      = Map.lookup "metadata.author" kvs
        , pedigreeLicense     = Map.lookup "metadata.license" kvs
        }
  let secLevel = case Map.lookup "security.trust_level" kvs of
        Just l  -> parseSecurityLevel l
        Nothing -> Right Kennel
  level <- secLevel
  let security = SecurityPolicy
        { securityLevel           = level
        , securityAllowNetwork    = lookupBool "security.allow_network" kvs
        , securityAllowFsWrite    = lookupBool "security.allow_filesystem_write" kvs
        , securityAllowSubprocess = lookupBool "security.allow_subprocess" kvs
        }
  let target = case Map.lookup "target.os" kvs of
        Nothing -> Nothing
        Just os -> Just Target
          { targetOS             = Just os
          , targetIsEdge         = lookupBool "target.is_edge" kvs
          , targetRequiresPodman = lookupBool "target.requires_podman" kvs
          , targetMemory         = Map.lookup "target.memory" kvs
          }
  let recipes = case Map.lookup "recipes.install" kvs of
        Nothing -> Nothing
        Just _  -> Just Recipes
          { recipeInstall  = Map.lookup "recipes.install" kvs
          , recipeValidate = Map.lookup "recipes.validate" kvs
          , recipeDeploy   = Map.lookup "recipes.deploy" kvs
          , recipeMigrate  = Map.lookup "recipes.migrate" kvs
          , recipeCustom   = Map.empty
          }
  let validation = case Map.lookup "validation.checksum" kvs of
        Nothing -> Nothing
        Just cs -> Just Validation
          { validationChecksum        = cs
          , validationPedigreeVersion = maybe "" id (Map.lookup "validation.pedigree_version" kvs)
          , validationHuntAuthorized  = lookupBool "validation.hunt_authorized" kvs
          }
  Right Component
    { componentPedigree   = pedigree
    , componentSecurity   = security
    , componentTarget     = target
    , componentRecipes    = recipes
    , componentValidation = validation
    , componentContent    = Map.empty
    , componentTags       = []
    }

-- | Require a field, returning ParseError if missing.
requireField :: Text -> Map.Map Text Text -> Either ParseError Text
requireField key kvs =
  case Map.lookup key kvs of
    Nothing -> Left (MissingPedigree key)
    Just v  -> Right v

-- | Parse a security level string.
parseSecurityLevel :: Text -> Either ParseError SecurityLevel
parseSecurityLevel t = case T.toLower (T.strip t) of
  "kennel"  -> Right Kennel
  "'kennel" -> Right Kennel
  "yard"    -> Right Yard
  "'yard"   -> Right Yard
  "hunt"    -> Right Hunt
  "'hunt"   -> Right Hunt
  other     -> Left (InvalidSecurityLevel other)

-- | Look up a boolean value (default False).
lookupBool :: Text -> Map.Map Text Text -> Bool
lookupBool key kvs =
  case Map.lookup key kvs of
    Just "true"  -> True
    Just "True"  -> True
    Just "yes"   -> True
    _            -> False
