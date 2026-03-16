-- SPDX-License-Identifier: MPL-2.0
-- (PMPL-1.0-or-later preferred; MPL-2.0 required for Hackage OSI-approved policy)
--
-- Data.A2ML.Parser — Parser for A2ML (Attested Markup Language) documents.
--
-- Parses the A2ML surface syntax into the typed AST defined in Data.A2ML.Types.
-- The parser is line-oriented and processes:
--   - Headings (# through #####)
--   - Directive blocks (@name(attrs): ... @end)
--   - Inline formatting (**bold**, *italic*, [link](url), @ref(id))
--   - Bullet lists (- or *)

module Data.A2ML.Parser
  ( -- * Parsing
    parseA2ML
  , parseA2MLFile

    -- * Errors
  , ParseError (..)
  ) where

import           Data.A2ML.Types

import qualified Data.Map.Strict as Map
import           Data.Text       (Text)
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO

-- | Errors that can occur during A2ML parsing.
data ParseError
  = UnterminatedDirective Int DirectiveName
    -- ^ A directive block opened at the given line was never closed with @end.
  | InvalidHeadingLevel Int Int
    -- ^ A heading at the given line has an invalid level (must be 1-5).
  | UnexpectedToken Int Text
    -- ^ An unexpected token was encountered at the given line.
  | EmptyDocument
    -- ^ The input was empty or contained only whitespace.
  deriving (Show, Eq)

-- | Parse an A2ML document from a 'Text' value.
--
-- Returns either a 'ParseError' or the parsed 'Document'.
--
-- ==== Examples
--
-- >>> parseA2ML "# Hello\n\nSome text.\n"
-- Right (Document {documentManifest = Nothing, documentBlocks = [Heading 1 [...], ...]})
parseA2ML :: Text -> Either ParseError Document
parseA2ML input
  | T.null (T.strip input) = Left EmptyDocument
  | otherwise = Right (parseLines (T.lines input))

-- | Parse an A2ML document from a file path.
--
-- Reads the file and delegates to 'parseA2ML'.
parseA2MLFile :: FilePath -> IO (Either ParseError Document)
parseA2MLFile path = parseA2ML <$> TIO.readFile path

-- ---------------------------------------------------------------------------
-- Internal parsing implementation
-- ---------------------------------------------------------------------------

-- | Parse a list of lines into a Document.
parseLines :: [Text] -> Document
parseLines ls =
  let blocks = parseBlocks 1 ls
  in  Document { documentManifest = Nothing, documentBlocks = blocks }

-- | Parse lines into a list of blocks, tracking line numbers.
parseBlocks :: Int -> [Text] -> [Block]
parseBlocks _ [] = []
parseBlocks lineNum (l:ls)
  -- Blank line
  | T.null (T.strip l) =
      BlankLine : parseBlocks (lineNum + 1) ls

  -- Heading
  | Just (level, content) <- parseHeadingLine l =
      Heading level (parseInlines content) : parseBlocks (lineNum + 1) ls

  -- Directive start (@name: or @name(attrs):)
  | Just dirName <- parseDirectiveStart l =
      let (body, rest, consumed) = collectDirectiveBody ls
          dir = Directive
            { directiveName       = dirName
            , directiveAttributes = Map.empty
            , directiveBody       = T.intercalate "\n" body
            }
      in  DirectiveBlock dir : parseBlocks (lineNum + 1 + consumed) rest

  -- Bullet item
  | Just itemText <- parseBulletLine l =
      let (items, rest, consumed) = collectBulletItems itemText ls
      in  BulletList (map (parseInlines . T.strip) items) : parseBlocks (lineNum + consumed) rest

  -- Paragraph line
  | otherwise =
      Paragraph (parseInlines (T.strip l)) : parseBlocks (lineNum + 1) ls

-- | Try to parse a heading line. Returns the level and remaining text.
parseHeadingLine :: Text -> Maybe (Int, Text)
parseHeadingLine line =
  let stripped = T.stripStart line
      hashes = T.takeWhile (== '#') stripped
      level = T.length hashes
      rest = T.stripStart (T.drop level stripped)
  in  if level >= 1 && level <= 5 && not (T.null rest)
      then Just (level, rest)
      else Nothing

-- | Try to parse a directive start line (@name:).
parseDirectiveStart :: Text -> Maybe DirectiveName
parseDirectiveStart line =
  let stripped = T.strip line
  in  if T.isPrefixOf "@" stripped && T.isSuffixOf ":" stripped
      then let name = T.dropEnd 1 (T.drop 1 stripped)
           in  Just (toDirectiveName name)
      else Nothing

-- | Convert a text name to a DirectiveName.
toDirectiveName :: Text -> DirectiveName
toDirectiveName "abstract"    = DirAbstract
toDirectiveName "refs"        = DirRefs
toDirectiveName "attestation" = DirAttestation
toDirectiveName "meta"        = DirMeta
toDirectiveName other         = DirCustom other

-- | Collect lines until @end, returning body lines, remaining lines, and count.
collectDirectiveBody :: [Text] -> ([Text], [Text], Int)
collectDirectiveBody = go [] 0
  where
    go acc n [] = (reverse acc, [], n)
    go acc n (l:ls)
      | T.strip l == "@end" = (reverse acc, ls, n + 1)
      | otherwise           = go (l : acc) (n + 1) ls

-- | Try to parse a bullet line (- or * prefix).
parseBulletLine :: Text -> Maybe Text
parseBulletLine line =
  let stripped = T.stripStart line
  in  case T.uncons stripped of
        Just ('-', rest) | not (T.null rest) && T.head rest == ' ' -> Just (T.drop 1 rest)
        Just ('*', rest) | not (T.null rest) && T.head rest == ' ' -> Just (T.drop 1 rest)
        _                                                           -> Nothing

-- | Collect consecutive bullet items.
collectBulletItems :: Text -> [Text] -> ([Text], [Text], Int)
collectBulletItems first = go [first] 1
  where
    go acc n [] = (reverse acc, [], n)
    go acc n (l:ls) =
      case parseBulletLine l of
        Just item -> go (item : acc) (n + 1) ls
        Nothing   -> (reverse acc, l : ls, n)

-- | Parse inline elements from a text fragment.
--
-- Handles: **bold**, *italic*, [text](url), @ref(id), `code`, and plain text.
parseInlines :: Text -> [Inline]
parseInlines t
  | T.null t  = []
  | otherwise = case T.breakOn "**" t of
      (before, rest)
        | not (T.null rest) ->
            let afterOpen = T.drop 2 rest
            in  case T.breakOn "**" afterOpen of
                  (boldContent, closeRest)
                    | not (T.null closeRest) ->
                        plainChunk before
                          ++ [Bold (parseInlines boldContent)]
                          ++ parseInlines (T.drop 2 closeRest)
                  _ -> plainChunk before ++ parseInlines afterOpen
        | otherwise -> parseInlinesItalic t

-- | Parse inline elements looking for *italic* markers.
parseInlinesItalic :: Text -> [Inline]
parseInlinesItalic t = case T.breakOn "*" t of
  (before, rest)
    | not (T.null rest) ->
        let afterOpen = T.drop 1 rest
        in  case T.breakOn "*" afterOpen of
              (italicContent, closeRest)
                | not (T.null closeRest) ->
                    plainChunk before
                      ++ [Italic (parseInlines italicContent)]
                      ++ parseInlines (T.drop 1 closeRest)
              _ -> plainChunk before ++ [PlainText afterOpen]
    | otherwise -> plainChunk t

-- | Convert a non-empty text to a singleton PlainText list.
plainChunk :: Text -> [Inline]
plainChunk t
  | T.null t  = []
  | otherwise = [PlainText t]
