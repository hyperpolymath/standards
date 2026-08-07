module A2ML.Parser

import A2ML.TypedCore
import A2ML.Proofs
import Data.String
import Data.List

%default total

-- ---------------------------------------------------------------------------
-- Totality helpers.
--
-- `Data.String.strIndex` is NON-COVERING in Idris2 0.7.0 (it is a partial
-- primitive), so it can never appear in a function under `%default total`.
-- peek/char previously called it AND pattern-matched its `Char` result as if
-- it were `Maybe Char`, which is where the unification errors came from.
--
-- `strIndexSafe` is total and obviously correct. It is O(n) per index, so
-- parsing is O(n^2); acceptable for a normative reference model where
-- correctness dominates. If that ever matters, carry `List Char` in
-- ParserState instead of (String, position).
-- ---------------------------------------------------------------------------

||| Safe, total character indexing.
public export
strIndexSafe : String -> Nat -> Maybe Char
strIndexSafe str n = case drop n (unpack str) of
  (c :: _) => Just c
  []       => Nothing

-- ============================================================================
-- Parser Types
-- ============================================================================

||| Parser state with position tracking
public export
record ParserState where
  constructor MkParserState
  input : String
  position : Nat
  line : Nat
  column : Nat

||| Parser result with error recovery
public export
data ParseResult a
  = Success a ParserState
  | Failure String ParserState

public export
Functor ParseResult where
  map f (Success x s) = Success (f x) s
  map f (Failure err s) = Failure err s

||| Parser monad
public export
record Parser a where
  constructor MkParser
  runParser : ParserState -> ParseResult a

public export
Functor Parser where
  map f (MkParser p) = MkParser (\s => map f (p s))

public export
Applicative Parser where
  pure x = MkParser (\s => Success x s)
  (MkParser pf) <*> (MkParser px) = MkParser $ \s =>
    case pf s of
      Success f s' => case px s' of
        Success x s'' => Success (f x) s''
        Failure err s'' => Failure err s''
      Failure err s' => Failure err s'

public export
Monad Parser where
  (MkParser p) >>= f = MkParser $ \s =>
    case p s of
      Success x s' => runParser (f x) s'
      Failure err s' => Failure err s'

-- ============================================================================
-- Basic Parsers
-- ============================================================================

||| Peek at the next character without consuming
export
peek : Parser (Maybe Char)
peek = MkParser $ \s =>
  case strIndexSafe s.input s.position of
    Just c => Success (Just c) s
    Nothing => Success Nothing s

||| An upper bound on the input still to be consumed. Every consuming loop
||| below recurses structurally on this, which is what makes them total.
public export
remaining : ParserState -> Nat
remaining s = minus (length s.input) s.position

||| First-success choice. Defined as a plain function rather than an
||| `Alternative` implementation to avoid the Lazy-argument subtleties of the
||| Prelude interface; the previous code declared `<|>` inside a `where` block,
||| where it did not resolve at all.
public export
orElse : Parser a -> Parser a -> Parser a
orElse (MkParser p1) (MkParser p2) = MkParser $ \s =>
  case p1 s of
    Success x s' => Success x s'
    Failure _ _  => p2 s

||| Consume one character
export
char : Parser (Maybe Char)
char = MkParser $ \s =>
  case strIndexSafe s.input s.position of
    Just c =>
      let newPos = s.position + 1
          newLine = if c == '\n' then s.line + 1 else s.line
          newCol = if c == '\n' then 0 else s.column + 1
      in Success (Just c) (MkParserState s.input newPos newLine newCol)
    Nothing => Success Nothing s

||| Match a specific character
export
charIs : Char -> Parser Bool
charIs expected = do
  mc <- peek
  case mc of
    Just c => if c == expected
              then do ignore char; pure True
              else pure False
    Nothing => pure False

||| Skip whitespace
export
skipWhitespace : Parser ()
skipWhitespace = MkParser $ \s => runParser (go (remaining s)) s
  where
    go : Nat -> Parser ()
    go Z = pure ()
    go (S fuel) = do
      mc <- peek
      case mc of
        Just c => if isSpace c
                  then do ignore char; go fuel
                  else pure ()
        Nothing => pure ()

||| Parse until end of line
export
parseUntilEOL : Parser String
parseUntilEOL = MkParser $ \s =>
  let (line, rest) = break (== '\n') (substr (cast s.position) (length s.input) s.input)
      newPos = s.position + cast (length line)
  in Success line (MkParserState s.input newPos s.line s.column)

||| Parse a heading (# Title)
export
parseHeading : Parser (Nat, String)
parseHeading = MkParser $ \s => runParser (body (remaining s)) s
  where
    countHashes : Nat -> Nat -> Parser Nat
    countHashes Z acc = pure acc
    countHashes (S fuel) acc = do
      isHash <- charIs '#'
      if isHash
        then countHashes fuel (acc + 1)
        else pure acc

    body : Nat -> Parser (Nat, String)
    body fuel = do
      level <- countHashes fuel 0
      skipWhitespace
      title <- parseUntilEOL
      pure (level, title)

||| Parse an ID directive (@id:value)
export
parseDirective : Parser (String, String)
parseDirective = do
  atSign <- charIs '@'
  if atSign
    then do
      name <- parseUntil ':'
      skipWhitespace
      value <- parseUntilEOL
      pure (name, value)
    else MkParser $ \s => Failure "Expected directive starting with @" s
  where
    parseUntil : Char -> Parser String
    parseUntil delim = MkParser $ \s =>
      let (word, rest) = break (== delim) (substr (cast s.position) (length s.input) s.input)
          newPos = s.position + cast (length word) + 1  -- +1 to skip delimiter
      in Success word (MkParserState s.input newPos s.line s.column)

-- ============================================================================
-- Block Parsers
-- ============================================================================

||| Parse a paragraph (non-directive, non-heading text)
export
parseParagraph : Parser Block
parseParagraph = do
  line <- parseUntilEOL
  ignore char  -- consume newline
  pure (Para line)

||| Parse a bullet list item
export
parseBullet : Parser (List String)
parseBullet = MkParser $ \s => runParser (parseBullets (remaining s) []) s
  where
    parseBullets : Nat -> List String -> Parser (List String)
    parseBullets Z acc = pure acc
    parseBullets (S fuel) acc = do
      isBullet <- orElse (charIs '-') (charIs '*')
      if isBullet
        then do
          skipWhitespace
          item <- parseUntilEOL
          ignore char  -- consume newline
          parseBullets fuel (acc ++ [item])
        else pure acc

||| Parse a section block
export
parseSection : Parser Block
parseSection = do
  (level, title) <- parseHeading
  ignore char  -- consume newline
  -- TODO: parse body recursively
  let body = []
  pure (Section (MkSec (MkId (pack (replicate level '#'))) title body))

||| Parse any block
export
parseBlock : Parser (Maybe Block)
parseBlock = do
  skipWhitespace
  mc <- peek
  case mc of
    Nothing => pure Nothing
    Just '#' => do
      sec <- parseSection
      pure (Just sec)
    Just '@' => do
      (name, value) <- parseDirective
      ignore char  -- consume newline
      -- Handle different directive types
      pure Nothing  -- TODO: map directives to blocks
    Just '-' => do
      bullets <- parseBullet
      pure (Just (Bullet bullets))
    Just '*' => do
      bullets <- parseBullet
      pure (Just (Bullet bullets))
    Just _ => do
      para <- parseParagraph
      pure (Just para)

-- ============================================================================
-- Document Parser
-- ============================================================================

||| Parse multiple blocks into a document
export
parseBlocks : Nat -> List Block -> Parser Doc
parseBlocks Z acc = pure (MkDoc acc)
parseBlocks (S fuel) acc = do
  mb <- parseBlock
  case mb of
    Just b => parseBlocks fuel (acc ++ [b])
    Nothing => pure (MkDoc acc)

||| Parse a complete A2ML document
export
parseDocument : String -> ParseResult Doc
parseDocument input =
  let initialState = MkParserState input 0 1 0
  in runParser (parseBlocks (remaining initialState) []) initialState

-- ============================================================================
-- Validation After Parsing
-- ============================================================================

||| Parse and validate a document in one step
export
parseAndValidate : String -> Either (List ValidationError) ValidatedDoc
parseAndValidate input =
  case parseDocument input of
    Success doc _ =>
      let ids = collectIds doc
          refs = collectRefs doc
      in validateDocument doc ids refs
    Failure err _ =>
      Left [MissingRequired ("Parse error: " ++ err)]

-- ============================================================================
-- Pretty Printer (for testing)
-- ============================================================================

-- Recursing on `List Block` directly (rather than re-wrapping as
-- `MkDoc s.body` and calling prettyPrint) and pattern-matching `MkSec` is what
-- lets the termination checker see section bodies as structural sub-terms: a
-- record *projection* is not treated as structural descent, a constructor
-- pattern is.
mutual
  export
  prettyBlocks : List Block -> String
  prettyBlocks [] = ""
  prettyBlocks (b :: bs) = prettyBlock b ++ prettyBlocks bs

  export
  prettyBlock : Block -> String
  prettyBlock (Section (MkSec sid title body)) =
      replicate (length sid.raw) '#' ++ " " ++ title ++ "\n" ++
      prettyBlocks body ++ "\n"
  prettyBlock (Para text) = text ++ "\n\n"
  prettyBlock (Bullet items) =
      concatMap (\item => "- " ++ item ++ "\n") items ++ "\n"
  prettyBlock (Figure f) =
      "@figure:" ++ f.id.raw ++ "\n" ++
      f.caption ++ "\n@end\n\n"
  prettyBlock (Table t) =
      "@table:" ++ t.id.raw ++ "\n" ++
      t.caption ++ "\n@end\n\n"
  prettyBlock (Refs refs) =
      "@refs:\n" ++
      concatMap (\r => "[" ++ r.label ++ "]\n") refs ++
      "@end\n\n"
  prettyBlock (Opaque p) =
      "@opaque" ++
      (case p.id of
        Just id => ":" ++ id.raw
        Nothing => "") ++
      (case p.lang of
        Just lang => " lang=" ++ lang
        Nothing => "") ++
      "\n" ++ p.bytes ++ "\n@end\n\n"

||| Pretty print a document (inverse of parser).
export
prettyPrint : Doc -> String
prettyPrint (MkDoc blocks) = prettyBlocks blocks

-- ============================================================================
-- Example Usage
-- ============================================================================

||| Example A2ML document
export
exampleA2ML : String
exampleA2ML = """
# Introduction

This is a paragraph in the introduction.

## Background

- First point
- Second point

@abstract:
This is the abstract.
@end

@refs:
[1] Reference One
[2] Reference Two
@end
"""

||| Parse the example document
export
exampleParse : ParseResult Doc
exampleParse = parseDocument exampleA2ML

||| Validate the example document
export
exampleValidate : Either (List ValidationError) ValidatedDoc
exampleValidate = parseAndValidate exampleA2ML
