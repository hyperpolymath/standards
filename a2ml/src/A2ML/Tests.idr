module A2ML.Tests

import A2ML.TypedCore
import A2ML.Proofs
import A2ML.Parser

%default total

-- ============================================================================
-- Test Helpers
-- ============================================================================

||| Test assertion
export
assert : String -> Bool -> IO ()
assert name test =
  if test
    then putStrLn ("✓ " ++ name)
    else putStrLn ("✗ " ++ name ++ " FAILED")

||| Test group
export
testGroup : String -> List (IO ()) -> IO ()
testGroup name tests = do
  putStrLn ("\n" ++ name ++ ":")
  sequence_ tests

-- ============================================================================
-- TypedCore Tests
-- ============================================================================

export
testTypedCore : IO ()
testTypedCore = testGroup "TypedCore Tests" [
  -- Test empty document
  assert "Empty document has no IDs" $
    let doc = MkDoc []
    in collectIds doc == []
  ,

  -- Test single section
  assert "Single section collects ID" $
    let doc = MkDoc [Section (MkSec (MkId "intro") "Introduction" [])]
        ids = collectIds doc
    in ids == [MkId "intro"]
  ,

  -- Test nested sections
  assert "Nested sections collect all IDs" $
    let inner = Section (MkSec (MkId "methods") "Methods" [])
        outer = Section (MkSec (MkId "intro") "Introduction" [inner])
        doc = MkDoc [outer]
        ids = collectIds doc
    in ids == [MkId "intro", MkId "methods"]
  ,

  -- Test figure with reference
  assert "Figure reference is collected" $
    let fig = Figure (MkFig (MkId "fig1") "Caption" (Just (MkId "intro")))
        doc = MkDoc [fig]
        refs = collectRefs doc
    in refs == [MkId "intro"]
  ,

  -- Test validation - unique IDs
  assert "Unique IDs validated correctly" $
    let doc = MkDoc [
          Section (MkSec (MkId "intro") "Introduction" []),
          Section (MkSec (MkId "methods") "Methods" [])
        ]
    in uniqueIdsB doc
  ,

  -- Test validation - duplicate IDs
  assert "Duplicate IDs detected" $
    let doc = MkDoc [
          Section (MkSec (MkId "intro") "Introduction" []),
          Section (MkSec (MkId "intro") "Duplicate" [])
        ]
    in not (uniqueIdsB doc)
  ,

  -- Test validation - resolved refs
  assert "Resolved references validated correctly" $
    let doc = MkDoc [
          Section (MkSec (MkId "intro") "Introduction" []),
          Figure (MkFig (MkId "fig1") "Caption" (Just (MkId "intro")))
        ]
    in refsResolveB doc
  ,

  -- Test validation - unresolved refs
  assert "Unresolved references detected" $
    let doc = MkDoc [
          Figure (MkFig (MkId "fig1") "Caption" (Just (MkId "missing")))
        ]
    in not (refsResolveB doc)
]

-- ============================================================================
-- Proofs Tests
-- ============================================================================

export
testProofs : IO ()
testProofs = testGroup "Proofs Tests" [
  -- Test DecEq for Id
  assert "DecEq for identical IDs" $
    case decEq (MkId "test") (MkId "test") of
      Yes _ => True
      No _ => False
  ,

  assert "DecEq for different IDs" $
    case decEq (MkId "test1") (MkId "test2") of
      Yes _ => False
      No _ => True
  ,

  -- Test Unique type
  assert "Empty list is unique" $
    case uniqueDec {a=Id} [] of
      Yes _ => True
      No _ => False
  ,

  assert "Single element list is unique" $
    case uniqueDec [MkId "test"] of
      Yes _ => True
      No _ => False
  ,

  assert "Duplicate detection in list" $
    case uniqueDec [MkId "test", MkId "test"] of
      Yes _ => False
      No _ => True
  ,

  -- Test AllIn type
  assert "Empty list is in any list" $
    case allInDec {a=Id} [] [MkId "a", MkId "b"] of
      Yes _ => True
      No _ => False
  ,

  assert "Subset detection works" $
    case allInDec [MkId "a"] [MkId "a", MkId "b"] of
      Yes _ => True
      No _ => False
  ,

  assert "Non-subset detection works" $
    case allInDec [MkId "c"] [MkId "a", MkId "b"] of
      Yes _ => False
      No _ => True
  ,

  -- Test ValidatedDoc construction
  assert "ValidatedDoc construction succeeds for valid doc" $
    let doc = MkDoc []
        ids = [MkId "intro"]
        refs = [MkId "intro"]
    in case mkValidatedDoc doc ids refs of
      Just _ => True
      Nothing => False
  ,

  assert "ValidatedDoc construction fails for duplicate IDs" $
    let doc = MkDoc []
        ids = [MkId "intro", MkId "intro"]
        refs = []
    in case mkValidatedDoc doc ids refs of
      Just _ => False
      Nothing => True
  ,

  assert "ValidatedDoc construction fails for unresolved refs" $
    let doc = MkDoc []
        ids = [MkId "intro"]
        refs = [MkId "missing"]
    in case mkValidatedDoc doc ids refs of
      Just _ => False
      Nothing => True
]

-- ============================================================================
-- Parser Tests
-- ============================================================================

export
testParser : IO ()
testParser = testGroup "Parser Tests" [
  -- Test heading parser
  assert "Parse heading level 1" $
    let input = "# Title"
        state = MkParserState input 0 1 0
    in case runParser parseHeading state of
      Success (1, "Title") _ => True
      _ => False
  ,

  assert "Parse heading level 2" $
    let input = "## Subtitle"
        state = MkParserState input 0 1 0
    in case runParser parseHeading state of
      Success (2, "Subtitle") _ => True
      _ => False
  ,

  -- Test directive parser
  assert "Parse directive with value" $
    let input = "@abstract:This is the abstract"
        state = MkParserState input 0 1 0
    in case runParser parseDirective state of
      Success ("abstract", "This is the abstract") _ => True
      _ => False
  ,

  -- Test paragraph parser
  assert "Parse paragraph" $
    let input = "This is a paragraph\n"
        state = MkParserState input 0 1 0
    in case runParser parseParagraph state of
      Success (Para "This is a paragraph") _ => True
      _ => False
  ,

  -- Test bullet parser
  assert "Parse bullet list" $
    let input = "- Item 1\n- Item 2\n"
        state = MkParserState input 0 1 0
    in case runParser parseBullet state of
      Success items _ => items == ["Item 1", "Item 2"]
      _ => False
  ,

  -- Test document parser
  assert "Parse simple document" $
    let input = "# Introduction\n\nThis is a paragraph.\n"
    in case parseDocument input of
      Success doc _ => length (blocks doc) > 0
      _ => False
  ,

  -- Test pretty printer (round trip)
  assert "Pretty print and parse round trip" $
    let doc = MkDoc [
          Section (MkSec (MkId "#") "Title" []),
          Para "Paragraph text"
        ]
        printed = prettyPrint doc
    in case parseDocument printed of
      Success parsed _ => length (blocks parsed) == length (blocks doc)
      _ => False
]

-- ============================================================================
-- Integration Tests
-- ============================================================================

export
testIntegration : IO ()
testIntegration = testGroup "Integration Tests" [
  -- Test end-to-end: parse and validate
  assert "Parse and validate valid document" $
    let input = """
# Introduction

This is the intro.

## Methods

Details here.
"""
    in case parseAndValidate input of
      Right _ => True
      Left _ => False
  ,

  -- Test end-to-end: detect validation error
  assert "Parse and validate detects unresolved refs" $
    let input = """
# Introduction

See Figure @fig:missing
"""
    in case parseAndValidate input of
      Right _ => False
      Left errs => length errs > 0
  ,

  -- Test attestation
  assert "Attestation verification works" $
    case exampleAttestation of
      Just attested => verifyAttestation attested
      Nothing => False
]

-- ============================================================================
-- Main Test Runner
-- ============================================================================

export
main : IO ()
main = do
  putStrLn "╔══════════════════════════════════════════════════════════╗"
  putStrLn "║            A2ML Idris2 Core Test Suite                  ║"
  putStrLn "╚══════════════════════════════════════════════════════════╝"

  testTypedCore
  testProofs
  testParser
  testIntegration

  putStrLn "\n╔══════════════════════════════════════════════════════════╗"
  putStrLn "║  ✅ Test Suite Complete                                 ║"
  putStrLn "╚══════════════════════════════════════════════════════════╝"
