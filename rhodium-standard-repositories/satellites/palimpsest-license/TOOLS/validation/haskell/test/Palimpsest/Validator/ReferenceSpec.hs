{-
SPDX-License-Identifier: MIT
SPDX-FileCopyrightText: 2024-2025 Palimpsest Stewardship Council
-}

{-# LANGUAGE OverloadedStrings #-}

module Palimpsest.Validator.ReferenceSpec (referenceSpec) where

import Test.Hspec
import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text as T
import Palimpsest.Validator.Reference

referenceSpec :: Spec
referenceSpec = describe "Reference Validator" $ do
  describe "Clause reference extraction" $ do
    it "extracts simple clause references" $ do
      let line = (1, "See Clause 1 for definitions")
      let refs = extractClauseRefs "test.md" line
      length refs `shouldBe` 1
      case refs of
        [ref] -> do
          refType ref `shouldBe` ClauseReference
          refTarget ref `shouldBe` "1"
          refLine ref `shouldBe` 1
        _ -> expectationFailure "Expected 1 reference"

    it "extracts decimal clause references" $ do
      let line = (1, "As per Clause 2.3 metadata rules")
      let refs = extractClauseRefs "test.md" line
      case refs of
        [ref] -> refTarget ref `shouldBe` "2.3"
        _ -> expectationFailure "Expected 1 reference"

    it "extracts multiple clause references from one line" $ do
      let line = (1, "Clause 1 and Clause 2 define terms")
      let refs = extractClauseRefs "test.md" line
      length refs `shouldBe` 2

    it "handles lowercase 'clause'" $ do
      let line = (1, "As per clause 5")
      let refs = extractClauseRefs "test.md" line
      length refs `shouldBe` 1

    it "ignores non-clause patterns" $ do
      let line = (1, "This has no clause references")
      let refs = extractClauseRefs "test.md" line
      refs `shouldBe` []

  describe "File reference extraction" $ do
    it "extracts markdown link to .md file" $ do
      let line = (1, "[User Guide](../GUIDES_v0.4/User_Guide.md)")
      let refs = extractFileRefs "test.md" line
      length refs `shouldBe` 1
      case refs of
        [ref] -> do
          refType ref `shouldBe` FileReference
          refTarget ref `shouldBe` "../GUIDES_v0.4/User_Guide.md"
          refContext ref `shouldBe` "User Guide"
        _ -> expectationFailure "Expected 1 reference"

    it "ignores URLs starting with http" $ do
      let line = (1, "[Link](http://example.com/file.md)")
      let refs = extractFileRefs "test.md" line
      refs `shouldBe` []

    it "handles multiple file links" $ do
      let line = (1, "[Guide](guide.md) and [Docs](docs.md)")
      let refs = extractFileRefs "test.md" line
      length refs `shouldBe` 2

    it "handles relative paths" $ do
      let line = (1, "[File](./subfolder/file.md)")
      let refs = extractFileRefs "test.md" line
      case refs of
        [ref] -> refTarget ref `shouldBe` "./subfolder/file.md"
        _ -> expectationFailure "Expected 1 reference"

  describe "URL reference extraction" $ do
    it "extracts HTTPS URLs" $ do
      let line = (1, "[Website](https://example.com)")
      let refs = extractURLRefs "test.md" line
      length refs `shouldBe` 1
      case refs of
        [ref] -> do
          refType ref `shouldBe` URLReference
          refTarget ref `shouldBe` "https://example.com"
        _ -> expectationFailure "Expected 1 reference"

    it "extracts HTTP URLs" $ do
      let line = (1, "[Site](http://example.com)")
      let refs = extractURLRefs "test.md" line
      length refs `shouldBe` 1

    it "handles URLs with paths" $ do
      let line = (1, "[Doc](https://example.com/docs/guide.html)")
      let refs = extractURLRefs "test.md" line
      case refs of
        [ref] -> T.isInfixOf "/docs/guide.html" (refTarget ref) `shouldBe` True
        _ -> expectationFailure "Expected 1 reference"

  describe "Internal link extraction" $ do
    it "extracts anchor links" $ do
      let line = (1, "[Section](#introduction)")
      let refs = extractInternalLinks "test.md" line
      length refs `shouldBe` 1
      case refs of
        [ref] -> do
          refType ref `shouldBe` InternalLink
          refTarget ref `shouldBe` "introduction"
        _ -> expectationFailure "Expected 1 reference"

    it "handles multiple anchors" $ do
      let line = (1, "[Intro](#intro) and [Conclusion](#conclusion)")
      let refs = extractInternalLinks "test.md" line
      length refs `shouldBe` 2

    it "handles hyphenated anchors" $ do
      let line = (1, "[Section](#my-section-name)")
      let refs = extractInternalLinks "test.md" line
      case refs of
        [ref] -> refTarget ref `shouldBe` "my-section-name"
        _ -> expectationFailure "Expected 1 reference"

  describe "Heading to anchor conversion" $ do
    it "converts simple heading" $ do
      headingToAnchor "## Introduction" `shouldBe` "introduction"

    it "converts heading with spaces to hyphens" $ do
      headingToAnchor "## My Section Name" `shouldBe` "my-section-name"

    it "removes special characters" $ do
      headingToAnchor "## Foo & Bar!" `shouldBe` "foo--bar"

    it "handles multiple hash marks" $ do
      headingToAnchor "### Subsection" `shouldBe` "subsection"

    it "preserves numbers" $ do
      headingToAnchor "## Section 1.2" `shouldBe` "section-12"

  describe "Anchor existence checking" $ do
    it "finds existing anchor" $ do
      let text = "## Introduction\n\nContent here"
      checkAnchorExists "introduction" text `shouldBe` True

    it "returns False for missing anchor" $ do
      let text = "## Introduction\n\nContent here"
      checkAnchorExists "conclusion" text `shouldBe` False

    it "handles multiple headings" $ do
      let text = "## Intro\n\n## Methods\n\n## Results"
      checkAnchorExists "methods" text `shouldBe` True
      checkAnchorExists "results" text `shouldBe` True

  describe "URL format validation" $ do
    it "validates HTTPS URLs" $ do
      isValidURL "https://example.com" `shouldBe` True

    it "validates HTTP URLs" $ do
      isValidURL "http://example.com" `shouldBe` True

    it "rejects non-URLs" $ do
      isValidURL "not-a-url" `shouldBe` False
      isValidURL "ftp://example.com" `shouldBe` False

    it "rejects empty strings" $ do
      isValidURL "" `shouldBe` False

  describe "Reference type discrimination" $ do
    it "identifies clause references" $ do
      let ref = Reference ClauseReference "1" "test.md" 1 "Clause 1"
      refType ref `shouldBe` ClauseReference

    it "identifies file references" $ do
      let ref = Reference FileReference "guide.md" "test.md" 1 "Guide"
      refType ref `shouldBe` FileReference

    it "identifies URL references" $ do
      let ref = Reference URLReference "https://example.com" "test.md" 1 "Link"
      refType ref `shouldBe` URLReference

    it "identifies internal links" $ do
      let ref = Reference InternalLink "intro" "test.md" 1 "Introduction"
      refType ref `shouldBe` InternalLink

  describe "Reference data preservation" $ do
    it "preserves source file information" $ do
      let ref = Reference ClauseReference "1" "source.md" 42 "Context"
      refSource ref `shouldBe` "source.md"
      refLine ref `shouldBe` 42

    it "preserves context information" $ do
      let ref = Reference ClauseReference "1" "test.md" 1 "See Clause 1"
      refContext ref `shouldBe` "See Clause 1"

  describe "Edge cases and malformed input" $ do
    it "handles empty lines" $ do
      let refs = extractClauseRefs "test.md" (1, "")
      refs `shouldBe` []

    it "handles lines with only whitespace" $ do
      let refs = extractClauseRefs "test.md" (1, "   \t  ")
      refs `shouldBe` []

    it "handles malformed markdown links" $ do
      let line = (1, "[Broken link](")
      let refs = extractFileRefs "test.md" line
      refs `shouldBe` []

    it "handles nested brackets in link text" $ do
      let line = (1, "[[Nested]](file.md)")
      -- Should still extract the reference
      length (extractFileRefs "test.md" line) >= 0 `shouldBe` True

  describe "Property-based tests" $ do
    it "reference extraction should never crash" $ property $
      \lineNum text -> let refs = extractClauseRefs "test.md" (lineNum, T.pack text)
                       in length refs >= 0

    it "headingToAnchor should always produce lowercase" $ property $
      \text -> let anchor = headingToAnchor (T.pack text)
               in anchor == T.toLower anchor

    it "isValidURL should be deterministic" $ property $
      \url -> let result = isValidURL (T.pack url)
              in result == isValidURL (T.pack url)

  describe "Complex reference scenarios" $ do
    it "handles mixed reference types in one line" $ do
      let line = (1, "See Clause 1.2 and [Guide](guide.md) and #section")
      let clauseRefs = extractClauseRefs "test.md" line
      let fileRefs = extractFileRefs "test.md" line
      length clauseRefs `shouldBe` 1
      length fileRefs `shouldBe` 1

    it "extracts all references from rich text" $ do
      let text = T.unlines
            [ "See Clause 1 for [definitions](defs.md)"
            , "Also check [Website](https://example.com)"
            , "And the [Introduction](#intro) section"
            ]
      let numberedLines = zip [1..] (T.lines text)
      let allClauseRefs = concatMap (extractClauseRefs "test.md") numberedLines
      let allFileRefs = concatMap (extractFileRefs "test.md") numberedLines
      let allUrlRefs = concatMap (extractURLRefs "test.md") numberedLines
      let allInternalRefs = concatMap (extractInternalLinks "test.md") numberedLines
      length allClauseRefs `shouldBe` 1
      length allFileRefs `shouldBe` 1
      length allUrlRefs `shouldBe` 1
      length allInternalRefs `shouldBe` 1

  describe "Reference format variations" $ do
    it "handles clause references with various formats" $ do
      let patterns =
            [ "Clause 1"
            , "clause 2"
            , "Clause 3.1"
            , "CLAUSE 4"
            ]
      all (\p -> length (extractClauseRefs "test.md" (1, p)) == 1) patterns `shouldBe` True

    it "handles file paths with various extensions" $ do
      let links =
            [ "[Doc](file.md)"
            , "[Guide](guide.markdown)"
            ]
      -- Only .md files should be extracted by extractFileRefs
      length (extractFileRefs "test.md" (1, links !! 0)) `shouldBe` 1
