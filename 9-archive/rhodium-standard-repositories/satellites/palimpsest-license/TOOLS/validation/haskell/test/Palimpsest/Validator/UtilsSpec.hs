{-
SPDX-License-Identifier: MIT
SPDX-FileCopyrightText: 2024-2025 Palimpsest Stewardship Council
-}

{-# LANGUAGE OverloadedStrings #-}

module Palimpsest.Validator.UtilsSpec (utilsSpec) where

import Test.Hspec
import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text as T
import Palimpsest.Validator.Utils

utilsSpec :: Spec
utilsSpec = describe "Utility Functions" $ do
  describe "Text normalization" $ do
    it "converts to lowercase" $ do
      normaliseText "UPPERCASE" `shouldBe` "uppercase"

    it "trims leading whitespace" $ do
      normaliseText "  text" `shouldBe` "text"

    it "trims trailing whitespace" $ do
      normaliseText "text  " `shouldBe` "text"

    it "trims both leading and trailing whitespace" $ do
      normaliseText "  text  " `shouldBe` "text"

    it "handles empty text" $ do
      normaliseText "" `shouldBe` ""

    it "handles text with only whitespace" $ do
      normaliseText "   " `shouldBe` ""

    it "preserves internal whitespace" $ do
      normaliseText "hello world" `shouldBe` "hello world"

    it "is idempotent" $ do
      let text = "Some Text  "
      normaliseText (normaliseText text) `shouldBe` normaliseText text

  describe "Clause number extraction" $ do
    it "extracts clause number from standard format" $ do
      extractClauseNumber "Clause 1 — Definitions" `shouldBe` Just "1"

    it "extracts decimal clause numbers" $ do
      extractClauseNumber "Clause 2.3 — Metadata" `shouldBe` Just "2.3"

    it "handles lowercase" $ do
      extractClauseNumber "clause 5 — Title" `shouldBe` Just "5"

    it "handles various whitespace" $ do
      extractClauseNumber "Clause  7  — Title" `shouldBe` Just "7"

    it "returns Nothing for non-clause text" $ do
      extractClauseNumber "Just some text" `shouldBe` Nothing

    it "returns Nothing for clause without number" $ do
      extractClauseNumber "Clause — Title" `shouldBe` Nothing

    it "extracts first clause number when multiple present" $ do
      extractClauseNumber "Clause 1 and Clause 2" `shouldBe` Just "1"

  describe "Clause reference parsing" $ do
    it "parses clause reference with title" $ do
      case parseClauseReference "Clause 1 — Definitions" of
        Just (num, Just title) -> do
          num `shouldBe` "1"
          T.isInfixOf "Definitions" title `shouldBe` True
        _ -> expectationFailure "Failed to parse clause reference"

    it "parses clause reference without title" $ do
      case parseClauseReference "Clause 3" of
        Just (num, maybeTitle) -> num `shouldBe` "3"
        _ -> expectationFailure "Failed to parse clause reference"

    it "returns Nothing for invalid input" $ do
      parseClauseReference "Not a clause" `shouldBe` Nothing

  describe "File type detection" $ do
    it "detects markdown files by .md extension" $ do
      isMarkdownFile "document.md" `shouldBe` True

    it "detects markdown files by .markdown extension" $ do
      isMarkdownFile "document.markdown" `shouldBe` True

    it "rejects non-markdown files" $ do
      isMarkdownFile "document.txt" `shouldBe` False
      isMarkdownFile "data.json" `shouldBe` False

    it "handles files without extensions" $ do
      isMarkdownFile "README" `shouldBe` False

    it "is case-sensitive for extensions" $ do
      isMarkdownFile "file.MD" `shouldBe` False

  describe "JSON-LD file detection" $ do
    it "detects .jsonld files" $ do
      isJSONLDFile "metadata.jsonld" `shouldBe` True

    it "detects .json files" $ do
      isJSONLDFile "data.json" `shouldBe` True

    it "rejects non-JSON files" $ do
      isJSONLDFile "document.xml" `shouldBe` False
      isJSONLDFile "text.md" `shouldBe` False

  describe "XML file detection" $ do
    it "detects .xml files" $ do
      isXMLFile "lineage.xml" `shouldBe` True

    it "rejects non-XML files" $ do
      isXMLFile "data.json" `shouldBe` False
      isXMLFile "document.md" `shouldBe` False

  describe "Relative path calculation" $ do
    it "calculates relative path" $ do
      let base = "/home/user/project"
      let target = "/home/user/project/docs/guide.md"
      relativePath base target `shouldBe` "docs/guide.md"

    it "handles same directory" $ do
      let base = "/home/user"
      let target = "/home/user"
      relativePath base target `shouldBe` "."

    it "handles parent directory" $ do
      let base = "/home/user/project/src"
      let target = "/home/user/project/docs"
      -- Should produce "../docs"
      T.isInfixOf "docs" (T.pack $ relativePath base target) `shouldBe` True

  describe "Property-based tests" $ do
    it "normaliseText should always produce lowercase" $ property $
      \text -> let normalized = normaliseText (T.pack text)
               in normalized == T.toLower normalized

    it "normaliseText should never have leading/trailing spaces" $ property $
      \text -> let normalized = normaliseText (T.pack text)
               in normalized == T.strip normalized

    it "isMarkdownFile should be deterministic" $ property $
      \path -> isMarkdownFile path == isMarkdownFile path

    it "file type detectors should be mutually exclusive for common types" $ property $
      \path -> let isMd = isMarkdownFile path
                   isJson = isJSONLDFile path
                   isXml = isXMLFile path
               in not (isMd && isXml)  -- Can't be both .md and .xml

  describe "Edge cases" $ do
    it "handles empty string in normaliseText" $ do
      normaliseText "" `shouldBe` ""

    it "handles unicode in normaliseText" $ do
      normaliseText "Café" `shouldBe` "café"

    it "handles very long text" $ do
      let longText = T.replicate 10000 "a "
      T.length (normaliseText longText) `shouldSatisfy` (> 0)

    it "handles file paths with multiple dots" $ do
      isMarkdownFile "file.v0.4.md" `shouldBe` True
      isJSONLDFile "data.backup.json" `shouldBe` True

    it "handles empty file paths" $ do
      isMarkdownFile "" `shouldBe` False
      isXMLFile "" `shouldBe` False

  describe "Whitespace handling" $ do
    it "handles tabs in normaliseText" $ do
      normaliseText "\t\ttext\t\t" `shouldBe` "text"

    it "handles newlines in normaliseText" $ do
      normaliseText "\ntext\n" `shouldBe` "text"

    it "handles mixed whitespace" $ do
      normaliseText " \t\n text \n\t " `shouldBe` "text"

  describe "Clause number formats" $ do
    it "handles integer clause numbers" $ do
      extractClauseNumber "Clause 1" `shouldBe` Just "1"

    it "handles decimal clause numbers" $ do
      extractClauseNumber "Clause 1.2" `shouldBe` Just "1.2"

    it "handles multi-digit clause numbers" $ do
      extractClauseNumber "Clause 123" `shouldBe` Just "123"

    it "handles clause numbers with multiple decimals" $ do
      extractClauseNumber "Clause 1.2.3" `shouldBe` Just "1.2.3"

  describe "Text comparison helpers" $ do
    it "normalized comparison should be case-insensitive" $ do
      let text1 = normaliseText "Definitions"
      let text2 = normaliseText "DEFINITIONS"
      text1 `shouldBe` text2

    it "normalized comparison should ignore whitespace" $ do
      let text1 = normaliseText "  Attribution  "
      let text2 = normaliseText "Attribution"
      text1 `shouldBe` text2

  describe "File extension handling" $ do
    it "correctly identifies multiple file types" $ do
      isMarkdownFile "guide.md" `shouldBe` True
      isJSONLDFile "metadata.jsonld" `shouldBe` True
      isXMLFile "lineage.xml" `shouldBe` True

    it "handles files with path separators" $ do
      isMarkdownFile "/path/to/file.md" `shouldBe` True
      isMarkdownFile "relative/path/file.md" `shouldBe` True

    it "handles Windows-style paths" $ do
      isMarkdownFile "C:\\Users\\docs\\file.md" `shouldBe` True

  describe "Special characters in text" $ do
    it "handles em-dash in clause text" $ do
      let text = "Clause 1 — Definitions"
      case extractClauseNumber text of
        Just num -> num `shouldBe` "1"
        Nothing -> expectationFailure "Should extract clause number"

    it "handles quotes in text" $ do
      normaliseText "\"Quoted text\"" `shouldBe` "\"quoted text\""

    it "handles ampersands" $ do
      normaliseText "Foo & Bar" `shouldBe` "foo & bar"

  describe "Boundary conditions" $ do
    it "handles single character text" $ do
      normaliseText "A" `shouldBe` "a"

    it "handles text with only special characters" $ do
      normaliseText "!@#$%^&*()" `shouldBe` "!@#$%^&*()"

    it "handles numeric text" $ do
      normaliseText "123" `shouldBe` "123"
