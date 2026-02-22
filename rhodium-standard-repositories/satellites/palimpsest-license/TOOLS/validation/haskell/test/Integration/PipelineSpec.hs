{-
SPDX-License-Identifier: MIT
SPDX-FileCopyrightText: 2024-2025 Palimpsest Stewardship Council
-}

{-# LANGUAGE OverloadedStrings #-}

module Integration.PipelineSpec (pipelineSpec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Palimpsest.Validator.Types
import Palimpsest.Validator.License
import Palimpsest.Validator.Metadata
import Palimpsest.Validator.Bilingual
import Palimpsest.Validator.Reference
import Palimpsest.Validator.Utils

pipelineSpec :: Spec
pipelineSpec = describe "Integration: Full Validation Pipeline" $ do
  describe "End-to-end license validation" $ do
    it "validates a complete license structure" $ do
      let licenseText = T.unlines
            [ "# Palimpsest License v0.4"
            , ""
            , "## Clause 1 — Definitions"
            , ""
            , "**Non-Interpretive (NI) Systems**: AI systems that process"
            , "content without generating interpretive outputs."
            , ""
            , "## Clause 2 — Permissions"
            , ""
            , "You may copy and distribute the work."
            , ""
            , "## Clause 3 — Conditions"
            , ""
            , "Attribution must be preserved per Clause 2.3."
            , ""
            , "Governing Law: Netherlands"
            ]
      let structure = parseLicenseStructure licenseText
      licenseVersion structure `shouldBe` "0.4"
      licenseHasHeader structure `shouldBe` True
      length (licenseClauses structure) `shouldBe` 3

    it "extracts and validates all components" $ do
      let licenseText = T.unlines
            [ "# Palimpsest License v0.4"
            , "## Clause 1 — Definitions"
            , "Content"
            ]
      let structure = parseLicenseStructure licenseText
      -- Verify structure is valid
      licenseVersion structure `shouldBe` "0.4"
      licenseClauses structure `shouldSatisfy` (not . null)
      licenseHasHeader structure `shouldBe` True

  describe "Metadata extraction and parsing pipeline" $ do
    it "validates JSON-LD metadata structure" $ do
      -- Test that metadata can be constructed and validated
      let meta = MetadataStructure JSONLD (Just "Palimpsest-0.4") (Just "0.4") (Just "http://schema.org") True
      metaFormat meta `shouldBe` JSONLD
      metaLicenseId meta `shouldBe` Just "Palimpsest-0.4"
      metaValid meta `shouldBe` True

    it "validates metadata format detection" $ do
      isJSONLDFile "metadata.jsonld" `shouldBe` True
      isXMLFile "lineage.xml" `shouldBe` True

  describe "Bilingual consistency workflow" $ do
    it "parses and validates bilingual mappings" $ do
      let mapText = T.unlines
            [ "| Clause No. | English | Dutch |"
            , "|---|---|---|"
            , "| 1 | Definitions | Definities |"
            , "| 2 | Permissions | Toestemmingen |"
            ]
      let mappings = parseBilingualMap mapText
      length mappings `shouldBe` 2

    it "validates title matching across languages" $ do
      titleMatches "Definitions" "Definities" `shouldBe` False  -- Different words
      titleMatches "Definitions" "DEFINITIONS" `shouldBe` True  -- Same word, different case

  describe "Reference validation workflow" $ do
    it "extracts and categorizes all reference types" $ do
      let text = T.unlines
            [ "See Clause 1 for definitions."
            , "Check [User Guide](guide.md) for details."
            , "Visit [Website](https://example.com)."
            , "Jump to [Introduction](#intro)."
            ]
      let numberedLines = zip [1..] (T.lines text)

      let clauseRefs = concatMap (extractClauseRefs "test.md") numberedLines
      let fileRefs = concatMap (extractFileRefs "test.md") numberedLines
      let urlRefs = concatMap (extractURLRefs "test.md") numberedLines
      let internalRefs = concatMap (extractInternalLinks "test.md") numberedLines

      length clauseRefs `shouldBe` 1
      length fileRefs `shouldBe` 1
      length urlRefs `shouldBe` 1
      length internalRefs `shouldBe` 1

    it "validates anchor existence in document" $ do
      let text = "## Introduction\n\nContent\n\n## Methods"
      checkAnchorExists "introduction" text `shouldBe` True
      checkAnchorExists "methods" text `shouldBe` True
      checkAnchorExists "conclusion" text `shouldBe` False

  describe "Multi-component validation" $ do
    it "validates license with clauses and references" $ do
      let licenseText = T.unlines
            [ "# Palimpsest License v0.4"
            , ""
            , "## Clause 1 — Definitions"
            , ""
            , "See Clause 2.3 for metadata requirements."
            , ""
            , "## Clause 2 — Permissions"
            , ""
            , "Refer to Clause 1 for term definitions."
            ]

      let structure = parseLicenseStructure licenseText
      let numberedLines = zip [1..] (T.lines licenseText)
      let clauseRefs = concatMap (extractClauseRefs "test.md") numberedLines

      length (licenseClauses structure) `shouldBe` 2
      length clauseRefs `shouldBe` 2

  describe "Error handling and recovery" $ do
    it "handles malformed license gracefully" $ do
      let malformed = "Not a valid license"
      let structure = parseLicenseStructure malformed
      licenseVersion structure `shouldBe` "unknown"
      licenseClauses structure `shouldBe` []

    it "handles empty input" $ do
      let structure = parseLicenseStructure ""
      licenseVersion structure `shouldBe` "unknown"
      licenseClauses structure `shouldBe` []

  describe "Real-world scenarios" $ do
    it "validates a minimal valid license" $ do
      let minimal = T.unlines
            [ "# Palimpsest License v0.4"
            , "## Clause 1 — Definitions"
            , "Content here."
            ]
      let structure = parseLicenseStructure minimal
      licenseVersion structure `shouldBe` "0.4"
      licenseHasHeader structure `shouldBe` True
      length (licenseClauses structure) `shouldBe` 1

    it "validates a complex multi-clause license" $ do
      let complex = T.unlines
            [ "# Palimpsest License v0.4"
            , ""
            , "## Clause 1 — Definitions"
            , "Definitions content"
            , ""
            , "## Clause 2 — Permissions"
            , "Permissions content"
            , ""
            , "## Clause 3 — Conditions"
            , "Conditions content"
            , ""
            , "## Clause 4 — Attribution"
            , "Attribution content"
            , ""
            , "Governing Law: Netherlands"
            ]
      let structure = parseLicenseStructure complex
      length (licenseClauses structure) `shouldBe` 4
      licenseGoverningLaw structure `shouldBe` Just "Netherlands"

  describe "Cross-validation scenarios" $ do
    it "validates clause references are valid" $ do
      let licenseText = T.unlines
            [ "## Clause 1 — First"
            , "## Clause 2 — Second"
            , "## Clause 3 — Third"
            ]
      let structure = parseLicenseStructure licenseText
      let clauseNums = map clauseNum (licenseClauses structure)

      -- Check that all clause numbers are present
      "1" `elem` clauseNums `shouldBe` True
      "2" `elem` clauseNums `shouldBe` True
      "3" `elem` clauseNums `shouldBe` True

    it "validates no duplicate clause numbers" $ do
      let licenseText = T.unlines
            [ "## Clause 1 — First"
            , "## Clause 2 — Second"
            , "## Clause 3 — Third"
            ]
      let structure = parseLicenseStructure licenseText
      let clauseNums = map clauseNum (licenseClauses structure)
      let duplicates = findDuplicates clauseNums
      duplicates `shouldBe` []

  describe "Utility function integration" $ do
    it "normalizes and compares text correctly" $ do
      let text1 = normaliseText "  Definitions  "
      let text2 = normaliseText "DEFINITIONS"
      text1 `shouldBe` text2

    it "extracts clause numbers for validation" $ do
      let clauses = ["Clause 1", "Clause 2", "Clause 3"]
      let extracted = mapMaybe extractClauseNumber clauses
      extracted `shouldBe` ["1", "2", "3"]

  describe "Performance and scalability" $ do
    it "handles large license documents" $ do
      let largeLicense = T.unlines $
            ["# Palimpsest License v0.4"] ++
            concatMap (\n -> ["## Clause " <> T.pack (show n) <> " — Section", "Content"]) [1..100]
      let structure = parseLicenseStructure largeLicense
      length (licenseClauses structure) `shouldBe` 100

    it "handles documents with many references" $ do
      let manyRefs = T.unlines $ map (\n -> "See Clause " <> T.pack (show n)) [1..100]
      let numberedLines = zip [1..] (T.lines manyRefs)
      let refs = concatMap (extractClauseRefs "test.md") numberedLines
      length refs `shouldBe` 100

-- Helper function
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = foldr (\x acc -> case f x of Just y -> y : acc; Nothing -> acc) []
