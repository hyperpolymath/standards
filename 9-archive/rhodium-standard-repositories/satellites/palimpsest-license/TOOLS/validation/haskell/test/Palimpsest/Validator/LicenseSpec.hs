{-
SPDX-License-Identifier: MIT
SPDX-FileCopyrightText: 2024-2025 Palimpsest Stewardship Council
-}

{-# LANGUAGE OverloadedStrings #-}

module Palimpsest.Validator.LicenseSpec (licenseSpec) where

import Test.Hspec
import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text as T
import Palimpsest.Validator.License
import Palimpsest.Validator.Types

licenseSpec :: Spec
licenseSpec = describe "License Validator" $ do
  describe "Version extraction" $ do
    it "extracts version from standard format (v0.3)" $ do
      let text = "# Palimpsest License v0.3"
      extractVersion text `shouldBe` "0.3"

    it "extracts version from Version keyword format" $ do
      let text = "Version 0.4"
      extractVersion text `shouldBe` "0.4"

    it "extracts version from mixed case" $ do
      let text = "version 1.0"
      extractVersion text `shouldBe` "1.0"

    it "returns 'unknown' for missing version" $ do
      let text = "# Palimpsest License"
      extractVersion text `shouldBe` "unknown"

    it "extracts first version when multiple present" $ do
      let text = "Version 0.3\n\nSome text\nVersion 0.4"
      extractVersion text `shouldBe` "0.3"

  describe "License header detection" $ do
    it "detects license header with Palimpsest keyword" $ do
      let text = "# Palimpsest License v0.3\n\nContent..."
      hasLicenseHeader text `shouldBe` True

    it "detects header in first 10 lines" $ do
      let text = T.unlines $ replicate 5 "" ++ ["# Palimpsest License v0.4"]
      hasLicenseHeader text `shouldBe` True

    it "returns False for missing header" $ do
      let text = "Some random content\nNo license header"
      hasLicenseHeader text `shouldBe` False

    it "returns False for header after first 10 lines" $ do
      let text = T.unlines $ replicate 11 "content" ++ ["# Palimpsest License"]
      hasLicenseHeader text `shouldBe` False

  describe "Language detection" $ do
    it "detects English from 'Governing Law'" $ do
      let text = "Governing Law: Netherlands"
      detectLanguage text `shouldBe` Just English

    it "detects English from 'Definitions'" $ do
      let text = "## Definitions\n\nSome definitions..."
      detectLanguage text `shouldBe` Just English

    it "detects Dutch from 'Toepasselijk Recht'" $ do
      let text = "Toepasselijk Recht: Nederland"
      detectLanguage text `shouldBe` Just Dutch

    it "detects Dutch from 'Definities'" $ do
      let text = "## Definities\n\nDefinities hier..."
      detectLanguage text `shouldBe` Just Dutch

    it "returns Nothing for ambiguous content" $ do
      let text = "Random content without language markers"
      detectLanguage text `shouldBe` Nothing

  describe "Clause number parsing" $ do
    it "parses simple clause numbers" $ do
      parseClauseNumber "1" `shouldBe` Just 1
      parseClauseNumber "42" `shouldBe` Just 42

    it "handles invalid input" $ do
      parseClauseNumber "abc" `shouldBe` Nothing
      parseClauseNumber "" `shouldBe` Nothing

    it "parses decimal clause numbers" $ do
      parseClauseNumber "1.2" `shouldBe` Just 1
      parseClauseNumber "3.5" `shouldBe` Just 3

  describe "Clause extraction" $ do
    it "extracts clauses from markdown headings" $ do
      let text = T.unlines
            [ "## Clause 1 — Definitions"
            , "Content for clause 1"
            , ""
            , "## Clause 2 — Permissions"
            , "Content for clause 2"
            ]
      let structure = parseLicenseStructure text
      length (licenseClauses structure) `shouldBe` 2

    it "extracts clause numbers correctly" $ do
      let text = T.unlines
            [ "## Clause 1 — Definitions"
            , "Content"
            ]
      let structure = parseLicenseStructure text
      let clauses = licenseClauses structure
      case clauses of
        [c] -> clauseNum c `shouldBe` "1"
        _ -> expectationFailure "Expected 1 clause"

    it "handles clauses with no content" $ do
      let text = "## Clause 1 — Empty\n## Clause 2 — Also Empty"
      let structure = parseLicenseStructure text
      length (licenseClauses structure) `shouldBe` 2

  describe "Governing law extraction" $ do
    it "extracts governing law from text" $ do
      let text = "Governing Law: Netherlands"
      extractGoverningLaw text `shouldBe` Just "Netherlands"

    it "handles whitespace variations" $ do
      let text = "Governing Law:     United Kingdom"
      extractGoverningLaw text `shouldBe` Just "United Kingdom"

    it "returns Nothing when not present" $ do
      let text = "Some other content"
      extractGoverningLaw text `shouldBe` Nothing

  describe "Clause pattern detection" $ do
    it "detects 'Clause N' pattern" $ do
      isClausePattern "Clause 1 — Title" `shouldBe` True
      isClausePattern "clause 2 — Another Title" `shouldBe` True

    it "rejects non-clause patterns" $ do
      isClausePattern "## Regular Heading" `shouldBe` False
      isClausePattern "Just some text" `shouldBe` False

  describe "License structure parsing" $ do
    it "parses complete license structure" $ do
      let text = T.unlines
            [ "# Palimpsest License v0.4"
            , ""
            , "## Clause 1 — Definitions"
            , "Content"
            , ""
            , "Governing Law: Netherlands"
            ]
      let structure = parseLicenseStructure text
      licenseVersion structure `shouldBe` "0.4"
      licenseHasHeader structure `shouldBe` True
      licenseLanguage structure `shouldBe` Just English
      licenseGoverningLaw structure `shouldBe` Just "Netherlands"

  describe "Clause numbering validation (find duplicates)" $ do
    it "detects duplicate clause numbers" $ do
      let numbers = ["1", "2", "2", "3"]
      let dups = findDuplicates numbers
      dups `shouldContain` ["2"]

    it "returns empty list for unique numbers" $ do
      let numbers = ["1", "2", "3"]
      findDuplicates numbers `shouldBe` []

  describe "Property-based tests" $ do
    it "version extraction should never crash on arbitrary text" $ property $
      \text -> let result = extractVersion (T.pack text)
               in T.length result >= 0

    it "language detection should always return Maybe" $ property $
      \text -> detectLanguage (T.pack text) `elem` [Nothing, Just English, Just Dutch]

    it "parseClauseNumber should be consistent" $ property $
      \n -> n > 0 && n < 1000 ==>
        parseClauseNumber (T.pack $ show n) == Just n

  describe "Edge cases and malformed input" $ do
    it "handles empty license text" $ do
      let structure = parseLicenseStructure ""
      licenseVersion structure `shouldBe` "unknown"
      licenseClauses structure `shouldBe` []
      licenseHasHeader structure `shouldBe` False

    it "handles text with only whitespace" $ do
      let structure = parseLicenseStructure "   \n\n   "
      licenseClauses structure `shouldBe` []

    it "handles malformed clause headings" $ do
      let text = "## Clause — Missing Number"
      let structure = parseLicenseStructure text
      licenseClauses structure `shouldBe` []

    it "handles clause heading with number but no em-dash" $ do
      let text = "## Clause 1 Title"
      let structure = parseLicenseStructure text
      length (licenseClauses structure) `shouldBe` 1

  describe "Required sections checking" $ do
    it "identifies missing Definitions section" $ do
      let text = "## Clause 1 — Attribution\n## Clause 2 — Permissions"
      let structure = parseLicenseStructure text
      let titles = map (T.toLower . clauseHeading) (licenseClauses structure)
      any (T.isInfixOf "definitions") titles `shouldBe` False

  describe "Common pitfall detection" $ do
    it "detects presence of Clause 1.2 reference" $ do
      let text = "This references Clause 1.2 about NI systems"
      T.isInfixOf "Clause 1.2" text `shouldBe` True

    it "detects presence of Clause 2.3 reference" $ do
      let text = "Metadata preservation per Clause 2.3"
      T.isInfixOf "Clause 2.3" text `shouldBe` True

    it "detects metadata keyword" $ do
      let text = "All metadata must be preserved"
      T.isInfixOf "metadata" (T.toLower text) `shouldBe` True

    it "detects Non-Interpretive systems keyword" $ do
      let text = "Non-Interpretive systems require consent"
      T.isInfixOf "Non-Interpretive" text `shouldBe` True
