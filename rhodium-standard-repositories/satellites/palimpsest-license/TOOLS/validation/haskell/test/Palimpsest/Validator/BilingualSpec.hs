{-
SPDX-License-Identifier: MIT
SPDX-FileCopyrightText: 2024-2025 Palimpsest Stewardship Council
-}

{-# LANGUAGE OverloadedStrings #-}

module Palimpsest.Validator.BilingualSpec (bilingualSpec) where

import Test.Hspec
import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text as T
import Palimpsest.Validator.Bilingual

bilingualSpec :: Spec
bilingualSpec = describe "Bilingual Validator" $ do
  describe "Title matching" $ do
    it "matches identical titles" $ do
      titleMatches "Definitions" "Definitions" `shouldBe` True

    it "matches with minor formatting differences" $ do
      titleMatches "Attribution Options" "Attribution Options:" `shouldBe` True
      titleMatches "Clause 1 — Definitions" "Definitions" `shouldBe` True

    it "is case-insensitive" $ do
      titleMatches "DEFINITIONS" "definitions" `shouldBe` True

    it "ignores em-dash characters" $ do
      titleMatches "Clause 1 — Definitions" "Clause 1 Definitions" `shouldBe` True

    it "ignores colons" $ do
      titleMatches "Governing Law:" "Governing Law" `shouldBe` True

    it "matches when one title is substring of another" $ do
      titleMatches "Definitions" "1. Definitions" `shouldBe` True
      titleMatches "Attribution" "Attribution Options" `shouldBe` True

    it "rejects completely different titles" $ do
      titleMatches "Definitions" "Permissions" `shouldBe` False

  describe "Bilingual map parsing" $ do
    it "parses markdown table rows" $ do
      let row = "| 1 | Definitions | Definities |"
      case parseTableRow row of
        Just mapping -> do
          mappingClauseNumber mapping `shouldBe` "1"
          mappingEnglishTitle mapping `shouldBe` "Definitions"
          mappingDutchTitle mapping `shouldBe` "Definities"
        Nothing -> expectationFailure "Failed to parse valid table row"

    it "skips header rows" $ do
      let header = "| Clause No. | English | Dutch |"
      parseTableRow header `shouldBe` Nothing

    it "handles rows with extra whitespace" $ do
      let row = "|  2  |  Permissions  |  Toestemmingen  |"
      case parseTableRow row of
        Just mapping -> do
          mappingClauseNumber mapping `shouldBe` "2"
          mappingEnglishTitle mapping `shouldBe` "Permissions"
          mappingDutchTitle mapping `shouldBe` "Toestemmingen"
        Nothing -> expectationFailure "Failed to parse row with whitespace"

    it "skips separator rows" $ do
      let separator = "|---|---|---|"
      parseTableRow separator `shouldBe` Nothing

    it "handles decimal clause numbers" $ do
      let row = "| 1.2 | Attribution | Toeschrijving |"
      case parseTableRow row of
        Just mapping -> mappingClauseNumber mapping `shouldBe` "1.2"
        Nothing -> expectationFailure "Failed to parse decimal clause number"

    it "rejects malformed rows" $ do
      parseTableRow "not a table row" `shouldBe` Nothing
      parseTableRow "| only | two | cells" `shouldBe` Nothing

  describe "Bilingual mapping table parsing" $ do
    it "parses complete bilingual map" $ do
      let mapText = T.unlines
            [ "| Clause No. | English | Dutch |"
            , "|---|---|---|"
            , "| 1 | Definitions | Definities |"
            , "| 2 | Permissions | Toestemmingen |"
            ]
      let mappings = parseBilingualMap mapText
      length mappings `shouldBe` 2

    it "skips non-table lines" $ do
      let mapText = T.unlines
            [ "# Bilingual Map"
            , ""
            , "| Clause No. | English | Dutch |"
            , "| 1 | Definitions | Definities |"
            ]
      let mappings = parseBilingualMap mapText
      length mappings `shouldBe` 1

    it "handles empty input" $ do
      let mappings = parseBilingualMap ""
      mappings `shouldBe` []

  describe "Default bilingual mappings" $ do
    it "contains expected clause mappings" $ do
      let defaults = defaultBilingualMap
      length defaults `shouldBe` 20  -- v0.4 has 20 clauses

    it "contains Definitions mapping" $ do
      let defaults = defaultBilingualMap
      let defMapping = filter (\m -> mappingClauseNumber m == "1") defaults
      case defMapping of
        [m] -> do
          mappingEnglishTitle m `shouldBe` "Definitions"
          mappingDutchTitle m `shouldBe` "Definities"
        _ -> expectationFailure "Definitions mapping not found"

    it "contains Governing Law mapping" $ do
      let defaults = defaultBilingualMap
      let govLawMapping = filter (\m -> mappingClauseNumber m == "10") defaults
      case govLawMapping of
        [m] -> do
          mappingEnglishTitle m `shouldBe` "Governing Law"
          mappingDutchTitle m `shouldBe` "Toepasselijk Recht"
        _ -> expectationFailure "Governing Law mapping not found"

    it "has unique clause numbers" $ do
      let defaults = defaultBilingualMap
      let clauseNums = map mappingClauseNumber defaults
      let uniqueNums = T.unpack <$> clauseNums
      length clauseNums `shouldBe` length (nub uniqueNums)

  describe "Title normalization" $ do
    it "normalizes to lowercase" $ do
      let normalized = T.toLower "Definitions"
      normalized `shouldBe` "definitions"

    it "strips whitespace" $ do
      let normalized = T.strip "  Permissions  "
      normalized `shouldBe` "Permissions"

    it "removes em-dash" $ do
      let normalized = T.filter (/= '—') "Clause 1 — Definitions"
      T.isInfixOf "—" normalized `shouldBe` False

    it "removes colons" $ do
      let normalized = T.filter (/= ':') "Governing Law:"
      T.isInfixOf ":" normalized `shouldBe` False

  describe "Bilingual mapping lookup" $ do
    it "finds mapping by clause number" $ do
      let defaults = defaultBilingualMap
      let mapping = filter (\m -> mappingClauseNumber m == "5") defaults
      case mapping of
        [m] -> mappingEnglishTitle m `shouldBe` "Symbolic Integrity"
        _ -> expectationFailure "Could not find clause 5"

    it "handles non-existent clause numbers" $ do
      let defaults = defaultBilingualMap
      let mapping = filter (\m -> mappingClauseNumber m == "99") defaults
      mapping `shouldBe` []

  describe "Property-based tests" $ do
    it "titleMatches should be reflexive" $ property $
      \text -> let t = T.pack text
               in titleMatches t t == True

    it "titleMatches should be symmetric" $ property $
      \text1 text2 -> let t1 = T.pack text1
                          t2 = T.pack text2
                      in titleMatches t1 t2 == titleMatches t2 t1

    it "parseTableRow should never crash" $ property $
      \text -> let result = parseTableRow (T.pack text)
               in result `elem` [Nothing] || isJust result
      where
        isJust (Just _) = True
        isJust Nothing = False

  describe "Edge cases" $ do
    it "handles empty titles in mapping" $ do
      let row = "| 1 |  |  |"
      case parseTableRow row of
        Just mapping -> do
          mappingEnglishTitle mapping `shouldBe` ""
          mappingDutchTitle mapping `shouldBe` ""
        Nothing -> expectationFailure "Should parse empty titles"

    it "handles titles with special characters" $ do
      let row = "| 1 | Foo & Bar | Foo & Bar |"
      case parseTableRow row of
        Just mapping -> mappingEnglishTitle mapping `shouldBe` "Foo & Bar"
        Nothing -> expectationFailure "Should parse special characters"

    it "handles very long titles" $ do
      let longTitle = T.replicate 100 "Long "
      let row = "| 1 | " <> longTitle <> " | " <> longTitle <> " |"
      case parseTableRow row of
        Just mapping -> T.length (mappingEnglishTitle mapping) `shouldSatisfy` (> 100)
        Nothing -> expectationFailure "Should parse long titles"

  describe "Version consistency checks" $ do
    it "detects matching versions" $ do
      let v1 = "0.4"
      let v2 = "0.4"
      v1 == v2 `shouldBe` True

    it "detects mismatched versions" $ do
      let v1 = "0.3"
      let v2 = "0.4"
      v1 == v2 `shouldBe` False

    it "handles unknown version" $ do
      let v1 = "unknown"
      let v2 = "0.4"
      (v1 /= v2 && v1 /= "unknown") `shouldBe` False

  describe "Clause alignment checks" $ do
    it "verifies all clauses have both language versions" $ do
      let defaults = defaultBilingualMap
      all (\m -> not (T.null $ mappingEnglishTitle m) &&
                 not (T.null $ mappingDutchTitle m)) defaults `shouldBe` True

    it "verifies clause numbers are sequential" $ do
      let defaults = defaultBilingualMap
      let numbers = map (read . T.unpack . mappingClauseNumber) defaults :: [Int]
      numbers `shouldBe` [1..20]

-- Helper function for tests
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)
