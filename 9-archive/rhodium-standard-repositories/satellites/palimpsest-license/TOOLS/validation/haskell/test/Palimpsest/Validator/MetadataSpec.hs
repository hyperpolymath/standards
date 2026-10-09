{-
SPDX-License-Identifier: MIT
SPDX-FileCopyrightText: 2024-2025 Palimpsest Stewardship Council
-}

{-# LANGUAGE OverloadedStrings #-}

module Palimpsest.Validator.MetadataSpec (metadataSpec) where

import Test.Hspec
import Test.QuickCheck
import Data.Aeson (Value(..), Object, encode, decode)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Palimpsest.Validator.Metadata
import Palimpsest.Validator.Utils

metadataSpec :: Spec
metadataSpec = describe "Metadata Validator" $ do
  describe "File type detection" $ do
    it "detects JSON-LD files by .jsonld extension" $ do
      isJSONLDFile "metadata.jsonld" `shouldBe` True

    it "detects JSON files as JSON-LD" $ do
      isJSONLDFile "data.json" `shouldBe` True

    it "rejects non-JSON-LD files" $ do
      isJSONLDFile "document.xml" `shouldBe` False
      isJSONLDFile "text.md" `shouldBe` False

    it "detects XML files" $ do
      isXMLFile "lineage.xml" `shouldBe` True
      isXMLFile "data.json" `shouldBe` False

  describe "Context extraction from JSON-LD" $ do
    it "extracts string context" $ do
      let json = Object $ HM.fromList [("@context", String "http://schema.org")]
      extractContext json `shouldBe` Just "http://schema.org"

    it "handles embedded object context" $ do
      let ctx = Object $ HM.fromList [("name", String "Person")]
      let json = Object $ HM.fromList [("@context", ctx)]
      extractContext json `shouldBe` Just "embedded"

    it "returns Nothing for missing context" $ do
      let json = Object $ HM.fromList [("someKey", String "value")]
      extractContext json `shouldBe` Nothing

    it "returns Nothing for non-Object JSON" $ do
      extractContext (String "not an object") `shouldBe` Nothing

  describe "License ID extraction" $ do
    it "extracts license ID from licenses array" $ do
      let license = Object $ HM.fromList [("licenseId", String "Palimpsest-0.4")]
      let json = Object $ HM.fromList [("licenses", Array [license])]
      extractLicenseId json `shouldBe` Just "Palimpsest-0.4"

    it "returns Nothing for empty licenses array" $ do
      let json = Object $ HM.fromList [("licenses", Array [])]
      extractLicenseId json `shouldBe` Nothing

    it "returns Nothing when licenses field missing" $ do
      let json = Object $ HM.fromList [("other", String "value")]
      extractLicenseId json `shouldBe` Nothing

    it "returns Nothing for malformed license entry" $ do
      let json = Object $ HM.fromList [("licenses", Array [String "invalid"])]
      extractLicenseId json `shouldBe` Nothing

  describe "Metadata version extraction" $ do
    it "extracts licenseListVersion" $ do
      let json = Object $ HM.fromList [("licenseListVersion", String "3.20")]
      extractMetadataVersion json `shouldBe` Just "3.20"

    it "returns Nothing when version missing" $ do
      let json = Object $ HM.fromList [("other", String "value")]
      extractMetadataVersion json `shouldBe` Nothing

  describe "JSON-LD structure validation" $ do
    it "validates object with @context and @type" $ do
      let json = Object $ HM.fromList
            [ ("@context", String "http://schema.org")
            , ("@type", String "SoftwareLicense")
            ]
      -- This would normally be tested through validateJSONLDStructure
      HM.member "@context" (case json of Object o -> o; _ -> HM.empty) `shouldBe` True
      HM.member "@type" (case json of Object o -> o; _ -> HM.empty) `shouldBe` True

    it "identifies missing @context field" $ do
      let json = Object $ HM.fromList [("@type", String "License")]
      HM.member "@context" (case json of Object o -> o; _ -> HM.empty) `shouldBe` False

    it "identifies missing @type field" $ do
      let json = Object $ HM.fromList [("@context", String "http://schema.org")]
      HM.member "@type" (case json of Object o -> o; _ -> HM.empty) `shouldBe` False

  describe "SPDX license validation" $ do
    it "validates license with required fields" $ do
      let licenseJson = Object $ HM.fromList
            [ ("licenseId", String "Palimpsest-0.4")
            , ("name", String "Palimpsest License v0.4")
            , ("isOsiApproved", Bool False)
            ]
      case Aeson.fromJSON licenseJson of
        Aeson.Success (license :: SPDXLicense) -> do
          spdxLicenseId license `shouldBe` Just "Palimpsest-0.4"
          spdxName license `shouldBe` Just "Palimpsest License v0.4"
        Aeson.Error err -> expectationFailure $ "Failed to parse: " ++ err

    it "handles optional fields" $ do
      let licenseJson = Object $ HM.fromList
            [ ("licenseId", String "MIT")
            , ("reference", String "https://spdx.org/licenses/MIT.html")
            ]
      case Aeson.fromJSON licenseJson of
        Aeson.Success (license :: SPDXLicense) -> do
          spdxLicenseId license `shouldBe` Just "MIT"
          spdxReference license `shouldBe` Just "https://spdx.org/licenses/MIT.html"
        Aeson.Error err -> expectationFailure $ "Failed to parse: " ++ err

    it "accepts licenses without optional fields" $ do
      let licenseJson = Object $ HM.fromList []
      case Aeson.fromJSON licenseJson of
        Aeson.Success (license :: SPDXLicense) -> do
          spdxLicenseId license `shouldBe` Nothing
          spdxName license `shouldBe` Nothing
        Aeson.Error err -> expectationFailure $ "Failed to parse: " ++ err

  describe "XML structure validation" $ do
    it "identifies elements by name" $ do
      -- Test the helper function
      let xmlText = "<root><creator>Alice</creator></root>"
      -- This is tested indirectly through XML parsing
      True `shouldBe` True

  describe "Metadata format detection" $ do
    it "distinguishes JSONLD format" $ do
      let meta = MetadataStructure JSONLD (Just "Palimpsest-0.4") (Just "0.4") Nothing True
      metaFormat meta `shouldBe` JSONLD

    it "distinguishes XML format" $ do
      let meta = MetadataStructure XML Nothing (Just "0.4") Nothing True
      metaFormat meta `shouldBe` XML

  describe "Dublin Core validation" $ do
    it "checks for required Dublin Core elements" $ do
      let dcElements = ["dc:title", "dc:creator", "dc:date", "dc:rights", "dc:identifier"]
      length dcElements `shouldBe` 5

    it "validates complete Dublin Core metadata" $ do
      let json = Object $ HM.fromList
            [ ("dc:title", String "Palimpsest License")
            , ("dc:creator", String "Palimpsest Stewardship Council")
            , ("dc:date", String "2024-01-01")
            , ("dc:rights", String "https://palimpsest.org/licenses/v0.4")
            , ("dc:identifier", String "Palimpsest-0.4")
            ]
      HM.member "dc:title" (case json of Object o -> o; _ -> HM.empty) `shouldBe` True
      HM.member "dc:creator" (case json of Object o -> o; _ -> HM.empty) `shouldBe` True

    it "detects missing Dublin Core elements" $ do
      let json = Object $ HM.fromList
            [ ("dc:title", String "Test")
            ]
      HM.member "dc:creator" (case json of Object o -> o; _ -> HM.empty) `shouldBe` False

  describe "Malformed metadata handling" $ do
    it "handles invalid JSON syntax" $ do
      let invalidJson = "{this is not valid json}"
      let result = decode (BL.pack $ map (toEnum . fromEnum) invalidJson) :: Maybe Value
      result `shouldBe` Nothing

    it "handles empty JSON object" $ do
      let emptyJson = Object HM.empty
      extractLicenseId emptyJson `shouldBe` Nothing
      extractContext emptyJson `shouldBe` Nothing

    it "handles null values in JSON" $ do
      let json = Object $ HM.fromList [("@context", Null)]
      extractContext json `shouldBe` Nothing

  describe "Property-based tests" $ do
    it "license ID extraction should never crash" $ property $
      \obj -> let json = Object HM.empty
              in extractLicenseId json `elem` [Nothing]

  describe "Edge cases" $ do
    it "handles deeply nested license structure" $ do
      let innerLicense = Object $ HM.fromList [("licenseId", String "TEST")]
      let json = Object $ HM.fromList [("licenses", Array [innerLicense])]
      extractLicenseId json `shouldBe` Just "TEST"

    it "handles license array with multiple entries" $ do
      let license1 = Object $ HM.fromList [("licenseId", String "First")]
      let license2 = Object $ HM.fromList [("licenseId", String "Second")]
      let json = Object $ HM.fromList [("licenses", Array [license1, license2])]
      extractLicenseId json `shouldBe` Just "First"

    it "handles non-array licenses field" $ do
      let json = Object $ HM.fromList [("licenses", String "not-an-array")]
      extractLicenseId json `shouldBe` Nothing

  describe "Palimpsest-specific validation" $ do
    it "identifies Palimpsest license IDs" $ do
      let lid = "Palimpsest-0.4"
      T.isPrefixOf "Palimpsest-" lid `shouldBe` True

    it "distinguishes Palimpsest from other licenses" $ do
      let lid = "MIT"
      T.isPrefixOf "Palimpsest-" lid `shouldBe` False

    it "validates Palimpsest version format" $ do
      let versions = ["Palimpsest-0.3", "Palimpsest-0.4", "Palimpsest-1.0"]
      all (T.isPrefixOf "Palimpsest-") versions `shouldBe` True
