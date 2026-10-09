{-
SPDX-License-Identifier: MIT
SPDX-FileCopyrightText: 2024-2025 Palimpsest Stewardship Council
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Metadata schema validator for JSON-LD, XML, and Dublin Core
module Palimpsest.Validator.Metadata
  ( validateMetadataFile
  , validateJSONLD
  , validateXML
  , validateDublinCore
  , MetadataStructure(..)
  ) where

import Control.Monad (when, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Aeson (Value(..), Object, FromJSON, parseJSON, withObject, (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (isNothing, isJust, fromMaybe)
import qualified Data.HashMap.Strict as HM
import GHC.Generics (Generic)
import Text.XML (Document, parseText_, def, Element(..), Node(..), Name(..))
import qualified Text.XML as XML
import Palimpsest.Validator.Types
import Palimpsest.Validator.Utils

-- | Metadata structure
data MetadataStructure = MetadataStructure
  { metaFormat     :: MetadataFormat
  , metaLicenseId  :: Maybe Text
  , metaVersion    :: Maybe Text
  , metaContext    :: Maybe Text
  , metaValid      :: Bool
  } deriving (Show, Eq, Generic)

-- | SPDX License structure
data SPDXLicense = SPDXLicense
  { spdxLicenseId      :: Maybe Text
  , spdxName           :: Maybe Text
  , spdxIsOsiApproved  :: Maybe Bool
  , spdxIsFsfLibre     :: Maybe Bool
  , spdxDetailsUrl     :: Maybe Text
  , spdxReference      :: Maybe Text
  , spdxSeeAlso        :: Maybe [Text]
  } deriving (Show, Eq, Generic)

instance FromJSON SPDXLicense where
  parseJSON = withObject "SPDXLicense" $ \v -> SPDXLicense
    <$> v .:? "licenseId"
    <*> v .:? "name"
    <*> v .:? "isOsiApproved"
    <*> v .:? "isFsfLibre"
    <*> v .:? "detailsUrl"
    <*> v .:? "reference"
    <*> v .:? "seeAlso"

-- | Validate metadata file based on extension
validateMetadataFile :: FilePath -> ValidatorM MetadataStructure
validateMetadataFile path = do
  config <- ask
  when (configVerbose config) $
    liftIO $ putStrLn $ "Validating metadata file: " ++ path

  content <- liftIO $ BL.readFile path

  if isJSONLDFile path
    then validateJSONLD path content
    else if isXMLFile path
      then validateXML path content
      else validationError Error (T.pack path)
             "Unknown metadata format (expected .jsonld, .json, or .xml)" Nothing

-- | Validate JSON-LD metadata
validateJSONLD :: FilePath -> BL.ByteString -> ValidatorM MetadataStructure
validateJSONLD path content = do
  case Aeson.decode content :: Maybe Value of
    Nothing -> validationError Critical (T.pack path)
                 "Invalid JSON syntax in metadata file" Nothing
    Just json -> do
      -- Check @context exists
      let context = extractContext json
      when (isNothing context) $
        validationWarning Warning (T.pack path)
          "JSON-LD @context not found"

      -- Validate structure
      validateJSONLDStructure path json

      -- Extract license information
      let licenseId = extractLicenseId json
          version = extractMetadataVersion json

      pure $ MetadataStructure
        { metaFormat = JSONLD
        , metaLicenseId = licenseId
        , metaVersion = version
        , metaContext = context
        , metaValid = True
        }

-- | Validate JSON-LD structure
validateJSONLDStructure :: FilePath -> Value -> ValidatorM ()
validateJSONLDStructure path (Object obj) = do
  -- Check for required SPDX fields
  let hasContext = HM.member "@context" obj
      hasType = HM.member "@type" obj

  unless hasContext $
    validationWarning Warning (T.pack path)
      "@context field missing (required for valid JSON-LD)"

  unless hasType $
    validationWarning Warning (T.pack path)
      "@type field missing"

  -- Validate SPDX license structure if present
  case HM.lookup "licenses" obj of
    Just (Array licenses) -> do
      when (null licenses) $
        validationWarning Warning (T.pack path)
          "Empty licenses array"
      -- Validate each license entry
      mapM_ (validateSPDXLicense path) licenses
    Just _ ->
      validationError Error (T.pack path)
        "licenses field must be an array" Nothing
    Nothing ->
      validationWarning Info (T.pack path)
        "No licenses array found in metadata"

validateJSONLDStructure _ _ =
  pure () -- Non-object values are handled elsewhere

-- | Validate SPDX license entry
validateSPDXLicense :: FilePath -> Value -> ValidatorM ()
validateSPDXLicense path val = do
  case Aeson.fromJSON val :: Aeson.Result SPDXLicense of
    Aeson.Error err ->
      validationWarning Warning (T.pack path)
        ("Invalid SPDX license structure: " <> T.pack err)
    Aeson.Success license -> do
      -- Check required fields
      when (isNothing $ spdxLicenseId license) $
        validationWarning Warning (T.pack path)
          "SPDX license entry missing licenseId"

      when (isNothing $ spdxName license) $
        validationWarning Warning (T.pack path)
          "SPDX license entry missing name"

      -- Validate Palimpsest-specific requirements
      case spdxLicenseId license of
        Just lid | T.isPrefixOf "Palimpsest-" lid -> do
          -- Check version matches
          when (isNothing $ spdxReference license) $
            validationWarning Warning (T.pack path)
              "Palimpsest license missing reference field"
        _ -> pure ()

-- | Extract @context from JSON-LD
extractContext :: Value -> Maybe Text
extractContext (Object obj) =
  case HM.lookup "@context" obj of
    Just (String ctx) -> Just ctx
    Just (Object _) -> Just "embedded"
    _ -> Nothing
extractContext _ = Nothing

-- | Extract license ID from metadata
extractLicenseId :: Value -> Maybe Text
extractLicenseId (Object obj) =
  case HM.lookup "licenses" obj of
    Just (Array licenses) ->
      case licenses !!? 0 of
        Just (Object licObj) ->
          case HM.lookup "licenseId" licObj of
            Just (String lid) -> Just lid
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing
  where
    xs !!? n = if n >= 0 && n < length xs then Just (xs !! n) else Nothing
extractLicenseId _ = Nothing

-- | Extract version from metadata
extractMetadataVersion :: Value -> Maybe Text
extractMetadataVersion (Object obj) =
  case HM.lookup "licenseListVersion" obj of
    Just (String v) -> Just v
    _ -> Nothing
extractMetadataVersion _ = Nothing

-- | Validate XML metadata
validateXML :: FilePath -> BL.ByteString -> ValidatorM MetadataStructure
validateXML path content = do
  let xmlText = TE.decodeUtf8 $ BL.toStrict content

  -- Parse XML
  let doc = parseText_ def xmlText

  -- Validate structure
  validateXMLStructure path doc

  -- Extract information
  let root = XML.documentRoot doc
      version = extractXMLVersion root

  pure $ MetadataStructure
    { metaFormat = XML
    , metaLicenseId = Nothing  -- Could extract from XML if needed
    , metaVersion = version
    , metaContext = Nothing
    , metaValid = True
    }

-- | Validate XML structure
validateXMLStructure :: FilePath -> Document -> ValidatorM ()
validateXMLStructure path doc = do
  let root = XML.documentRoot doc
      rootName = nameLocalName $ elementName root

  -- Check for lineage tag structure (from lineage_tag_example.xml)
  when (rootName == "lineageTag") $ do
    let children = elementNodes root
        hasCreator = any (isElementWithName "creator") children
        hasWorkId = any (isElementWithName "workId") children
        hasTimestamp = any (isElementWithName "timestamp") children

    unless hasCreator $
      validationWarning Warning (T.pack path)
        "Lineage tag missing creator element"

    unless hasWorkId $
      validationWarning Warning (T.pack path)
        "Lineage tag missing workId element"

    unless hasTimestamp $
      validationWarning Warning (T.pack path)
        "Lineage tag missing timestamp element"

-- | Check if node is element with given name
isElementWithName :: Text -> Node -> Bool
isElementWithName name (NodeElement el) =
  nameLocalName (elementName el) == name
isElementWithName _ _ = False

-- | Extract version from XML
extractXMLVersion :: Element -> Maybe Text
extractXMLVersion root =
  case findElementByName "version" root of
    Just el -> Just $ T.concat $ elementText el
    Nothing -> Nothing

-- | Find element by name in tree
findElementByName :: Text -> Element -> Maybe Element
findElementByName name el
  | nameLocalName (elementName el) == name = Just el
  | otherwise = findInChildren (elementNodes el)
  where
    findInChildren [] = Nothing
    findInChildren (NodeElement child : rest) =
      case findElementByName name child of
        Just found -> Just found
        Nothing -> findInChildren rest
    findInChildren (_ : rest) = findInChildren rest

-- | Get all text content from element
elementText :: Element -> [Text]
elementText el = concatMap nodeText (elementNodes el)
  where
    nodeText (NodeContent txt) = [txt]
    nodeText (NodeElement child) = elementText child
    nodeText _ = []

-- | Validate Dublin Core metadata
validateDublinCore :: FilePath -> Value -> ValidatorM ()
validateDublinCore path (Object obj) = do
  -- Check for required Dublin Core elements
  let dcElements =
        [ "dc:title"
        , "dc:creator"
        , "dc:date"
        , "dc:rights"
        , "dc:identifier"
        ]

  mapM_ (\elem ->
    unless (HM.member elem obj) $
      validationWarning Warning (T.pack path)
        ("Missing Dublin Core element: " <> elem)
    ) dcElements

validateDublinCore _ _ = pure ()
