-- SPDX-License-Identifier: MPL-2.0
-- (PMPL-1.0-or-later preferred; MPL-2.0 required for Hackage OSI-approved policy)
--
-- Data.K9.Renderer — Render K9 AST back to K9 surface syntax.
--
-- Converts the typed AST from Data.K9.Types into the YAML-like .k9 format,
-- including the K9! magic number, pedigree, security, target, and recipes.

module Data.K9.Renderer
  ( -- * Rendering
    renderK9
  , renderComponent
  , renderSecurityLevel
  ) where

import           Data.K9.Types

import qualified Data.Map.Strict as Map
import           Data.Text       (Text)
import qualified Data.Text       as T

-- | Render a complete K9 'Component' to the .k9 YAML-like format.
--
-- ==== Examples
--
-- >>> renderK9 component
-- "K9!\n---\nmetadata:\n  name: hello-k9\n  ..."
renderK9 :: Component -> Text
renderK9 = renderComponent

-- | Render a K9 component to text.
renderComponent :: Component -> Text
renderComponent c = T.intercalate "\n" $ filter (not . T.null) $
  [ "K9!"
  , "---"
  , "metadata:"
  , "  name: " <> pedigreeName (componentPedigree c)
  , "  version: " <> pedigreeVersion (componentPedigree c)
  , "  description: " <> pedigreeDescription (componentPedigree c)
  ]
  ++ renderOptional "  author" (pedigreeAuthor (componentPedigree c))
  ++ renderOptional "  license" (pedigreeLicense (componentPedigree c))
  ++
  [ ""
  , "security:"
  , "  trust_level: " <> renderSecurityLevel (securityLevel (componentSecurity c))
  , "  allow_network: " <> renderBool (securityAllowNetwork (componentSecurity c))
  , "  allow_filesystem_write: " <> renderBool (securityAllowFsWrite (componentSecurity c))
  , "  allow_subprocess: " <> renderBool (securityAllowSubprocess (componentSecurity c))
  ]
  ++ renderTargetSection (componentTarget c)
  ++ renderRecipesSection (componentRecipes c)
  ++ renderValidationSection (componentValidation c)
  ++ renderTagsSection (componentTags c)
  ++ [""]

-- | Render a security level to its canonical text representation.
renderSecurityLevel :: SecurityLevel -> Text
renderSecurityLevel Kennel = "'Kennel"
renderSecurityLevel Yard   = "'Yard"
renderSecurityLevel Hunt   = "'Hunt"

-- | Render a boolean as lowercase text.
renderBool :: Bool -> Text
renderBool True  = "true"
renderBool False = "false"

-- | Render an optional field.
renderOptional :: Text -> Maybe Text -> [Text]
renderOptional key (Just val) = [key <> ": " <> val]
renderOptional _   Nothing    = []

-- | Render the target section if present.
renderTargetSection :: Maybe Target -> [Text]
renderTargetSection Nothing = []
renderTargetSection (Just t) =
  [ ""
  , "target:"
  ]
  ++ renderOptional "  os" (targetOS t)
  ++
  [ "  is_edge: " <> renderBool (targetIsEdge t)
  , "  requires_podman: " <> renderBool (targetRequiresPodman t)
  ]
  ++ renderOptional "  memory" (targetMemory t)

-- | Render the recipes section if present.
renderRecipesSection :: Maybe Recipes -> [Text]
renderRecipesSection Nothing = []
renderRecipesSection (Just r) =
  ["", "recipes:"]
  ++ renderOptional "  install" (recipeInstall r)
  ++ renderOptional "  validate" (recipeValidate r)
  ++ renderOptional "  deploy" (recipeDeploy r)
  ++ renderOptional "  migrate" (recipeMigrate r)
  ++ concatMap (\(k, v) -> ["  " <> k <> ": " <> v]) (Map.toList (recipeCustom r))

-- | Render the validation section if present.
renderValidationSection :: Maybe Validation -> [Text]
renderValidationSection Nothing = []
renderValidationSection (Just v) =
  [ ""
  , "validation:"
  , "  checksum: " <> validationChecksum v
  , "  pedigree_version: " <> validationPedigreeVersion v
  , "  hunt_authorized: " <> renderBool (validationHuntAuthorized v)
  ]

-- | Render the tags section if non-empty.
renderTagsSection :: [Text] -> [Text]
renderTagsSection [] = []
renderTagsSection tags =
  ["", "tags:"] ++ map (\t -> "  - " <> t) tags
