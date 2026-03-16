-- SPDX-License-Identifier: MPL-2.0
-- (PMPL-1.0-or-later preferred; MPL-2.0 required for Hackage OSI-approved policy)
--
-- Data.A2ML — Top-level re-export module for the A2ML library.
--
-- Provides a convenient single import for working with A2ML documents:
--
-- @
-- import Data.A2ML
--
-- main :: IO ()
-- main = do
--   result <- parseA2MLFile "document.a2ml"
--   case result of
--     Left err  -> print err
--     Right doc -> putStrLn (renderA2ML doc)
-- @

module Data.A2ML
  ( -- * Document types
    module Data.A2ML.Types

    -- * Parsing
  , module Data.A2ML.Parser

    -- * Rendering
  , module Data.A2ML.Renderer
  ) where

import Data.A2ML.Parser
import Data.A2ML.Renderer
import Data.A2ML.Types
