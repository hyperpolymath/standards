-- SPDX-License-Identifier: MPL-2.0
-- (PMPL-1.0-or-later preferred; MPL-2.0 required for Hackage OSI-approved policy)
--
-- Data.K9 — Top-level re-export module for the K9 library.
--
-- Provides a convenient single import for working with K9 components:
--
-- @
-- import Data.K9
--
-- main :: IO ()
-- main = do
--   result <- parseK9File "component.k9"
--   case result of
--     Left err -> print err
--     Right c  -> putStrLn (renderK9 c)
-- @

module Data.K9
  ( -- * Component types
    module Data.K9.Types

    -- * Parsing
  , module Data.K9.Parser

    -- * Rendering
  , module Data.K9.Renderer
  ) where

import Data.K9.Parser
import Data.K9.Renderer
import Data.K9.Types
