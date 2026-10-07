{-# LANGUAGE StrictData #-}
-- SPDX-License-Identifier: MPL-2.0
module Sanctify.Transform.Strict
    ( transformStrict
    , transformWordPressSecurity
    ) where

import Sanctify.AST
import Sanctify.Transform.StrictTypes

-- | Add strict types to a PHP file.
transformStrict :: PhpFile -> PhpFile
transformStrict = addStrictTypes

-- | Apply WordPress-specific hardening (ABSPATH + admin checks).
transformWordPressSecurity :: PhpFile -> PhpFile
transformWordPressSecurity file =
    addAbspathCheck file
