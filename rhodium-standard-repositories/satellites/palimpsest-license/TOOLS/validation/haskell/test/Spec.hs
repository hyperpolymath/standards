{-
SPDX-License-Identifier: MIT
SPDX-FileCopyrightText: 2024-2025 Palimpsest Stewardship Council
-}

module Main where

import Test.Hspec
import Palimpsest.Validator.LicenseSpec
import Palimpsest.Validator.MetadataSpec
import Palimpsest.Validator.BilingualSpec
import Palimpsest.Validator.ReferenceSpec
import Palimpsest.Validator.UtilsSpec
import Integration.PipelineSpec

main :: IO ()
main = hspec $ do
  describe "Palimpsest Validator Test Suite" $ do
    describe "Unit Tests" $ do
      licenseSpec
      metadataSpec
      bilingualSpec
      referenceSpec
      utilsSpec

    describe "Integration Tests" $ do
      pipelineSpec
