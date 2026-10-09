{-
SPDX-License-Identifier: MIT
SPDX-FileCopyrightText: 2024-2025 Palimpsest Stewardship Council
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import Palimpsest.Validator

-- | Command line options
data Options = Options
  { optRootPath      :: FilePath
  , optCommand       :: Command
  , optStrictMode    :: Bool
  , optVerbose       :: Bool
  , optNoBilingual   :: Bool
  , optNoReferences  :: Bool
  }

-- | Available commands
data Command
  = ValidateProject
  | ValidateLicense FilePath
  | ValidateMetadata FilePath
  | ValidateBilingual FilePath FilePath FilePath
  | ValidateReferences [FilePath]

-- | Main entry point
main :: IO ()
main = do
  opts <- execParser optsParser
  runValidation opts

-- | Options parser
optsParser :: ParserInfo Options
optsParser = info (optionsParser <**> helper)
  ( fullDesc
  <> progDesc "Validate Palimpsest License documents and metadata"
  <> header "palimpsest-validate - Functional validation toolkit for the Palimpsest License"
  )

-- | Parse command line options
optionsParser :: Parser Options
optionsParser = Options
  <$> strOption
      ( long "root"
      <> short 'r'
      <> metavar "PATH"
      <> value "."
      <> help "Root path of the Palimpsest License project"
      )
  <*> commandParser
  <*> switch
      ( long "strict"
      <> short 's'
      <> help "Strict mode: fail on warnings"
      )
  <*> switch
      ( long "verbose"
      <> short 'v'
      <> help "Verbose output"
      )
  <*> switch
      ( long "no-bilingual"
      <> help "Skip bilingual consistency checks"
      )
  <*> switch
      ( long "no-references"
      <> help "Skip cross-reference validation"
      )

-- | Parse subcommands
commandParser :: Parser Command
commandParser = subparser
  ( command "project"
      (info (pure ValidateProject)
        (progDesc "Validate entire project structure"))
  <> command "license"
      (info (ValidateLicense <$> argument str (metavar "FILE"))
        (progDesc "Validate a single license file"))
  <> command "metadata"
      (info (ValidateMetadata <$> argument str (metavar "FILE"))
        (progDesc "Validate a metadata file (JSON-LD or XML)"))
  <> command "bilingual"
      (info (ValidateBilingual
              <$> argument str (metavar "ENGLISH_FILE")
              <*> argument str (metavar "DUTCH_FILE")
              <*> argument str (metavar "MAP_FILE"))
        (progDesc "Validate bilingual consistency"))
  <> command "references"
      (info (ValidateReferences <$> some (argument str (metavar "FILES...")))
        (progDesc "Validate cross-references in files"))
  )
  <|> pure ValidateProject  -- Default command

-- | Run validation based on options
runValidation :: Options -> IO ()
runValidation opts = do
  let config = ValidationConfig
        { configRootPath = optRootPath opts
        , configStrictMode = optStrictMode opts
        , configCheckBilingual = not (optNoBilingual opts)
        , configCheckReferences = not (optNoReferences opts)
        , configVerbose = optVerbose opts
        }

  result <- case optCommand opts of
    ValidateProject ->
      runValidator config validateProject

    ValidateLicense path ->
      runValidator config (validateLicense path >> pure (validationSuccess "License validated"))

    ValidateMetadata path ->
      runValidator config (validateMetadata path >> pure (validationSuccess "Metadata validated"))

    ValidateBilingual enPath nlPath mapPath ->
      runValidator config (validateBilingual enPath nlPath mapPath >> pure (validationSuccess "Bilingual consistency validated"))

    ValidateReferences paths ->
      runValidator config (validateAllReferences paths >> pure (validationSuccess "References validated"))

  -- Process results
  case result of
    Left err -> do
      putStrLn "\n❌ Validation FAILED\n"
      TIO.putStrLn $ "Error: " <> errorMessage err
      when (errorSeverity err == Critical) $
        TIO.putStrLn $ "Location: " <> errorLocation err
      case errorDetails err of
        Just details -> TIO.putStrLn $ "Details: " <> details
        Nothing -> pure ()
      exitFailure

    Right (valResult, warnings) -> do
      -- Display warnings
      when (not $ null warnings) $ do
        putStrLn "\n⚠️  Warnings:\n"
        mapM_ displayWarning warnings

      -- Check if we should fail on warnings in strict mode
      let hasErrors = any (\w -> warningSeverity w == Error) warnings
          shouldFail = optStrictMode opts && not (null warnings) || hasErrors

      if shouldFail
        then do
          putStrLn "\n❌ Validation FAILED (strict mode or errors present)\n"
          exitFailure
        else do
          putStrLn "\n✅ Validation PASSED\n"
          TIO.putStrLn $ validationSummary valResult
          exitSuccess

-- | Display a validation warning
displayWarning :: ValidationWarning -> IO ()
displayWarning warning = do
  let symbol = case warningSeverity warning of
        Critical -> "🔴"
        Error -> "🔴"
        Warning -> "⚠️ "
        Info -> "ℹ️ "
  TIO.putStrLn $ symbol <> " [" <> warningLocation warning <> "] " <> warningMessage warning
