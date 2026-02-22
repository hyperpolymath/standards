-- | Sanctify-PHP CLI entry point
-- SPDX-License-Identifier: AGPL-3.0-or-later
module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import Control.Monad (forM, filterM, when)
import Data.Either (partitionEithers)

import Sanctify.Parser
import Sanctify.AST
import Sanctify.Analysis.Security
import Sanctify.Analysis.Types
import Sanctify.WordPress.Constraints
import Sanctify.Transform.StrictTypes
import Sanctify.Transform.Sanitize
import Sanctify.Transform.TypeHints
import Sanctify.Emit
import Sanctify.Config
import Sanctify.Report

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> printHelp
        ["-h"] -> printHelp
        ["--version"] -> putStrLn "sanctify-php 0.1.0"
        ["analyze", path] -> analyzeCommand path
        ["fix", path] -> fixCommand path
        ["report", path] -> reportCommand path
        ["export", "--php-ini", path] -> exportPhpIniCommand path
        ["export", "--nginx", path] -> exportNginxCommand path
        ["export", "--guix", path] -> exportGuixCommand path
        _ -> do
            putStrLn "Usage: sanctify <command> [options] <path>"
            putStrLn "Run 'sanctify --help' for more information."
            exitFailure

printHelp :: IO ()
printHelp = putStrLn $ unlines
    [ "sanctify-php - Haskell-based PHP hardening and security analysis"
    , ""
    , "USAGE:"
    , "    sanctify <command> [options] <path>"
    , ""
    , "COMMANDS:"
    , "    analyze <path>     Analyze PHP files for security issues"
    , "    fix <path>         Auto-fix safe issues and report others"
    , "    report <path>      Generate detailed report"
    , "    export             Export configuration for infrastructure"
    , ""
    , "EXPORT SUBCOMMANDS:"
    , "    --php-ini <path>   Generate recommended php.ini settings"
    , "    --nginx <path>     Generate nginx security rules"
    , "    --guix <path>      Generate Guix channel overrides"
    , ""
    , "OPTIONS:"
    , "    -h, --help         Show this help"
    , "    --version          Show version"
    , ""
    , "EXAMPLES:"
    , "    sanctify analyze ./wp-content/plugins/my-plugin/"
    , "    sanctify fix --auto-only ./src/"
    , "    sanctify report --format=sarif ./theme/ > report.sarif"
    , "    sanctify export --php-ini ./project/ >> php.ini"
    , ""
    , "For container integration, see:"
    , "    guix/wordpress-container.scm"
    ]

-- | Analyze command
analyzeCommand :: FilePath -> IO ()
analyzeCommand path = do
    files <- findPhpFiles path
    when (null files) $ do
        putStrLn $ "No PHP files found in: " ++ path
        exitFailure

    putStrLn $ "Analyzing " ++ show (length files) ++ " PHP files..."

    results <- forM files $ \file -> do
        content <- TIO.readFile file
        case parsePhpString file content of
            Left err -> do
                putStrLn $ "  Parse error in " ++ file ++ ": " ++ show err
                pure (file, [], [])
            Right ast -> do
                let secIssues = analyzeSecurityIssues ast
                let wpIssues = if isWordPressCode ast
                               then checkWordPressConstraints ast
                               else []
                pure (file, secIssues, wpIssues)

    -- Print results
    let totalSec = sum $ map (\(_, s, _) -> length s) results
    let totalWp = sum $ map (\(_, _, w) -> length w) results

    putStrLn ""
    putStrLn $ "Found " ++ show totalSec ++ " security issues"
    putStrLn $ "Found " ++ show totalWp ++ " WordPress issues"
    putStrLn ""

    forM_ results $ \(file, secIssues, wpIssues) ->
        when (not (null secIssues) || not (null wpIssues)) $ do
            putStrLn $ file ++ ":"
            forM_ secIssues $ \issue ->
                putStrLn $ "  [" ++ show (issueSeverity issue) ++ "] " ++ T.unpack (issueDescription issue)
            forM_ wpIssues $ \issue ->
                putStrLn $ "  [WP:" ++ show (wpIssueType issue) ++ "] " ++ T.unpack (wpDescription issue)
            putStrLn ""

    if totalSec + totalWp > 0
        then exitFailure
        else exitSuccess

-- | Fix command
fixCommand :: FilePath -> IO ()
fixCommand path = do
    files <- findPhpFiles path
    putStrLn $ "Processing " ++ show (length files) ++ " PHP files..."

    forM_ files $ \file -> do
        content <- TIO.readFile file
        case parsePhpString file content of
            Left _ -> putStrLn $ "  Skipping (parse error): " ++ file
            Right ast -> do
                -- Apply safe transformations
                let transformed = applyTransforms ast
                let output = emitPhp transformed
                -- Show diff (don't modify in-place by default)
                when (content /= output) $ do
                    putStrLn $ "  Would fix: " ++ file
                    -- In production, write to file or show diff

    putStrLn "Done. Use --in-place to apply changes."

-- | Apply safe transformations
applyTransforms :: PhpFile -> PhpFile
applyTransforms = addStrictTypes . addAbspathCheck . addTypeHintsFile
  where
    addTypeHintsFile file = addAllTypeHints emptyContext file

-- | Report command
reportCommand :: FilePath -> IO ()
reportCommand path = do
    files <- findPhpFiles path
    fileReports <- forM files $ \file -> do
        content <- TIO.readFile file
        case parsePhpString file content of
            Left _ -> pure $ generateFileReport file [] [] 0 0 False
            Right ast -> do
                let secIssues = analyzeSecurityIssues ast
                let wpIssues = if isWordPressCode ast
                               then checkWordPressConstraints ast
                               else []
                let autoFixed = length $ filter (canAutoFix . issueType) secIssues
                let manual = length secIssues - autoFixed
                pure $ generateFileReport file secIssues wpIssues autoFixed manual False

    report <- generateReport defaultConfig fileReports
    TIO.putStrLn $ renderText report
  where
    canAutoFix :: IssueType -> Bool
    canAutoFix MissingStrictTypes = True
    canAutoFix _ = False

-- | Export php.ini recommendations
exportPhpIniCommand :: FilePath -> IO ()
exportPhpIniCommand path = do
    issues <- collectIssues path
    TIO.putStrLn $ emitPhpIniRecommendations issues

-- | Export nginx rules
exportNginxCommand :: FilePath -> IO ()
exportNginxCommand path = do
    issues <- collectIssues path
    TIO.putStrLn $ emitNginxRules issues

-- | Export Guix overrides
exportGuixCommand :: FilePath -> IO ()
exportGuixCommand path = do
    issues <- collectIssues path
    TIO.putStrLn $ emitGuixOverrides issues

-- | Collect all issues from a path
collectIssues :: FilePath -> IO [SecurityIssue]
collectIssues path = do
    files <- findPhpFiles path
    concat <$> forM files (\file -> do
        content <- TIO.readFile file
        case parsePhpString file content of
            Left _ -> pure []
            Right ast -> pure $ analyzeSecurityIssues ast)

-- | Find all PHP files in a path
findPhpFiles :: FilePath -> IO [FilePath]
findPhpFiles path = do
    isFile <- doesFileExist path
    if isFile
        then if takeExtension path == ".php"
             then pure [path]
             else pure []
        else do
            isDir <- doesDirectoryExist path
            if isDir
                then do
                    entries <- listDirectory path
                    let fullPaths = map (path </>) entries
                    files <- filterM doesFileExist fullPaths
                    dirs <- filterM doesDirectoryExist fullPaths
                    let phpFiles = filter ((== ".php") . takeExtension) files
                    subFiles <- concat <$> mapM findPhpFiles dirs
                    pure $ phpFiles ++ subFiles
                else pure []
