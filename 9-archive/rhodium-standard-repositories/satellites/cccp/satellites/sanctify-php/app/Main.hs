-- | Sanctify-PHP CLI entry point
-- SPDX-License-Identifier: MPL-2.0
module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stdout, hPutStrLn, stderr)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy.Char8 as BL8
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory, getModificationTime)
import System.FilePath ((</>), takeExtension)
import Control.Monad (forM, forM_, filterM, when, unless, forever)
import Control.Concurrent (threadDelay)
import Data.Either (partitionEithers)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (isJust, fromMaybe, catMaybes)
import Data.Time.Clock (UTCTime)
import qualified Data.Map.Strict as Map

import Sanctify.Parser
import Sanctify.AST
import Sanctify.Analysis.Security
import Sanctify.Analysis.Types (emptyTypeContext)
import Sanctify.WordPress.Constraints
import Sanctify.Transform.StrictTypes
import Sanctify.Transform.Sanitize
import Sanctify.Transform.TypeHints
import Sanctify.Emit (emitPhp, emitPhpIniRecommendations, emitNginxRules, emitGuixOverrides)
import Sanctify.Config
import qualified Sanctify.Report as SReport

-- | CLI options
data Options = Options
    { optCommand :: Command
    , optInteractive :: Bool
    , optWatch :: Bool
    , optFormat :: OutputFormat
    , optSeverity :: [Severity]
    , optTypes :: [Text]
    , optInPlace :: Bool
    , optDiff :: Bool
    , optVerbose :: Bool
    }

data Command
    = Analyze FilePath
    | Fix FilePath
    | Report FilePath
    | ExportPhpIni FilePath
    | ExportNginx FilePath
    | ExportGuix FilePath
    | Help
    | Version

data OutputFormat = FormatText | FormatJSON | FormatSARIF | FormatHTML
    deriving (Eq, Show)

data Severity = Critical | High | Medium | Low | Info
    deriving (Eq, Show, Read)

main :: IO ()
main = do
    args <- getArgs
    opts <- parseArgs args
    runCommand opts

-- | Parse command-line arguments
parseArgs :: [String] -> IO Options
parseArgs args = case args of
    [] -> printHelp >> exitFailure
    ("--help":_) -> pure $ Options Help False False FormatText [] [] False False False
    ("-h":_) -> pure $ Options Help False False FormatText [] [] False False False
    ("--version":_) -> pure $ Options Version False False FormatText [] [] False False False
    _ -> parseCommand args defaultOptions
  where
    defaultOptions = Options
        { optCommand = Help
        , optInteractive = False
        , optWatch = False
        , optFormat = FormatText
        , optSeverity = []
        , optTypes = []
        , optInPlace = False
        , optDiff = False
        , optVerbose = False
        }

-- | Parse command and options
parseCommand :: [String] -> Options -> IO Options
parseCommand [] opts = pure opts
parseCommand ("analyze":rest) opts = parseOptions rest (opts { optCommand = Analyze "" })
parseCommand ("fix":rest) opts = parseOptions rest (opts { optCommand = Fix "" })
parseCommand ("report":rest) opts = parseOptions rest (opts { optCommand = Report "" })
parseCommand ("export":"--php-ini":rest) opts = parseOptions rest (opts { optCommand = ExportPhpIni "" })
parseCommand ("export":"--nginx":rest) opts = parseOptions rest (opts { optCommand = ExportNginx "" })
parseCommand ("export":"--guix":rest) opts = parseOptions rest (opts { optCommand = ExportGuix "" })
parseCommand (arg:rest) opts
    | "--interactive" `isPrefixOf` arg = parseCommand rest (opts { optInteractive = True })
    | "--watch" `isPrefixOf` arg = parseCommand rest (opts { optWatch = True })
    | "--in-place" `isPrefixOf` arg = parseCommand rest (opts { optInPlace = True })
    | "--diff" `isPrefixOf` arg = parseCommand rest (opts { optDiff = True })
    | "-v" == arg || "--verbose" == arg = parseCommand rest (opts { optVerbose = True })
    | "--format=json" `isPrefixOf` arg = parseCommand rest (opts { optFormat = FormatJSON })
    | "--format=sarif" `isPrefixOf` arg = parseCommand rest (opts { optFormat = FormatSARIF })
    | "--format=html" `isPrefixOf` arg = parseCommand rest (opts { optFormat = FormatHTML })
    | "--format=text" `isPrefixOf` arg = parseCommand rest (opts { optFormat = FormatText })
    | "--severity=" `isPrefixOf` arg =
        let sevs = parseSeverities (drop 11 arg)
        in parseCommand rest (opts { optSeverity = sevs })
    | "--type=" `isPrefixOf` arg =
        let types = T.splitOn "," $ T.pack $ drop 7 arg
        in parseCommand rest (opts { optTypes = types })
    | not ("-" `isPrefixOf` arg) = pure $ setPath arg opts
    | otherwise = do
        hPutStrLn stderr $ "Unknown option: " ++ arg
        exitFailure

parseOptions :: [String] -> Options -> IO Options
parseOptions = parseCommand

setPath :: FilePath -> Options -> Options
setPath path opts = opts { optCommand = updatePath path (optCommand opts) }
  where
    updatePath p (Analyze _) = Analyze p
    updatePath p (Fix _) = Fix p
    updatePath p (Report _) = Report p
    updatePath p (ExportPhpIni _) = ExportPhpIni p
    updatePath p (ExportNginx _) = ExportNginx p
    updatePath p (ExportGuix _) = ExportGuix p
    updatePath _ cmd = cmd

parseSeverities :: String -> [Severity]
parseSeverities str = catMaybes $ map readSev $ splitOn ',' str
  where
    readSev s = case map toLower s of
        "critical" -> Just Critical
        "high" -> Just High
        "medium" -> Just Medium
        "low" -> Just Low
        "info" -> Just Info
        _ -> Nothing
    toLower c | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
              | otherwise = c
    splitOn _ [] = []
    splitOn delim s = case break (== delim) s of
        (a, []) -> [a]
        (a, _:b) -> a : splitOn delim b

-- | Run the command
runCommand :: Options -> IO ()
runCommand opts = case optCommand opts of
    Help -> printHelp
    Version -> putStrLn "sanctify-php 0.2.0-alpha"
    Analyze path -> analyzeCommandNew opts path
    Fix path -> fixCommandNew opts path
    Report path -> reportCommandNew opts path
    ExportPhpIni path -> exportPhpIniCommand path
    ExportNginx path -> exportNginxCommand path
    ExportGuix path -> exportGuixCommand path

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
    , "    -h, --help                Show this help"
    , "    --version                 Show version"
    , "    --interactive             Interactive fix mode (prompt for each change)"
    , "    --watch                   Watch mode (re-analyze on file changes)"
    , "    --format=<fmt>            Output format: text, json, sarif, html (default: text)"
    , "    --severity=<sevs>         Filter by severity: critical,high,medium,low,info"
    , "    --type=<types>            Filter by issue type (comma-separated)"
    , "    --in-place                Apply fixes in-place (modifies files)"
    , "    --diff                    Show diff preview of changes"
    , "    -v, --verbose             Verbose output"
    , ""
    , "EXAMPLES:"
    , "    # Basic analysis"
    , "    sanctify analyze ./wp-content/plugins/my-plugin/"
    , ""
    , "    # Filter high and critical issues only"
    , "    sanctify analyze --severity=high,critical ./src/"
    , ""
    , "    # Interactive fix with diff preview"
    , "    sanctify fix --interactive --diff ./theme/"
    , ""
    , "    # Watch mode for development"
    , "    sanctify analyze --watch ./src/"
    , ""
    , "    # Generate SARIF report for CI/CD"
    , "    sanctify report --format=sarif ./project/ > report.sarif"
    , ""
    , "    # Export infrastructure configuration"
    , "    sanctify export --php-ini ./project/ >> php.ini"
    , ""
    , "For container integration, see:"
    , "    guix/wordpress-container.scm"
    ]

-- | Enhanced analyze command with watch mode and filtering
analyzeCommandNew :: Options -> FilePath -> IO ()
analyzeCommandNew opts path
    | optWatch opts = watchMode opts path analyzeOnce
    | otherwise = analyzeOnce opts path

analyzeOnce :: Options -> FilePath -> IO ()
analyzeOnce opts path = do
    files <- findPhpFiles path
    when (null files) $ do
        putStrLn $ "No PHP files found in: " ++ path
        exitFailure

    when (optVerbose opts) $
        putStrLn $ "Analyzing " ++ show (length files) ++ " PHP files..."

    results <- forM files $ \file -> do
        content <- TIO.readFile file
        case parsePhpString file content of
            Left err -> do
                when (optVerbose opts) $
                    putStrLn $ "  Parse error in " ++ file ++ ": " ++ show err
                pure (file, [], [])
            Right ast -> do
                let secIssues = filterIssues opts $ analyzeSecurityIssues ast
                let wpIssues = if isWordPressCode ast
                               then checkWordPressConstraints ast
                               else []
                pure (file, secIssues, wpIssues)

    -- Output results based on format
    case optFormat opts of
        FormatText -> outputTextResults opts results
        FormatJSON -> outputJSONResults results
        FormatSARIF -> outputSARIFResults results
        FormatHTML -> outputHTMLResults results

    let totalIssues = sum $ map (\(_, s, w) -> length s + length w) results
    if totalIssues > 0
        then exitFailure
        else exitSuccess

-- | Filter issues by severity and type
filterIssues :: Options -> [SecurityIssue] -> [SecurityIssue]
filterIssues opts issues =
    let bySeverity = if null (optSeverity opts)
                     then issues
                     else filter (\i -> issueSeverity i `elem` optSeverity opts) issues
        byType = if null (optTypes opts)
                 then bySeverity
                 else filter (\i -> T.pack (show (issueType i)) `elem` optTypes opts) bySeverity
    in byType

-- | Output results as text
outputTextResults :: Options -> [(FilePath, [SecurityIssue], [WordPressIssue])] -> IO ()
outputTextResults opts results = do
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
                    ++ " (line " ++ show (posLine $ issueLocation issue) ++ ")"
            forM_ wpIssues $ \issue ->
                putStrLn $ "  [WP:" ++ show (wpIssueType issue) ++ "] " ++ T.unpack (wpDescription issue)
            putStrLn ""

-- | Output results as JSON
outputJSONResults :: [(FilePath, [SecurityIssue], [WordPressIssue])] -> IO ()
outputJSONResults results = do
    putStrLn "{"
    putStrLn "  \"issues\": ["
    let allIssues = concatMap (\(file, sec, wp) ->
            map (\i -> "    {\"file\": \"" ++ file ++ "\", \"type\": \"security\", \"severity\": \"" ++ show (issueSeverity i) ++ "\", \"message\": \"" ++ T.unpack (issueDescription i) ++ "\", \"line\": " ++ show (posLine $ issueLocation i) ++ "}") sec
            ++ map (\i -> "    {\"file\": \"" ++ file ++ "\", \"type\": \"wordpress\", \"message\": \"" ++ T.unpack (wpDescription i) ++ "\"}") wp) results
    putStrLn $ concat $ insertCommas allIssues
    putStrLn "  ]"
    putStrLn "}"
  where
    insertCommas [] = []
    insertCommas [x] = [x]
    insertCommas (x:xs) = (x ++ ",") : insertCommas xs

-- | Output results as SARIF
outputSARIFResults :: [(FilePath, [SecurityIssue], [WordPressIssue])] -> IO ()
outputSARIFResults results = do
    putStrLn "{"
    putStrLn "  \"version\": \"2.1.0\","
    putStrLn "  \"$schema\": \"https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json\","
    putStrLn "  \"runs\": [{"
    putStrLn "    \"tool\": {"
    putStrLn "      \"driver\": {"
    putStrLn "        \"name\": \"sanctify-php\","
    putStrLn "        \"version\": \"0.2.0-alpha\""
    putStrLn "      }"
    putStrLn "    },"
    putStrLn "    \"results\": ["
    let sarifResults = concatMap (\(file, sec, _) ->
            map (\i -> "      {\"ruleId\": \"" ++ show (issueType i) ++ "\", \"level\": \"" ++ severityToLevel (issueSeverity i) ++ "\", \"message\": {\"text\": \"" ++ T.unpack (issueDescription i) ++ "\"}, \"locations\": [{\"physicalLocation\": {\"artifactLocation\": {\"uri\": \"" ++ file ++ "\"}, \"region\": {\"startLine\": " ++ show (posLine $ issueLocation i) ++ "}}}]}") sec) results
    putStrLn $ concat $ insertCommas sarifResults
    putStrLn "    ]"
    putStrLn "  }]"
    putStrLn "}"
  where
    severityToLevel Critical = "error"
    severityToLevel High = "error"
    severityToLevel Medium = "warning"
    severityToLevel Low = "note"
    severityToLevel Info = "note"
    insertCommas [] = []
    insertCommas [x] = [x]
    insertCommas (x:xs) = (x ++ ",") : insertCommas xs

-- | Output results as HTML
outputHTMLResults :: [(FilePath, [SecurityIssue], [WordPressIssue])] -> IO ()
outputHTMLResults results = do
    putStrLn "<!DOCTYPE html><html><head><title>Sanctify-PHP Report</title>"
    putStrLn "<style>body{font-family:sans-serif;margin:20px;}h1{color:#333;}.issue{margin:10px 0;padding:10px;border-left:4px solid #ccc;}.critical{border-color:#d32f2f;}.high{border-color:#f57c00;}.medium{border-color:#fbc02d;}.low{border-color:#388e3c;}</style>"
    putStrLn "</head><body><h1>Sanctify-PHP Security Report</h1>"
    forM_ results $ \(file, secIssues, wpIssues) ->
        unless (null secIssues && null wpIssues) $ do
            putStrLn $ "<h2>" ++ file ++ "</h2>"
            forM_ secIssues $ \issue ->
                putStrLn $ "<div class='issue " ++ map toLower (show (issueSeverity issue)) ++ "'>"
                    ++ "<strong>" ++ show (issueSeverity issue) ++ "</strong>: "
                    ++ T.unpack (issueDescription issue)
                    ++ " (line " ++ show (posLine $ issueLocation issue) ++ ")</div>"
    putStrLn "</body></html>"
  where
    toLower c | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
              | otherwise = c

-- | Watch mode - re-analyze on file changes
watchMode :: Options -> FilePath -> (Options -> FilePath -> IO ()) -> IO ()
watchMode opts path action = do
    putStrLn "Watch mode enabled. Press Ctrl+C to exit."
    initialMTimes <- getFileMTimes path
    action opts path
    watchLoop initialMTimes
  where
    watchLoop lastMTimes = do
        threadDelay 1000000  -- 1 second
        currentMTimes <- getFileMTimes path
        if currentMTimes /= lastMTimes
            then do
                putStrLn "\n=== Files changed, re-analyzing... ===\n"
                action opts path
                watchLoop currentMTimes
            else watchLoop lastMTimes

    getFileMTimes :: FilePath -> IO (Map.Map FilePath UTCTime)
    getFileMTimes dir = do
        files <- findPhpFiles dir
        mtimes <- forM files $ \f -> do
            mtime <- getModificationTime f
            pure (f, mtime)
        pure $ Map.fromList mtimes

-- | Old analyze command (kept for compatibility)
analyzeCommand :: FilePath -> IO ()
analyzeCommand path = analyzeOnce (Options (Analyze path) False False FormatText [] [] False False False) path

-- | Enhanced fix command with interactive mode and diff preview
fixCommandNew :: Options -> FilePath -> IO ()
fixCommandNew opts path
    | optWatch opts = watchMode opts path fixOnce
    | otherwise = fixOnce opts path

fixOnce :: Options -> FilePath -> IO ()
fixOnce opts path = do
    files <- findPhpFiles path
    when (optVerbose opts) $
        putStrLn $ "Processing " ++ show (length files) ++ " PHP files..."

    fixed <- forM files $ \file -> do
        content <- TIO.readFile file
        case parsePhpString file content of
            Left err -> do
                when (optVerbose opts) $
                    hPutStrLn stderr $ "  Parse error in " ++ file ++ ": " ++ show err
                pure Nothing
            Right ast -> do
                let transformed = applyTransforms ast
                let output = emitPhp transformed

                if content == output
                    then pure Nothing
                    else do
                        if optInteractive opts
                            then interactiveFix file content output opts
                            else autoFix file content output opts

    let fixedCount = length $ filter isJust fixed
    putStrLn $ "\nFixed " ++ show fixedCount ++ " file(s)."
    unless (optInPlace opts) $
        putStrLn "Use --in-place to apply changes."

-- | Interactive fix mode
interactiveFix :: FilePath -> Text -> Text -> Options -> IO (Maybe FilePath)
interactiveFix file original modified opts = do
    putStrLn $ "\n" ++ file ++ ":"
    when (optDiff opts) $
        showDiff original modified
    putStr "Apply this fix? [y/N/d(iff)/s(kip all)] "
    hFlush stdout
    response <- getLine
    case map toLower $ take 1 response of
        "y" -> do
            when (optInPlace opts) $
                TIO.writeFile file modified
            putStrLn "  ✓ Applied"
            pure $ Just file
        "d" -> do
            showDiff original modified
            interactiveFix file original modified opts
        "s" -> do
            putStrLn "  Skipping remaining files..."
            exitSuccess
        _ -> do
            putStrLn "  Skipped"
            pure Nothing
  where
    toLower c | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
              | otherwise = c

-- | Auto fix mode
autoFix :: FilePath -> Text -> Text -> Options -> IO (Maybe FilePath)
autoFix file original modified opts = do
    when (optVerbose opts) $
        putStrLn $ "  Would fix: " ++ file
    when (optDiff opts) $ do
        putStrLn $ "\n" ++ file ++ ":"
        showDiff original modified
    when (optInPlace opts) $
        TIO.writeFile file modified
    pure $ Just file

-- | Show unified diff between two texts
showDiff :: Text -> Text -> IO ()
showDiff original modified = do
    let origLines = T.lines original
    let modLines = T.lines modified
    putStrLn "--- original"
    putStrLn "+++ modified"
    putStrLn $ "@@ -1," ++ show (length origLines) ++ " +1," ++ show (length modLines) ++ " @@"
    showDiffLines origLines modLines
  where
    showDiffLines [] [] = pure ()
    showDiffLines (o:os) (m:ms)
        | o == m = do
            putStrLn $ " " ++ T.unpack o
            showDiffLines os ms
        | otherwise = do
            putStrLn $ "-" ++ T.unpack o
            putStrLn $ "+" ++ T.unpack m
            showDiffLines os ms
    showDiffLines (o:os) [] = do
        putStrLn $ "-" ++ T.unpack o
        showDiffLines os []
    showDiffLines [] (m:ms) = do
        putStrLn $ "+" ++ T.unpack m
        showDiffLines [] ms

-- | Old fix command (kept for compatibility)
fixCommand :: FilePath -> IO ()
fixCommand path = fixOnce (Options (Fix path) False False FormatText [] [] False False False) path

-- | Apply safe transformations
applyTransforms :: PhpFile -> PhpFile
applyTransforms = addStrictTypes . addAbspathCheck . addTypeHintsFile
  where
    addTypeHintsFile file = addAllTypeHints emptyTypeContext file

-- | Enhanced report command with multiple output formats
reportCommandNew :: Options -> FilePath -> IO ()
reportCommandNew opts path = do
    files <- findPhpFiles path

    when (optVerbose opts) $
        putStrLn $ "Generating report for " ++ show (length files) ++ " PHP files..."

    fileReports <- forM files $ \file -> do
        content <- TIO.readFile file
        case parsePhpString file content of
            Left _ -> pure $ SReport.generateFileReport file [] [] 0 0 False
            Right ast -> do
                let secIssues = filterIssues opts $ analyzeSecurityIssues ast
                let wpIssues = if isWordPressCode ast
                               then checkWordPressConstraints ast
                               else []
                let autoFixed = length $ filter (canAutoFix . issueType) secIssues
                let manual = length secIssues - autoFixed
                pure $ SReport.generateFileReport file secIssues wpIssues autoFixed manual False

    case optFormat opts of
        FormatText -> do
            report <- SReport.generateReport defaultConfig fileReports
            TIO.putStrLn $ SReport.renderText report
        FormatJSON -> do
            report <- SReport.generateReport defaultConfig fileReports
            BL8.putStrLn $ SReport.renderJson report
        FormatSARIF -> do
            report <- SReport.generateReport defaultConfig fileReports
            BL8.putStrLn $ SReport.renderSarif report
        FormatHTML -> do
            report <- SReport.generateReport defaultConfig fileReports
            TIO.putStrLn $ SReport.renderHtml report
  where
    canAutoFix :: IssueType -> Bool
    canAutoFix MissingStrictTypes = True
    canAutoFix _ = False

-- | Old report command (kept for compatibility)
reportCommand :: FilePath -> IO ()
reportCommand path = reportCommandNew (Options (Report path) False False FormatText [] [] False False False) path

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
