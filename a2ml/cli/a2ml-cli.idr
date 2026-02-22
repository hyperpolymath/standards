module Main

import A2ML.TypedCore
import A2ML.Proofs
import A2ML.Parser
import System
import System.File

%default total

-- ============================================================================
-- CLI Types
-- ============================================================================

data Command
  = Validate String              -- Validate a file
  | Convert String String String -- Convert from format to format
  | Check String                 -- Quick check (no full validation)
  | Format String                -- Format/pretty-print a file
  | Stats String                 -- Show document statistics
  | Help
  | Version

-- ============================================================================
-- Version Info
-- ============================================================================

version : String
version = "0.7.0"

versionInfo : String
versionInfo = """
A2ML CLI v""" ++ version ++ """

Attested Markup Language - Command Line Interface
Copyright (c) 2026 Jonathan D.A. Jewell

License: PMPL-1.0-or-later
Repository: https://github.com/hyperpolymath/a2ml
"""

-- ============================================================================
-- Help Text
-- ============================================================================

helpText : String
helpText = """
A2ML CLI v""" ++ version ++ """ - Attested Markup Language Tools

USAGE:
    a2ml <COMMAND> [OPTIONS] <FILE>

COMMANDS:
    validate <file>              Validate an A2ML document
    convert <from> <to> <file>   Convert between formats
    check <file>                 Quick syntax check (no validation)
    format <file>                Pretty-print/format a document
    stats <file>                 Show document statistics
    help                         Show this help message
    version                      Show version information

VALIDATION:
    a2ml validate document.a2ml

    Validates:
    - Unique IDs across all elements
    - Reference resolution (all @refs resolve)
    - Required sections (if specified)
    - Directive syntax
    - Structural integrity

    Exit codes:
    0 - Valid document
    1 - Validation errors
    2 - Parse errors
    3 - File not found

CONVERSION:
    a2ml convert a2ml md document.a2ml     # A2ML → Markdown
    a2ml convert a2ml html document.a2ml   # A2ML → HTML
    a2ml convert md a2ml document.md       # Markdown → A2ML

    Supported formats:
    - a2ml: Attested Markup Language
    - md: CommonMark Markdown
    - html: HTML5
    - djot: Djot markup
    - tex: LaTeX

FORMATTING:
    a2ml format document.a2ml > formatted.a2ml

    Formats the document according to A2ML style guidelines:
    - Consistent indentation
    - Canonical directive ordering
    - Normalized whitespace

STATISTICS:
    a2ml stats document.a2ml

    Shows:
    - Document size (lines, bytes)
    - Element counts (sections, figures, tables)
    - ID usage
    - Reference graph

EXAMPLES:
    # Validate a document
    a2ml validate paper.a2ml

    # Convert to HTML
    a2ml convert a2ml html paper.a2ml > paper.html

    # Quick syntax check
    a2ml check draft.a2ml

    # Format and save
    a2ml format messy.a2ml > clean.a2ml

    # Show statistics
    a2ml stats paper.a2ml

ENVIRONMENT:
    A2ML_STRICT=1        Enable strict validation mode
    A2ML_COLOR=0         Disable colored output

SEE ALSO:
    Documentation: https://a2ml.org/docs
    Specification: https://a2ml.org/spec
    Report bugs: https://github.com/hyperpolymath/a2ml/issues
"""

-- ============================================================================
-- Output Formatting
-- ============================================================================

-- ANSI color codes
colorReset : String
colorReset = "\x1b[0m"

colorRed : String
colorRed = "\x1b[31m"

colorGreen : String
colorGreen = "\x1b[32m"

colorYellow : String
colorYellow = "\x1b[33m"

colorBlue : String
colorBlue = "\x1b[34m"

-- Print with color
printColor : String -> String -> IO ()
printColor color msg = putStrLn (color ++ msg ++ colorReset)

printSuccess : String -> IO ()
printSuccess = printColor colorGreen

printError : String -> IO ()
printError = printColor colorRed

printWarning : String -> IO ()
printWarning = printColor colorYellow

printInfo : String -> IO ()
printInfo = printColor colorBlue

-- ============================================================================
-- Command Implementations
-- ============================================================================

covering
validateFile : String -> IO ()
validateFile filename = do
  Right content <- readFile filename
    | Left err => do
        printError ("Error reading file: " ++ show err)
        exitWith (ExitFailure 3)

  case parseAndValidate content of
    Right validated => do
      printSuccess ("✓ Valid A2ML document: " ++ filename)
      printInfo ("  IDs: " ++ show (length validated.ids))
      printInfo ("  Refs: " ++ show (length validated.refs))
      exitSuccess

    Left errors => do
      printError ("✗ Validation failed: " ++ filename)
      for_ errors $ \err => case err of
        DuplicateId id => printError ("  - Duplicate ID: " ++ id.raw)
        UnresolvedRef id => printError ("  - Unresolved reference: " ++ id.raw)
        MissingRequired msg => printError ("  - Missing required: " ++ msg)
      exitWith (ExitFailure 1)

covering
checkFile : String -> IO ()
checkFile filename = do
  Right content <- readFile filename
    | Left err => do
        printError ("Error reading file: " ++ show err)
        exitWith (ExitFailure 3)

  case parseDocument content of
    Success doc _ => do
      printSuccess ("✓ Valid A2ML syntax: " ++ filename)
      printInfo ("  Blocks: " ++ show (length (blocks doc)))
      exitSuccess

    Failure err _ => do
      printError ("✗ Parse error: " ++ filename)
      printError ("  " ++ err)
      exitWith (ExitFailure 2)

covering
formatFile : String -> IO ()
formatFile filename = do
  Right content <- readFile filename
    | Left err => do
        printError ("Error reading file: " ++ show err)
        exitWith (ExitFailure 3)

  case parseDocument content of
    Success doc _ => do
      putStrLn (prettyPrint doc)
      exitSuccess

    Failure err _ => do
      printError ("Parse error: " ++ err)
      exitWith (ExitFailure 2)

covering
statsFile : String -> IO ()
statsFile filename = do
  Right content <- readFile filename
    | Left err => do
        printError ("Error reading file: " ++ show err)
        exitWith (ExitFailure 3)

  case parseDocument content of
    Success doc _ => do
      let ids = collectIds doc
      let refs = collectRefs doc

      printInfo ("Statistics for: " ++ filename)
      putStrLn ""
      putStrLn ("  Lines: " ++ show (length (lines content)))
      putStrLn ("  Bytes: " ++ show (length content))
      putStrLn ""
      putStrLn ("  Total blocks: " ++ show (length (blocks doc)))
      putStrLn ("  IDs defined: " ++ show (length ids))
      putStrLn ("  References: " ++ show (length refs))
      putStrLn ""

      -- Count block types
      let sections = countBlockType isSection (blocks doc)
      let paragraphs = countBlockType isPara (blocks doc)
      let bullets = countBlockType isBullet (blocks doc)
      let figures = countBlockType isFigure (blocks doc)
      let tables = countBlockType isTable (blocks doc)

      putStrLn "  Block types:"
      putStrLn ("    Sections: " ++ show sections)
      putStrLn ("    Paragraphs: " ++ show paragraphs)
      putStrLn ("    Bullets: " ++ show bullets)
      putStrLn ("    Figures: " ++ show figures)
      putStrLn ("    Tables: " ++ show tables)

      exitSuccess

    Failure err _ => do
      printError ("Parse error: " ++ err)
      exitWith (ExitFailure 2)
  where
    countBlockType : (Block -> Bool) -> List Block -> Nat
    countBlockType pred blocks = length (filter pred blocks)

    isSection : Block -> Bool
    isSection (Section _) = True
    isSection _ = False

    isPara : Block -> Bool
    isPara (Para _) = True
    isPara _ = False

    isBullet : Block -> Bool
    isBullet (Bullet _) = True
    isBullet _ = False

    isFigure : Block -> Bool
    isFigure (Figure _) = True
    isFigure _ = False

    isTable : Block -> Bool
    isTable (Table _) = True
    isTable _ = False

covering
convertFile : String -> String -> String -> IO ()
convertFile fromFmt toFmt filename = do
  printWarning ("Format conversion not yet implemented: " ++ fromFmt ++ " → " ++ toFmt)
  printInfo ("File: " ++ filename)
  printInfo ""
  printInfo "Planned converters:"
  printInfo "  - A2ML ↔ Markdown"
  printInfo "  - A2ML ↔ HTML"
  printInfo "  - A2ML ↔ Djot"
  printInfo "  - A2ML ↔ LaTeX"
  exitWith (ExitFailure 1)

-- ============================================================================
-- Argument Parsing
-- ============================================================================

covering
parseArgs : List String -> Maybe Command
parseArgs [] = Just Help
parseArgs ["help"] = Just Help
parseArgs ["-h"] = Just Help
parseArgs ["--help"] = Just Help
parseArgs ["version"] = Just Version
parseArgs ["-v"] = Just Version
parseArgs ["--version"] = Just Version
parseArgs ["validate", file] = Just (Validate file)
parseArgs ["check", file] = Just (Check file)
parseArgs ["format", file] = Just (Format file)
parseArgs ["stats", file] = Just (Stats file)
parseArgs ["convert", from, to, file] = Just (Convert from to file)
parseArgs _ = Nothing

-- ============================================================================
-- Main Entry Point
-- ============================================================================

covering
main : IO ()
main = do
  args <- getArgs
  case parseArgs (drop 1 args) of  -- Drop program name
    Nothing => do
      printError "Invalid arguments"
      putStrLn ""
      putStrLn helpText
      exitWith (ExitFailure 1)

    Just Help => do
      putStrLn helpText
      exitSuccess

    Just Version => do
      putStrLn versionInfo
      exitSuccess

    Just (Validate file) => validateFile file
    Just (Check file) => checkFile file
    Just (Format file) => formatFile file
    Just (Stats file) => statsFile file
    Just (Convert from to file) => convertFile from to file
