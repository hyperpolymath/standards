module A2ML.REPL

import A2ML.TypedCore
import A2ML.Proofs
import A2ML.Parser
import A2ML.Converters
import System
import System.File
import Data.String

%default covering

-- ============================================================================
-- REPL State
-- ============================================================================

record ReplState where
  constructor MkReplState
  currentDoc : Maybe Doc
  filename : Maybe String
  modified : Bool
  history : List String
  strictMode : Bool

initialState : ReplState
initialState = MkReplState Nothing Nothing False [] False

-- ============================================================================
-- REPL Commands
-- ============================================================================

data ReplCommand
  = Load String           -- :load file.a2ml
  | Parse String          -- :parse "# Test"
  | Validate              -- :validate
  | Show                  -- :show
  | Ids                   -- :ids
  | Refs                  -- :refs
  | Convert String        -- :convert md|html|djot|tex
  | Stats                 -- :stats
  | Strict Bool           -- :strict on|off
  | Save String           -- :save file.a2ml
  | Clear                 -- :clear
  | Help                  -- :help
  | Quit                  -- :quit
  | Eval String           -- Evaluate A2ML text

-- ============================================================================
-- Command Parser
-- ============================================================================

parseCommand : String -> Maybe ReplCommand
parseCommand input =
  let trimmed = trim input
  in case words trimmed of
    [] => Nothing
    (":load" :: file :: _) => Just (Load file)
    [":parse", text] => Just (Parse text)
    [":validate"] => Just Validate
    [":show"] => Just Show
    [":ids"] => Just Ids
    [":refs"] => Just Refs
    (":convert" :: fmt :: _) => Just (Convert fmt)
    [":stats"] => Just Stats
    (":strict" :: "on" :: _) => Just (Strict True)
    (":strict" :: "off" :: _) => Just (Strict False)
    (":save" :: file :: _) => Just (Save file)
    [":clear"] => Just Clear
    [":help"] => Just Help
    [":quit"] => Just Quit
    [":q"] => Just Quit
    _ =>
      if isPrefixOf ":" trimmed
        then Nothing  -- Unknown command
        else Just (Eval trimmed)  -- Evaluate as A2ML

-- ============================================================================
-- Command Execution
-- ============================================================================

covering
executeCommand : ReplCommand -> ReplState -> IO ReplState
executeCommand (Load filename) state = do
  Right content <- readFile filename
    | Left err => do
        putStrLn ("Error reading file: " ++ show err)
        pure state

  case parseDocument content of
    Success doc _ => do
      putStrLn ("✓ Loaded: " ++ filename)
      pure (MkReplState (Just doc) (Just filename) False state.history state.strictMode)
    Failure err _ => do
      putStrLn ("✗ Parse error: " ++ err)
      pure state

executeCommand (Parse text) state = do
  case parseDocument text of
    Success doc _ => do
      putStrLn "✓ Parsed successfully"
      pure (MkReplState (Just doc) Nothing True state.history state.strictMode)
    Failure err _ => do
      putStrLn ("✗ Parse error: " ++ err)
      pure state

executeCommand Validate state = do
  case state.currentDoc of
    Nothing => do
      putStrLn "No document loaded. Use :load <file> or :parse <text>"
      pure state

    Just doc => do
      let ids = collectIds doc
      let refs = collectRefs doc

      case validateDocument doc ids refs of
        Right validated => do
          putStrLn "✓ Document is valid"
          putStrLn ("  Unique IDs: " ++ show (length validated.ids))
          putStrLn ("  Resolved refs: " ++ show (length validated.refs))
          pure state

        Left errors => do
          putStrLn ("✗ Validation errors (" ++ show (length errors) ++ "):")
          for_ errors $ \err => case err of
            DuplicateId id => putStrLn ("  - Duplicate ID: " ++ id.raw)
            UnresolvedRef id => putStrLn ("  - Unresolved reference: " ++ id.raw)
            MissingRequired msg => putStrLn ("  - " ++ msg)
          pure state

executeCommand Show state = do
  case state.currentDoc of
    Nothing => putStrLn "No document loaded"
    Just doc => putStrLn (prettyPrint doc)
  pure state

executeCommand Ids state = do
  case state.currentDoc of
    Nothing => putStrLn "No document loaded"
    Just doc => do
      let ids = collectIds doc
      putStrLn ("IDs defined (" ++ show (length ids) ++ "):")
      for_ ids $ \id => putStrLn ("  - " ++ id.raw)
  pure state

executeCommand Refs state = do
  case state.currentDoc of
    Nothing => putStrLn "No document loaded"
    Just doc => do
      let refs = collectRefs doc
      putStrLn ("References used (" ++ show (length refs) ++ "):")
      for_ refs $ \id => putStrLn ("  - " ++ id.raw)
  pure state

executeCommand (Convert fmt) state = do
  case state.currentDoc of
    Nothing => do
      putStrLn "No document loaded"
      pure state

    Just doc => do
      case parseFormat fmt of
        Just Markdown => putStrLn (toMarkdown doc)
        Just Html => putStrLn (toHtml doc)
        Just Djot => putStrLn (toDjot doc)
        Just Latex => putStrLn (toLatex doc)
        Just PlainText => putStrLn (toPlainText doc)
        Nothing => putStrLn ("Unknown format: " ++ fmt)
      pure state

executeCommand Stats state = do
  case state.currentDoc of
    Nothing => putStrLn "No document loaded"
    Just doc => do
      let ids = collectIds doc
      let refs = collectRefs doc
      putStrLn "Document statistics:"
      putStrLn ("  Blocks: " ++ show (length (blocks doc)))
      putStrLn ("  IDs: " ++ show (length ids))
      putStrLn ("  Refs: " ++ show (length refs))
  pure state

executeCommand (Strict enabled) state = do
  putStrLn ("Strict mode: " ++ if enabled then "ON" else "OFF")
  pure (record { strictMode = enabled } state)

executeCommand (Save filename) state = do
  case state.currentDoc of
    Nothing => do
      putStrLn "No document to save"
      pure state

    Just doc => do
      let content = prettyPrint doc
      Right () <- writeFile filename content
        | Left err => do
            putStrLn ("Error writing file: " ++ show err)
            pure state
      putStrLn ("✓ Saved to: " ++ filename)
      pure (record { modified = False, filename = Just filename } state)

executeCommand Clear state = do
  putStrLn "Document cleared"
  pure (record { currentDoc = Nothing, filename = Nothing, modified = False } state)

executeCommand Help state = do
  putStrLn """
A2ML REPL Commands:

  :load <file>       Load an A2ML document
  :parse <text>      Parse A2ML text
  :validate          Validate current document
  :show              Show current document
  :ids               List all IDs in document
  :refs              List all references in document
  :convert <fmt>     Convert to format (md|html|djot|tex|txt)
  :stats             Show document statistics
  :strict on|off     Enable/disable strict validation
  :save <file>       Save current document
  :clear             Clear current document
  :help              Show this help
  :quit              Exit REPL

  <text>             Evaluate A2ML text and add to current document

Examples:
  :load paper.a2ml
  :validate
  :convert html
  :stats
  """
  pure state

executeCommand Quit state = do
  if state.modified then do
    putStrLn "Warning: Document has unsaved changes"
    putStr "Save before quitting? (y/n): "
    response <- getLine
    if response == "y" then
      case state.filename of
        Just fname => do
          _ <- executeCommand (Save fname) state
          pure state
        Nothing => do
          putStr "Filename: "
          fname <- getLine
          _ <- executeCommand (Save fname) state
          pure state
    else
      pure state
  else
    pure state

executeCommand (Eval text) state = do
  case parseDocument text of
    Success doc _ => do
      let newDoc = case state.currentDoc of
                     Just existing => MkDoc (blocks existing ++ blocks doc)
                     Nothing => doc
      putStrLn "✓ Evaluated"
      pure (MkReplState (Just newDoc) state.filename True state.history state.strictMode)
    Failure err _ => do
      putStrLn ("✗ Parse error: " ++ err)
      pure state

-- ============================================================================
-- REPL Loop
-- ============================================================================

covering
replLoop : ReplState -> IO ()
replLoop state = do
  putStr "a2ml> "
  line <- getLine

  case parseCommand line of
    Nothing => do
      putStrLn "Unknown command. Type :help for help."
      replLoop state

    Just Quit => do
      newState <- executeCommand Quit state
      if newState.modified then
        replLoop newState  -- User cancelled quit
      else
        putStrLn "Goodbye!"

    Just cmd => do
      newState <- executeCommand cmd state
      replLoop newState

-- ============================================================================
-- Main Entry Point
-- ============================================================================

covering
main : IO ()
main = do
  putStrLn "╔══════════════════════════════════════════════════════════╗"
  putStrLn "║           A2ML REPL v0.7.0                              ║"
  putStrLn "╚══════════════════════════════════════════════════════════╝"
  putStrLn ""
  putStrLn "Type :help for help, :quit to exit"
  putStrLn ""

  replLoop initialState
