-- | Dead code analysis for PHP
-- Detects unused variables and unreachable code
-- SPDX-License-Identifier: MPL-2.0
module Sanctify.Analysis.DeadCode
    ( -- * Main analysis
      analyzeDeadCode
    , DeadCodeIssue(..)
    , DeadCodeType(..)

      -- * Specific checks
    , findUnusedVariables
    , findUnreachableCode
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad (forM_, mapM_, unless, when)
import Control.Monad.State.Strict
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

import Sanctify.AST

-- | Types of dead code issues
data DeadCodeType
    = UnusedVariable        -- ^ Variable declared but never used
    | UnreachableCode       -- ^ Code after return/throw/exit
    | UnusedParameter       -- ^ Function parameter never used
    | UnusedImport          -- ^ Use statement never referenced
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

-- | A detected dead code issue
data DeadCodeIssue = DeadCodeIssue
    { dcType        :: DeadCodeType
    , dcLocation    :: SourcePos
    , dcDescription :: Text
    , dcIdentifier  :: Text        -- The name of the unused variable/etc.
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

-- | Analysis state for tracking variable usage
data AnalysisState = AnalysisState
    { asDeclared :: Map Text SourcePos   -- ^ Variables that have been declared/assigned
    , asUsed     :: Set Text             -- ^ Variables that have been read
    , asIssues   :: [DeadCodeIssue]      -- ^ Accumulated issues
    }
    deriving stock (Eq, Show)

type AnalysisM = State AnalysisState

-- | Initial analysis state
initialState :: AnalysisState
initialState = AnalysisState Map.empty Set.empty []

-- | Analyze a PHP file for dead code issues
analyzeDeadCode :: PhpFile -> [DeadCodeIssue]
analyzeDeadCode file =
    let finalState = execState (analyzeStatements (phpStatements file)) initialState
        unusedVars = findUnusedFromState finalState
    in unusedVars ++ asIssues finalState

-- | Find unused variables from the final state
findUnusedFromState :: AnalysisState -> [DeadCodeIssue]
findUnusedFromState state =
    let declared = asDeclared state
        used = asUsed state
        unused = Map.filterWithKey (\k _ -> not (Set.member k used)) declared
    in map mkUnusedIssue (Map.toList unused)
  where
    mkUnusedIssue (name, pos) = DeadCodeIssue
        { dcType = UnusedVariable
        , dcLocation = pos
        , dcDescription = "Variable '$" <> name <> "' is assigned but never used"
        , dcIdentifier = name
        }

-- | Analyze a list of statements, detecting unreachable code
analyzeStatements :: [Located Statement] -> AnalysisM ()
analyzeStatements [] = pure ()
analyzeStatements (stmt:rest) = do
    analyzeStatement stmt
    -- Check if current statement is a terminator
    when (isTerminator (locNode stmt)) $ do
        -- Mark remaining statements as unreachable
        forM_ rest $ \(Located pos s) ->
            unless (isNoop s) $
                addIssue DeadCodeIssue
                    { dcType = UnreachableCode
                    , dcLocation = pos
                    , dcDescription = "Unreachable code after " <> terminatorName (locNode stmt)
                    , dcIdentifier = ""
                    }
    -- Continue analyzing rest only if not a terminator
    unless (isTerminator (locNode stmt)) $
        analyzeStatements rest

-- | Check if a statement is a control flow terminator
isTerminator :: Statement -> Bool
isTerminator = \case
    StmtReturn _    -> True
    StmtThrow _     -> True
    StmtBreak _     -> True
    StmtContinue _  -> True
    _               -> False

-- | Get the name of the terminator for error messages
terminatorName :: Statement -> Text
terminatorName = \case
    StmtReturn _   -> "return statement"
    StmtThrow _    -> "throw statement"
    StmtBreak _    -> "break statement"
    StmtContinue _ -> "continue statement"
    _              -> "terminating statement"

-- | Check if a statement is a no-op (empty statement)
isNoop :: Statement -> Bool
isNoop StmtNoop = True
isNoop _ = False

-- | Add an issue to the state
addIssue :: DeadCodeIssue -> AnalysisM ()
addIssue issue = modify' $ \s -> s { asIssues = issue : asIssues s }

-- | Record that a variable was declared/assigned
declareVar :: Text -> SourcePos -> AnalysisM ()
declareVar name pos = modify' $ \s ->
    s { asDeclared = Map.insert name pos (asDeclared s) }

-- | Record that a variable was used/read
useVar :: Text -> AnalysisM ()
useVar name = modify' $ \s ->
    s { asUsed = Set.insert name (asUsed s) }

-- | Analyze a single statement
analyzeStatement :: Located Statement -> AnalysisM ()
analyzeStatement (Located pos stmt) = case stmt of
    StmtExpr expr -> analyzeExpr expr

    StmtIf cond thenStmts elseStmts -> do
        analyzeExpr cond
        -- Analyze branches in isolated scopes for unreachable code
        analyzeStatements thenStmts
        maybe (pure ()) analyzeStatements elseStmts

    StmtWhile cond body -> do
        analyzeExpr cond
        analyzeStatements body

    StmtFor init cond update body -> do
        maybe (pure ()) analyzeExpr init
        maybe (pure ()) analyzeExpr cond
        maybe (pure ()) analyzeExpr update
        analyzeStatements body

    StmtForeach expr keyVar valVar body -> do
        analyzeExpr expr
        -- The foreach variables are declared
        declareVar (varName keyVar) pos
        maybe (pure ()) (\v -> declareVar (varName v) pos) valVar
        analyzeStatements body

    StmtSwitch expr cases -> do
        analyzeExpr expr
        forM_ cases $ \c -> do
            maybe (pure ()) analyzeExpr (caseExpr c)
            analyzeStatements (caseBody c)

    StmtMatch expr arms -> do
        analyzeExpr expr
        forM_ arms $ \arm -> do
            mapM_ analyzeExpr (matchConditions arm)
            analyzeExpr (matchResult arm)

    StmtTry tryBody catches finally -> do
        analyzeStatements tryBody
        forM_ catches $ \c -> do
            -- Catch variable is declared
            maybe (pure ()) (\v -> declareVar (varName v) pos) (catchVar c)
            analyzeStatements (catchBody c)
        maybe (pure ()) analyzeStatements finally

    StmtReturn mexpr -> maybe (pure ()) analyzeExpr mexpr

    StmtThrow expr -> analyzeExpr expr

    StmtEcho exprs -> mapM_ analyzeExpr exprs

    StmtGlobal vars -> forM_ vars $ \v -> useVar (varName v)

    StmtStatic pairs -> forM_ pairs $ \(v, mexpr) -> do
        declareVar (varName v) pos
        maybe (pure ()) analyzeExpr mexpr

    StmtUnset exprs -> mapM_ analyzeExpr exprs

    StmtDecl decl -> analyzeDeclaration pos decl

    StmtDeclare _ body -> analyzeStatements body

    _ -> pure ()

-- | Analyze a declaration
analyzeDeclaration :: SourcePos -> Declaration -> AnalysisM ()
analyzeDeclaration pos decl = case decl of
    DeclFunction{fnParams = params, fnBody = body} -> do
        -- Track parameters
        let paramState = foldl' (\m p -> Map.insert (varName (paramName p)) pos m) Map.empty params
        -- Analyze body with fresh state for unused parameter detection
        oldState <- get
        put $ initialState { asDeclared = paramState }
        analyzeStatements body
        newState <- get
        -- Report unused parameters
        let unusedParams = Map.filterWithKey
                (\k _ -> not (Set.member k (asUsed newState)))
                paramState
        forM_ (Map.toList unusedParams) $ \(name, ppos) ->
            addIssue DeadCodeIssue
                { dcType = UnusedParameter
                , dcLocation = ppos
                , dcDescription = "Parameter '$" <> name <> "' is never used"
                , dcIdentifier = name
                }
        -- Restore outer state, keeping issues
        put $ oldState { asIssues = asIssues newState ++ asIssues oldState }

    DeclClass{clsMembers = members} ->
        forM_ members (analyzeClassMember pos)

    _ -> pure ()

-- | Analyze class members
analyzeClassMember :: SourcePos -> ClassMember -> AnalysisM ()
analyzeClassMember pos member = case member of
    MemberMethod{methParams = params, methBody = Just body} -> do
        let paramState = foldl' (\m p -> Map.insert (varName (paramName p)) pos m) Map.empty params
        oldState <- get
        put $ initialState { asDeclared = paramState }
        analyzeStatements body
        newState <- get
        -- Report unused parameters
        let unusedParams = Map.filterWithKey
                (\k _ -> not (Set.member k (asUsed newState)))
                paramState
        forM_ (Map.toList unusedParams) $ \(name, ppos) ->
            addIssue DeadCodeIssue
                { dcType = UnusedParameter
                , dcLocation = ppos
                , dcDescription = "Parameter '$" <> name <> "' is never used"
                , dcIdentifier = name
                }
        put $ oldState { asIssues = asIssues newState ++ asIssues oldState }

    MemberProperty{propDefault = Just expr} -> analyzeExpr expr

    _ -> pure ()

-- | Analyze an expression, tracking variable usage
analyzeExpr :: Located Expr -> AnalysisM ()
analyzeExpr (Located pos expr) = case expr of
    ExprVariable (Variable name) ->
        -- This is a variable read
        useVar name

    ExprAssign target value -> do
        -- Check if target is a simple variable assignment
        case locNode target of
            ExprVariable (Variable name) -> declareVar name pos
            _ -> analyzeExpr target
        analyzeExpr value

    ExprAssignOp _ target value -> do
        -- Compound assignment both reads and writes
        analyzeExpr target
        analyzeExpr value

    ExprBinary _ left right -> do
        analyzeExpr left
        analyzeExpr right

    ExprUnary _ operand -> analyzeExpr operand

    ExprTernary cond mtrue false -> do
        analyzeExpr cond
        maybe (pure ()) analyzeExpr mtrue
        analyzeExpr false

    ExprCall callee args -> do
        analyzeExpr callee
        mapM_ (analyzeExpr . argValue) args

    ExprMethodCall obj _ args -> do
        analyzeExpr obj
        mapM_ (analyzeExpr . argValue) args

    ExprStaticCall _ _ args ->
        mapM_ (analyzeExpr . argValue) args

    ExprNullsafeMethodCall obj _ args -> do
        analyzeExpr obj
        mapM_ (analyzeExpr . argValue) args

    ExprPropertyAccess obj _ -> analyzeExpr obj

    ExprNullsafePropertyAccess obj _ -> analyzeExpr obj

    ExprArrayAccess base mindex -> do
        analyzeExpr base
        maybe (pure ()) analyzeExpr mindex

    ExprNew _ args ->
        mapM_ (analyzeExpr . argValue) args

    ExprClosure{closureUses = uses, closureBody = body} -> do
        -- Variables in use() clause are used in outer scope
        forM_ uses $ \(v, _) -> useVar (varName v)
        analyzeStatements body

    ExprArrowFunction{arrowExpr = e} -> analyzeExpr e

    ExprCast _ e -> analyzeExpr e

    ExprIsset exprs -> mapM_ analyzeExpr exprs

    ExprEmpty e -> analyzeExpr e

    ExprEval e -> analyzeExpr e

    ExprInclude _ e -> analyzeExpr e

    ExprYield mkey mval -> do
        maybe (pure ()) analyzeExpr mkey
        maybe (pure ()) analyzeExpr mval

    ExprYieldFrom e -> analyzeExpr e

    ExprThrow e -> analyzeExpr e

    ExprLiteral (LitArray pairs) ->
        forM_ pairs $ \(mkey, val) -> do
            maybe (pure ()) analyzeExpr mkey
            analyzeExpr val

    ExprList exprs ->
        forM_ exprs $ maybe (pure ()) analyzeExpr

    _ -> pure ()

-- | Find unused variables in a PHP file (convenience function)
findUnusedVariables :: PhpFile -> [DeadCodeIssue]
findUnusedVariables = filter isUnusedVar . analyzeDeadCode
  where
    isUnusedVar issue = dcType issue `elem` [UnusedVariable, UnusedParameter]

-- | Find unreachable code in a PHP file (convenience function)
findUnreachableCode :: PhpFile -> [DeadCodeIssue]
findUnreachableCode = filter isUnreachable . analyzeDeadCode
  where
    isUnreachable issue = dcType issue == UnreachableCode
