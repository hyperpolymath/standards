-- | Taint tracking analysis for PHP code
-- SPDX-License-Identifier: AGPL-3.0-or-later
module Sanctify.Analysis.Taint
    ( -- * Taint analysis
      TaintAnalysis(..)
    , TaintSource(..)
    , TaintSink(..)
    , TaintFlow(..)

      -- * Running analysis
    , analyzeTaint
    , findTaintFlows

      -- * Taint queries
    , isTainted
    , getTaintSources
    , hasDangerousFlow
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

import Sanctify.AST

-- | Sources of tainted data
data TaintSource
    = SourceGet           -- ^ $_GET
    | SourcePost          -- ^ $_POST
    | SourceRequest       -- ^ $_REQUEST
    | SourceCookie        -- ^ $_COOKIE
    | SourceServer        -- ^ $_SERVER
    | SourceFiles         -- ^ $_FILES
    | SourceSession       -- ^ $_SESSION (partially trusted)
    | SourceDatabase      -- ^ Data from database queries
    | SourceFileRead      -- ^ Data read from files
    | SourceExternalApi   -- ^ Data from external APIs
    | SourceUserCallback  -- ^ Data from user-provided callbacks
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON)

-- | Dangerous sinks where tainted data shouldn't flow
data TaintSink
    = SinkSqlQuery        -- ^ SQL queries
    | SinkHtmlOutput      -- ^ HTML output (echo, print)
    | SinkShellExec       -- ^ Shell execution
    | SinkFileInclude     -- ^ File inclusion
    | SinkFileWrite       -- ^ File writing
    | SinkEval            -- ^ eval() and similar
    | SinkUnserialize     -- ^ unserialize()
    | SinkHeader          -- ^ HTTP headers
    | SinkRedirect        -- ^ Redirects
    | SinkReflection      -- ^ Reflection APIs
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON)

-- | A taint flow from source to sink
data TaintFlow = TaintFlow
    { flowSource     :: TaintSource
    , flowSink       :: TaintSink
    , flowPath       :: [Text]        -- ^ Variable/expression path
    , flowSanitized  :: Bool          -- ^ Was it sanitized along the way?
    , flowSanitizers :: [Text]        -- ^ Sanitizers applied
    , flowLocation   :: SourcePos
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

-- | Taint analysis state
data TaintAnalysis = TaintAnalysis
    { taintedVars    :: Map Text (Set TaintSource)
    , taintFlows     :: [TaintFlow]
    , sanitizedVars  :: Set Text
    }
    deriving stock (Eq, Show, Generic)

-- | Initial empty analysis
emptyTaintAnalysis :: TaintAnalysis
emptyTaintAnalysis = TaintAnalysis Map.empty [] Set.empty

type TaintM = State TaintAnalysis

-- | Analyze a PHP file for taint flows
analyzeTaint :: PhpFile -> TaintAnalysis
analyzeTaint file = execState (mapM_ analyzeStatement (phpStatements file)) emptyTaintAnalysis

-- | Analyze statement for taint
analyzeStatement :: Located Statement -> TaintM ()
analyzeStatement (Located pos stmt) = case stmt of
    StmtExpr expr -> analyzeExpr pos expr

    StmtEcho exprs -> forM_ exprs $ \expr -> do
        sources <- getExprTaint expr
        unless (Set.null sources) $
            recordFlow sources SinkHtmlOutput pos (exprToPath expr)

    StmtReturn (Just expr) -> void $ getExprTaint expr

    StmtIf cond thenStmts elseStmts -> do
        void $ getExprTaint cond
        mapM_ analyzeStatement thenStmts
        maybe (pure ()) (mapM_ analyzeStatement) elseStmts

    StmtWhile cond body -> do
        void $ getExprTaint cond
        mapM_ analyzeStatement body

    StmtFor mInit mCond mUpdate body -> do
        maybe (pure ()) (void . getExprTaint) mInit
        maybe (pure ()) (void . getExprTaint) mCond
        maybe (pure ()) (void . getExprTaint) mUpdate
        mapM_ analyzeStatement body

    StmtForeach expr _ _ body -> do
        void $ getExprTaint expr
        mapM_ analyzeStatement body

    StmtDecl decl -> analyzeDeclaration pos decl

    _ -> pure ()

-- | Analyze declaration
analyzeDeclaration :: SourcePos -> Declaration -> TaintM ()
analyzeDeclaration pos decl = case decl of
    DeclFunction{fnBody = body} -> mapM_ analyzeStatement body
    DeclClass{clsMembers = members} -> mapM_ (analyzeMember pos) members
    _ -> pure ()

-- | Analyze class member
analyzeMember :: SourcePos -> ClassMember -> TaintM ()
analyzeMember pos member = case member of
    MemberMethod{methBody = Just body} -> mapM_ analyzeStatement body
    _ -> pure ()

-- | Analyze expression for taint
analyzeExpr :: SourcePos -> Located Expr -> TaintM ()
analyzeExpr pos expr = do
    sources <- getExprTaint expr
    checkSinks pos expr sources

-- | Get taint sources for an expression
getExprTaint :: Located Expr -> TaintM (Set TaintSource)
getExprTaint (Located _ expr) = case expr of
    -- Superglobals are tainted
    ExprVariable (Variable name) -> do
        case name of
            "_GET" -> pure $ Set.singleton SourceGet
            "_POST" -> pure $ Set.singleton SourcePost
            "_REQUEST" -> pure $ Set.singleton SourceRequest
            "_COOKIE" -> pure $ Set.singleton SourceCookie
            "_SERVER" -> pure $ Set.singleton SourceServer
            "_FILES" -> pure $ Set.singleton SourceFiles
            "_SESSION" -> pure $ Set.singleton SourceSession
            _ -> do
                analysis <- get
                pure $ Map.findWithDefault Set.empty name (taintedVars analysis)

    -- Array access propagates taint
    ExprArrayAccess base _ -> getExprTaint base

    -- Binary operations propagate taint
    ExprBinary _ left right -> do
        lt <- getExprTaint left
        rt <- getExprTaint right
        pure $ Set.union lt rt

    -- Assignment propagates taint to target
    ExprAssign target value -> do
        sources <- getExprTaint value
        case locNode target of
            ExprVariable (Variable name) -> do
                modify $ \s -> s { taintedVars = Map.insert name sources (taintedVars s) }
            _ -> pure ()
        pure sources

    -- Function calls may sanitize or introduce taint
    ExprCall callee args -> do
        argTaint <- Set.unions <$> mapM (getExprTaint . argValue) args
        callTaint callee argTaint

    -- Method calls
    ExprMethodCall obj _ args -> do
        objTaint <- getExprTaint obj
        argTaint <- Set.unions <$> mapM (getExprTaint . argValue) args
        pure $ Set.union objTaint argTaint

    _ -> pure Set.empty

-- | Handle function call taint
callTaint :: Located Expr -> Set TaintSource -> TaintM (Set TaintSource)
callTaint (Located _ (ExprConstant qn)) argTaint = do
    let fname = T.toLower $ unName $ last $ qnParts qn

    -- Sanitizers remove taint
    if isSanitizer fname
        then pure Set.empty
        else pure argTaint
  where
    isSanitizer :: Text -> Bool
    isSanitizer fn = fn `elem`
        [ "esc_html", "esc_attr", "esc_url", "esc_js", "esc_textarea", "esc_sql"
        , "htmlspecialchars", "htmlentities"
        , "sanitize_text_field", "sanitize_email", "sanitize_url", "sanitize_file_name"
        , "wp_kses", "wp_kses_post", "wp_kses_data"
        , "intval", "absint", "floatval"
        , "strip_tags", "stripslashes"
        , "addslashes", "mysqli_real_escape_string", "pg_escape_string"
        ]
callTaint _ argTaint = pure argTaint

-- | Check if expression flows to a dangerous sink
checkSinks :: SourcePos -> Located Expr -> Set TaintSource -> TaintM ()
checkSinks pos (Located _ expr) sources = case expr of
    ExprCall callee args -> do
        let fname = getFunctionName callee
        let sink = classifySink fname
        case sink of
            Just s -> unless (Set.null sources) $
                recordFlow sources s pos [fname]
            Nothing -> pure ()

    ExprEval _ -> unless (Set.null sources) $
        recordFlow sources SinkEval pos ["eval"]

    ExprShellExec _ -> unless (Set.null sources) $
        recordFlow sources SinkShellExec pos ["shell_exec"]

    _ -> pure ()

-- | Classify a function as a sink
classifySink :: Text -> Maybe TaintSink
classifySink fname
    | fname `elem` ["mysql_query", "mysqli_query", "pg_query", "sqlite_query"] = Just SinkSqlQuery
    | fname `elem` ["echo", "print", "printf"] = Just SinkHtmlOutput
    | fname `elem` ["exec", "shell_exec", "system", "passthru", "popen", "proc_open"] = Just SinkShellExec
    | fname `elem` ["include", "include_once", "require", "require_once"] = Just SinkFileInclude
    | fname `elem` ["file_put_contents", "fwrite", "fputs"] = Just SinkFileWrite
    | fname `elem` ["eval", "assert", "create_function"] = Just SinkEval
    | fname == "unserialize" = Just SinkUnserialize
    | fname == "header" = Just SinkHeader
    | fname `elem` ["wp_redirect", "wp_safe_redirect", "header"] = Just SinkRedirect
    | otherwise = Nothing

-- | Get function name from callee expression
getFunctionName :: Located Expr -> Text
getFunctionName (Located _ (ExprConstant qn)) = unName $ last $ qnParts qn
getFunctionName _ = ""

-- | Record a taint flow
recordFlow :: Set TaintSource -> TaintSink -> SourcePos -> [Text] -> TaintM ()
recordFlow sources sink pos path = do
    forM_ (Set.toList sources) $ \source ->
        modify $ \s -> s
            { taintFlows = TaintFlow source sink path False [] pos : taintFlows s
            }

-- | Convert expression to path for reporting
exprToPath :: Located Expr -> [Text]
exprToPath (Located _ expr) = case expr of
    ExprVariable (Variable name) -> ["$" <> name]
    ExprArrayAccess base (Just key) -> exprToPath base ++ ["[" <> keyToText key <> "]"]
    ExprArrayAccess base Nothing -> exprToPath base ++ ["[]"]
    ExprMethodCall obj (Name method) _ -> exprToPath obj ++ ["->" <> method]
    ExprPropertyAccess obj (Name prop) -> exprToPath obj ++ ["->" <> prop]
    ExprCall callee _ -> exprToPath callee ++ ["()"]
    ExprConstant qn -> [T.intercalate "\\" $ map unName $ qnParts qn]
    _ -> ["<expr>"]
  where
    keyToText (Located _ (ExprLiteral (LitString s))) = "'" <> s <> "'"
    keyToText (Located _ (ExprLiteral (LitInt n))) = T.pack (show n)
    keyToText _ = "..."

-- | Find all dangerous taint flows
findTaintFlows :: TaintAnalysis -> [TaintFlow]
findTaintFlows = filter (not . flowSanitized) . taintFlows

-- | Check if a variable is tainted
isTainted :: Text -> TaintAnalysis -> Bool
isTainted var analysis = not $ Set.null $ Map.findWithDefault Set.empty var (taintedVars analysis)

-- | Get taint sources for a variable
getTaintSources :: Text -> TaintAnalysis -> Set TaintSource
getTaintSources var analysis = Map.findWithDefault Set.empty var (taintedVars analysis)

-- | Check if there's a dangerous (unsanitized) flow to critical sink
hasDangerousFlow :: TaintAnalysis -> Bool
hasDangerousFlow analysis = any isDangerous (taintFlows analysis)
  where
    isDangerous flow = not (flowSanitized flow) && isCriticalSink (flowSink flow)
    isCriticalSink sink = sink `elem` [SinkSqlQuery, SinkShellExec, SinkEval, SinkFileInclude]
