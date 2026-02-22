-- | WordPress-specific constraints and best practices
-- SPDX-License-Identifier: AGPL-3.0-or-later
module Sanctify.WordPress.Constraints
    ( -- * Constraint checking
      WordPressIssue(..)
    , WpIssueType(..)
    , checkWordPressConstraints
    , isWordPressCode

      -- * Safe function mappings
    , wpEscapeFunctions
    , wpSanitizeFunctions
    , wpNonceFunctions
    , wpHookFunctions
    , wpDatabaseFunctions

      -- * WordPress-specific transforms
    , wrapWithEscHtml
    , wrapWithEscAttr
    , wrapWithEscUrl
    , addNonceCheck
    , useWpdbPrepare
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.Writer
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

import Sanctify.AST
import Sanctify.Analysis.Security (Severity(..))

-- | WordPress-specific issue types
data WpIssueType
    = MissingEscaping           -- ^ Output not escaped
    | MissingSanitization       -- ^ Input not sanitized
    | MissingNonce              -- ^ Form/AJAX without nonce
    | MissingCapabilityCheck    -- ^ No current_user_can() check
    | DirectDatabaseQuery       -- ^ Using $wpdb without prepare()
    | DirectFileAccess          -- ^ Missing ABSPATH check
    | NonPrefixedFunction       -- ^ Function without plugin prefix
    | NonPrefixedHook           -- ^ Hook without plugin prefix
    | DeprecatedFunction        -- ^ Using deprecated WP function
    | UnsafeOptionStorage       -- ^ Storing sensitive data in options
    | MissingTextDomain         -- ^ i18n string without text domain
    | DirectSuperglobalAccess   -- ^ Using $_GET instead of $_REQUEST or wp functions
    | UnsafeRedirect            -- ^ wp_redirect without exit
    | MissingPluginHeaders      -- ^ Plugin file missing required headers
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

-- | A WordPress-specific issue
data WordPressIssue = WordPressIssue
    { wpIssueType     :: WpIssueType
    , wpSeverity      :: Severity
    , wpLocation      :: SourcePos
    , wpDescription   :: Text
    , wpRemedy        :: Text
    , wpAutoFixable   :: Bool       -- ^ Can be automatically fixed
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

type WpAnalysisM = Writer [WordPressIssue]

-- | WordPress escaping functions (output context -> function)
wpEscapeFunctions :: Map Text Text
wpEscapeFunctions = Map.fromList
    [ ("html", "esc_html")
    , ("attr", "esc_attr")
    , ("url", "esc_url")
    , ("js", "esc_js")
    , ("textarea", "esc_textarea")
    , ("sql", "esc_sql")  -- But prefer $wpdb->prepare()
    , ("html_e", "esc_html_e")  -- Echo variants
    , ("attr_e", "esc_attr_e")
    ]

-- | WordPress sanitization functions (input type -> function)
wpSanitizeFunctions :: Map Text Text
wpSanitizeFunctions = Map.fromList
    [ ("text", "sanitize_text_field")
    , ("textarea", "sanitize_textarea_field")
    , ("email", "sanitize_email")
    , ("url", "sanitize_url")
    , ("title", "sanitize_title")
    , ("filename", "sanitize_file_name")
    , ("key", "sanitize_key")
    , ("html", "wp_kses_post")
    , ("int", "absint")
    ]

-- | WordPress nonce functions
wpNonceFunctions :: Set Text
wpNonceFunctions = Set.fromList
    [ "wp_nonce_field"
    , "wp_nonce_url"
    , "wp_create_nonce"
    , "wp_verify_nonce"
    , "check_admin_referer"
    , "check_ajax_referer"
    ]

-- | WordPress hook functions
wpHookFunctions :: Set Text
wpHookFunctions = Set.fromList
    [ "add_action"
    , "add_filter"
    , "remove_action"
    , "remove_filter"
    , "do_action"
    , "apply_filters"
    , "has_action"
    , "has_filter"
    ]

-- | Safe WordPress database functions
wpDatabaseFunctions :: Set Text
wpDatabaseFunctions = Set.fromList
    [ "prepare"
    , "insert"
    , "update"
    , "delete"
    , "replace"
    , "get_var"
    , "get_row"
    , "get_col"
    , "get_results"
    ]

-- | Check if code appears to be WordPress code
isWordPressCode :: PhpFile -> Bool
isWordPressCode file =
    -- Look for WordPress indicators
    any isWordPressIndicator (phpStatements file)
  where
    isWordPressIndicator :: Located Statement -> Bool
    isWordPressIndicator (Located _ stmt) = case stmt of
        StmtDecl (DeclFunction{fnName = Name name}) ->
            any (`T.isInfixOf` name) ["wp_", "wordpress", "plugin", "theme"]
        StmtExpr (Located _ (ExprCall (Located _ (ExprConstant qn)) _)) ->
            let fn = unName $ last $ qnParts qn
            in fn `Set.member` wpHookFunctions || fn `T.isPrefixOf` "wp_"
        _ -> False

-- | Check WordPress constraints
checkWordPressConstraints :: PhpFile -> [WordPressIssue]
checkWordPressConstraints file = execWriter $ do
    -- Check for ABSPATH protection in plugin/theme files
    checkAbspathProtection file

    -- Analyze all statements
    mapM_ analyzeWpStatement (phpStatements file)

-- | Check for ABSPATH protection at file start
checkAbspathProtection :: PhpFile -> WpAnalysisM ()
checkAbspathProtection file =
    unless (hasAbspathCheck file) $
        tell [WordPressIssue
            { wpIssueType = DirectFileAccess
            , wpSeverity = Medium
            , wpLocation = SourcePos "" 1 1
            , wpDescription = "Plugin/theme file can be accessed directly"
            , wpRemedy = "Add: defined('ABSPATH') || exit;"
            , wpAutoFixable = True
            }]

hasAbspathCheck :: PhpFile -> Bool
hasAbspathCheck file = case phpStatements file of
    [] -> False
    (Located _ stmt : _) -> isAbspathCheck stmt
  where
    isAbspathCheck :: Statement -> Bool
    isAbspathCheck (StmtIf cond _ _) = isAbspathCondition (locNode cond)
    isAbspathCheck (StmtExpr (Located _ (ExprBinary OpOr left right))) =
        isAbspathCondition (locNode left) || isExitCall (locNode right)
    isAbspathCheck _ = False

    isAbspathCondition :: Expr -> Bool
    isAbspathCondition (ExprUnary OpNot (Located _ (ExprCall (Located _ (ExprConstant qn)) _))) =
        unName (last (qnParts qn)) == "defined"
    isAbspathCondition (ExprCall (Located _ (ExprConstant qn)) _) =
        unName (last (qnParts qn)) == "defined"
    isAbspathCondition _ = False

    isExitCall :: Expr -> Bool
    isExitCall (ExprCall (Located _ (ExprConstant qn)) _) =
        unName (last (qnParts qn)) `elem` ["exit", "die"]
    isExitCall _ = False

-- | Analyze a WordPress statement
analyzeWpStatement :: Located Statement -> WpAnalysisM ()
analyzeWpStatement (Located pos stmt) = case stmt of
    StmtEcho exprs -> mapM_ (checkWpEscaping pos) exprs

    StmtExpr expr -> analyzeWpExpr expr

    StmtIf cond thenStmts elseStmts -> do
        analyzeWpExpr cond
        mapM_ analyzeWpStatement thenStmts
        maybe (pure ()) (mapM_ analyzeWpStatement) elseStmts

    StmtDecl decl -> analyzeWpDeclaration pos decl

    _ -> pure ()

-- | Analyze a WordPress expression
analyzeWpExpr :: Located Expr -> WpAnalysisM ()
analyzeWpExpr (Located pos expr) = case expr of
    -- Check for direct superglobal access
    ExprArrayAccess (Located _ (ExprVariable (Variable name))) _ ->
        when (name `elem` ["_GET", "_POST", "_REQUEST"]) $
            tell [WordPressIssue
                { wpIssueType = DirectSuperglobalAccess
                , wpSeverity = Medium
                , wpLocation = pos
                , wpDescription = "Direct access to $" <> name <> " without sanitization"
                , wpRemedy = "Use sanitize_text_field($_" <> name <> "['key']) or wp_unslash()"
                , wpAutoFixable = True
                }]

    -- Check for wp_redirect without exit
    ExprCall (Located _ (ExprConstant qn)) _ -> do
        let fn = unName $ last $ qnParts qn
        when (fn == "wp_redirect" || fn == "wp_safe_redirect") $
            tell [WordPressIssue
                { wpIssueType = UnsafeRedirect
                , wpSeverity = Medium
                , wpLocation = pos
                , wpDescription = fn <> "() should be followed by exit"
                , wpRemedy = "Add exit; after " <> fn <> "()"
                , wpAutoFixable = True
                }]

    -- Check for direct $wpdb queries
    ExprMethodCall obj (Name method) args -> do
        when (isWpdbVariable obj && method `elem` ["query", "get_results", "get_var", "get_row", "get_col"]) $
            unless (hasWpdbPrepare args) $
                tell [WordPressIssue
                    { wpIssueType = DirectDatabaseQuery
                    , wpSeverity = High
                    , wpLocation = pos
                    , wpDescription = "Database query without $wpdb->prepare()"
                    , wpRemedy = "Use $wpdb->prepare() with placeholders"
                    , wpAutoFixable = False
                    }]

    -- Check i18n functions for text domain
    ExprCall (Located _ (ExprConstant qn)) args -> do
        let fn = unName $ last $ qnParts qn
        when (fn `elem` ["__", "_e", "_x", "_n", "esc_html__", "esc_html_e", "esc_attr__", "esc_attr_e"]) $
            unless (hasTextDomainArg fn args) $
                tell [WordPressIssue
                    { wpIssueType = MissingTextDomain
                    , wpSeverity = Low
                    , wpLocation = pos
                    , wpDescription = "i18n function " <> fn <> "() missing text domain"
                    , wpRemedy = "Add text domain as the last argument"
                    , wpAutoFixable = True
                    }]

    _ -> pure ()

-- | Check if a variable is $wpdb
isWpdbVariable :: Located Expr -> Bool
isWpdbVariable (Located _ (ExprVariable (Variable "wpdb"))) = True
isWpdbVariable _ = False

-- | Check if arguments include $wpdb->prepare()
hasWpdbPrepare :: [Argument] -> Bool
hasWpdbPrepare args = case args of
    [] -> False
    (Argument _ (Located _ (ExprMethodCall obj (Name "prepare") _)) _ : _) ->
        isWpdbVariable obj
    _ -> False

-- | Check if i18n function has text domain argument
hasTextDomainArg :: Text -> [Argument] -> Bool
hasTextDomainArg fn args =
    let requiredArgs = case fn of
            "__"          -> 2
            "_e"          -> 2
            "esc_html__"  -> 2
            "esc_html_e"  -> 2
            "esc_attr__"  -> 2
            "esc_attr_e"  -> 2
            "_x"          -> 3
            "_n"          -> 4
            _             -> 2
    in length args >= requiredArgs

-- | Analyze WordPress declarations
analyzeWpDeclaration :: SourcePos -> Declaration -> WpAnalysisM ()
analyzeWpDeclaration pos decl = case decl of
    DeclFunction{fnName = Name name, fnBody = body} -> do
        -- Check for function prefix
        unless (hasValidPrefix name) $
            tell [WordPressIssue
                { wpIssueType = NonPrefixedFunction
                , wpSeverity = Low
                , wpLocation = pos
                , wpDescription = "Function '" <> name <> "' should have a plugin prefix"
                , wpRemedy = "Prefix function name to avoid conflicts"
                , wpAutoFixable = False
                }]
        mapM_ analyzeWpStatement body

    _ -> pure ()

-- | Check for valid WordPress function prefix
hasValidPrefix :: Text -> Bool
hasValidPrefix name =
    -- Allow common WordPress patterns
    T.isPrefixOf "wp_" name ||
    T.isPrefixOf "wc_" name ||  -- WooCommerce
    T.isPrefixOf "_" name ||    -- Private functions
    T.isInfixOf "_" name        -- Namespaced (plugin_name_function)

-- | Check WordPress-safe escaping
checkWpEscaping :: SourcePos -> Located Expr -> WpAnalysisM ()
checkWpEscaping pos expr =
    unless (isEscaped expr) $
        when (containsVariable expr) $
            tell [WordPressIssue
                { wpIssueType = MissingEscaping
                , wpSeverity = High
                , wpLocation = pos
                , wpDescription = "Output may not be properly escaped"
                , wpRemedy = "Wrap with esc_html(), esc_attr(), or esc_url()"
                , wpAutoFixable = True
                }]

-- | Check if expression is already escaped
isEscaped :: Located Expr -> Bool
isEscaped (Located _ (ExprCall (Located _ (ExprConstant qn)) _)) =
    let fn = unName $ last $ qnParts qn
    in fn `elem` Map.elems wpEscapeFunctions || T.isPrefixOf "esc_" fn
isEscaped _ = False

-- | Check if expression contains a variable
containsVariable :: Located Expr -> Bool
containsVariable (Located _ expr) = case expr of
    ExprVariable _ -> True
    ExprArrayAccess base _ -> containsVariable base
    ExprBinary _ left right -> containsVariable left || containsVariable right
    ExprCall _ args -> any (containsVariable . argValue) args
    ExprMethodCall obj _ args -> containsVariable obj || any (containsVariable . argValue) args
    _ -> False

-- | Transform: wrap expression with esc_html()
wrapWithEscHtml :: Located Expr -> Located Expr
wrapWithEscHtml expr@(Located pos _) =
    Located pos $ ExprCall
        (Located pos $ ExprConstant $ QualifiedName [Name "esc_html"] False)
        [Argument Nothing expr False]

-- | Transform: wrap expression with esc_attr()
wrapWithEscAttr :: Located Expr -> Located Expr
wrapWithEscAttr expr@(Located pos _) =
    Located pos $ ExprCall
        (Located pos $ ExprConstant $ QualifiedName [Name "esc_attr"] False)
        [Argument Nothing expr False]

-- | Transform: wrap expression with esc_url()
wrapWithEscUrl :: Located Expr -> Located Expr
wrapWithEscUrl expr@(Located pos _) =
    Located pos $ ExprCall
        (Located pos $ ExprConstant $ QualifiedName [Name "esc_url"] False)
        [Argument Nothing expr False]

-- | Transform: add nonce verification check
addNonceCheck :: Text -> Located Statement -> Located Statement
addNonceCheck nonceName body@(Located pos _) =
    Located pos $ StmtIf
        (Located pos $ ExprUnary OpNot $ Located pos $ ExprCall
            (Located pos $ ExprConstant $ QualifiedName [Name "wp_verify_nonce"] False)
            [ Argument Nothing
                (Located pos $ ExprArrayAccess
                    (Located pos $ ExprVariable $ Variable "_POST")
                    (Just $ Located pos $ ExprLiteral $ LitString nonceName))
                False
            , Argument Nothing (Located pos $ ExprLiteral $ LitString "nonce_action") False
            ])
        [Located pos $ StmtExpr $ Located pos $ ExprCall
            (Located pos $ ExprConstant $ QualifiedName [Name "wp_die"] False)
            [Argument Nothing (Located pos $ ExprLiteral $ LitString "Security check failed") False]]
        (Just [body])

-- | Transform: wrap SQL with $wpdb->prepare()
useWpdbPrepare :: Located Expr -> [Located Expr] -> Located Expr
useWpdbPrepare query@(Located pos _) params =
    Located pos $ ExprMethodCall
        (Located pos $ ExprVariable $ Variable "wpdb")
        (Name "prepare")
        (Argument Nothing query False : map (\p -> Argument Nothing p False) params)
