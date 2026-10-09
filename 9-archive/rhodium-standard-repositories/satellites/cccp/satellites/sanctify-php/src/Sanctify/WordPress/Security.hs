{-# LANGUAGE OverloadedStrings #-}
-- | WordPress-Specific Security Deep Analysis
-- SPDX-License-Identifier: MPL-2.0
module Sanctify.WordPress.Security
    ( -- * WordPress security analysis
      analyzeWordPressSecurity
    , WordPressIssue(..)
    , WordPressIssueType(..)

      -- * Specific WordPress checks
    , checkNonceVerification
    , checkCapabilityChecks
    , checkDatabaseQueries
    , checkHookPriorities
    , checkAjaxSecurity
    , checkRestApiSecurity
    , checkFileUploadSecurity
    , checkUserMetaSecurity
    , checkTransientSecurity
    , checkCronSecurity
    , checkShortcodeSecurity
    , checkWidgetSecurity
    , checkGutenbergBlockSecurity
    , checkI18nSecurity
    , checkEscapingContext
    , checkCustomTableSecurity
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (unless, when)
import Control.Monad.Writer
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Data.Maybe (catMaybes)

import Sanctify.AST
import Sanctify.Analysis.Security (Severity(..))

-- | WordPress-specific issue types
data WordPressIssueType
    = MissingNonce
    | MissingCapability
    | InsecureDirectoryQuery
    | UnsafeMeta
    | InsecureAjax
    | InsecureRestApi
    | InsecureFileUpload
    | InsecureTransient
    | InsecureCron
    | ShortcodeXSS
    | WidgetXSS
    | BlockXSS
    | MissingTextDomain
    | ImproperEscaping
    | HookPriorityConflict
    | CustomTableInjection
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

-- | WordPress security issue
data WordPressIssue = WordPressIssue
    { wpType        :: WordPressIssueType
    , wpSeverity    :: Severity
    , wpLocation    :: SourcePos
    , wpDescription :: Text
    , wpRemedy      :: Text
    , wpCode        :: Maybe Text
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

type WordPressM = Writer [WordPressIssue]

-- | Analyze WordPress-specific security issues
analyzeWordPressSecurity :: PhpFile -> [WordPressIssue]
analyzeWordPressSecurity file = execWriter $ do
    mapM_ analyzeStatement (phpStatements file)
    checkNonceUsage file
    checkCapabilityUsage file

-- | Analyze a statement for WordPress issues
analyzeStatement :: Located Statement -> WordPressM ()
analyzeStatement (Located pos stmt) = case stmt of
    StmtExpr expr -> analyzeExpr pos expr
    StmtIf cond thenStmts elseStmts -> do
        analyzeExpr pos cond
        mapM_ analyzeStatement thenStmts
        maybe (pure ()) (mapM_ analyzeStatement) elseStmts
    StmtDecl decl -> analyzeDeclaration pos decl
    _ -> pure ()

-- | Analyze a declaration
analyzeDeclaration :: SourcePos -> Declaration -> WordPressM ()
analyzeDeclaration pos decl = case decl of
    DeclFunction{fnBody = body} -> mapM_ analyzeStatement body
    DeclClass{clsMembers = members} -> mapM_ (analyzeClassMember pos) members
    _ -> pure ()

-- | Analyze class members
analyzeClassMember :: SourcePos -> ClassMember -> WordPressM ()
analyzeClassMember pos member = case member of
    MemberMethod{methBody = Just body} -> mapM_ analyzeStatement body
    _ -> pure ()

-- | Analyze an expression for WordPress issues
analyzeExpr :: SourcePos -> Located Expr -> WordPressM ()
analyzeExpr pos (Located _ expr) = case expr of
    ExprCall callee args -> checkWordPressFunction pos callee args
    ExprMethodCall obj name args -> checkWordPressMethod pos obj name args
    _ -> pure ()

-- | Check WordPress function calls
checkWordPressFunction :: SourcePos -> Located Expr -> [Argument] -> WordPressM ()
checkWordPressFunction pos (Located _ (ExprConstant qn)) args = do
    let fname = T.toLower $ unName $ last $ qnParts qn

    -- Check database queries
    when (fname `elem` wpdbFunctions && not (isWpdbPrepare args)) $
        tell [WordPressIssue CustomTableInjection Critical pos
            ("Custom table query without preparation: " <> fname)
            "Use $wpdb->prepare() for all queries with variables"
            (Just fname)]

    -- Check AJAX actions
    when (fname `elem` ["add_action"] && isAjaxAction args && not (hasNonceInContext fname)) $
        tell [WordPressIssue InsecureAjax High pos
            "AJAX action without nonce verification"
            "Add check_ajax_referer() at the beginning of the handler"
            Nothing]

    -- Check REST API registration
    when (fname == "register_rest_route" && not (hasPermissionCallback args)) $
        tell [WordPressIssue InsecureRestApi High pos
            "REST API route without permission_callback"
            "Add 'permission_callback' to check user capabilities"
            (Just "register_rest_route")]

    -- Check file uploads
    when (fname == "move_uploaded_file" && not (hasFileValidation args)) $
        tell [WordPressIssue InsecureFileUpload Critical pos
            "File upload without validation"
            "Validate file type, size, and use wp_handle_upload()"
            (Just "move_uploaded_file")]

    -- Check transients
    when (fname `elem` ["set_transient", "set_site_transient"] && hasUserInput args) $
        tell [WordPressIssue InsecureTransient Medium pos
            "Transient set with unsanitized user input"
            "Sanitize data before storing in transients"
            (Just fname)]

    -- Check cron
    when (fname == "wp_schedule_single_event" && hasUserInput args) $
        tell [WordPressIssue InsecureCron High pos
            "Cron event scheduled with user-controlled data"
            "Validate cron arguments and sanitize callback parameters"
            (Just "wp_schedule_single_event")]

    -- Check user meta
    when (fname `elem` ["update_user_meta", "add_user_meta"] && not (hasMetaKeyValidation args)) $
        tell [WordPressIssue UnsafeMeta Medium pos
            ("User meta update without key validation: " <> fname)
            "Validate meta keys against whitelist"
            (Just fname)]

checkWordPressFunction _ _ _ = pure ()

-- | Check WordPress method calls
checkWordPressMethod :: SourcePos -> Located Expr -> Name -> [Argument] -> WordPressM ()
checkWordPressMethod pos obj (Name method) args = do
    let lmethod = T.toLower method

    -- Check $wpdb methods
    when (isWpdbObject obj) $ do
        when (lmethod `elem` ["query", "get_results", "get_row", "get_col", "get_var"] &&
              not (null args) && hasVariableInQuery (head args)) $
            tell [WordPressIssue InsecureDirectoryQuery Critical pos
                ("$wpdb->" <> method <> " with variable: use prepare()")
                "Always use $wpdb->prepare() for queries with variables"
                (Just $ "$wpdb->" <> method)]

-- | Check for nonce verification in file
checkNonceUsage :: PhpFile -> WordPressM ()
checkNonceUsage file =
    when (hasFormProcessing file && not (hasNonceVerification file)) $
        tell [WordPressIssue MissingNonce High (mkPos file)
            "Form processing without nonce verification"
            "Add wp_verify_nonce() or check_admin_referer()"
            Nothing]

-- | Check for capability checks in admin pages
checkCapabilityUsage :: PhpFile -> WordPressM ()
checkCapabilityUsage file =
    when (hasAdminPageRegistration file && not (hasCapabilityCheck file)) $
        tell [WordPressIssue MissingCapability High (mkPos file)
            "Admin page without capability check"
            "Add current_user_can() check"
            Nothing]

-- | Check nonce verification specifically
checkNonceVerification :: Located Expr -> Maybe WordPressIssue
checkNonceVerification (Located pos expr) = case expr of
    ExprCall (Located _ (ExprConstant qn)) _
        | fname `elem` nonceVerifyFunctions -> Nothing  -- Good!
      where fname = T.toLower $ unName $ last $ qnParts qn
    _ -> Just $ WordPressIssue MissingNonce High pos
        "Missing nonce verification in form handler"
        "Add wp_verify_nonce(), check_admin_referer(), or check_ajax_referer()"
        Nothing
  where
    nonceVerifyFunctions = ["wp_verify_nonce", "check_admin_referer", "check_ajax_referer"]

-- | Check capability checks
checkCapabilityChecks :: Located Expr -> Maybe WordPressIssue
checkCapabilityChecks (Located pos expr) = case expr of
    ExprCall (Located _ (ExprConstant qn)) _
        | fname `elem` capabilityFunctions -> Nothing  -- Good!
      where fname = T.toLower $ unName $ last $ qnParts qn
    _ -> Just $ WordPressIssue MissingCapability High pos
        "Missing capability check before sensitive operation"
        "Add current_user_can(), user_can(), or is_admin() check"
        Nothing
  where
    capabilityFunctions = ["current_user_can", "user_can", "is_admin", "is_super_admin"]

-- | Check $wpdb queries for SQL injection
checkDatabaseQueries :: Located Expr -> [WordPressIssue]
checkDatabaseQueries (Located pos expr) = case expr of
    ExprMethodCall obj (Name method) args
        | isWpdbObject obj
        , T.toLower method `elem` ["query", "get_results", "get_row", "get_col", "get_var"]
        , not (null args)
        , hasVariableInQuery (head args) ->
            [WordPressIssue InsecureDirectoryQuery Critical pos
                ("Unsafe $wpdb->" <> method <> ": variable in query")
                "Use $wpdb->prepare() with placeholders (%s, %d, %f)"
                (Just $ "$wpdb->" <> method)]
    _ -> []

-- | Check hook priorities for conflicts
checkHookPriorities :: PhpFile -> [WordPressIssue]
checkHookPriorities file = execWriter $ mapM_ checkStmt (phpStatements file)
  where
    checkStmt :: Located Statement -> Writer [WordPressIssue] ()
    checkStmt (Located pos stmt) = case stmt of
        StmtExpr (Located _ (ExprCall (Located _ (ExprConstant qn)) args))
            | fname `elem` ["add_action", "add_filter"]
            , length args >= 3 ->
                case args !! 2 of
                    Argument _ (Located _ (ExprLiteral (LitInt prio))) _
                        | prio < 0 || prio > 1000 ->
                            tell [WordPressIssue HookPriorityConflict Medium pos
                                ("Unusual hook priority: " <> T.pack (show prio))
                                "Use standard priorities (1-20) unless necessary"
                                (Just $ fname <> " with priority " <> T.pack (show prio))]
                    _ -> pure ()
          where fname = T.toLower $ unName $ last $ qnParts qn
        _ -> pure ()

-- | Check AJAX handler security
checkAjaxSecurity :: Located Expr -> [WordPressIssue]
checkAjaxSecurity (Located pos expr) = case expr of
    ExprCall (Located _ (ExprConstant qn)) args
        | fname == "add_action"
        , not (null args)
        , isAjaxAction args ->
            [WordPressIssue InsecureAjax High pos
                "AJAX handler may lack nonce verification"
                "Add check_ajax_referer() at handler start"
                (Just "add_action('wp_ajax_...')")]
      where fname = T.toLower $ unName $ last $ qnParts qn
    _ -> []

-- | Check REST API endpoint security
checkRestApiSecurity :: Located Expr -> [WordPressIssue]
checkRestApiSecurity (Located pos expr) = case expr of
    ExprCall (Located _ (ExprConstant qn)) args
        | fname == "register_rest_route"
        , not (hasPermissionCallback args) ->
            [WordPressIssue InsecureRestApi Critical pos
                "REST API route without permission_callback"
                "Add 'permission_callback' to validate user permissions"
                (Just "register_rest_route")]
      where fname = T.toLower $ unName $ last $ qnParts qn
    _ -> []

-- | Check file upload security
checkFileUploadSecurity :: Located Expr -> [WordPressIssue]
checkFileUploadSecurity (Located pos expr) = case expr of
    ExprVariable (Variable "_FILES") ->
        [WordPressIssue InsecureFileUpload High pos
            "Direct $_FILES access without validation"
            "Use wp_handle_upload() or wp_handle_sideload() with validation"
            (Just "$_FILES")]

    ExprCall (Located _ (ExprConstant qn)) _
        | fname == "move_uploaded_file" ->
            [WordPressIssue InsecureFileUpload High pos
                "Direct move_uploaded_file() usage"
                "Use WordPress upload functions: wp_handle_upload()"
                (Just "move_uploaded_file")]
      where fname = T.toLower $ unName $ last $ qnParts qn
    _ -> []

-- | Check user meta security
checkUserMetaSecurity :: Located Expr -> [WordPressIssue]
checkUserMetaSecurity (Located pos expr) = case expr of
    ExprCall (Located _ (ExprConstant qn)) args
        | fname `elem` ["update_user_meta", "add_user_meta", "delete_user_meta"]
        , hasUserInput args ->
            [WordPressIssue UnsafeMeta High pos
                ("User meta operation with unsanitized input: " <> fname)
                "Sanitize meta key and value, validate against whitelist"
                (Just fname)]
      where fname = T.toLower $ unName $ last $ qnParts qn
    _ -> []

-- | Check transient security
checkTransientSecurity :: Located Expr -> [WordPressIssue]
checkTransientSecurity (Located pos expr) = case expr of
    ExprCall (Located _ (ExprConstant qn)) args
        | fname `elem` ["set_transient", "set_site_transient"]
        , hasUserInput args ->
            [WordPressIssue InsecureTransient Medium pos
                "Transient stored with unsanitized user input"
                "Sanitize data before storing, consider expiration time"
                (Just fname)]
      where fname = T.toLower $ unName $ last $ qnParts qn
    _ -> []

-- | Check cron security
checkCronSecurity :: Located Expr -> [WordPressIssue]
checkCronSecurity (Located pos expr) = case expr of
    ExprCall (Located _ (ExprConstant qn)) args
        | fname `elem` ["wp_schedule_single_event", "wp_schedule_event"]
        , hasUserInput args ->
            [WordPressIssue InsecureCron High pos
                "Cron event with user-controlled parameters"
                "Validate and sanitize cron arguments"
                (Just fname)]
      where fname = T.toLower $ unName $ last $ qnParts qn
    _ -> []

-- | Check shortcode security
checkShortcodeSecurity :: Located Expr -> [WordPressIssue]
checkShortcodeSecurity (Located pos expr) = case expr of
    ExprCall (Located _ (ExprConstant qn)) _
        | fname == "add_shortcode" ->
            [WordPressIssue ShortcodeXSS Medium pos
                "Shortcode registration - verify output escaping"
                "Ensure shortcode output is escaped with esc_html() or esc_attr()"
                (Just "add_shortcode")]
      where fname = T.toLower $ unName $ last $ qnParts qn
    _ -> []

-- | Check widget security
checkWidgetSecurity :: Located Declaration -> [WordPressIssue]
checkWidgetSecurity (Located pos decl) = case decl of
    DeclClass{clsName = Name className}
        | "Widget" `T.isInfixOf` className ->
            [WordPressIssue WidgetXSS Medium pos
                "Widget class - verify form() and update() escape output"
                "Escape all widget output and sanitize widget settings"
                (Just className)]
    _ -> []

-- | Check Gutenberg block security
checkGutenbergBlockSecurity :: Located Expr -> [WordPressIssue]
checkGutenbergBlockSecurity (Located pos expr) = case expr of
    ExprCall (Located _ (ExprConstant qn)) _
        | fname == "register_block_type" ->
            [WordPressIssue BlockXSS Medium pos
                "Gutenberg block - verify render_callback escaping"
                "Escape block output, validate attributes, sanitize user input"
                (Just "register_block_type")]
      where fname = T.toLower $ unName $ last $ qnParts qn
    _ -> []

-- | Check i18n security
checkI18nSecurity :: Located Expr -> [WordPressIssue]
checkI18nSecurity (Located pos expr) = case expr of
    ExprCall (Located _ (ExprConstant qn)) args
        | fname `elem` i18nFunctions
        , not (hasTextDomain args) ->
            [WordPressIssue MissingTextDomain Low pos
                ("Missing text domain in " <> fname)
                "Add text domain for proper translation"
                (Just fname)]
      where fname = T.toLower $ unName $ last $ qnParts qn
    _ -> []
  where
    i18nFunctions = ["__", "_e", "_x", "_ex", "_n", "_nx", "esc_html__", "esc_html_e",
                     "esc_attr__", "esc_attr_e"]

-- | Check escaping context
checkEscapingContext :: Located Expr -> [WordPressIssue]
checkEscapingContext (Located pos expr) = case expr of
    -- esc_html() in URL context
    ExprCall (Located _ (ExprConstant qn)) _
        | fname == "esc_html" && isInUrlContext pos ->
            [WordPressIssue ImproperEscaping Medium pos
                "esc_html() used in URL context"
                "Use esc_url() for URLs"
                (Just "esc_html in href/src")]
      where fname = T.toLower $ unName $ last $ qnParts qn
    _ -> []

-- | Check custom table queries
checkCustomTableSecurity :: Located Expr -> [WordPressIssue]
checkCustomTableSecurity (Located pos expr) = case expr of
    ExprMethodCall obj (Name method) args
        | isWpdbObject obj
        , T.toLower method `elem` wpdbQueryMethods
        , hasCustomTable args ->
            [WordPressIssue CustomTableInjection High pos
                "Custom table query - verify sanitization"
                "Use $wpdb->prepare() with %s/%d/%f placeholders"
                (Just $ "$wpdb->" <> method)]
    _ -> []
  where
    wpdbQueryMethods = ["query", "get_results", "get_row", "get_col", "get_var"]

-- Helper functions

wpdbFunctions :: [Text]
wpdbFunctions = ["query", "get_results", "get_row", "get_col", "get_var"]

isWpdbPrepare :: [Argument] -> Bool
isWpdbPrepare args = not (null args) && case locNode (argValue $ head args) of
    ExprMethodCall _ (Name "prepare") _ -> True
    _ -> False

isWpdbObject :: Located Expr -> Bool
isWpdbObject (Located _ (ExprVariable (Variable "wpdb"))) = True
isWpdbObject _ = False

isAjaxAction :: [Argument] -> Bool
isAjaxAction args = not (null args) && case locNode (argValue $ head args) of
    ExprLiteral (LitString s) -> "wp_ajax_" `T.isPrefixOf` s
    _ -> False

hasPermissionCallback :: [Argument] -> Bool
hasPermissionCallback args = any isPermissionCallbackArg args
  where
    isPermissionCallbackArg arg = case locNode (argValue arg) of
        ExprLiteral (LitString s) -> "permission_callback" `T.isInfixOf` s
        _ -> False

hasFileValidation :: [Argument] -> Bool
hasFileValidation _args = False  -- Simplified

hasUserInput :: [Argument] -> Bool
hasUserInput = any (containsUserInput . argValue)
  where
    containsUserInput (Located _ e) = case e of
        ExprVariable (Variable v) -> v `elem` ["_GET", "_POST", "_REQUEST", "_COOKIE"]
        ExprArrayAccess base _ -> containsUserInput base
        _ -> False

hasMetaKeyValidation :: [Argument] -> Bool
hasMetaKeyValidation _args = False  -- Simplified

hasVariableInQuery :: Argument -> Bool
hasVariableInQuery arg = containsVariable (argValue arg)
  where
    containsVariable (Located _ e) = case e of
        ExprVariable _ -> True
        ExprBinary OpConcat left right -> containsVariable left || containsVariable right
        _ -> False

hasFormProcessing :: PhpFile -> Bool
hasFormProcessing file = any (hasPost . locNode) (phpStatements file)
  where
    hasPost (StmtExpr (Located _ (ExprVariable (Variable "_POST")))) = True
    hasPost (StmtExpr (Located _ (ExprArrayAccess (Located _ (ExprVariable (Variable "_POST"))) _))) = True
    hasPost _ = False

hasNonceVerification :: PhpFile -> Bool
hasNonceVerification file = any (hasNonce . locNode) (phpStatements file)
  where
    hasNonce (StmtExpr (Located _ (ExprCall (Located _ (ExprConstant qn)) _))) =
        let fn = T.toLower $ unName $ last $ qnParts qn
        in fn `elem` ["wp_verify_nonce", "check_admin_referer", "check_ajax_referer"]
    hasNonce _ = False

hasAdminPageRegistration :: PhpFile -> Bool
hasAdminPageRegistration file = any (hasAdminPage . locNode) (phpStatements file)
  where
    hasAdminPage (StmtExpr (Located _ (ExprCall (Located _ (ExprConstant qn)) _))) =
        let fn = T.toLower $ unName $ last $ qnParts qn
        in "add_menu_page" `T.isInfixOf` fn || "add_submenu_page" `T.isInfixOf` fn
    hasAdminPage _ = False

hasCapabilityCheck :: PhpFile -> Bool
hasCapabilityCheck file = any (hasCap . locNode) (phpStatements file)
  where
    hasCap (StmtExpr (Located _ (ExprCall (Located _ (ExprConstant qn)) _))) =
        let fn = T.toLower $ unName $ last $ qnParts qn
        in fn `elem` ["current_user_can", "user_can", "is_admin"]
    hasCap _ = False

hasTextDomain :: [Argument] -> Bool
hasTextDomain args = length args >= 2

hasNonceInContext :: Text -> Bool
hasNonceInContext _fname = False  -- Simplified

isInUrlContext :: SourcePos -> Bool
isInUrlContext _pos = False  -- Would need more context

hasCustomTable :: [Argument] -> Bool
hasCustomTable args = any hasTableRef args
  where
    hasTableRef arg = case locNode (argValue arg) of
        ExprBinary OpConcat _ _ -> True  -- Likely custom table prefix
        _ -> False

mkPos :: PhpFile -> SourcePos
mkPos _file = SourcePos "<file>" 1 1
