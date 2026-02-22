-- | WordPress hooks analysis and transformation
-- SPDX-License-Identifier: AGPL-3.0-or-later
module Sanctify.WordPress.Hooks
    ( -- * Hook analysis
      HookUsage(..)
    , HookType(..)
    , analyzeHooks

      -- * Hook validation
    , validateHookPriority
    , validateHookCallback
    , findOrphanedHooks

      -- * Common hook patterns
    , commonActionHooks
    , commonFilterHooks
    , securityRelevantHooks

      -- * Hook transformations
    , prefixHookName
    , ensureUnhookOnDeactivation
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

import Sanctify.AST

-- | Type of WordPress hook
data HookType
    = ActionHook     -- ^ add_action / do_action
    | FilterHook     -- ^ add_filter / apply_filters
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

-- | A hook usage in the code
data HookUsage = HookUsage
    { hookType     :: HookType
    , hookName     :: Text
    , hookCallback :: Maybe Text
    , hookPriority :: Maybe Int
    , hookArgs     :: Maybe Int
    , hookLocation :: SourcePos
    , hookIsCustom :: Bool      -- ^ Is this a custom (plugin-defined) hook?
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

-- | Analyze all hook usages in a file
analyzeHooks :: PhpFile -> [HookUsage]
analyzeHooks file = concatMap analyzeStatement (phpStatements file)

-- | Analyze statement for hooks
analyzeStatement :: Located Statement -> [HookUsage]
analyzeStatement (Located pos stmt) = case stmt of
    StmtExpr expr -> analyzeExpr pos expr
    StmtIf _ thenStmts elseStmts ->
        concatMap analyzeStatement thenStmts ++
        maybe [] (concatMap analyzeStatement) elseStmts
    StmtWhile _ body -> concatMap analyzeStatement body
    StmtFor _ _ _ body -> concatMap analyzeStatement body
    StmtForeach _ _ _ body -> concatMap analyzeStatement body
    StmtDecl decl -> analyzeDeclaration pos decl
    _ -> []

-- | Analyze declaration for hooks
analyzeDeclaration :: SourcePos -> Declaration -> [HookUsage]
analyzeDeclaration pos decl = case decl of
    DeclFunction{fnBody = body} -> concatMap analyzeStatement body
    DeclClass{clsMembers = members} -> concatMap (analyzeMember pos) members
    _ -> []

-- | Analyze class member
analyzeMember :: SourcePos -> ClassMember -> [HookUsage]
analyzeMember pos member = case member of
    MemberMethod{methBody = Just body} -> concatMap analyzeStatement body
    _ -> []

-- | Analyze expression for hooks
analyzeExpr :: SourcePos -> Located Expr -> [HookUsage]
analyzeExpr pos (Located _ expr) = case expr of
    ExprCall (Located _ (ExprConstant qn)) args ->
        let fname = unName $ last $ qnParts qn
        in maybeToList $ parseHookCall pos fname args
    _ -> []
  where
    maybeToList Nothing = []
    maybeToList (Just x) = [x]

-- | Parse a hook function call
parseHookCall :: SourcePos -> Text -> [Argument] -> Maybe HookUsage
parseHookCall pos fname args = case fname of
    "add_action" -> Just $ parseAddHook ActionHook pos args
    "add_filter" -> Just $ parseAddHook FilterHook pos args
    "do_action" -> Just $ parseDoHook ActionHook pos args
    "apply_filters" -> Just $ parseDoHook FilterHook pos args
    "remove_action" -> Just $ parseRemoveHook ActionHook pos args
    "remove_filter" -> Just $ parseRemoveHook FilterHook pos args
    "has_action" -> Just $ parseHasHook ActionHook pos args
    "has_filter" -> Just $ parseHasHook FilterHook pos args
    _ -> Nothing

-- | Parse add_action / add_filter
parseAddHook :: HookType -> SourcePos -> [Argument] -> HookUsage
parseAddHook htype pos args = HookUsage
    { hookType = htype
    , hookName = getStringArg 0 args
    , hookCallback = Just $ getCallbackArg 1 args
    , hookPriority = getIntArg 2 args
    , hookArgs = getIntArg 3 args
    , hookLocation = pos
    , hookIsCustom = not $ isWpCoreHook (getStringArg 0 args)
    }

-- | Parse do_action / apply_filters
parseDoHook :: HookType -> SourcePos -> [Argument] -> HookUsage
parseDoHook htype pos args = HookUsage
    { hookType = htype
    , hookName = getStringArg 0 args
    , hookCallback = Nothing
    , hookPriority = Nothing
    , hookArgs = Just (length args - 1)
    , hookLocation = pos
    , hookIsCustom = not $ isWpCoreHook (getStringArg 0 args)
    }

-- | Parse remove_action / remove_filter
parseRemoveHook :: HookType -> SourcePos -> [Argument] -> HookUsage
parseRemoveHook htype pos args = HookUsage
    { hookType = htype
    , hookName = getStringArg 0 args
    , hookCallback = Just $ getCallbackArg 1 args
    , hookPriority = getIntArg 2 args
    , hookArgs = Nothing
    , hookLocation = pos
    , hookIsCustom = False
    }

-- | Parse has_action / has_filter
parseHasHook :: HookType -> SourcePos -> [Argument] -> HookUsage
parseHasHook htype pos args = HookUsage
    { hookType = htype
    , hookName = getStringArg 0 args
    , hookCallback = if length args > 1 then Just (getCallbackArg 1 args) else Nothing
    , hookPriority = Nothing
    , hookArgs = Nothing
    , hookLocation = pos
    , hookIsCustom = False
    }

-- | Get string argument at position
getStringArg :: Int -> [Argument] -> Text
getStringArg n args
    | n < length args = case locNode (argValue (args !! n)) of
        ExprLiteral (LitString s) -> s
        _ -> "<dynamic>"
    | otherwise = ""

-- | Get callback argument at position
getCallbackArg :: Int -> [Argument] -> Text
getCallbackArg n args
    | n < length args = extractCallback (argValue (args !! n))
    | otherwise = ""
  where
    extractCallback (Located _ expr) = case expr of
        ExprLiteral (LitString s) -> s
        ExprConstant qn -> T.intercalate "\\" $ map unName $ qnParts qn
        ExprArrayAccess _ _ -> "<array_callback>"
        ExprClosure{} -> "<closure>"
        _ -> "<callback>"

-- | Get integer argument at position
getIntArg :: Int -> [Argument] -> Maybe Int
getIntArg n args
    | n < length args = case locNode (argValue (args !! n)) of
        ExprLiteral (LitInt i) -> Just (fromIntegral i)
        _ -> Nothing
    | otherwise = Nothing

-- | Check if hook is WordPress core hook
isWpCoreHook :: Text -> Bool
isWpCoreHook name = name `Set.member` commonActionHooks || name `Set.member` commonFilterHooks

-- | Common WordPress action hooks
commonActionHooks :: Set Text
commonActionHooks = Set.fromList
    [ "init"
    , "wp_loaded"
    , "admin_init"
    , "admin_menu"
    , "admin_enqueue_scripts"
    , "wp_enqueue_scripts"
    , "wp_head"
    , "wp_footer"
    , "wp_ajax_*"
    , "wp_ajax_nopriv_*"
    , "rest_api_init"
    , "plugins_loaded"
    , "after_setup_theme"
    , "widgets_init"
    , "save_post"
    , "delete_post"
    , "wp_insert_post"
    , "user_register"
    , "profile_update"
    , "delete_user"
    , "activated_plugin"
    , "deactivated_plugin"
    , "switch_theme"
    , "shutdown"
    ]

-- | Common WordPress filter hooks
commonFilterHooks :: Set Text
commonFilterHooks = Set.fromList
    [ "the_content"
    , "the_title"
    , "the_excerpt"
    , "wp_title"
    , "body_class"
    , "post_class"
    , "the_permalink"
    , "get_the_excerpt"
    , "comment_text"
    , "widget_text"
    , "posts_where"
    , "posts_join"
    , "posts_orderby"
    , "query_vars"
    , "template_include"
    , "template_redirect"
    , "wp_nav_menu_items"
    , "authenticate"
    , "login_redirect"
    , "logout_redirect"
    , "upload_mimes"
    , "sanitize_file_name"
    , "wp_handle_upload_prefilter"
    ]

-- | Security-relevant hooks that need special attention
securityRelevantHooks :: Set Text
securityRelevantHooks = Set.fromList
    [ "authenticate"
    , "check_password"
    , "wp_authenticate_user"
    , "allowed_redirect_hosts"
    , "send_auth_cookies"
    , "auth_cookie_valid"
    , "wp_set_auth_cookie"
    , "wp_clear_auth_cookie"
    , "user_has_cap"
    , "map_meta_cap"
    , "rest_authentication_errors"
    , "xmlrpc_enabled"
    , "xmlrpc_methods"
    , "upload_mimes"
    , "wp_handle_upload_prefilter"
    , "wp_handle_upload"
    , "sanitize_file_name"
    , "wp_insert_post_data"
    , "wp_update_post"
    , "delete_post"
    , "nonce_life"
    , "nonce_user_logged_out"
    ]

-- | Validate hook priority is reasonable
validateHookPriority :: HookUsage -> Maybe Text
validateHookPriority hook = case hookPriority hook of
    Just p | p < -1000 -> Just "Priority is extremely low, may cause issues"
    Just p | p > 1000 -> Just "Priority is extremely high, may cause issues"
    Just p | p /= 10 && hookName hook `Set.member` securityRelevantHooks ->
        Just "Security hook has non-default priority, ensure this is intentional"
    _ -> Nothing

-- | Validate hook callback exists and is callable
validateHookCallback :: HookUsage -> Map Text Bool -> Maybe Text
validateHookCallback hook definedFunctions = case hookCallback hook of
    Nothing -> Nothing
    Just cb | cb == "<closure>" -> Nothing  -- Closures are fine
    Just cb | cb == "<array_callback>" -> Nothing  -- Can't validate
    Just cb | cb == "<callback>" -> Nothing  -- Can't validate
    Just cb | cb == "<dynamic>" -> Just "Dynamic callback - cannot validate"
    Just cb -> case Map.lookup cb definedFunctions of
        Just True -> Nothing
        Just False -> Just $ "Callback '" <> cb <> "' exists but is not callable"
        Nothing -> Just $ "Callback '" <> cb <> "' not found in codebase"

-- | Find hooks that are added but never removed on deactivation
findOrphanedHooks :: [HookUsage] -> [HookUsage]
findOrphanedHooks hooks =
    let added = filter isAddHook hooks
        removed = Set.fromList $ map (\h -> (hookName h, hookCallback h)) $ filter isRemoveHook hooks
        orphaned = filter (\h -> (hookName h, hookCallback h) `Set.notMember` removed) added
    in filter hookIsCustom orphaned
  where
    isAddHook h = hookCallback h /= Nothing && not (isRemoveHook h)
    isRemoveHook h = False  -- Would need to track remove_* calls separately

-- | Prefix a hook name with plugin slug
prefixHookName :: Text -> Text -> Text
prefixHookName prefix name
    | prefix `T.isPrefixOf` name = name
    | otherwise = prefix <> "_" <> name

-- | Generate code to unhook on plugin deactivation
ensureUnhookOnDeactivation :: Text -> [HookUsage] -> Text
ensureUnhookOnDeactivation pluginSlug hooks =
    T.unlines $
        [ "// Deactivation cleanup - generated by sanctify-php"
        , "register_deactivation_hook(__FILE__, '" <> pluginSlug <> "_deactivate');"
        , ""
        , "function " <> pluginSlug <> "_deactivate() {"
        ] ++
        map generateUnhook (filter shouldUnhook hooks) ++
        [ "}"
        ]
  where
    shouldUnhook h = hookIsCustom h && hookCallback h /= Nothing

    generateUnhook h =
        let fn = case hookType h of
                ActionHook -> "remove_action"
                FilterHook -> "remove_filter"
            callback = maybe "" id (hookCallback h)
            priority = maybe "10" (T.pack . show) (hookPriority h)
        in "    " <> fn <> "('" <> hookName h <> "', '" <> callback <> "', " <> priority <> ");"
