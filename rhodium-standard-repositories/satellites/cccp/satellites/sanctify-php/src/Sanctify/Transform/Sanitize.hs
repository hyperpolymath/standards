-- | Transform PHP code to add sanitization and escaping
-- SPDX-License-Identifier: AGPL-3.0-or-later
module Sanctify.Transform.Sanitize
    ( -- * Output escaping
      wrapWithEscape
    , EscapeContext(..)
    , detectEscapeContext

      -- * Input sanitization
    , wrapWithSanitize
    , SanitizeType(..)
    , detectSanitizeType

      -- * SQL safety
    , wrapWithPrepare
    , convertToParameterizedQuery

      -- * Transformations
    , sanitizeSuperglobalAccess
    , escapeEchoStatement
    , addExitAfterRedirect
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import Sanctify.AST

-- | Context for output escaping
data EscapeContext
    = HtmlContent      -- ^ Inside HTML body -> esc_html()
    | HtmlAttribute    -- ^ Inside HTML attribute -> esc_attr()
    | UrlContext       -- ^ URL attribute (href, src) -> esc_url()
    | JavaScriptContext -- ^ Inside <script> or onclick -> esc_js()
    | SqlContext       -- ^ SQL query -> esc_sql() or prepare()
    | TextareaContent  -- ^ Inside <textarea> -> esc_textarea()
    deriving stock (Eq, Show)

-- | Type of input to sanitize
data SanitizeType
    = TextField        -- ^ sanitize_text_field()
    | TextareaField    -- ^ sanitize_textarea_field()
    | EmailField       -- ^ sanitize_email()
    | UrlField         -- ^ sanitize_url()
    | TitleField       -- ^ sanitize_title()
    | FileName         -- ^ sanitize_file_name()
    | KeyField         -- ^ sanitize_key()
    | HtmlField        -- ^ wp_kses_post()
    | IntField         -- ^ absint() / intval()
    deriving stock (Eq, Show)

-- | Wrap expression with appropriate escape function
wrapWithEscape :: EscapeContext -> Located Expr -> Located Expr
wrapWithEscape ctx expr@(Located pos _) =
    Located pos $ ExprCall
        (Located pos $ ExprConstant $ QualifiedName [Name escFn] False)
        [Argument Nothing expr False]
  where
    escFn = case ctx of
        HtmlContent -> "esc_html"
        HtmlAttribute -> "esc_attr"
        UrlContext -> "esc_url"
        JavaScriptContext -> "esc_js"
        SqlContext -> "esc_sql"
        TextareaContent -> "esc_textarea"

-- | Wrap expression with appropriate sanitization function
wrapWithSanitize :: SanitizeType -> Located Expr -> Located Expr
wrapWithSanitize stype expr@(Located pos _) =
    Located pos $ ExprCall
        (Located pos $ ExprConstant $ QualifiedName [Name sanFn] False)
        [Argument Nothing expr False]
  where
    sanFn = case stype of
        TextField -> "sanitize_text_field"
        TextareaField -> "sanitize_textarea_field"
        EmailField -> "sanitize_email"
        UrlField -> "sanitize_url"
        TitleField -> "sanitize_title"
        FileName -> "sanitize_file_name"
        KeyField -> "sanitize_key"
        HtmlField -> "wp_kses_post"
        IntField -> "absint"

-- | Detect appropriate escape context from surrounding code
detectEscapeContext :: Located Statement -> EscapeContext
detectEscapeContext (Located _ stmt) = case stmt of
    -- Echo in general defaults to HTML
    StmtEcho _ -> HtmlContent
    -- Would need more context to determine attribute vs content
    _ -> HtmlContent

-- | Detect appropriate sanitization type from variable name or context
detectSanitizeType :: Text -> SanitizeType
detectSanitizeType name
    | "email" `T.isInfixOf` lower = EmailField
    | "url" `T.isInfixOf` lower || "link" `T.isInfixOf` lower = UrlField
    | "id" `T.isSuffixOf` lower || "_id" `T.isInfixOf` lower = IntField
    | "count" `T.isInfixOf` lower || "num" `T.isInfixOf` lower = IntField
    | "title" `T.isInfixOf` lower || "name" `T.isInfixOf` lower = TitleField
    | "file" `T.isInfixOf` lower || "path" `T.isInfixOf` lower = FileName
    | "key" `T.isInfixOf` lower || "slug" `T.isInfixOf` lower = KeyField
    | "content" `T.isInfixOf` lower || "body" `T.isInfixOf` lower = HtmlField
    | "message" `T.isInfixOf` lower || "description" `T.isInfixOf` lower = TextareaField
    | otherwise = TextField
  where
    lower = T.toLower name

-- | Wrap SQL query with $wpdb->prepare()
wrapWithPrepare :: Located Expr -> [Located Expr] -> Located Expr
wrapWithPrepare query@(Located pos _) params =
    Located pos $ ExprMethodCall
        (Located pos $ ExprVariable $ Variable "wpdb")
        (Name "prepare")
        (Argument Nothing query False : map makeArg params)
  where
    makeArg p = Argument Nothing p False

-- | Convert a string concatenation query to parameterized query
-- e.g., "SELECT * FROM users WHERE id = " . $id
-- becomes: $wpdb->prepare("SELECT * FROM users WHERE id = %d", $id)
convertToParameterizedQuery :: Located Expr -> Maybe (Located Expr, [Located Expr])
convertToParameterizedQuery expr = extractParams expr []
  where
    extractParams :: Located Expr -> [Located Expr] -> Maybe (Located Expr, [Located Expr])
    extractParams (Located pos (ExprBinary OpConcat left right)) params =
        case locNode right of
            ExprVariable _ -> extractParams left (right : params)
            ExprArrayAccess _ _ -> extractParams left (right : params)
            ExprLiteral (LitString _) -> extractParams left params >>= \(q, ps) ->
                Just (appendStr q right, ps)
            _ -> Nothing
    extractParams lit@(Located _ (ExprLiteral (LitString _))) params =
        Just (lit, params)
    extractParams _ _ = Nothing

    appendStr :: Located Expr -> Located Expr -> Located Expr
    appendStr (Located pos (ExprLiteral (LitString s))) (Located _ (ExprLiteral (LitString s2))) =
        Located pos $ ExprLiteral $ LitString (s <> s2)
    appendStr e _ = e  -- Shouldn't happen

-- | Transform superglobal access to sanitized version
-- $_GET['key'] -> sanitize_text_field($_GET['key'])
sanitizeSuperglobalAccess :: Located Expr -> Located Expr
sanitizeSuperglobalAccess expr@(Located pos (ExprArrayAccess base keyExpr)) =
    case locNode base of
        ExprVariable (Variable name) | name `elem` ["_GET", "_POST", "_REQUEST"] ->
            let sanitizeType = case keyExpr of
                    Just (Located _ (ExprLiteral (LitString key))) -> detectSanitizeType key
                    _ -> TextField
            in wrapWithSanitize sanitizeType expr
        _ -> expr
sanitizeSuperglobalAccess expr = expr

-- | Transform echo statement to escape all variable content
escapeEchoStatement :: Located Statement -> Located Statement
escapeEchoStatement (Located pos (StmtEcho exprs)) =
    Located pos $ StmtEcho $ map escapeIfNeeded exprs
  where
    escapeIfNeeded :: Located Expr -> Located Expr
    escapeIfNeeded e
        | needsEscaping e = wrapWithEscape HtmlContent e
        | otherwise = e

    needsEscaping :: Located Expr -> Bool
    needsEscaping (Located _ expr) = case expr of
        ExprVariable _ -> True
        ExprArrayAccess _ _ -> True
        ExprMethodCall _ _ _ -> True
        ExprPropertyAccess _ _ -> True
        ExprBinary OpConcat l r -> needsEscaping l || needsEscaping r
        ExprCall (Located _ (ExprConstant qn)) _ ->
            let fn = unName $ last $ qnParts qn
            in not $ isEscapeFunction fn
        _ -> False

    isEscapeFunction :: Text -> Bool
    isEscapeFunction fn = fn `elem`
        [ "esc_html", "esc_attr", "esc_url", "esc_js", "esc_textarea"
        , "esc_html__", "esc_html_e", "esc_attr__", "esc_attr_e"
        , "wp_kses", "wp_kses_post", "wp_kses_data"
        , "__", "_e", "_x", "_n"  -- Translation functions (they escape)
        , "htmlspecialchars", "htmlentities"
        ]

escapeEchoStatement stmt = stmt

-- | Add exit after wp_redirect/wp_safe_redirect
addExitAfterRedirect :: [Located Statement] -> [Located Statement]
addExitAfterRedirect = concatMap processStmt
  where
    processStmt :: Located Statement -> [Located Statement]
    processStmt stmt@(Located pos (StmtExpr expr@(Located _ (ExprCall callee _)))) =
        if isRedirectCall callee
            then [stmt, exitStmt pos]
            else [stmt]
    processStmt stmt = [stmt]

    isRedirectCall :: Located Expr -> Bool
    isRedirectCall (Located _ (ExprConstant qn)) =
        let fn = unName $ last $ qnParts qn
        in fn `elem` ["wp_redirect", "wp_safe_redirect"]
    isRedirectCall _ = False

    exitStmt :: SourcePos -> Located Statement
    exitStmt pos = Located pos $ StmtExpr $ Located pos $
        ExprCall
            (Located pos $ ExprConstant $ QualifiedName [Name "exit"] False)
            []
