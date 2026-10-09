-- | Transform PHP code to add sanitization and escaping
-- SPDX-License-Identifier: MPL-2.0
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
    , transformSanitizeOutput
    , transformSanitizeInput
    , transformSQLPrepare
    , transformRedirectSafety
    , transformModernizeCrypto
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import Sanctify.AST
import Sanctify.WordPress.Constraints (isWpdbObject)

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

-- | Transform: escape outputs across the file
transformSanitizeOutput :: PhpFile -> PhpFile
transformSanitizeOutput file = file { phpStatements = map escapeStmt (phpStatements file) }
  where
    escapeStmt stmt@(Located pos (StmtEcho exprs)) =
        Located pos $ StmtEcho $ map (wrapWithEscape $ detectEscapeContext stmt) exprs
    escapeStmt stmt = stmt

-- | Transform: sanitize superglobal inputs everywhere
transformSanitizeInput :: PhpFile -> PhpFile
transformSanitizeInput file = file { phpStatements = map (mapStatement sanitizeExpr) (phpStatements file) }
  where
    sanitizeExpr = mapExpr sanitizeSuperglobalAccess

-- | Transform: wrap unsafe $wpdb queries with prepare()
transformSQLPrepare :: PhpFile -> PhpFile
transformSQLPrepare file = file { phpStatements = map (mapStatement prepareQueries) (phpStatements file) }
  where
    prepareQueries = mapExpr prepareNode

    prepareNode loc@(Located pos expr@(ExprMethodCall obj method args))
        | isWpdbObject obj
        , let name = unName method
        , name `elem` ["query", "get_results", "get_row", "get_col", "get_var"]
        , not (null args)
        , let firstArg@(Argument n argVal unpack) = head args
        , Just (query, params) <- convertToParameterizedQuery argVal
        = Located pos $ ExprMethodCall obj method $ Argument n (wrapWithPrepare query params) unpack : tail args
    prepareNode loc = loc

-- | Transform: ensure redirects end with exit()
transformRedirectSafety :: PhpFile -> PhpFile
transformRedirectSafety file = file { phpStatements = addExitAfterRedirect (phpStatements file) }

-- | Transform: modernize weak crypto helpers
transformModernizeCrypto :: PhpFile -> PhpFile
transformModernizeCrypto file = file { phpStatements = map (mapStatement modernizeExpr) (phpStatements file) }
  where
    modernizeExpr = mapExpr modernizeNode

    modernizeNode loc@(Located pos expr@(ExprCall callee args)) =
        case functionName callee of
            Just fn | fn == "rand" -> Located pos $ ExprCall (makeConst pos "random_int") (map updateArg args)
                    | fn == "md5" -> Located pos $ ExprCall (makeConst pos "hash") (Argument Nothing (Located pos $ ExprLiteral $ LitString "sha3-256") False : map updateArg args)
                    | fn == "sha1" -> Located pos $ ExprCall (makeConst pos "sodium_crypto_generichash") (map updateArg args)
            _ -> loc

    modernizeNode loc = loc

    updateArg (Argument name value unpack) = Argument name (modernizeExpr value) unpack

    makeConst p name = Located p $ ExprConstant $ QualifiedName [Name name] False

    functionName (Located _ (ExprConstant (QualifiedName parts _))) = Just $ unName $ last parts
    functionName _ = Nothing

mapArgument :: (Located Expr -> Located Expr) -> Argument -> Argument
mapArgument f (Argument name value unpack) = Argument name (f value) unpack

mapExpr :: (Located Expr -> Located Expr) -> Located Expr -> Located Expr
mapExpr f (Located pos expr) = f $ Located pos (case expr of
    ExprBinary op l r -> ExprBinary op (mapExpr f l) (mapExpr f r)
    ExprUnary op e -> ExprUnary op (mapExpr f e)
    ExprAssign e1 e2 -> ExprAssign (mapExpr f e1) (mapExpr f e2)
    ExprAssignOp op e1 e2 -> ExprAssignOp op (mapExpr f e1) (mapExpr f e2)
    ExprTernary c t e -> ExprTernary (mapExpr f c) (fmap (mapExpr f) t) (mapExpr f e)
    ExprCall callee args -> ExprCall (mapExpr f callee) (map (mapArgument f) args)
    ExprMethodCall obj name args -> ExprMethodCall (mapExpr f obj) name (map (mapArgument f) args)
    ExprStaticCall qn name args -> ExprStaticCall qn name (map (mapArgument f) args)
    ExprNullsafeMethodCall obj name args -> ExprNullsafeMethodCall (mapExpr f obj) name (map (mapArgument f) args)
    ExprPropertyAccess obj name -> ExprPropertyAccess (mapExpr f obj) name
    ExprNullsafePropertyAccess obj name -> ExprNullsafePropertyAccess (mapExpr f obj) name
    ExprStaticPropertyAccess qn name -> ExprStaticPropertyAccess qn name
    ExprArrayAccess base idx -> ExprArrayAccess (mapExpr f base) (fmap (mapExpr f) idx)
    ExprNew qn args -> ExprNew qn (map (mapArgument f) args)
    ExprClosure{closureStatic=st, closureParams=ps, closureUses=us, closureReturn=ret, closureBody=body} ->
        ExprClosure st ps us ret (map (mapStatement f) body)
    ExprArrowFunction{arrowParams=params, arrowReturn=ret, arrowExpr=expr} ->
        ExprArrowFunction params ret (mapExpr f expr)
    ExprCast ty e -> ExprCast ty (mapExpr f e)
    ExprIsset exprs -> ExprIsset (map (mapExpr f) exprs)
    ExprEmpty e -> ExprEmpty (mapExpr f e)
    ExprEval e -> ExprEval (mapExpr f e)
    ExprInclude ty e -> ExprInclude ty (mapExpr f e)
    ExprYield m1 m2 -> ExprYield (fmap (mapExpr f) m1) (fmap (mapExpr f) m2)
    ExprYieldFrom e -> ExprYieldFrom (mapExpr f e)
    ExprThrow e -> ExprThrow (mapExpr f e)
    ExprClassConstAccess qn name -> ExprClassConstAccess qn name
    ExprConstant qn -> ExprConstant qn
    ExprShellExec t -> ExprShellExec t
    ExprHeredoc t -> ExprHeredoc t
    ExprList items -> ExprList (map (fmap (mapExpr f)) items)
    _ -> expr)

mapStatement :: (Located Expr -> Located Expr) -> Located Statement -> Located Statement
mapStatement f (Located pos stmt) = Located pos (case stmt of
    StmtExpr expr -> StmtExpr (f expr)
    StmtDecl decl -> StmtDecl decl
    StmtIf cond thenStmts elseStmts -> StmtIf (f cond) (map (mapStatement f) thenStmts) (fmap (map (mapStatement f)) elseStmts)
    StmtWhile cond body -> StmtWhile (f cond) (map (mapStatement f) body)
    StmtFor mInit mCond mUpdate body -> StmtFor (fmap f mInit) (fmap f mCond) (fmap f mUpdate) (map (mapStatement f) body)
    StmtForeach expr var forKey body -> StmtForeach (f expr) var forKey (map (mapStatement f) body)
    StmtSwitch expr cases -> StmtSwitch (f expr) (map (mapCase f) cases)
    StmtMatch expr arms -> StmtMatch (f expr) (map (mapArm f) arms)
    StmtTry tryBody catches finally -> StmtTry (map (mapStatement f) tryBody) (map (mapCatch f) catches) (fmap (map (mapStatement f)) finally)
    StmtReturn mExpr -> StmtReturn (fmap f mExpr)
    StmtThrow expr -> StmtThrow (f expr)
    StmtBreak n -> StmtBreak n
    StmtContinue n -> StmtContinue n
    StmtEcho exprs -> StmtEcho (map f exprs)
    StmtGlobal vars -> StmtGlobal vars
    StmtStatic pairs -> StmtStatic (map (\(var, mexpr) -> (var, fmap f mexpr)) pairs)
    StmtUnset exprs -> StmtUnset (map f exprs)
    StmtDeclare decls body -> StmtDeclare decls (map (mapStatement f) body)
    StmtNoop -> StmtNoop)

mapCase :: (Located Expr -> Located Expr) -> SwitchCase -> SwitchCase
mapCase f (SwitchCase cond body) = SwitchCase (fmap (mapExpr f) cond) (map (mapStatement f) body)

mapArm :: (Located Expr -> Located Expr) -> MatchArm -> MatchArm
mapArm f (MatchArm cond result) = MatchArm (map (mapExpr f) cond) (mapExpr f result)

mapCatch :: (Located Expr -> Located Expr) -> CatchClause -> CatchClause
mapCatch f (CatchClause types var body) = CatchClause types var (map (mapStatement f) body)
