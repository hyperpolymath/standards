-- | PHP code emission / pretty printing
-- SPDX-License-Identifier: AGPL-3.0-or-later
module Sanctify.Emit
    ( -- * Main emission
      emitPhp
    , emitPhpFile
    , emitStatement
    , emitExpr

      -- * Output formats
    , OutputFormat(..)
    , emitWithFormat

      -- * Infrastructure export
    , emitPhpIniRecommendations
    , emitNginxRules
    , emitGuixOverrides
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder, toLazyText, fromText, singleton)
import qualified Data.Text.Lazy.Builder as B
import Data.Maybe (fromMaybe, catMaybes)
import Data.List (intersperse)

import Sanctify.AST
import Sanctify.Analysis.Security (SecurityIssue(..), IssueType(..))

-- | Output format options
data OutputFormat
    = FormatPhp           -- ^ Standard PHP output
    | FormatPhpMinified   -- ^ Minified PHP (no extra whitespace)
    | FormatDiff          -- ^ Show only changes as unified diff
    deriving stock (Eq, Show)

-- | Emit a complete PHP file
emitPhp :: PhpFile -> Text
emitPhp = toStrict . toLazyText . buildPhpFile

-- | Emit to a file with specified format
emitPhpFile :: OutputFormat -> PhpFile -> Text
emitPhpFile FormatPhp file = emitPhp file
emitPhpFile FormatPhpMinified file = T.filter (not . (`elem` ['\n', '\t'])) $ emitPhp file
emitPhpFile FormatDiff _ = "-- Diff format not yet implemented --"

-- | Emit with specific format
emitWithFormat :: OutputFormat -> PhpFile -> Text
emitWithFormat = emitPhpFile

-- | Build the complete PHP file
buildPhpFile :: PhpFile -> Builder
buildPhpFile file = mconcat
    [ fromText "<?php\n"
    , if phpDeclareStrict file
        then fromText "declare(strict_types=1);\n\n"
        else mempty
    , buildNamespace (phpNamespace file)
    , buildUses (phpUses file)
    , mconcat $ map buildStatement (phpStatements file)
    ]

-- | Build namespace declaration
buildNamespace :: Maybe QualifiedName -> Builder
buildNamespace Nothing = mempty
buildNamespace (Just qn) =
    fromText "namespace " <> buildQualifiedName qn <> fromText ";\n\n"

-- | Build use declarations
buildUses :: [UseDecl] -> Builder
buildUses [] = mempty
buildUses uses = mconcat (map buildUse uses) <> singleton '\n'
  where
    buildUse :: UseDecl -> Builder
    buildUse use = mconcat
        [ fromText "use "
        , case useKind use of
            UseFunction -> fromText "function "
            UseConstant -> fromText "const "
            UseClass -> mempty
        , buildQualifiedName (useName use)
        , case useAlias use of
            Nothing -> mempty
            Just (Name alias) -> fromText " as " <> fromText alias
        , fromText ";\n"
        ]

-- | Build a statement
buildStatement :: Located Statement -> Builder
buildStatement (Located _ stmt) = case stmt of
    StmtExpr expr -> buildExpr expr <> fromText ";\n"

    StmtReturn Nothing -> fromText "return;\n"
    StmtReturn (Just expr) -> fromText "return " <> buildExpr expr <> fromText ";\n"

    StmtEcho exprs -> mconcat
        [ fromText "echo "
        , mconcat $ intersperse (fromText ", ") $ map buildExpr exprs
        , fromText ";\n"
        ]

    StmtIf cond thenStmts elseStmts -> mconcat
        [ fromText "if ("
        , buildExpr cond
        , fromText ") {\n"
        , indent $ mconcat $ map buildStatement thenStmts
        , fromText "}"
        , case elseStmts of
            Nothing -> fromText "\n"
            Just stmts -> fromText " else {\n" <> indent (mconcat $ map buildStatement stmts) <> fromText "}\n"
        ]

    StmtWhile cond body -> mconcat
        [ fromText "while ("
        , buildExpr cond
        , fromText ") {\n"
        , indent $ mconcat $ map buildStatement body
        , fromText "}\n"
        ]

    StmtFor mInit mCond mUpdate body -> mconcat
        [ fromText "for ("
        , maybe mempty buildExpr mInit
        , fromText "; "
        , maybe mempty buildExpr mCond
        , fromText "; "
        , maybe mempty buildExpr mUpdate
        , fromText ") {\n"
        , indent $ mconcat $ map buildStatement body
        , fromText "}\n"
        ]

    StmtForeach expr (Variable v) mKey body -> mconcat
        [ fromText "foreach ("
        , buildExpr expr
        , fromText " as "
        , case mKey of
            Nothing -> mempty
            Just (Variable k) -> fromText "$" <> fromText k <> fromText " => "
        , fromText "$"
        , fromText v
        , fromText ") {\n"
        , indent $ mconcat $ map buildStatement body
        , fromText "}\n"
        ]

    StmtSwitch expr cases -> mconcat
        [ fromText "switch ("
        , buildExpr expr
        , fromText ") {\n"
        , mconcat $ map buildCase cases
        , fromText "}\n"
        ]

    StmtTry tryBody catches mFinally -> mconcat
        [ fromText "try {\n"
        , indent $ mconcat $ map buildStatement tryBody
        , fromText "}"
        , mconcat $ map buildCatch catches
        , case mFinally of
            Nothing -> fromText "\n"
            Just finally -> mconcat
                [ fromText " finally {\n"
                , indent $ mconcat $ map buildStatement finally
                , fromText "}\n"
                ]
        ]

    StmtThrow expr -> fromText "throw " <> buildExpr expr <> fromText ";\n"

    StmtBreak Nothing -> fromText "break;\n"
    StmtBreak (Just n) -> fromText "break " <> B.fromString (show n) <> fromText ";\n"

    StmtContinue Nothing -> fromText "continue;\n"
    StmtContinue (Just n) -> fromText "continue " <> B.fromString (show n) <> fromText ";\n"

    StmtDecl decl -> buildDeclaration decl

    StmtNoop -> fromText ";\n"

    _ -> fromText "/* unsupported statement */\n"

-- | Build switch case
buildCase :: SwitchCase -> Builder
buildCase sc = mconcat
    [ case caseExpr sc of
        Nothing -> fromText "default:\n"
        Just expr -> fromText "case " <> buildExpr expr <> fromText ":\n"
    , indent $ mconcat $ map buildStatement (caseBody sc)
    ]

-- | Build catch clause
buildCatch :: CatchClause -> Builder
buildCatch c = mconcat
    [ fromText " catch ("
    , mconcat $ intersperse (fromText "|") $ map buildQualifiedName (catchTypes c)
    , case catchVar c of
        Nothing -> mempty
        Just (Variable v) -> fromText " $" <> fromText v
    , fromText ") {\n"
    , indent $ mconcat $ map buildStatement (catchBody c)
    , fromText "}"
    ]

-- | Build a declaration
buildDeclaration :: Declaration -> Builder
buildDeclaration decl = case decl of
    DeclFunction{fnName = Name name, fnParams = params, fnReturnType = ret, fnBody = body} ->
        mconcat
            [ fromText "function "
            , fromText name
            , fromText "("
            , buildParams params
            , fromText ")"
            , buildReturnType ret
            , fromText " {\n"
            , indent $ mconcat $ map buildStatement body
            , fromText "}\n\n"
            ]

    DeclClass{clsName = Name name, clsModifiers = mods, clsExtends = ext, clsImplements = impls, clsMembers = members} ->
        mconcat
            [ buildModifiers mods
            , fromText "class "
            , fromText name
            , case ext of
                Nothing -> mempty
                Just qn -> fromText " extends " <> buildQualifiedName qn
            , case impls of
                [] -> mempty
                qs -> fromText " implements " <> mconcat (intersperse (fromText ", ") (map buildQualifiedName qs))
            , fromText " {\n"
            , indent $ mconcat $ map buildClassMember members
            , fromText "}\n\n"
            ]

    DeclConst (Name name) expr -> mconcat
        [ fromText "const "
        , fromText name
        , fromText " = "
        , buildExpr expr
        , fromText ";\n"
        ]

    _ -> fromText "/* unsupported declaration */\n"

-- | Build class member
buildClassMember :: ClassMember -> Builder
buildClassMember member = case member of
    MemberProperty{propVisibility = vis, propModifiers = mods, propType = mType, propName = Name name, propDefault = mDefault} ->
        mconcat
            [ buildVisibility vis
            , buildModifiers mods
            , maybe mempty (\t -> buildTypeHint t <> fromText " ") mType
            , fromText "$"
            , fromText name
            , case mDefault of
                Nothing -> mempty
                Just expr -> fromText " = " <> buildExpr expr
            , fromText ";\n"
            ]

    MemberMethod{methVisibility = vis, methModifiers = mods, methName = Name name, methParams = params, methReturn = ret, methBody = mBody} ->
        mconcat
            [ buildVisibility vis
            , buildModifiers mods
            , fromText "function "
            , fromText name
            , fromText "("
            , buildParams params
            , fromText ")"
            , buildReturnType ret
            , case mBody of
                Nothing -> fromText ";\n"
                Just body -> mconcat
                    [ fromText " {\n"
                    , indent $ mconcat $ map buildStatement body
                    , fromText "}\n"
                    ]
            ]

    MemberConst{constVisibility = vis, constName = Name name, constValue = expr} ->
        mconcat
            [ buildVisibility vis
            , fromText "const "
            , fromText name
            , fromText " = "
            , buildExpr expr
            , fromText ";\n"
            ]

    MemberTraitUse traits _ ->
        mconcat
            [ fromText "use "
            , mconcat $ intersperse (fromText ", ") $ map buildQualifiedName traits
            , fromText ";\n"
            ]

-- | Build parameter list
buildParams :: [Parameter] -> Builder
buildParams = mconcat . intersperse (fromText ", ") . map buildParam
  where
    buildParam :: Parameter -> Builder
    buildParam p = mconcat
        [ maybe mempty (\t -> buildTypeHint t <> fromText " ") (paramType p)
        , if paramByRef p then fromText "&" else mempty
        , if paramVariadic p then fromText "..." else mempty
        , fromText "$"
        , fromText (varName $ paramName p)
        , case paramDefault p of
            Nothing -> mempty
            Just expr -> fromText " = " <> buildExpr expr
        ]

-- | Build return type
buildReturnType :: Maybe ReturnType -> Builder
buildReturnType Nothing = mempty
buildReturnType (Just rt) = mconcat
    [ fromText ": "
    , if rtNullable rt then fromText "?" else mempty
    , buildPhpType (rtType rt)
    ]

-- | Build type hint
buildTypeHint :: TypeHint -> Builder
buildTypeHint th = mconcat
    [ if thNullable th then fromText "?" else mempty
    , buildPhpType (thType th)
    ]

-- | Build PHP type
buildPhpType :: PhpType -> Builder
buildPhpType t = case t of
    TInt -> fromText "int"
    TFloat -> fromText "float"
    TString -> fromText "string"
    TBool -> fromText "bool"
    TArray Nothing -> fromText "array"
    TArray (Just _) -> fromText "array"
    TObject Nothing -> fromText "object"
    TObject (Just qn) -> buildQualifiedName qn
    TCallable -> fromText "callable"
    TIterable -> fromText "iterable"
    TMixed -> fromText "mixed"
    TVoid -> fromText "void"
    TNever -> fromText "never"
    TNull -> fromText "null"
    TUnion types -> mconcat $ intersperse (fromText "|") $ map buildPhpType types
    TIntersection types -> mconcat $ intersperse (fromText "&") $ map buildPhpType types
    TNullable inner -> fromText "?" <> buildPhpType inner
    TClass qn -> buildQualifiedName qn
    TSelf -> fromText "self"
    TStatic -> fromText "static"
    TParent -> fromText "parent"

-- | Build visibility
buildVisibility :: Visibility -> Builder
buildVisibility Public = fromText "public "
buildVisibility Protected = fromText "protected "
buildVisibility Private = fromText "private "

-- | Build modifiers
buildModifiers :: [Modifier] -> Builder
buildModifiers = mconcat . map buildMod
  where
    buildMod Static = fromText "static "
    buildMod Final = fromText "final "
    buildMod Abstract = fromText "abstract "
    buildMod Readonly = fromText "readonly "

-- | Build qualified name
buildQualifiedName :: QualifiedName -> Builder
buildQualifiedName qn = mconcat
    [ if qnAbsolute qn then fromText "\\" else mempty
    , mconcat $ intersperse (fromText "\\") $ map (\(Name n) -> fromText n) (qnParts qn)
    ]

-- | Build an expression
buildExpr :: Located Expr -> Builder
buildExpr (Located _ expr) = case expr of
    ExprLiteral lit -> buildLiteral lit

    ExprVariable (Variable name) -> fromText "$" <> fromText name

    ExprBinary op left right -> mconcat
        [ buildExpr left
        , fromText " "
        , buildBinOp op
        , fromText " "
        , buildExpr right
        ]

    ExprUnary op operand -> case op of
        OpPostInc -> buildExpr operand <> fromText "++"
        OpPostDec -> buildExpr operand <> fromText "--"
        _ -> buildUnaryOp op <> buildExpr operand

    ExprAssign target value ->
        buildExpr target <> fromText " = " <> buildExpr value

    ExprAssignOp op target value ->
        buildExpr target <> fromText " " <> buildBinOp op <> fromText "= " <> buildExpr value

    ExprTernary cond mTrue false -> mconcat
        [ buildExpr cond
        , fromText " ? "
        , maybe mempty buildExpr mTrue
        , fromText " : "
        , buildExpr false
        ]

    ExprCall callee args ->
        buildExpr callee <> fromText "(" <> buildArgs args <> fromText ")"

    ExprMethodCall obj (Name method) args ->
        buildExpr obj <> fromText "->" <> fromText method <> fromText "(" <> buildArgs args <> fromText ")"

    ExprStaticCall qn (Name method) args ->
        buildQualifiedName qn <> fromText "::" <> fromText method <> fromText "(" <> buildArgs args <> fromText ")"

    ExprNullsafeMethodCall obj (Name method) args ->
        buildExpr obj <> fromText "?->" <> fromText method <> fromText "(" <> buildArgs args <> fromText ")"

    ExprPropertyAccess obj (Name prop) ->
        buildExpr obj <> fromText "->" <> fromText prop

    ExprNullsafePropertyAccess obj (Name prop) ->
        buildExpr obj <> fromText "?->" <> fromText prop

    ExprStaticPropertyAccess qn (Name prop) ->
        buildQualifiedName qn <> fromText "::$" <> fromText prop

    ExprArrayAccess base mKey ->
        buildExpr base <> fromText "[" <> maybe mempty buildExpr mKey <> fromText "]"

    ExprNew qn args ->
        fromText "new " <> buildQualifiedName qn <> fromText "(" <> buildArgs args <> fromText ")"

    ExprCast t e ->
        fromText "(" <> buildPhpType t <> fromText ")" <> buildExpr e

    ExprIsset exprs ->
        fromText "isset(" <> mconcat (intersperse (fromText ", ") (map buildExpr exprs)) <> fromText ")"

    ExprEmpty e ->
        fromText "empty(" <> buildExpr e <> fromText ")"

    ExprConstant qn -> buildQualifiedName qn

    _ -> fromText "/* expr */"

-- | Build literal
buildLiteral :: Literal -> Builder
buildLiteral lit = case lit of
    LitInt n -> B.fromString (show n)
    LitFloat f -> B.fromString (show f)
    LitString s -> fromText "'" <> fromText (escapeString s) <> fromText "'"
    LitBool True -> fromText "true"
    LitBool False -> fromText "false"
    LitNull -> fromText "null"
    LitArray items -> fromText "[" <> mconcat (intersperse (fromText ", ") (map buildArrayItem items)) <> fromText "]"
  where
    buildArrayItem (Nothing, v) = buildExpr v
    buildArrayItem (Just k, v) = buildExpr k <> fromText " => " <> buildExpr v

-- | Build arguments
buildArgs :: [Argument] -> Builder
buildArgs = mconcat . intersperse (fromText ", ") . map buildArg
  where
    buildArg :: Argument -> Builder
    buildArg arg = mconcat
        [ case argName arg of
            Nothing -> mempty
            Just (Name n) -> fromText n <> fromText ": "
        , if argUnpack arg then fromText "..." else mempty
        , buildExpr (argValue arg)
        ]

-- | Build binary operator
buildBinOp :: BinOp -> Builder
buildBinOp op = fromText $ case op of
    OpAdd -> "+"
    OpSub -> "-"
    OpMul -> "*"
    OpDiv -> "/"
    OpMod -> "%"
    OpPow -> "**"
    OpConcat -> "."
    OpEq -> "=="
    OpNeq -> "!="
    OpIdentical -> "==="
    OpNotIdentical -> "!=="
    OpLt -> "<"
    OpGt -> ">"
    OpLte -> "<="
    OpGte -> ">="
    OpSpaceship -> "<=>"
    OpAnd -> "&&"
    OpOr -> "||"
    OpXor -> "xor"
    OpBitAnd -> "&"
    OpBitOr -> "|"
    OpBitXor -> "^"
    OpShiftL -> "<<"
    OpShiftR -> ">>"
    OpCoalesce -> "??"
    OpInstanceOf -> "instanceof"

-- | Build unary operator
buildUnaryOp :: UnaryOp -> Builder
buildUnaryOp op = fromText $ case op of
    OpNot -> "!"
    OpBitNot -> "~"
    OpNeg -> "-"
    OpPos -> "+"
    OpPreInc -> "++"
    OpPreDec -> "--"
    OpPostInc -> "++"
    OpPostDec -> "--"
    OpClone -> "clone "
    OpPrint -> "print "
    OpSuppress -> "@"

-- | Escape string for PHP output
escapeString :: Text -> Text
escapeString = T.concatMap escape
  where
    escape '\'' = "\\'"
    escape '\\' = "\\\\"
    escape c = T.singleton c

-- | Indent a builder
indent :: Builder -> Builder
indent b = fromText "    " <> b  -- Simplified; real impl would indent each line

-- | Emit as statement (for single statements)
emitStatement :: Located Statement -> Text
emitStatement = toStrict . toLazyText . buildStatement

-- | Emit as expression (for single expressions)
emitExpr :: Located Expr -> Text
emitExpr = toStrict . toLazyText . buildExpr

-- | === Infrastructure Export Functions ===

-- | Generate php.ini recommendations based on detected issues
emitPhpIniRecommendations :: [SecurityIssue] -> Text
emitPhpIniRecommendations issues = T.unlines $
    [ "; Sanctify-PHP recommended php.ini settings"
    , "; Based on " <> T.pack (show (length issues)) <> " detected issues"
    , ""
    ] ++ catMaybes (map issueToIni issues)
  where
    issueToIni :: SecurityIssue -> Maybe Text
    issueToIni issue = case issueType issue of
        CommandInjection -> Just "disable_functions = exec,passthru,shell_exec,system,proc_open,popen"
        UnsafeDeserialization -> Just "; Consider: unserialize only with allowed_classes option"
        DangerousFunction -> Just "; Review disable_functions for: eval, assert, create_function"
        _ -> Nothing

-- | Generate nginx security rules
emitNginxRules :: [SecurityIssue] -> Text
emitNginxRules issues = T.unlines $
    [ "# Sanctify-PHP recommended nginx rules"
    , "# Complement to code-level hardening"
    , ""
    , "# Security headers"
    , "add_header X-Frame-Options \"SAMEORIGIN\" always;"
    , "add_header X-Content-Type-Options \"nosniff\" always;"
    , ""
    ] ++ if hasXss then xssRules else []
  where
    hasXss = any ((== CrossSiteScripting) . issueType) issues
    xssRules =
        [ "# XSS protection (code has potential XSS)"
        , "add_header Content-Security-Policy \"default-src 'self'\" always;"
        ]

-- | Generate Guix channel overrides
emitGuixOverrides :: [SecurityIssue] -> Text
emitGuixOverrides issues = T.unlines
    [ ";; Sanctify-PHP Guix overrides"
    , ";; Detected " <> T.pack (show (length issues)) <> " issues"
    , ""
    , "(define-public %sanctify-php-overrides"
    , "  '(" <> T.intercalate "\n    " (map issueToGuix issues) <> "))"
    ]
  where
    issueToGuix :: SecurityIssue -> Text
    issueToGuix issue = "(issue \"" <> T.pack (show (issueType issue)) <> "\" . \"" <> issueDescription issue <> "\")"
