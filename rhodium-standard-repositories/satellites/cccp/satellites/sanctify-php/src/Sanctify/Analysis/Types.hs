-- | Type inference for PHP code
-- SPDX-License-Identifier: AGPL-3.0-or-later
module Sanctify.Analysis.Types
    ( -- * Type inference
      inferTypes
    , InferredType(..)
    , TypeContext(..)
    , emptyContext

      -- * Type checking
    , checkTypeCompatibility
    , isNullable
    , isScalar

      -- * Type annotations
    , suggestTypeHint
    , suggestReturnType
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

import Sanctify.AST

-- | Inferred type with confidence level
data InferredType = InferredType
    { inferredPhpType :: PhpType
    , inferredConfidence :: Confidence
    , inferredNullable :: Bool
    , inferredSource :: TypeSource
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

-- | How confident we are in the inference
data Confidence = Certain | Likely | Possible | Unknown
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON)

-- | Where the type information came from
data TypeSource
    = FromLiteral           -- ^ Inferred from literal value
    | FromDefault           -- ^ Inferred from default parameter value
    | FromAssignment        -- ^ Inferred from assignment
    | FromReturn            -- ^ Inferred from return statements
    | FromDocBlock          -- ^ From PHPDoc annotation
    | FromCallPattern       -- ^ From how the value is used
    | FromWpFunction        -- ^ From WordPress function signature
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

-- | Type inference context
data TypeContext = TypeContext
    { ctxVariables :: Map Text InferredType
    , ctxFunctions :: Map Text FunctionSig
    , ctxClasses   :: Map Text ClassInfo
    , ctxConstants :: Map Text InferredType
    }
    deriving stock (Eq, Show, Generic)

-- | Function signature for inference
data FunctionSig = FunctionSig
    { sigParams :: [(Text, Maybe InferredType)]
    , sigReturn :: Maybe InferredType
    }
    deriving stock (Eq, Show, Generic)

-- | Class information for inference
data ClassInfo = ClassInfo
    { classProperties :: Map Text InferredType
    , classMethods    :: Map Text FunctionSig
    }
    deriving stock (Eq, Show, Generic)

type InferM = State TypeContext

-- | Empty context
emptyContext :: TypeContext
emptyContext = TypeContext Map.empty Map.empty Map.empty Map.empty

-- | Infer types for a PHP file
inferTypes :: TypeContext -> PhpFile -> TypeContext
inferTypes ctx file = execState (mapM_ inferStatement (phpStatements file)) ctx

-- | Infer types from a statement
inferStatement :: Located Statement -> InferM ()
inferStatement (Located _ stmt) = case stmt of
    StmtExpr expr -> void $ inferExpr expr

    StmtDecl decl -> inferDeclaration decl

    StmtIf cond thenStmts elseStmts -> do
        void $ inferExpr cond
        mapM_ inferStatement thenStmts
        maybe (pure ()) (mapM_ inferStatement) elseStmts

    StmtWhile cond body -> do
        void $ inferExpr cond
        mapM_ inferStatement body

    StmtFor init cond update body -> do
        maybe (pure ()) (void . inferExpr) init
        maybe (pure ()) (void . inferExpr) cond
        maybe (pure ()) (void . inferExpr) update
        mapM_ inferStatement body

    StmtForeach expr _ _ body -> do
        void $ inferExpr expr
        mapM_ inferStatement body

    StmtReturn (Just expr) -> void $ inferExpr expr

    _ -> pure ()

-- | Infer declaration types
inferDeclaration :: Declaration -> InferM ()
inferDeclaration decl = case decl of
    DeclFunction{fnName = Name name, fnParams = params, fnBody = body} -> do
        -- Infer parameter types from defaults
        paramTypes <- forM params $ \param -> do
            let pname = varName $ paramName param
            ptype <- case paramDefault param of
                Just defExpr -> Just <$> inferExpr defExpr
                Nothing -> pure Nothing
            pure (pname, ptype)

        -- Infer return type from body
        let returnExprs = concatMap extractReturns body
        returnType <- case returnExprs of
            [] -> pure Nothing
            (r:rs) -> do
                rt <- inferExpr r
                -- Check if all returns have same type
                rts <- mapM inferExpr rs
                if all ((== inferredPhpType rt) . inferredPhpType) rts
                    then pure $ Just rt
                    else pure $ Just rt { inferredConfidence = Possible }

        -- Store function signature
        modify $ \ctx -> ctx
            { ctxFunctions = Map.insert name (FunctionSig paramTypes returnType) (ctxFunctions ctx)
            }

        -- Process body
        mapM_ inferStatement body

    DeclClass{clsName = Name name, clsMembers = members} -> do
        -- Collect property types
        props <- fmap Map.fromList $ forM (concatMap extractProperties members) $ \prop ->
            case propDefault prop of
                Just defExpr -> do
                    t <- inferExpr defExpr
                    pure (unName $ propName prop, t)
                Nothing -> pure (unName $ propName prop, unknownType)

        -- Collect method signatures
        methods <- fmap Map.fromList $ forM (concatMap extractMethods members) $ \meth -> do
            let mname = unName $ methName meth
            -- Simplified: just record that method exists
            pure (mname, FunctionSig [] Nothing)

        modify $ \ctx -> ctx
            { ctxClasses = Map.insert name (ClassInfo props methods) (ctxClasses ctx)
            }

    _ -> pure ()

-- | Extract return expressions from statements
extractReturns :: Located Statement -> [Located Expr]
extractReturns (Located _ stmt) = case stmt of
    StmtReturn (Just expr) -> [expr]
    StmtIf _ thenStmts elseStmts ->
        concatMap extractReturns thenStmts ++
        maybe [] (concatMap extractReturns) elseStmts
    StmtWhile _ body -> concatMap extractReturns body
    StmtFor _ _ _ body -> concatMap extractReturns body
    StmtForeach _ _ _ body -> concatMap extractReturns body
    StmtTry tryBody catches finally ->
        concatMap extractReturns tryBody ++
        concatMap (concatMap extractReturns . catchBody) catches ++
        maybe [] (concatMap extractReturns) finally
    _ -> []

-- | Extract properties from class member
extractProperties :: ClassMember -> [ClassMember]
extractProperties m@MemberProperty{} = [m]
extractProperties _ = []

-- | Extract methods from class member
extractMethods :: ClassMember -> [ClassMember]
extractMethods m@MemberMethod{} = [m]
extractMethods _ = []

-- | Infer type of an expression
inferExpr :: Located Expr -> InferM InferredType
inferExpr (Located _ expr) = case expr of
    ExprLiteral lit -> pure $ inferLiteral lit

    ExprVariable (Variable name) -> do
        ctx <- get
        case Map.lookup name (ctxVariables ctx) of
            Just t -> pure t
            Nothing -> pure unknownType

    ExprBinary op left right -> do
        lt <- inferExpr left
        rt <- inferExpr right
        pure $ inferBinaryOp op lt rt

    ExprUnary OpNot _ -> pure $ InferredType TBool Certain False FromCallPattern
    ExprUnary OpNeg _ -> pure $ InferredType TInt Likely False FromCallPattern
    ExprUnary _ operand -> inferExpr operand

    ExprAssign (Located _ (ExprVariable (Variable name))) value -> do
        vtype <- inferExpr value
        modify $ \ctx -> ctx
            { ctxVariables = Map.insert name vtype (ctxVariables ctx)
            }
        pure vtype

    ExprCall callee args -> inferCallType callee args

    ExprMethodCall _ (Name method) _ -> inferMethodCallType method

    ExprNew qn _ -> pure $ InferredType (TClass qn) Certain False FromCallPattern

    ExprArrayAccess base _ -> do
        bt <- inferExpr base
        case inferredPhpType bt of
            TArray (Just elemType) -> pure $ InferredType elemType Likely True FromCallPattern
            _ -> pure $ unknownType { inferredNullable = True }

    ExprTernary _ (Just trueExpr) falseExpr -> do
        tt <- inferExpr trueExpr
        ft <- inferExpr falseExpr
        if inferredPhpType tt == inferredPhpType ft
            then pure tt
            else pure $ InferredType TMixed Possible (inferredNullable tt || inferredNullable ft) FromCallPattern

    ExprCast targetType _ -> pure $ InferredType targetType Certain False FromCallPattern

    ExprClosure{closureReturn = Just rt} ->
        pure $ InferredType (rtType rt) Certain (rtNullable rt) FromCallPattern

    ExprArrowFunction{arrowReturn = Just rt} ->
        pure $ InferredType (rtType rt) Certain (rtNullable rt) FromCallPattern

    _ -> pure unknownType

-- | Infer type from literal
inferLiteral :: Literal -> InferredType
inferLiteral lit = case lit of
    LitInt _ -> InferredType TInt Certain False FromLiteral
    LitFloat _ -> InferredType TFloat Certain False FromLiteral
    LitString _ -> InferredType TString Certain False FromLiteral
    LitBool _ -> InferredType TBool Certain False FromLiteral
    LitNull -> InferredType TNull Certain True FromLiteral
    LitArray _ -> InferredType (TArray Nothing) Certain False FromLiteral

-- | Infer result type of binary operation
inferBinaryOp :: BinOp -> InferredType -> InferredType -> InferredType
inferBinaryOp op _ _ = case op of
    OpAdd -> InferredType TInt Likely False FromCallPattern  -- Could be float
    OpSub -> InferredType TInt Likely False FromCallPattern
    OpMul -> InferredType TInt Likely False FromCallPattern
    OpDiv -> InferredType TFloat Likely False FromCallPattern
    OpMod -> InferredType TInt Certain False FromCallPattern
    OpPow -> InferredType TInt Likely False FromCallPattern
    OpConcat -> InferredType TString Certain False FromCallPattern
    OpEq -> InferredType TBool Certain False FromCallPattern
    OpNeq -> InferredType TBool Certain False FromCallPattern
    OpIdentical -> InferredType TBool Certain False FromCallPattern
    OpNotIdentical -> InferredType TBool Certain False FromCallPattern
    OpLt -> InferredType TBool Certain False FromCallPattern
    OpGt -> InferredType TBool Certain False FromCallPattern
    OpLte -> InferredType TBool Certain False FromCallPattern
    OpGte -> InferredType TBool Certain False FromCallPattern
    OpSpaceship -> InferredType TInt Certain False FromCallPattern
    OpAnd -> InferredType TBool Certain False FromCallPattern
    OpOr -> InferredType TBool Certain False FromCallPattern
    OpXor -> InferredType TBool Certain False FromCallPattern
    OpBitAnd -> InferredType TInt Certain False FromCallPattern
    OpBitOr -> InferredType TInt Certain False FromCallPattern
    OpBitXor -> InferredType TInt Certain False FromCallPattern
    OpShiftL -> InferredType TInt Certain False FromCallPattern
    OpShiftR -> InferredType TInt Certain False FromCallPattern
    OpCoalesce -> InferredType TMixed Possible False FromCallPattern
    OpInstanceOf -> InferredType TBool Certain False FromCallPattern

-- | Infer type from function call
inferCallType :: Located Expr -> [Argument] -> InferM InferredType
inferCallType (Located _ (ExprConstant qn)) _ = do
    let fname = T.toLower $ unName $ last $ qnParts qn
    pure $ case fname of
        -- String functions
        "strlen" -> InferredType TInt Certain False FromWpFunction
        "substr" -> InferredType TString Certain False FromWpFunction
        "str_replace" -> InferredType TString Certain False FromWpFunction
        "trim" -> InferredType TString Certain False FromWpFunction
        "strtolower" -> InferredType TString Certain False FromWpFunction
        "strtoupper" -> InferredType TString Certain False FromWpFunction
        "sprintf" -> InferredType TString Certain False FromWpFunction

        -- Array functions
        "count" -> InferredType TInt Certain False FromWpFunction
        "array_keys" -> InferredType (TArray (Just TString)) Certain False FromWpFunction
        "array_values" -> InferredType (TArray Nothing) Certain False FromWpFunction
        "array_merge" -> InferredType (TArray Nothing) Certain False FromWpFunction
        "in_array" -> InferredType TBool Certain False FromWpFunction
        "array_key_exists" -> InferredType TBool Certain False FromWpFunction

        -- Type checking
        "is_array" -> InferredType TBool Certain False FromWpFunction
        "is_string" -> InferredType TBool Certain False FromWpFunction
        "is_int" -> InferredType TBool Certain False FromWpFunction
        "is_numeric" -> InferredType TBool Certain False FromWpFunction
        "is_null" -> InferredType TBool Certain False FromWpFunction
        "isset" -> InferredType TBool Certain False FromWpFunction
        "empty" -> InferredType TBool Certain False FromWpFunction

        -- WordPress functions
        "get_option" -> InferredType TMixed Certain True FromWpFunction
        "get_post_meta" -> InferredType TMixed Certain True FromWpFunction
        "get_user_meta" -> InferredType TMixed Certain True FromWpFunction
        "get_the_ID" -> InferredType TInt Certain False FromWpFunction
        "get_current_user_id" -> InferredType TInt Certain False FromWpFunction
        "wp_get_current_user" -> InferredType (TClass (QualifiedName [Name "WP_User"] False)) Certain False FromWpFunction
        "get_post" -> InferredType (TClass (QualifiedName [Name "WP_Post"] False)) Certain True FromWpFunction
        "get_posts" -> InferredType (TArray (Just (TClass (QualifiedName [Name "WP_Post"] False)))) Certain False FromWpFunction
        "wp_insert_post" -> InferredType TInt Certain False FromWpFunction
        "current_user_can" -> InferredType TBool Certain False FromWpFunction
        "is_admin" -> InferredType TBool Certain False FromWpFunction
        "is_user_logged_in" -> InferredType TBool Certain False FromWpFunction

        -- Escaping functions return strings
        "esc_html" -> InferredType TString Certain False FromWpFunction
        "esc_attr" -> InferredType TString Certain False FromWpFunction
        "esc_url" -> InferredType TString Certain False FromWpFunction
        "esc_js" -> InferredType TString Certain False FromWpFunction
        "wp_kses_post" -> InferredType TString Certain False FromWpFunction

        -- Sanitization
        "sanitize_text_field" -> InferredType TString Certain False FromWpFunction
        "sanitize_email" -> InferredType TString Certain False FromWpFunction
        "absint" -> InferredType TInt Certain False FromWpFunction
        "intval" -> InferredType TInt Certain False FromWpFunction
        "floatval" -> InferredType TFloat Certain False FromWpFunction
        "boolval" -> InferredType TBool Certain False FromWpFunction
        "strval" -> InferredType TString Certain False FromWpFunction

        _ -> unknownType

inferCallType _ _ = pure unknownType

-- | Infer type from method call
inferMethodCallType :: Text -> InferM InferredType
inferMethodCallType method = pure $ case T.toLower method of
    -- PDO methods
    "prepare" -> InferredType (TClass (QualifiedName [Name "PDOStatement"] False)) Certain False FromCallPattern
    "execute" -> InferredType TBool Certain False FromCallPattern
    "fetch" -> InferredType (TArray Nothing) Certain True FromCallPattern
    "fetchall" -> InferredType (TArray Nothing) Certain False FromCallPattern
    "rowcount" -> InferredType TInt Certain False FromCallPattern

    -- WordPress $wpdb methods
    "get_var" -> InferredType TString Certain True FromWpFunction
    "get_row" -> InferredType (TArray Nothing) Certain True FromWpFunction
    "get_col" -> InferredType (TArray (Just TString)) Certain False FromWpFunction
    "get_results" -> InferredType (TArray Nothing) Certain False FromWpFunction
    "insert" -> InferredType TBool Certain False FromWpFunction
    "update" -> InferredType TBool Certain False FromWpFunction
    "delete" -> InferredType TBool Certain False FromWpFunction

    _ -> unknownType

-- | Unknown type placeholder
unknownType :: InferredType
unknownType = InferredType TMixed Unknown True FromCallPattern

-- | Check if types are compatible
checkTypeCompatibility :: PhpType -> PhpType -> Bool
checkTypeCompatibility expected actual = case (expected, actual) of
    (TMixed, _) -> True
    (_, TMixed) -> True
    (TNull, _) -> True  -- null is compatible with nullable types
    (TUnion types, t) -> any (`checkTypeCompatibility` t) types
    (t, TUnion types) -> any (checkTypeCompatibility t) types
    (TNullable t1, t2) -> checkTypeCompatibility t1 t2 || t2 == TNull
    (t1, TNullable t2) -> checkTypeCompatibility t1 t2
    (TArray _, TArray _) -> True  -- Arrays are covariant in PHP
    (TClass n1, TClass n2) -> n1 == n2  -- Simplified: would need inheritance check
    (t1, t2) -> t1 == t2

-- | Check if a type is nullable
isNullable :: InferredType -> Bool
isNullable = inferredNullable

-- | Check if a type is scalar
isScalar :: PhpType -> Bool
isScalar t = t `elem` [TInt, TFloat, TString, TBool]

-- | Suggest a type hint for a parameter based on usage
suggestTypeHint :: InferredType -> Maybe TypeHint
suggestTypeHint inferred
    | inferredConfidence inferred >= Likely =
        Just $ TypeHint (inferredPhpType inferred) (inferredNullable inferred)
    | otherwise = Nothing

-- | Suggest a return type for a function
suggestReturnType :: [InferredType] -> Maybe ReturnType
suggestReturnType [] = Just $ ReturnType TVoid False
suggestReturnType types
    | all ((== inferredPhpType (head types)) . inferredPhpType) types
    , all ((>= Likely) . inferredConfidence) types =
        let nullable = any inferredNullable types
        in Just $ ReturnType (inferredPhpType (head types)) nullable
    | otherwise = Nothing
