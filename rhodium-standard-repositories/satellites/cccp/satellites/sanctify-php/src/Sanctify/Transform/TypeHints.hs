-- | Transform PHP code to add type hints
-- SPDX-License-Identifier: AGPL-3.0-or-later
module Sanctify.Transform.TypeHints
    ( -- * Adding type hints
      addParameterTypeHints
    , addReturnTypeHints
    , addPropertyTypeHints

      -- * Type inference helpers
    , inferParameterType
    , inferReturnType
    , inferPropertyType

      -- * Full file transformation
    , addAllTypeHints
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe, mapMaybe)

import Sanctify.AST
import Sanctify.Analysis.Types

-- | Add type hints to function parameters based on defaults and usage
addParameterTypeHints :: TypeContext -> Declaration -> Declaration
addParameterTypeHints ctx decl@DeclFunction{fnParams = params, fnBody = body} =
    decl { fnParams = map addParamHint params }
  where
    addParamHint :: Parameter -> Parameter
    addParamHint param
        | Just _ <- paramType param = param  -- Already has type
        | otherwise = param { paramType = inferParameterType ctx param body }

addParameterTypeHints _ decl = decl

-- | Add return type to function based on return statements
addReturnTypeHints :: TypeContext -> Declaration -> Declaration
addReturnTypeHints ctx decl@DeclFunction{fnReturnType = Nothing, fnBody = body} =
    decl { fnReturnType = inferReturnType ctx body }
addReturnTypeHints _ decl = decl

-- | Add type hints to class properties
addPropertyTypeHints :: TypeContext -> ClassMember -> ClassMember
addPropertyTypeHints ctx prop@MemberProperty{propType = Nothing, propDefault = Just defExpr} =
    prop { propType = inferPropertyType ctx defExpr }
addPropertyTypeHints _ member = member

-- | Infer parameter type from default value and usage
inferParameterType :: TypeContext -> Parameter -> [Located Statement] -> Maybe TypeHint
inferParameterType ctx param body =
    case paramDefault param of
        Just defExpr ->
            let inferred = inferFromDefault defExpr
            in Just $ TypeHint (inferredPhpType inferred) (inferredNullable inferred)
        Nothing ->
            -- Try to infer from usage in body
            inferFromUsage (varName $ paramName param) body
  where
    inferFromDefault :: Located Expr -> InferredType
    inferFromDefault (Located _ expr) = case expr of
        ExprLiteral lit -> inferLiteralType lit
        ExprLiteral LitNull -> InferredType TMixed Unknown True FromDefault
        ExprNew qn _ -> InferredType (TClass qn) Certain False FromDefault
        _ -> InferredType TMixed Unknown False FromDefault

    inferLiteralType :: Literal -> InferredType
    inferLiteralType lit = case lit of
        LitInt _ -> InferredType TInt Certain False FromDefault
        LitFloat _ -> InferredType TFloat Certain False FromDefault
        LitString _ -> InferredType TString Certain False FromDefault
        LitBool _ -> InferredType TBool Certain False FromDefault
        LitNull -> InferredType TNull Certain True FromDefault
        LitArray _ -> InferredType (TArray Nothing) Certain False FromDefault

    inferFromUsage :: Text -> [Located Statement] -> Maybe TypeHint
    inferFromUsage _name _stmts = Nothing  -- TODO: analyze usage patterns

-- | Infer return type from function body
inferReturnType :: TypeContext -> [Located Statement] -> Maybe ReturnType
inferReturnType ctx body =
    let returns = extractReturns body
    in case returns of
        [] -> Just $ ReturnType TVoid False
        rets ->
            let types = map (inferExprType ctx) rets
            in if allSameType types
                then Just $ ReturnType (inferredPhpType $ head types) (any inferredNullable types)
                else Nothing  -- Can't infer union type safely
  where
    allSameType :: [InferredType] -> Bool
    allSameType [] = True
    allSameType (t:ts) = all ((== inferredPhpType t) . inferredPhpType) ts

-- | Extract all return expressions from statements
extractReturns :: [Located Statement] -> [Located Expr]
extractReturns = concatMap extractFromStmt
  where
    extractFromStmt :: Located Statement -> [Located Expr]
    extractFromStmt (Located _ stmt) = case stmt of
        StmtReturn (Just expr) -> [expr]
        StmtIf _ thenStmts elseStmts ->
            extractReturns thenStmts ++
            maybe [] extractReturns elseStmts
        StmtWhile _ stmts -> extractReturns stmts
        StmtFor _ _ _ stmts -> extractReturns stmts
        StmtForeach _ _ _ stmts -> extractReturns stmts
        StmtSwitch _ cases -> concatMap (extractReturns . caseBody) cases
        StmtMatch _ arms -> map matchResult arms
        StmtTry tryStmts catches finally ->
            extractReturns tryStmts ++
            concatMap (extractReturns . catchBody) catches ++
            maybe [] extractReturns finally
        _ -> []

-- | Infer expression type
inferExprType :: TypeContext -> Located Expr -> InferredType
inferExprType _ctx (Located _ expr) = case expr of
    ExprLiteral (LitInt _) -> InferredType TInt Certain False FromLiteral
    ExprLiteral (LitFloat _) -> InferredType TFloat Certain False FromLiteral
    ExprLiteral (LitString _) -> InferredType TString Certain False FromLiteral
    ExprLiteral (LitBool _) -> InferredType TBool Certain False FromLiteral
    ExprLiteral LitNull -> InferredType TNull Certain True FromLiteral
    ExprLiteral (LitArray _) -> InferredType (TArray Nothing) Certain False FromLiteral
    ExprNew qn _ -> InferredType (TClass qn) Certain False FromCallPattern
    ExprBinary op _ _ -> inferBinOpType op
    ExprUnary OpNot _ -> InferredType TBool Certain False FromCallPattern
    ExprCall callee _ -> inferCallReturnType callee
    _ -> InferredType TMixed Unknown False FromCallPattern

-- | Infer type from binary operator
inferBinOpType :: BinOp -> InferredType
inferBinOpType op = case op of
    OpConcat -> InferredType TString Certain False FromCallPattern
    OpAdd -> InferredType TInt Likely False FromCallPattern
    OpSub -> InferredType TInt Likely False FromCallPattern
    OpMul -> InferredType TInt Likely False FromCallPattern
    OpDiv -> InferredType TFloat Likely False FromCallPattern
    OpMod -> InferredType TInt Certain False FromCallPattern
    OpEq -> InferredType TBool Certain False FromCallPattern
    OpNeq -> InferredType TBool Certain False FromCallPattern
    OpIdentical -> InferredType TBool Certain False FromCallPattern
    OpNotIdentical -> InferredType TBool Certain False FromCallPattern
    OpLt -> InferredType TBool Certain False FromCallPattern
    OpGt -> InferredType TBool Certain False FromCallPattern
    OpLte -> InferredType TBool Certain False FromCallPattern
    OpGte -> InferredType TBool Certain False FromCallPattern
    OpAnd -> InferredType TBool Certain False FromCallPattern
    OpOr -> InferredType TBool Certain False FromCallPattern
    _ -> InferredType TMixed Unknown False FromCallPattern

-- | Infer return type from function call
inferCallReturnType :: Located Expr -> InferredType
inferCallReturnType (Located _ (ExprConstant qn)) =
    let fname = T.toLower $ unName $ last $ qnParts qn
    in case fname of
        -- String functions
        "strlen" -> InferredType TInt Certain False FromWpFunction
        "substr" -> InferredType TString Certain False FromWpFunction
        "trim" -> InferredType TString Certain False FromWpFunction
        "strtolower" -> InferredType TString Certain False FromWpFunction
        "strtoupper" -> InferredType TString Certain False FromWpFunction
        "sprintf" -> InferredType TString Certain False FromWpFunction

        -- Array functions
        "count" -> InferredType TInt Certain False FromWpFunction
        "array_keys" -> InferredType (TArray (Just TString)) Certain False FromWpFunction
        "array_values" -> InferredType (TArray Nothing) Certain False FromWpFunction
        "in_array" -> InferredType TBool Certain False FromWpFunction

        -- WordPress escaping
        "esc_html" -> InferredType TString Certain False FromWpFunction
        "esc_attr" -> InferredType TString Certain False FromWpFunction
        "esc_url" -> InferredType TString Certain False FromWpFunction

        -- WordPress sanitization
        "sanitize_text_field" -> InferredType TString Certain False FromWpFunction
        "absint" -> InferredType TInt Certain False FromWpFunction
        "intval" -> InferredType TInt Certain False FromWpFunction

        -- WordPress queries
        "get_option" -> InferredType TMixed Certain True FromWpFunction
        "get_post" -> InferredType (TClass (QualifiedName [Name "WP_Post"] False)) Likely True FromWpFunction
        "current_user_can" -> InferredType TBool Certain False FromWpFunction

        _ -> InferredType TMixed Unknown False FromCallPattern
inferCallReturnType _ = InferredType TMixed Unknown False FromCallPattern

-- | Infer property type from default value
inferPropertyType :: TypeContext -> Located Expr -> Maybe TypeHint
inferPropertyType _ctx (Located _ expr) = case expr of
    ExprLiteral (LitInt _) -> Just $ TypeHint TInt False
    ExprLiteral (LitFloat _) -> Just $ TypeHint TFloat False
    ExprLiteral (LitString _) -> Just $ TypeHint TString False
    ExprLiteral (LitBool _) -> Just $ TypeHint TBool False
    ExprLiteral LitNull -> Nothing  -- Can't infer type from null alone
    ExprLiteral (LitArray _) -> Just $ TypeHint (TArray Nothing) False
    ExprNew qn _ -> Just $ TypeHint (TClass qn) False
    _ -> Nothing

-- | Add all type hints to a PHP file
addAllTypeHints :: TypeContext -> PhpFile -> PhpFile
addAllTypeHints ctx file = file
    { phpStatements = map (transformStatement ctx) (phpStatements file)
    }
  where
    transformStatement :: TypeContext -> Located Statement -> Located Statement
    transformStatement c (Located pos (StmtDecl decl)) =
        Located pos $ StmtDecl $ transformDecl c decl
    transformStatement c (Located pos stmt) = Located pos $ case stmt of
        StmtIf cond thenStmts elseStmts ->
            StmtIf cond
                (map (transformStatement c) thenStmts)
                (fmap (map (transformStatement c)) elseStmts)
        StmtWhile cond body ->
            StmtWhile cond (map (transformStatement c) body)
        StmtFor i cond u body ->
            StmtFor i cond u (map (transformStatement c) body)
        StmtForeach e v k body ->
            StmtForeach e v k (map (transformStatement c) body)
        StmtTry tryBody catches finally ->
            StmtTry
                (map (transformStatement c) tryBody)
                (map (\catch -> catch { catchBody = map (transformStatement c) (catchBody catch) }) catches)
                (fmap (map (transformStatement c)) finally)
        other -> other

    transformDecl :: TypeContext -> Declaration -> Declaration
    transformDecl c decl = case decl of
        fn@DeclFunction{} ->
            addReturnTypeHints c $ addParameterTypeHints c fn
        cls@DeclClass{clsMembers = members} ->
            cls { clsMembers = map (transformMember c) members }
        other -> other

    transformMember :: TypeContext -> ClassMember -> ClassMember
    transformMember c member = case member of
        prop@MemberProperty{} -> addPropertyTypeHints c prop
        meth@MemberMethod{methBody = Just body} ->
            let withParams = addMethodParamHints c meth
                withReturn = addMethodReturnHint c withParams
            in withReturn { methBody = Just $ map (transformStatement c) body }
        other -> other

    addMethodParamHints :: TypeContext -> ClassMember -> ClassMember
    addMethodParamHints c meth@MemberMethod{methParams = params, methBody = Just body} =
        meth { methParams = map (addParamHint c body) params }
    addMethodParamHints _ meth = meth

    addParamHint :: TypeContext -> [Located Statement] -> Parameter -> Parameter
    addParamHint c body param
        | Just _ <- paramType param = param
        | otherwise = param { paramType = inferParameterType c param body }

    addMethodReturnHint :: TypeContext -> ClassMember -> ClassMember
    addMethodReturnHint c meth@MemberMethod{methReturn = Nothing, methBody = Just body} =
        meth { methReturn = inferReturnType c body }
    addMethodReturnHint _ meth = meth
