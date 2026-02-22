-- | Transform PHP code to add strict types
-- SPDX-License-Identifier: AGPL-3.0-or-later
module Sanctify.Transform.StrictTypes
    ( -- * Transformations
      addStrictTypes
    , addAbspathCheck
    , addNonceVerification
    , addCapabilityCheck

      -- * Analysis
    , needsStrictTypes
    , needsAbspathCheck
    , needsNonceCheck
    , needsCapabilityCheck
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import Sanctify.AST

-- | Add declare(strict_types=1) if missing
addStrictTypes :: PhpFile -> PhpFile
addStrictTypes file
    | phpDeclareStrict file = file  -- Already has it
    | otherwise = file { phpDeclareStrict = True }

-- | Check if file needs strict_types
needsStrictTypes :: PhpFile -> Bool
needsStrictTypes = not . phpDeclareStrict

-- | Add ABSPATH check at the start of a WordPress plugin/theme file
addAbspathCheck :: PhpFile -> PhpFile
addAbspathCheck file
    | hasAbspathCheck file = file
    | otherwise = file
        { phpStatements = abspathCheckStmt : phpStatements file
        }
  where
    -- defined('ABSPATH') || exit;
    abspathCheckStmt :: Located Statement
    abspathCheckStmt = Located dummyPos $ StmtExpr $ Located dummyPos $
        ExprBinary OpOr
            (Located dummyPos $ ExprUnary OpNot $ Located dummyPos $
                ExprCall
                    (Located dummyPos $ ExprConstant $ QualifiedName [Name "defined"] False)
                    [Argument Nothing (Located dummyPos $ ExprLiteral $ LitString "ABSPATH") False])
            (Located dummyPos $ ExprCall
                (Located dummyPos $ ExprConstant $ QualifiedName [Name "exit"] False)
                [])

-- | Check if file has ABSPATH protection
hasAbspathCheck :: PhpFile -> Bool
hasAbspathCheck file = case phpStatements file of
    [] -> False
    (Located _ stmt : _) -> isAbspathCheck stmt
  where
    isAbspathCheck :: Statement -> Bool
    isAbspathCheck (StmtExpr (Located _ (ExprBinary OpOr left _))) =
        isDefinedAbspath (locNode left)
    isAbspathCheck (StmtIf cond _ _) =
        isDefinedAbspath (locNode cond)
    isAbspathCheck _ = False

    isDefinedAbspath :: Expr -> Bool
    isDefinedAbspath (ExprUnary OpNot (Located _ inner)) = isDefinedCall inner
    isDefinedAbspath expr = isDefinedCall expr

    isDefinedCall :: Expr -> Bool
    isDefinedCall (ExprCall (Located _ (ExprConstant qn)) args) =
        unName (last (qnParts qn)) == "defined" &&
        case args of
            [Argument _ (Located _ (ExprLiteral (LitString s))) _] ->
                s == "ABSPATH"
            _ -> False
    isDefinedCall _ = False

-- | Check if file needs ABSPATH protection
needsAbspathCheck :: PhpFile -> Bool
needsAbspathCheck = not . hasAbspathCheck

-- | Add nonce verification to a form handler
addNonceVerification :: Text -> Text -> [Located Statement] -> [Located Statement]
addNonceVerification nonceField nonceAction body =
    [nonceCheckStmt] ++ body
  where
    nonceCheckStmt :: Located Statement
    nonceCheckStmt = Located dummyPos $ StmtIf
        (Located dummyPos $ ExprUnary OpNot $ Located dummyPos $
            ExprCall
                (Located dummyPos $ ExprConstant $ QualifiedName [Name "wp_verify_nonce"] False)
                [ Argument Nothing
                    (Located dummyPos $ ExprArrayAccess
                        (Located dummyPos $ ExprVariable $ Variable "_POST")
                        (Just $ Located dummyPos $ ExprLiteral $ LitString nonceField))
                    False
                , Argument Nothing
                    (Located dummyPos $ ExprLiteral $ LitString nonceAction)
                    False
                ])
        [wpDieStmt]
        Nothing

    wpDieStmt :: Located Statement
    wpDieStmt = Located dummyPos $ StmtExpr $ Located dummyPos $
        ExprCall
            (Located dummyPos $ ExprConstant $ QualifiedName [Name "wp_die"] False)
            [Argument Nothing
                (Located dummyPos $ ExprLiteral $ LitString "Security check failed")
                False]

-- | Check if handler needs nonce verification
needsNonceCheck :: [Located Statement] -> Bool
needsNonceCheck stmts = not $ any hasNonceVerify stmts
  where
    hasNonceVerify :: Located Statement -> Bool
    hasNonceVerify (Located _ stmt) = case stmt of
        StmtExpr (Located _ expr) -> isNonceCall expr
        StmtIf (Located _ cond) _ _ -> isNonceCall cond || containsNonceCall cond
        _ -> False

    isNonceCall :: Expr -> Bool
    isNonceCall (ExprCall (Located _ (ExprConstant qn)) _) =
        let fn = unName $ last $ qnParts qn
        in fn `elem` ["wp_verify_nonce", "check_admin_referer", "check_ajax_referer"]
    isNonceCall _ = False

    containsNonceCall :: Expr -> Bool
    containsNonceCall (ExprUnary _ (Located _ inner)) = isNonceCall inner || containsNonceCall inner
    containsNonceCall (ExprBinary _ (Located _ l) (Located _ r)) =
        isNonceCall l || isNonceCall r || containsNonceCall l || containsNonceCall r
    containsNonceCall expr = isNonceCall expr

-- | Add capability check to an admin handler
addCapabilityCheck :: Text -> [Located Statement] -> [Located Statement]
addCapabilityCheck capability body =
    [capCheckStmt] ++ body
  where
    capCheckStmt :: Located Statement
    capCheckStmt = Located dummyPos $ StmtIf
        (Located dummyPos $ ExprUnary OpNot $ Located dummyPos $
            ExprCall
                (Located dummyPos $ ExprConstant $ QualifiedName [Name "current_user_can"] False)
                [Argument Nothing
                    (Located dummyPos $ ExprLiteral $ LitString capability)
                    False])
        [wpDieStmt]
        Nothing

    wpDieStmt :: Located Statement
    wpDieStmt = Located dummyPos $ StmtExpr $ Located dummyPos $
        ExprCall
            (Located dummyPos $ ExprConstant $ QualifiedName [Name "wp_die"] False)
            [Argument Nothing
                (Located dummyPos $ ExprLiteral $ LitString "You do not have permission to access this page.")
                False]

-- | Check if handler needs capability check
needsCapabilityCheck :: [Located Statement] -> Bool
needsCapabilityCheck stmts = not $ any hasCapCheck stmts
  where
    hasCapCheck :: Located Statement -> Bool
    hasCapCheck (Located _ stmt) = case stmt of
        StmtExpr (Located _ expr) -> isCapCall expr
        StmtIf (Located _ cond) _ _ -> isCapCall cond || containsCapCall cond
        _ -> False

    isCapCall :: Expr -> Bool
    isCapCall (ExprCall (Located _ (ExprConstant qn)) _) =
        unName (last (qnParts qn)) == "current_user_can"
    isCapCall _ = False

    containsCapCall :: Expr -> Bool
    containsCapCall (ExprUnary _ (Located _ inner)) = isCapCall inner || containsCapCall inner
    containsCapCall (ExprBinary _ (Located _ l) (Located _ r)) =
        isCapCall l || isCapCall r || containsCapCall l || containsCapCall r
    containsCapCall expr = isCapCall expr

-- | Dummy source position for generated code
dummyPos :: SourcePos
dummyPos = SourcePos "" 0 0
