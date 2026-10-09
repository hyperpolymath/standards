{-# LANGUAGE OverloadedStrings #-}
-- | Advanced Security Analysis - Beyond OWASP Top 10
-- SPDX-License-Identifier: MPL-2.0
module Sanctify.Analysis.Advanced
    ( -- * Advanced vulnerability detection
      checkReDoS
    , checkSSRF
    , checkXXE
    , checkTOCTOU
    , checkMassAssignment
    , checkPrototypePollution
    , checkOpenRedirect

      -- * WordPress-specific advanced checks
    , checkPrivilegeEscalation
    , checkObjectInjection
    , checkFileInclusionChain

      -- * Timing attacks
    , checkTimingAttacks
    , checkConstantTimeComparison

      -- * Data exposure
    , checkSensitiveDataExposure
    , checkInformationDisclosure

      -- * Types
    , AdvancedIssue(..)
    , AdvancedIssueType(..)
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (when)
import Control.Monad.Writer
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

import Sanctify.AST
import Sanctify.Analysis.Security (SecurityIssue(..), Severity(..))

-- | Types of advanced security issues
data AdvancedIssueType
    = ReDoS                 -- ^ Regular expression denial of service
    | SSRF                  -- ^ Server-side request forgery
    | XXE                   -- ^ XML external entity injection
    | TOCTOU                -- ^ Time-of-check-time-of-use
    | MassAssignment        -- ^ Mass assignment vulnerability
    | ObjectInjection       -- ^ PHP object injection
    | PrototypePollution    -- ^ Prototype pollution (array key overwrite)
    | OpenRedirect          -- ^ Open redirect vulnerability
    | PrivilegeEscalation   -- ^ Privilege escalation
    | TimingAttack          -- ^ Timing attack vulnerability
    | SensitiveDataExposure -- ^ Sensitive data in logs/errors
    | InformationDisclosure -- ^ Information disclosure
    | FileInclusionChain    -- ^ Chained file inclusion vulnerability
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

-- | Advanced security issue
data AdvancedIssue = AdvancedIssue
    { advType        :: AdvancedIssueType
    , advSeverity    :: Severity
    , advLocation    :: SourcePos
    , advDescription :: Text
    , advRemedy      :: Text
    , advCode        :: Maybe Text
    , advCWE         :: Maybe Int  -- CWE identifier
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

type AdvancedM = Writer [AdvancedIssue]

-- | Check for Regular Expression Denial of Service (ReDoS)
-- Detects catastrophic backtracking patterns in regexes
checkReDoS :: Located Expr -> [AdvancedIssue]
checkReDoS (Located pos expr) = case expr of
    ExprCall (Located _ (ExprConstant qn)) args
        | fname `elem` ["preg_match", "preg_match_all", "preg_replace", "preg_split"]
        , not (null args) ->
            let pattern = argValue $ head args
            in case extractStringLiteral pattern of
                Just regex | hasDangerousPattern regex ->
                    [AdvancedIssue ReDoS High pos
                        "Regular expression may cause catastrophic backtracking (ReDoS)"
                        "Avoid nested quantifiers, alternation with overlap, or use atomic groups"
                        (Just regex)
                        (Just 1333)]  -- CWE-1333: Inefficient Regular Expression Complexity
                _ -> []
      where fname = T.toLower $ unName $ last $ qnParts qn
    _ -> []
  where
    -- Detect dangerous regex patterns
    hasDangerousPattern :: Text -> Bool
    hasDangerousPattern regex =
        -- Nested quantifiers: (a+)+ or (a*)* or (a+)*
        ("(.*)*" `T.isInfixOf` regex) ||
        ("(.+)+" `T.isInfixOf` regex) ||
        ("(.*++" `T.isInfixOf` regex) ||
        -- Alternation with overlap: (a|a)*
        ("(a|a)" `T.isInfixOf` regex) ||
        -- Many quantifiers in succession
        (T.length (T.filter (`elem` ['*', '+', '?']) regex) > 5)

-- | Check for Server-Side Request Forgery (SSRF)
checkSSRF :: Located Expr -> [AdvancedIssue]
checkSSRF (Located pos expr) = case expr of
    ExprCall (Located _ (ExprConstant qn)) args
        | fname `elem` ssrfFunctions
        , any (containsUserInput . argValue) args ->
            [AdvancedIssue SSRF High pos
                ("SSRF vulnerability in " <> fname <> ": user controls URL")
                "Validate URLs against whitelist, use URL parsing to check scheme/host"
                (Just fname)
                (Just 918)]  -- CWE-918: Server-Side Request Forgery
      where fname = T.toLower $ unName $ last $ qnParts qn

    ExprMethodCall obj (Name method) args
        | T.toLower method `elem` ["get", "post", "request", "fetch"]
        , containsUserInput obj || any (containsUserInput . argValue) args ->
            [AdvancedIssue SSRF High pos
                "SSRF via HTTP client: user controls request URL"
                "Validate URL scheme (http/https only), check against whitelist"
                (Just method)
                (Just 918)]
    _ -> []
  where
    ssrfFunctions = ["file_get_contents", "fopen", "curl_exec", "curl_init",
                     "wp_remote_get", "wp_remote_post", "wp_remote_request"]

    containsUserInput :: Located Expr -> Bool
    containsUserInput (Located _ e) = case e of
        ExprVariable (Variable name) ->
            name `elem` ["_GET", "_POST", "_REQUEST", "_COOKIE", "_SERVER"]
        ExprArrayAccess base _ -> containsUserInput base
        ExprBinary OpConcat left right -> containsUserInput left || containsUserInput right
        _ -> False

-- | Check for XML External Entity (XXE) injection
checkXXE :: Located Expr -> [AdvancedIssue]
checkXXE (Located pos expr) = case expr of
    ExprCall (Located _ (ExprConstant qn)) args
        | let fname = T.toLower $ unName $ last $ qnParts qn
        , fname `elem` ["simplexml_load_string", "simplexml_load_file", "SimpleXMLElement"]
        , not (hasLibxmlDisableEntityLoader args)
        , any (exprContainsUserInput . argValue) args ->
            [AdvancedIssue XXE Critical pos
                ("XXE vulnerability: " <> fname <> " without entity loader disabled")
                "Call libxml_disable_entity_loader(true) before parsing XML"
                (Just fname)
                (Just 611)]  -- CWE-611: Improper Restriction of XML External Entity Reference

    ExprNew qn args
        | className `elem` ["DOMDocument", "SimpleXMLElement"]
        , not (hasLibxmlDisableEntityLoader args) ->
            [AdvancedIssue XXE Critical pos
                ("XXE vulnerability: " <> className <> " without protection")
                "Disable entity loading and set LIBXML_NOENT | LIBXML_DTDLOAD options"
                (Just className)
                (Just 611)]
      where className = T.pack $ show qn
    _ -> []
  where
    hasLibxmlDisableEntityLoader :: [Argument] -> Bool
    hasLibxmlDisableEntityLoader _args = False  -- TODO: check for LIBXML_NOENT option

    exprContainsUserInput :: Located Expr -> Bool
    exprContainsUserInput (Located _ e) = case e of
        ExprVariable (Variable name) ->
            name `elem` ["_GET", "_POST", "_REQUEST", "_COOKIE", "_SERVER"]
        ExprArrayAccess base _ -> exprContainsUserInput base
        ExprBinary OpConcat left right -> exprContainsUserInput left || exprContainsUserInput right
        _ -> False

-- | Check for Time-of-Check-Time-of-Use (TOCTOU) race conditions
checkTOCTOU :: [Located Statement] -> [AdvancedIssue]
checkTOCTOU stmts = execWriter $ checkStatements stmts
  where
    checkStatements :: [Located Statement] -> AdvancedM ()
    checkStatements ss = mapM_ checkPair (zip ss (drop 1 ss))

    checkPair :: (Located Statement, Located Statement) -> AdvancedM ()
    checkPair (Located pos1 stmt1, Located pos2 stmt2) = do
        -- Check for file_exists() followed by file operation
        when (isFileCheck stmt1 && isFileOp stmt2) $
            tell [AdvancedIssue TOCTOU Medium pos1
                "TOCTOU race condition: file checked then operated on"
                "Use atomic operations or file locking (flock)"
                Nothing
                (Just 367)]  -- CWE-367: Time-of-Check Time-of-Use Race Condition

        -- Check for is_writable/is_readable followed by file write/read
        when (isPermissionCheck stmt1 && isFileOp stmt2) $
            tell [AdvancedIssue TOCTOU Medium pos1
                "TOCTOU: permission check followed by file operation"
                "Check permissions after opening file or use exception handling"
                Nothing
                (Just 367)]

    isFileCheck :: Statement -> Bool
    isFileCheck (StmtExpr (Located _ (ExprCall (Located _ (ExprConstant qn)) _))) =
        let fn = T.toLower $ unName $ last $ qnParts qn
        in fn `elem` ["file_exists", "is_file", "is_dir"]
    isFileCheck _ = False

    isPermissionCheck :: Statement -> Bool
    isPermissionCheck (StmtExpr (Located _ (ExprCall (Located _ (ExprConstant qn)) _))) =
        let fn = T.toLower $ unName $ last $ qnParts qn
        in fn `elem` ["is_writable", "is_writeable", "is_readable"]
    isPermissionCheck _ = False

    isFileOp :: Statement -> Bool
    isFileOp (StmtExpr (Located _ (ExprCall (Located _ (ExprConstant qn)) _))) =
        let fn = T.toLower $ unName $ last $ qnParts qn
        in fn `elem` ["fopen", "file_get_contents", "file_put_contents",
                      "unlink", "rename", "copy", "move_uploaded_file"]
    isFileOp _ = False

-- | Check for mass assignment vulnerabilities
checkMassAssignment :: Located Expr -> [AdvancedIssue]
checkMassAssignment (Located pos expr) = case expr of
    -- Direct $_POST assignment to object properties
    ExprAssign (Located _ (ExprPropertyAccess _ _)) (Located _ (ExprVariable (Variable "_POST"))) ->
        [AdvancedIssue MassAssignment High pos
            "Mass assignment: entire $_POST assigned to object"
            "Explicitly assign only allowed fields after validation"
            Nothing
            (Just 915)]  -- CWE-915: Improperly Controlled Modification of Dynamically-Determined Object Attributes

    -- Array merge with $_POST
    ExprCall (Located _ (ExprConstant qn)) args
        | fname == "array_merge"
        , any isPostVariable args ->
            [AdvancedIssue MassAssignment Medium pos
                "Mass assignment via array_merge with $_POST"
                "Filter $_POST array to only allowed keys before merging"
                (Just "array_merge")
                (Just 915)]
      where fname = T.toLower $ unName $ last $ qnParts qn
    _ -> []
  where
    isPostVariable :: Argument -> Bool
    isPostVariable arg = case locNode (argValue arg) of
        ExprVariable (Variable "_POST") -> True
        ExprVariable (Variable "_REQUEST") -> True
        _ -> False

-- | Check for PHP object injection via unserialize
checkObjectInjection :: Located Expr -> [AdvancedIssue]
checkObjectInjection (Located pos expr) = case expr of
    ExprCall (Located _ (ExprConstant qn)) args
        | fname == "unserialize"
        , not (hasAllowedClassesOption args) ->
            let severity = if any containsUserInput args then Critical else High
            in [AdvancedIssue ObjectInjection severity pos
                   "PHP object injection: unserialize without allowed_classes restriction"
                   "Specify 'allowed_classes' => [] or use json_decode instead"
                   (Just "unserialize")
                   (Just 502)]  -- CWE-502: Deserialization of Untrusted Data
      where fname = T.toLower $ unName $ last $ qnParts qn
    _ -> []
  where
    hasAllowedClassesOption :: [Argument] -> Bool
    hasAllowedClassesOption args = length args >= 2  -- Simplified check

    containsUserInput :: Argument -> Bool
    containsUserInput arg = case locNode (argValue arg) of
        ExprVariable (Variable v) -> v `elem` ["_GET", "_POST", "_COOKIE", "_REQUEST"]
        ExprArrayAccess base _ -> containsUserInput (Argument Nothing base False)
        _ -> False

-- | Check for open redirect vulnerabilities
checkOpenRedirect :: Located Expr -> [AdvancedIssue]
checkOpenRedirect (Located pos expr) = case expr of
    ExprCall (Located _ (ExprConstant qn)) args
        | fname `elem` ["header", "wp_redirect", "wp_safe_redirect"]
        , any hasLocationHeader args ->
            [AdvancedIssue OpenRedirect Medium pos
                "Open redirect: user controls redirect URL"
                "Validate redirect URL against whitelist or use wp_safe_redirect()"
                (Just fname)
                (Just 601)]  -- CWE-601: URL Redirection to Untrusted Site
      where fname = T.toLower $ unName $ last $ qnParts qn
    _ -> []
  where
    hasLocationHeader :: Argument -> Bool
    hasLocationHeader arg = case locNode (argValue arg) of
        ExprBinary OpConcat (Located _ (ExprLiteral (LitString s))) right
            | "Location:" `T.isPrefixOf` s -> containsUserInput right
        _ -> False

    containsUserInput :: Located Expr -> Bool
    containsUserInput (Located _ e) = case e of
        ExprVariable (Variable v) -> v `elem` ["_GET", "_POST", "_REQUEST"]
        ExprArrayAccess base _ -> containsUserInput base
        _ -> False

-- | Check for WordPress privilege escalation
checkPrivilegeEscalation :: PhpFile -> [AdvancedIssue]
checkPrivilegeEscalation file = execWriter $ mapM_ checkStmt (phpStatements file)
  where
    checkStmt :: Located Statement -> AdvancedM ()
    checkStmt (Located pos stmt) = case stmt of
        StmtExpr expr -> checkExpr pos expr
        StmtDecl (DeclFunction{fnBody = body}) -> mapM_ checkStmt body
        _ -> pure ()

    checkExpr :: SourcePos -> Located Expr -> AdvancedM ()
    checkExpr pos (Located _ expr) = case expr of
        -- wp_set_current_user without capability check
        ExprCall (Located _ (ExprConstant qn)) _
            | fname == "wp_set_current_user"
            , not (hasCapabilityCheck file) ->
                tell [AdvancedIssue PrivilegeEscalation Critical pos
                    "Privilege escalation: wp_set_current_user without capability check"
                    "Add current_user_can() check before changing user context"
                    (Just "wp_set_current_user")
                    (Just 269)]  -- CWE-269: Improper Privilege Management
          where fname = T.toLower $ unName $ last $ qnParts qn
        _ -> pure ()

    hasCapabilityCheck :: PhpFile -> Bool
    hasCapabilityCheck f = any (hasCapCheck . locNode) (phpStatements f)

    hasCapCheck :: Statement -> Bool
    hasCapCheck (StmtExpr (Located _ (ExprCall (Located _ (ExprConstant qn)) _))) =
        let fn = T.toLower $ unName $ last $ qnParts qn
        in fn `elem` ["current_user_can", "user_can", "is_admin", "is_super_admin"]
    hasCapCheck _ = False

-- | Check for timing attack vulnerabilities in comparisons
checkTimingAttacks :: Located Expr -> [AdvancedIssue]
checkTimingAttacks (Located pos expr) = case expr of
    -- Direct string comparison of sensitive values
    ExprBinary op left right
        | op `elem` [OpEq, OpIdentical, OpNeq, OpNotIdentical]
        , isSensitiveComparison left right ->
            [AdvancedIssue TimingAttack Medium pos
                "Timing attack: non-constant-time comparison of sensitive data"
                "Use hash_equals() for cryptographic comparisons"
                Nothing
                (Just 208)]  -- CWE-208: Observable Timing Discrepancy
    _ -> []
  where
    isSensitiveComparison :: Located Expr -> Located Expr -> Bool
    isSensitiveComparison l r =
        isSensitive l || isSensitive r

    isSensitive :: Located Expr -> Bool
    isSensitive (Located _ e) = case e of
        ExprVariable (Variable v) ->
            any (`T.isInfixOf` T.toLower v) ["password", "token", "secret", "key", "hash"]
        ExprArrayAccess base _ -> isSensitive base
        _ -> False

-- | Check for constant-time comparison usage
checkConstantTimeComparison :: Located Expr -> Maybe AdvancedIssue
checkConstantTimeComparison (Located pos expr) = case expr of
    ExprCall (Located _ (ExprConstant qn)) _
        | fname == "hash_equals" -> Nothing  -- Good! Using constant-time comparison
      where fname = T.toLower $ unName $ last $ qnParts qn
    _ -> Just $ AdvancedIssue TimingAttack Info pos
        "Consider using hash_equals() for cryptographic comparisons"
        "Replace string comparison with hash_equals() for secrets/tokens"
        Nothing
        (Just 208)

-- | Check for sensitive data exposure in logs/errors
checkSensitiveDataExposure :: Located Expr -> [AdvancedIssue]
checkSensitiveDataExposure (Located pos expr) = case expr of
    ExprCall (Located _ (ExprConstant qn)) args
        | fname `elem` ["error_log", "trigger_error", "var_dump", "print_r", "var_export"]
        , any hasSensitiveData args ->
            [AdvancedIssue SensitiveDataExposure High pos
                ("Sensitive data in " <> fname <> ": may expose secrets in logs")
                "Redact sensitive data before logging"
                (Just fname)
                (Just 532)]  -- CWE-532: Insertion of Sensitive Information into Log File
      where fname = T.toLower $ unName $ last $ qnParts qn
    _ -> []
  where
    hasSensitiveData :: Argument -> Bool
    hasSensitiveData arg = case locNode (argValue arg) of
        ExprVariable (Variable v) ->
            any (`T.isInfixOf` T.toLower v) ["password", "token", "secret", "key", "credit_card"]
        _ -> False

-- | Check for information disclosure
checkInformationDisclosure :: PhpFile -> [AdvancedIssue]
checkInformationDisclosure file = execWriter $ mapM_ checkStmt (phpStatements file)
  where
    checkStmt :: Located Statement -> AdvancedM ()
    checkStmt (Located pos stmt) = case stmt of
        -- phpinfo() call
        StmtExpr (Located _ (ExprCall (Located _ (ExprConstant qn)) _))
            | fname == "phpinfo" ->
                tell [AdvancedIssue InformationDisclosure High pos
                    "Information disclosure: phpinfo() exposes server configuration"
                    "Remove phpinfo() from production code"
                    (Just "phpinfo()")
                    (Just 200)]  -- CWE-200: Exposure of Sensitive Information
          where fname = T.toLower $ unName $ last $ qnParts qn
        _ -> pure ()

-- | Check for chained file inclusion vulnerabilities
checkFileInclusionChain :: [Located Statement] -> [AdvancedIssue]
checkFileInclusionChain stmts = execWriter $ checkForChain stmts
  where
    checkForChain :: [Located Statement] -> AdvancedM ()
    checkForChain ss = do
        let includes = filter isIncludeStmt ss
        when (length includes > 3) $
            tell [AdvancedIssue FileInclusionChain Medium (locPos $ head includes)
                "Multiple file inclusions may create attack chain"
                "Limit dynamic inclusions and validate paths"
                Nothing
                (Just 829)]  -- CWE-829: Inclusion of Functionality from Untrusted Control Sphere

    isIncludeStmt :: Located Statement -> Bool
    isIncludeStmt (Located _ (StmtExpr (Located _ (ExprInclude _ _)))) = True
    isIncludeStmt _ = False

-- | Check for prototype pollution (array key manipulation)
checkPrototypePollution :: Located Expr -> [AdvancedIssue]
checkPrototypePollution (Located pos expr) = case expr of
    -- User-controlled array keys
    ExprArrayAccess base (Just key)
        | containsUserInput base || containsUserInput key ->
            [AdvancedIssue PrototypePollution Medium pos
                "Array key manipulation: user controls array index"
                "Validate array keys against whitelist before use"
                Nothing
                (Just 1321)]  -- CWE-1321: Improperly Controlled Modification of Object Prototype Attributes
    _ -> []
  where
    containsUserInput :: Located Expr -> Bool
    containsUserInput (Located _ e) = case e of
        ExprVariable (Variable v) -> v `elem` ["_GET", "_POST", "_REQUEST"]
        ExprArrayAccess b _ -> containsUserInput b
        _ -> False

-- Helper: Extract string literal from expression
extractStringLiteral :: Located Expr -> Maybe Text
extractStringLiteral (Located _ (ExprLiteral (LitString s))) = Just s
extractStringLiteral _ = Nothing
