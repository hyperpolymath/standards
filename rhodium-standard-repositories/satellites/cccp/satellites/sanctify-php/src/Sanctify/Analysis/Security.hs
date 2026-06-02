-- | Security analysis for PHP code
-- SPDX-License-Identifier: AGPL-3.0-or-later
module Sanctify.Analysis.Security
    ( -- * Main analysis
      analyzeSecurityIssues
    , SecurityIssue(..)
    , Severity(..)
    , IssueType(..)

      -- * Specific checks
    , checkSqlInjection
    , checkXss
    , checkCsrf
    , checkCommandInjection
    , checkPathTraversal
    , checkUnsafeDeserialization
    , checkWeakCrypto
    , checkHardcodedSecrets
    , checkDangerousFunctions
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Writer
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

import Sanctify.AST

-- | Severity levels
data Severity = Critical | High | Medium | Low | Info
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON)

-- | Types of security issues
data IssueType
    = SqlInjection
    | CrossSiteScripting
    | CrossSiteRequestForgery
    | CommandInjection
    | PathTraversal
    | UnsafeDeserialization
    | WeakCryptography
    | HardcodedSecret
    | DangerousFunction
    | InsecureFileUpload
    | OpenRedirect
    | XPathInjection
    | LdapInjection
    | XxeVulnerability
    | InsecureRandom
    | MissingStrictTypes
    | TypeCoercionRisk
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

-- | A detected security issue
data SecurityIssue = SecurityIssue
    { issueType        :: IssueType
    , issueSeverity    :: Severity
    , issueLocation    :: SourcePos
    , issueDescription :: Text
    , issueRemedy      :: Text
    , issueCode        :: Maybe Text  -- Affected code snippet
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

type SecurityM = Writer [SecurityIssue]

-- | Analyze a PHP file for security issues
analyzeSecurityIssues :: PhpFile -> [SecurityIssue]
analyzeSecurityIssues file = execWriter $ do
    -- Check for missing strict_types
    unless (phpDeclareStrict file) $
        tell [SecurityIssue
            { issueType = MissingStrictTypes
            , issueSeverity = Medium
            , issueLocation = SourcePos (maybe "" (T.unpack . unName . head . qnParts) (phpNamespace file)) 1 1
            , issueDescription = "Missing declare(strict_types=1)"
            , issueRemedy = "Add declare(strict_types=1); at the top of the file"
            , issueCode = Nothing
            }]

    -- Analyze all statements
    mapM_ analyzeStatement (phpStatements file)

-- | Analyze a statement for security issues
analyzeStatement :: Located Statement -> SecurityM ()
analyzeStatement (Located pos stmt) = case stmt of
    StmtExpr expr -> analyzeExpr expr
    StmtIf cond thenStmts elseStmts -> do
        analyzeExpr cond
        mapM_ analyzeStatement thenStmts
        maybe (pure ()) (mapM_ analyzeStatement) elseStmts
    StmtWhile cond body -> do
        analyzeExpr cond
        mapM_ analyzeStatement body
    StmtFor init cond update body -> do
        maybe (pure ()) analyzeExpr init
        maybe (pure ()) analyzeExpr cond
        maybe (pure ()) analyzeExpr update
        mapM_ analyzeStatement body
    StmtForeach expr _ _ body -> do
        analyzeExpr expr
        mapM_ analyzeStatement body
    StmtTry tryBody catches finally -> do
        mapM_ analyzeStatement tryBody
        mapM_ (\c -> mapM_ analyzeStatement (catchBody c)) catches
        maybe (pure ()) (mapM_ analyzeStatement) finally
    StmtReturn (Just expr) -> analyzeExpr expr
    StmtEcho exprs -> mapM_ (checkXssOutput pos) exprs
    StmtDecl decl -> analyzeDeclaration pos decl
    _ -> pure ()

-- | Analyze a declaration
analyzeDeclaration :: SourcePos -> Declaration -> SecurityM ()
analyzeDeclaration pos decl = case decl of
    DeclFunction{fnBody = body} -> mapM_ analyzeStatement body
    DeclClass{clsMembers = members} -> mapM_ (analyzeClassMember pos) members
    _ -> pure ()

-- | Analyze class members
analyzeClassMember :: SourcePos -> ClassMember -> SecurityM ()
analyzeClassMember pos member = case member of
    MemberMethod{methBody = Just body} -> mapM_ analyzeStatement body
    _ -> pure ()

-- | Analyze an expression for security issues
analyzeExpr :: Located Expr -> SecurityM ()
analyzeExpr (Located pos expr) = case expr of
    ExprCall callee args -> do
        checkDangerousCall pos callee args
        analyzeExpr callee
        mapM_ (analyzeExpr . argValue) args

    ExprMethodCall obj name args -> do
        checkDangerousMethod pos name args
        analyzeExpr obj
        mapM_ (analyzeExpr . argValue) args

    ExprNew className args -> do
        checkDangerousConstruction pos className args
        mapM_ (analyzeExpr . argValue) args

    ExprEval arg -> do
        tell [SecurityIssue
            { issueType = CommandInjection
            , issueSeverity = Critical
            , issueLocation = pos
            , issueDescription = "Use of eval() is extremely dangerous"
            , issueRemedy = "Remove eval() and use safe alternatives"
            , issueCode = Just "eval(...)"
            }]
        analyzeExpr arg

    ExprShellExec cmd -> do
        tell [SecurityIssue
            { issueType = CommandInjection
            , issueSeverity = Critical
            , issueLocation = pos
            , issueDescription = "Shell execution via backticks"
            , issueRemedy = "Use escapeshellarg/escapeshellcmd or avoid shell execution"
            , issueCode = Just ("`" <> cmd <> "`")
            }]

    ExprBinary _ left right -> do
        analyzeExpr left
        analyzeExpr right

    ExprUnary _ operand -> analyzeExpr operand

    ExprAssign target value -> do
        analyzeExpr target
        analyzeExpr value

    ExprTernary cond true false -> do
        analyzeExpr cond
        maybe (pure ()) analyzeExpr true
        analyzeExpr false

    ExprClosure{closureBody = body} -> mapM_ analyzeStatement body

    ExprArrowFunction{arrowExpr = e} -> analyzeExpr e

    ExprLiteral (LitString str) -> checkHardcodedSecrets pos str

    _ -> pure ()

-- | Check for dangerous function calls
checkDangerousCall :: SourcePos -> Located Expr -> [Argument] -> SecurityM ()
checkDangerousCall pos (Located _ (ExprConstant qn)) args = do
    let fname = T.toLower $ unName $ last $ qnParts qn

    -- Critical: Code execution
    when (fname `elem` ["eval", "assert", "create_function", "preg_replace"]) $
        tell [SecurityIssue
            { issueType = CommandInjection
            , issueSeverity = Critical
            , issueLocation = pos
            , issueDescription = "Dangerous function: " <> fname
            , issueRemedy = "Avoid dynamic code execution"
            , issueCode = Just fname
            }]

    -- Critical: Shell execution
    when (fname `elem` ["exec", "shell_exec", "system", "passthru", "popen", "proc_open", "pcntl_exec"]) $
        tell [SecurityIssue
            { issueType = CommandInjection
            , issueSeverity = Critical
            , issueLocation = pos
            , issueDescription = "Shell execution function: " <> fname
            , issueRemedy = "Validate/escape input with escapeshellarg()"
            , issueCode = Just fname
            }]

    -- High: SQL - check for string concatenation in args
    when (fname `elem` ["mysql_query", "mysqli_query", "pg_query", "sqlite_query"]) $
        checkSqlInjectionArgs pos args

    -- High: Deserialization
    when (fname == "unserialize") $
        tell [SecurityIssue
            { issueType = UnsafeDeserialization
            , issueSeverity = High
            , issueLocation = pos
            , issueDescription = "unserialize() on untrusted data can lead to RCE"
            , issueRemedy = "Use json_decode() or specify allowed_classes option"
            , issueCode = Just "unserialize(...)"
            }]

    -- Medium: File inclusion
    when (fname `elem` ["include", "include_once", "require", "require_once"]) $
        when (hasUserInputArg args) $
            tell [SecurityIssue
                { issueType = PathTraversal
                , issueSeverity = High
                , issueLocation = pos
                , issueDescription = "Dynamic file inclusion may allow path traversal"
                , issueRemedy = "Use basename() and validate against whitelist"
                , issueCode = Just fname
                }]

    -- Weak crypto
    when (fname `elem` ["md5", "sha1", "crypt"]) $
        tell [SecurityIssue
            { issueType = WeakCryptography
            , issueSeverity = Medium
            , issueLocation = pos
            , issueDescription = "Weak hashing algorithm: " <> fname
            , issueRemedy = "Use password_hash() for passwords, hash('sha256', ...) for general hashing"
            , issueCode = Just fname
            }]

    -- Insecure random
    when (fname `elem` ["rand", "mt_rand", "srand", "mt_srand"]) $
        tell [SecurityIssue
            { issueType = InsecureRandom
            , issueSeverity = Medium
            , issueLocation = pos
            , issueDescription = "Insecure random number generator: " <> fname
            , issueRemedy = "Use random_int() or random_bytes() for security-sensitive operations"
            , issueCode = Just fname
            }]

checkDangerousCall _ _ _ = pure ()

-- | Check for SQL injection in arguments
checkSqlInjectionArgs :: SourcePos -> [Argument] -> SecurityM ()
checkSqlInjectionArgs pos args =
    when (any (containsUserInput . argValue) args) $
        tell [SecurityIssue
            { issueType = SqlInjection
            , issueSeverity = Critical
            , issueLocation = pos
            , issueDescription = "Potential SQL injection: query contains user input"
            , issueRemedy = "Use prepared statements with bound parameters"
            , issueCode = Nothing
            }]

-- | Check for dangerous method calls
checkDangerousMethod :: SourcePos -> Name -> [Argument] -> SecurityM ()
checkDangerousMethod pos (Name name) args = do
    let lname = T.toLower name

    -- PDO without prepared statements
    when (lname `elem` ["query", "exec"] && any (containsUserInput . argValue) args) $
        tell [SecurityIssue
            { issueType = SqlInjection
            , issueSeverity = Critical
            , issueLocation = pos
            , issueDescription = "SQL query may contain unsanitized input"
            , issueRemedy = "Use prepare() + execute() with bound parameters"
            , issueCode = Just name
            }]

-- | Check for dangerous object instantiation
checkDangerousConstruction :: SourcePos -> QualifiedName -> [Argument] -> SecurityM ()
checkDangerousConstruction pos qn args = do
    let className = T.toLower $ unName $ last $ qnParts qn

    -- Check for ReflectionClass with user input
    when (className == "reflectionclass" && any (containsUserInput . argValue) args) $
        tell [SecurityIssue
            { issueType = CommandInjection
            , issueSeverity = High
            , issueLocation = pos
            , issueDescription = "ReflectionClass with user input can be dangerous"
            , issueRemedy = "Validate class name against whitelist"
            , issueCode = Just "new ReflectionClass(...)"
            }]

-- | Check for XSS in output
checkXssOutput :: SourcePos -> Located Expr -> SecurityM ()
checkXssOutput pos expr =
    when (containsUserInput expr) $
        tell [SecurityIssue
            { issueType = CrossSiteScripting
            , issueSeverity = High
            , issueLocation = pos
            , issueDescription = "Outputting user input without escaping"
            , issueRemedy = "Use htmlspecialchars() or esc_html() for WordPress"
            , issueCode = Nothing
            }]

-- | Check for hardcoded secrets in strings
checkHardcodedSecrets :: SourcePos -> Text -> SecurityM ()
checkHardcodedSecrets pos str = do
    let lower = T.toLower str

    -- Check for API keys, passwords, secrets
    when (any (`T.isInfixOf` lower)
            ["api_key", "apikey", "api-key", "password", "passwd",
             "secret", "private_key", "privatekey", "access_token",
             "auth_token", "bearer"]) $
        when (T.length str > 10) $  -- Avoid false positives on short strings
            tell [SecurityIssue
                { issueType = HardcodedSecret
                , issueSeverity = High
                , issueLocation = pos
                , issueDescription = "Possible hardcoded secret detected"
                , issueRemedy = "Use environment variables or secure configuration"
                , issueCode = Just (T.take 20 str <> "...")
                }]

-- | Check if expression contains user input (superglobals, etc.)
containsUserInput :: Located Expr -> Bool
containsUserInput (Located _ expr) = case expr of
    ExprVariable (Variable name) ->
        name `elem` ["_GET", "_POST", "_REQUEST", "_COOKIE", "_SERVER", "_FILES", "_SESSION"]
    ExprArrayAccess base _ -> containsUserInput base
    ExprBinary OpConcat left right -> containsUserInput left || containsUserInput right
    _ -> False

-- | Check if any argument contains user input
hasUserInputArg :: [Argument] -> Bool
hasUserInputArg = any (containsUserInput . argValue)

-- | Convenience aliases for export
checkSqlInjection :: Located Expr -> Maybe SecurityIssue
checkSqlInjection = const Nothing  -- TODO: implement standalone

checkXss :: Located Expr -> Maybe SecurityIssue
checkXss = const Nothing  -- TODO: implement standalone

checkCsrf :: PhpFile -> [SecurityIssue]
checkCsrf = const []  -- TODO: implement

checkCommandInjection :: Located Expr -> Maybe SecurityIssue
checkCommandInjection = const Nothing  -- TODO: implement standalone

checkPathTraversal :: Located Expr -> Maybe SecurityIssue
checkPathTraversal = const Nothing  -- TODO: implement standalone

checkUnsafeDeserialization :: Located Expr -> Maybe SecurityIssue
checkUnsafeDeserialization = const Nothing  -- TODO: implement standalone

checkWeakCrypto :: Located Expr -> Maybe SecurityIssue
checkWeakCrypto = const Nothing  -- TODO: implement standalone

checkHardcodedSecrets :: Located Expr -> [SecurityIssue]
checkHardcodedSecrets = const []  -- TODO: implement standalone

checkDangerousFunctions :: Located Expr -> [SecurityIssue]
checkDangerousFunctions = const []  -- TODO: implement standalone
