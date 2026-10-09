-- | Security analysis for PHP code
-- SPDX-License-Identifier: MPL-2.0
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
import Control.Monad (forM_, unless, when)
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
checkHardcodedSecrets pos str = tell (checkSecretPatterns pos str)

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

-- | Standalone check for SQL injection
checkSqlInjection :: Located Expr -> Maybe SecurityIssue
checkSqlInjection (Located pos expr) = case expr of
    ExprCall (Located _ (ExprConstant qn)) args
        | fname `elem` ["mysql_query", "mysqli_query", "pg_query", "sqlite_query", "query"]
        , any (containsUserInput . argValue) args ->
            Just $ SecurityIssue SqlInjection Critical pos
                "Potential SQL injection: query contains user input"
                "Use prepared statements with bound parameters"
                (Just fname)
      where fname = T.toLower $ unName $ last $ qnParts qn
    ExprMethodCall _ (Name method) args
        | T.toLower method `elem` ["query", "exec"]
        , any (containsUserInput . argValue) args ->
            Just $ SecurityIssue SqlInjection Critical pos
                "SQL query may contain unsanitized input"
                "Use prepare() + execute() with bound parameters"
                (Just method)
    _ -> Nothing

-- | Standalone check for XSS
checkXss :: Located Expr -> Maybe SecurityIssue
checkXss (Located pos expr) = case expr of
    _ | containsUserInput (Located pos expr) ->
        Just $ SecurityIssue CrossSiteScripting High pos
            "Outputting user input without escaping"
            "Use htmlspecialchars() or esc_html() for WordPress"
            Nothing
    _ -> Nothing

-- | Check for CSRF vulnerabilities in form handlers
checkCsrf :: PhpFile -> [SecurityIssue]
checkCsrf file = execWriter $ mapM_ checkStmt (phpStatements file)
  where
    checkStmt :: Located Statement -> Writer [SecurityIssue] ()
    checkStmt (Located pos stmt) = case stmt of
        StmtIf cond _ _ -> checkCsrfCond pos cond
        StmtDecl (DeclFunction{fnBody = body}) -> mapM_ checkStmt body
        StmtDecl (DeclClass{clsMembers = members}) ->
            forM_ members $ \m -> case m of
                MemberMethod{methBody = Just body} -> mapM_ checkStmt body
                _ -> pure ()
        _ -> pure ()

    checkCsrfCond :: SourcePos -> Located Expr -> Writer [SecurityIssue] ()
    checkCsrfCond pos (Located _ expr) = case expr of
        -- Check for $_POST without nonce verification
        ExprBinary _ left right -> do
            when (hasPostAccess left || hasPostAccess right) $
                unless (hasNonceCheck file) $
                    tell [SecurityIssue CrossSiteRequestForgery High pos
                        "Form handler processes $_POST without CSRF protection"
                        "Add wp_verify_nonce() or check_admin_referer() verification"
                        Nothing]
        _ -> pure ()

    hasPostAccess :: Located Expr -> Bool
    hasPostAccess (Located _ (ExprVariable (Variable "_POST"))) = True
    hasPostAccess (Located _ (ExprArrayAccess base _)) = hasPostAccess base
    hasPostAccess _ = False

    hasNonceCheck :: PhpFile -> Bool
    hasNonceCheck f = any (hasNonceInStmt . locNode) (phpStatements f)

    hasNonceInStmt :: Statement -> Bool
    hasNonceInStmt (StmtExpr (Located _ (ExprCall (Located _ (ExprConstant qn)) _))) =
        let fn = T.toLower $ unName $ last $ qnParts qn
        in fn `elem` ["wp_verify_nonce", "check_admin_referer", "check_ajax_referer"]
    hasNonceInStmt (StmtIf _ thenStmts elseStmts) =
        any (hasNonceInStmt . locNode) thenStmts ||
        maybe False (any (hasNonceInStmt . locNode)) elseStmts
    hasNonceInStmt _ = False

-- | Standalone check for command injection
checkCommandInjection :: Located Expr -> Maybe SecurityIssue
checkCommandInjection (Located pos expr) = case expr of
    ExprEval _ -> Just $ SecurityIssue CommandInjection Critical pos
        "Use of eval() is extremely dangerous"
        "Remove eval() and use safe alternatives"
        (Just "eval(...)")
    ExprShellExec cmd -> Just $ SecurityIssue CommandInjection Critical pos
        "Shell execution via backticks"
        "Use escapeshellarg/escapeshellcmd or avoid shell execution"
        (Just $ "`" <> cmd <> "`")
    ExprCall (Located _ (ExprConstant qn)) args
        | fname `elem` ["exec", "shell_exec", "system", "passthru", "popen", "proc_open", "pcntl_exec"]
        , any (containsUserInput . argValue) args ->
            Just $ SecurityIssue CommandInjection Critical pos
                ("Shell execution with user input: " <> fname)
                "Validate/escape input with escapeshellarg()"
                (Just fname)
      where fname = T.toLower $ unName $ last $ qnParts qn
    _ -> Nothing

-- | Standalone check for path traversal
checkPathTraversal :: Located Expr -> Maybe SecurityIssue
checkPathTraversal (Located pos expr) = case expr of
    ExprCall (Located _ (ExprConstant qn)) args
        | fname `elem` ["include", "include_once", "require", "require_once",
                        "file_get_contents", "file_put_contents", "fopen", "readfile"]
        , any (containsUserInput . argValue) args ->
            Just $ SecurityIssue PathTraversal High pos
                ("Path traversal risk in " <> fname <> " with user input")
                "Use basename() and validate against whitelist"
                (Just fname)
      where fname = T.toLower $ unName $ last $ qnParts qn
    _ -> Nothing

-- | Standalone check for unsafe deserialization
checkUnsafeDeserialization :: Located Expr -> Maybe SecurityIssue
checkUnsafeDeserialization (Located pos expr) = case expr of
    ExprCall (Located _ (ExprConstant qn)) args
        | T.toLower (unName $ last $ qnParts qn) == "unserialize"
        , any (containsUserInput . argValue) args ->
            Just $ SecurityIssue UnsafeDeserialization Critical pos
                "unserialize() on untrusted data can lead to RCE"
                "Use json_decode() or specify allowed_classes option"
                (Just "unserialize(...)")
    _ -> Nothing

-- | Standalone check for weak cryptography
-- Updated to flag anything weaker than SHAKE3-256/BLAKE3/Argon2id
checkWeakCrypto :: Located Expr -> Maybe SecurityIssue
checkWeakCrypto (Located pos expr) = case expr of
    ExprCall (Located _ (ExprConstant qn)) _
        | fname `elem` weakHashFunctions ->
            Just $ SecurityIssue WeakCryptography severity pos
                ("Weak cryptographic function: " <> fname)
                (getRemedy fname)
                (Just fname)
      where
        fname = T.toLower $ unName $ last $ qnParts qn
        severity = if fname `elem` ["md5", "sha1", "crypt", "md4", "md2"] then High else Medium

        weakHashFunctions :: [Text]
        weakHashFunctions =
            -- Critical: broken
            [ "md2", "md4", "md5", "sha1", "crypt"
            -- Medium: not recommended for new code
            , "sha256", "sha384", "sha512"  -- Use SHAKE3-256 or BLAKE3 instead
            -- Insecure random
            , "rand", "mt_rand", "srand", "mt_srand", "lcg_value"
            -- Weak password hashing
            , "password_hash"  -- Only if not using Argon2id
            ]

        getRemedy :: Text -> Text
        getRemedy fn
            | fn `elem` ["md5", "sha1", "md4", "md2", "crypt"] =
                "Use SHAKE3-256 (hash('shake256', ...)) or BLAKE3 for hashing"
            | fn `elem` ["sha256", "sha384", "sha512"] =
                "Consider SHAKE3-256 or BLAKE3 for better security margins"
            | fn `elem` ["rand", "mt_rand", "srand", "mt_srand", "lcg_value"] =
                "Use random_int() or random_bytes() for cryptographic randomness"
            | fn == "password_hash" =
                "Ensure using PASSWORD_ARGON2ID algorithm"
            | otherwise = "Use modern cryptographic primitives"
    _ -> Nothing

checkSecretPatterns :: SourcePos -> Text -> [SecurityIssue]
checkSecretPatterns p str
    | T.length str < 8 = []
    | otherwise = catMaybes
        [ checkApiKey p str
        , checkPassword p str
        , checkPrivateKey p str
        , checkToken p str
        ]

checkApiKey :: SourcePos -> Text -> Maybe SecurityIssue
checkApiKey p str
    | any (`T.isInfixOf` T.toLower str) ["api_key", "apikey", "api-key"] =
        Just $ SecurityIssue HardcodedSecret High p
            "Possible hardcoded API key"
            "Use environment variables: getenv('API_KEY')"
            (Just $ T.take 15 str <> "...")
    | otherwise = Nothing

checkPassword :: SourcePos -> Text -> Maybe SecurityIssue
checkPassword p str
    | any (`T.isInfixOf` T.toLower str) ["password", "passwd", "pwd"] =
        Just $ SecurityIssue HardcodedSecret High p
            "Possible hardcoded password"
            "Use environment variables or secure vault"
            (Just $ T.take 15 str <> "...")
    | otherwise = Nothing

checkPrivateKey :: SourcePos -> Text -> Maybe SecurityIssue
checkPrivateKey p str
    | any (`T.isInfixOf` T.toLower str) ["private_key", "privatekey", "secret_key", "secretkey"] =
        Just $ SecurityIssue HardcodedSecret Critical p
            "Possible hardcoded private key"
            "Store keys in secure key management system"
            (Just $ T.take 15 str <> "...")
    | "-----BEGIN" `T.isInfixOf` str =
        Just $ SecurityIssue HardcodedSecret Critical p
            "PEM-encoded key detected in source"
            "Never embed cryptographic keys in source code"
            (Just "-----BEGIN...")
    | otherwise = Nothing

checkToken :: SourcePos -> Text -> Maybe SecurityIssue
checkToken p str
    | any (`T.isInfixOf` T.toLower str) ["access_token", "auth_token", "bearer", "jwt"] =
        Just $ SecurityIssue HardcodedSecret High p
            "Possible hardcoded authentication token"
            "Use secure token storage, never commit tokens"
            (Just $ T.take 15 str <> "...")
    | otherwise = Nothing

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\mx acc -> maybe acc (:acc) mx) []

-- | Standalone check for dangerous functions
checkDangerousFunctions :: Located Expr -> [SecurityIssue]
checkDangerousFunctions (Located pos expr) = case expr of
    ExprCall (Located _ (ExprConstant qn)) args ->
        let fname = T.toLower $ unName $ last $ qnParts qn
        in catMaybes
            [ checkCodeExec fname pos
            , checkShellExec fname pos args
            , checkFileOps fname pos args
            , checkDeserialize fname pos args
            , checkReflection fname pos args
            ]
    _ -> []
  where
    catMaybes :: [Maybe a] -> [a]
    catMaybes = foldr (\mx acc -> maybe acc (:acc) mx) []

    checkCodeExec :: Text -> SourcePos -> Maybe SecurityIssue
    checkCodeExec fn p
        | fn `elem` ["eval", "assert", "create_function", "preg_replace"] =
            Just $ SecurityIssue DangerousFunction Critical p
                ("Dangerous code execution function: " <> fn)
                "Avoid dynamic code execution entirely"
                (Just fn)
        | otherwise = Nothing

    checkShellExec :: Text -> SourcePos -> [Argument] -> Maybe SecurityIssue
    checkShellExec fn p args
        | fn `elem` ["exec", "shell_exec", "system", "passthru", "popen", "proc_open", "pcntl_exec"] =
            Just $ SecurityIssue DangerousFunction
                (if any (containsUserInput . argValue) args then Critical else High) p
                ("Shell execution function: " <> fn)
                "Avoid shell execution or strictly validate/escape input"
                (Just fn)
        | otherwise = Nothing

    checkFileOps :: Text -> SourcePos -> [Argument] -> Maybe SecurityIssue
    checkFileOps fn p args
        | fn `elem` ["include", "include_once", "require", "require_once"]
        , any (containsUserInput . argValue) args =
            Just $ SecurityIssue DangerousFunction High p
                ("Dynamic file inclusion: " <> fn)
                "Never include files based on user input"
                (Just fn)
        | otherwise = Nothing

    checkDeserialize :: Text -> SourcePos -> [Argument] -> Maybe SecurityIssue
    checkDeserialize fn p args
        | fn == "unserialize"
        , any (containsUserInput . argValue) args =
            Just $ SecurityIssue DangerousFunction Critical p
                "unserialize() with untrusted input"
                "Use json_decode() instead or specify allowed_classes"
                (Just "unserialize")
        | otherwise = Nothing

    checkReflection :: Text -> SourcePos -> [Argument] -> Maybe SecurityIssue
    checkReflection fn p args
        | fn `elem` ["call_user_func", "call_user_func_array"]
        , any (containsUserInput . argValue) args =
            Just $ SecurityIssue DangerousFunction High p
                ("Dynamic function call with user input: " <> fn)
                "Validate function name against strict whitelist"
                (Just fn)
        | otherwise = Nothing
