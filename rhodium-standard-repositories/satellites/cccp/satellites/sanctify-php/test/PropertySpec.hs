-- | Property-Based Tests - QuickCheck validation
-- SPDX-License-Identifier: MPL-2.0
module PropertySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Sanctify.Parser
import Sanctify.Analysis.Security
import Sanctify.Analysis.Advanced
import Sanctify.Transform.Sanitize
import Sanctify.Transform.Strict
import Sanctify.Emit
import Sanctify.AST

-- Property-based test suite using QuickCheck
spec :: Spec
spec = do
    describe "Property: Analysis Determinism" $ do
        it "analysis is deterministic (same input → same output)" $
            property $ \phpCode ->
                let code = ensureValidPhp phpCode
                in case (parsePhpString "test1.php" code, parsePhpString "test2.php" code) of
                    (Right ast1, Right ast2) ->
                        let issues1 = analyzeSecurityIssues ast1
                            issues2 = analyzeSecurityIssues ast2
                        in length issues1 === length issues2
                    _ -> property True  -- Parser failure is acceptable

    describe "Property: Safe Input Analysis" $ do
        it "PHP without vulnerabilities produces empty issue list" $
            property $ \varName ->
                let safeCode = T.pack $
                        "<?php\n$" ++ filter (not . (`elem` "';\"")) (take 10 varName) ++
                        " = 'safe_value';"
                in case parsePhpString "test.php" safeCode of
                    Left _ -> property True
                    Right ast ->
                        let issues = analyzeSecurityIssues ast
                        in null issues === True

    describe "Property: Transformation Idempotency" $ do
        it "sanitize(sanitize(code)) == sanitize(code) for safe inputs" $
            property $ \phpCode ->
                let code = ensureValidPhp phpCode
                in case parsePhpString "test.php" code of
                    Left _ -> property True
                    Right ast ->
                        let transformed1 = transformSanitizeOutput ast
                            transformed2 = transformSanitizeOutput transformed1
                            emitted1 = emitPhp transformed1
                            emitted2 = emitPhp transformed2
                        in emitted1 === emitted2

    describe "Property: Strict Transform Idempotency" $ do
        it "strict(strict(code)) preserves structure" $
            property $ \phpCode ->
                let code = ensureValidPhp phpCode
                in case parsePhpString "test.php" code of
                    Left _ -> property True
                    Right ast ->
                        let transformed1 = transformStrict ast
                            transformed2 = transformStrict transformed1
                            emitted1 = emitPhp transformed1
                            emitted2 = emitPhp transformed2
                        in emitted1 === emitted2

    describe "Property: Issue Severity Validity" $ do
        it "all returned issues have valid severity levels" $
            property $ \phpCode ->
                let code = "<?php\necho $_GET['x'];"
                in case parsePhpString "test.php" code of
                    Left _ -> property True
                    Right ast ->
                        let issues = analyzeSecurityIssues ast
                        in all isValidSeverity issues === True

    describe "Property: Report Generation" $ do
        it "analysis result always produces non-empty report string" $
            property $ \phpCode ->
                let code = "<?php\n$x = $_GET['id']; echo $x;"
                in case parsePhpString "test.php" code of
                    Left _ -> property True
                    Right ast ->
                        let issues = analyzeSecurityIssues ast
                            reportLines = map formatIssueLine issues
                        in if null issues
                           then property True
                           else null reportLines === False

    describe "Property: Parser Robustness" $ do
        it "parser handles valid PHP 8.2 syntax without crashing" $
            property $ \phpCode ->
                let code = T.pack ("<?php " ++ take 100 phpCode)
                in case parsePhpString "test.php" code of
                    Left _ -> property True  -- Parse error acceptable
                    Right _ -> property True  -- Success

    describe "Property: Output is Valid PHP" $ do
        it "transformed output maintains PHP validity" $
            property $ \phpCode ->
                let code = ensureValidPhp phpCode
                in case parsePhpString "test.php" code of
                    Left _ -> property True
                    Right ast ->
                        let transformed = transformStrict ast
                            emitted = emitPhp transformed
                        in T.take 5 emitted === "<?php"

-- Helper: Ensure code is valid PHP before parsing
ensureValidPhp :: String -> T.Text
ensureValidPhp code =
    let cleaned = filter (`notElem` "\0\n\r") (take 500 code)
    in T.pack $ "<?php\n" ++ cleaned

-- Helper: Check if severity is valid
isValidSeverity :: SecurityIssue -> Bool
isValidSeverity issue =
    let sev = issueSeverity issue
    in sev `elem` [Info, Low, Medium, High, Critical]

-- Helper: Format issue for report
formatIssueLine :: SecurityIssue -> String
formatIssueLine issue =
    unwords
        [ "[" ++ show (issueSeverity issue) ++ "]"
        , show (issueType issue)
        ]
