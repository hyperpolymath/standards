-- | End-to-End test suite - Full pipeline validation
-- SPDX-License-Identifier: MPL-2.0
module E2ESpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import Control.Exception (catch, SomeException)

import Sanctify.Parser
import Sanctify.Analysis.Security
import Sanctify.Analysis.Advanced
import Sanctify.WordPress.Security
import Sanctify.Transform.Sanitize
import Sanctify.Transform.Strict
import Sanctify.Emit

-- E2E test suite: run full sanctify-php pipeline on fixture files
spec :: Spec
spec = do
    describe "E2E: Full Pipeline on Fixtures" $ do
        it "analyzes vulnerable-sql.php and detects SQL injection" $ do
            let fixturePath = "test" </> "fixtures" </> "vulnerable-sql.php"
            exists <- doesFileExist fixturePath
            unless exists $ expectationFailure $ "Fixture not found: " ++ fixturePath

            code <- TIO.readFile fixturePath
            case parsePhpString fixturePath code of
                Left err -> expectationFailure $ "Parser error: " ++ show err
                Right ast -> do
                    let issues = analyzeSecurityIssues ast
                    let sqlIssues = filter (\i -> issueType i == SqlInjection) issues
                    length sqlIssues `shouldSatisfy` (> 0)

        it "analyzes vulnerable-xss.php and detects XSS" $ do
            let fixturePath = "test" </> "fixtures" </> "vulnerable-xss.php"
            exists <- doesFileExist fixturePath
            unless exists $ expectationFailure $ "Fixture not found: " ++ fixturePath

            code <- TIO.readFile fixturePath
            case parsePhpString fixturePath code of
                Left err -> expectationFailure $ "Parser error: " ++ show err
                Right ast -> do
                    let issues = analyzeSecurityIssues ast
                    let xssIssues = filter (\i -> issueType i == CrossSiteScripting) issues
                    length xssIssues `shouldSatisfy` (> 0)

        it "analyzes wordpress-unsafe.php and detects WordPress issues" $ do
            let fixturePath = "test" </> "fixtures" </> "wordpress-unsafe.php"
            exists <- doesFileExist fixturePath
            unless exists $ expectationFailure $ "Fixture not found: " ++ fixturePath

            code <- TIO.readFile fixturePath
            case parsePhpString fixturePath code of
                Left err -> expectationFailure $ "Parser error: " ++ show err
                Right ast -> do
                    let issues = analyzeWordPressSecurity ast
                    length issues `shouldSatisfy` (> 0)

        it "transforms vulnerable PHP to sanitized form" $ do
            let fixturePath = "test" </> "fixtures" </> "vulnerable-sql.php"
            exists <- doesFileExist fixturePath
            unless exists $ expectationFailure $ "Fixture not found: " ++ fixturePath

            code <- TIO.readFile fixturePath
            case parsePhpString fixturePath code of
                Left err -> expectationFailure $ "Parser error: " ++ show err
                Right ast -> do
                    let transformed = transformSanitizeOutput (transformStrict ast)
                    let emitted = emitPhp transformed
                    -- Verify output contains PHP opening tag and is not empty
                    emitted `shouldSatisfy` T.isPrefixOf "<?php"
                    T.length emitted `shouldSatisfy` (> 5)

        it "handles empty PHP file gracefully" $ do
            let code = "<?php\n"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ "Parser error: " ++ show err
                Right ast -> do
                    let issues = analyzeSecurityIssues ast
                    issues `shouldBe` []

        it "analyzes PHP 8.2 features file without errors" $ do
            let fixturePath = "test" </> "fixtures" </> "php82-features.php"
            exists <- doesFileExist fixturePath
            unless exists $ expectationFailure $ "Fixture not found: " ++ fixturePath

            code <- TIO.readFile fixturePath
            case parsePhpString fixturePath code of
                Left err -> expectationFailure $ "Parser error: " ++ show err
                Right ast -> do
                    let issues = analyzeSecurityIssues ast
                    -- Should parse without error, regardless of issues found
                    length (phpStatements ast) `shouldSatisfy` (>= 0)

        it "generates valid report for analyzed code" $ do
            let code = "<?php\necho $_GET['name'];"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ "Parser error: " ++ show err
                Right ast -> do
                    let issues = analyzeSecurityIssues ast
                    issues `shouldSatisfy` (not . null)
                    -- Report generation should not crash
                    let report = formatTextReport "test.php" issues
                    T.length report `shouldSatisfy` (> 0)

    describe "E2E: Clean Code Path" $ do
        it "clean_code.php produces no security issues" $ do
            let fixturePath = "test" </> "fixtures" </> "clean_code.php"
            exists <- doesFileExist fixturePath
            unless exists $ expectationFailure $ "Fixture not found: " ++ fixturePath

            code <- TIO.readFile fixturePath
            case parsePhpString fixturePath code of
                Left err -> expectationFailure $ "Parser error: " ++ show err
                Right ast -> do
                    let issues = analyzeSecurityIssues ast
                    length issues `shouldBe` 0

        it "wordpress-safe.php passes WordPress security checks" $ do
            let fixturePath = "test" </> "fixtures" </> "wordpress-safe.php"
            exists <- doesFileExist fixturePath
            unless exists $ expectationFailure $ "Fixture not found: " ++ fixturePath

            code <- TIO.readFile fixturePath
            case parsePhpString fixturePath code of
                Left err -> expectationFailure $ "Parser error: " ++ show err
                Right ast -> do
                    let issues = analyzeWordPressSecurity ast
                    filter isCriticalWPIssue issues `shouldBe` []
  where
    isCriticalWPIssue issue = wpIssueSeverity issue == Critical

-- Helper function for report formatting
formatTextReport :: String -> [SecurityIssue] -> T.Text
formatTextReport filename issues =
    T.unlines $
        [ T.pack $ "Report for " ++ filename
        , T.replicate 40 "-"
        ] ++ map formatSecurityIssue issues

formatSecurityIssue :: SecurityIssue -> T.Text
formatSecurityIssue issue =
    T.pack $ unwords
        [ "["
        , show (issueSeverity issue)
        , "]"
        , show (issueType issue)
        ]
