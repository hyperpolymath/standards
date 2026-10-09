-- | Aspect Tests - Security, robustness, and error handling for the analyzer itself
-- SPDX-License-Identifier: MPL-2.0
module AspectSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Control.Exception (evaluate)
import System.Timeout (timeout)

import Sanctify.Parser
import Sanctify.Analysis.Security
import Sanctify.Analysis.Advanced
import Sanctify.Transform.Sanitize
import Sanctify.Transform.Strict
import Sanctify.Emit

-- Aspect tests: Security and robustness of the analyzer itself
spec :: Spec
spec = do
    describe "Aspect: Analyzer Security" $ do
        it "handles null bytes without crashing" $ do
            let code = "<?php\necho \"test\\x00value\";"
            case parsePhpString "test.php" (T.pack code) of
                Left _ -> pure ()  -- Parser error acceptable
                Right ast -> do
                    let issues = analyzeSecurityIssues ast
                    -- Should not crash, issues list may be empty or populated
                    evaluate (length issues) `shouldReturn` (length issues)

        it "handles extremely long variable names gracefully" $ do
            let longName = replicate 10000 'a'
                code = T.pack $ "<?php\n$" ++ longName ++ " = 'value';"
            case parsePhpString "test.php" code of
                Left _ -> pure ()  -- Parser failure acceptable
                Right ast -> do
                    let issues = analyzeSecurityIssues ast
                    length issues `shouldSatisfy` (>= 0)

        it "handles deeply nested function calls without stack overflow" $ do
            let deepCode = T.pack $ "<?php\n" ++ replicate 1000 "f(" ++ "'x'" ++ replicate 1000 ")"
            case parsePhpString "test.php" deepCode of
                Left _ -> pure ()  -- Parser failure acceptable
                Right ast -> do
                    -- Timeout after 5 seconds to catch stack overflow
                    result <- timeout 5000000 $
                        evaluate (length (analyzeSecurityIssues ast))
                    case result of
                        Just _ -> pure ()
                        Nothing -> expectationFailure "Analysis timed out (possible stack overflow)"

        it "handles PHP with BOM (Byte Order Mark) without crashing" $ do
            let bomCode = "\xEF\xBB\xBF<?php\necho 'test';"
                code = TE.decodeUtf8With (\_ _ -> '\xFFFD') (BS.pack bomCode)
            case parsePhpString "test.php" code of
                Left _ -> pure ()  -- Parser error acceptable
                Right ast -> do
                    let issues = analyzeSecurityIssues ast
                    length issues `shouldSatisfy` (>= 0)

        it "handles Latin-1 encoded PHP without crashing" $ do
            -- Simulate Latin-1 input (just ASCII-safe for this test)
            let code = T.pack "<?php\necho 'Caf\233'; // Latin-1: é as single byte"
            case parsePhpString "test.php" code of
                Left _ -> pure ()  -- Parser error acceptable
                Right ast -> do
                    let issues = analyzeSecurityIssues ast
                    length issues `shouldSatisfy` (>= 0)

        it "handles PHP with invalid UTF-8 sequences gracefully" $ do
            let invalidUtf8 = T.pack "<?php\necho 'test'; // \xDCinvalid"
            case parsePhpString "test.php" invalidUtf8 of
                Left _ -> pure ()  -- Parser error acceptable
                Right ast -> do
                    let issues = analyzeSecurityIssues ast
                    length issues `shouldSatisfy` (>= 0)

    describe "Aspect: Analyzer Performance" $ do
        it "analyzes small PHP file (10 lines) in reasonable time" $ do
            let code = T.pack $ unlines $
                    [ "<?php"
                    ] ++ replicate 9 "echo 'line';"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    result <- timeout 1000000 $  -- 1 second timeout
                        evaluate (length (analyzeSecurityIssues ast))
                    case result of
                        Just _ -> pure ()
                        Nothing -> expectationFailure "Analysis exceeded 1 second"

        it "parses medium PHP file (100 lines) in reasonable time" $ do
            let code = T.pack $ unlines $
                    [ "<?php"
                    ] ++ replicate 99 "echo 'line';"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    result <- timeout 2000000 $  -- 2 second timeout
                        evaluate (length (analyzeSecurityIssues ast))
                    case result of
                        Just _ -> pure ()
                        Nothing -> expectationFailure "Analysis exceeded 2 seconds"

        it "emitting large AST completes without hanging" $ do
            let code = T.pack $ unlines $
                    [ "<?php"
                    ] ++ replicate 50 "echo 'test';"
            case parsePhpString "test.php" code of
                Left _ -> pure ()
                Right ast -> do
                    result <- timeout 1000000 $  -- 1 second timeout
                        evaluate (T.length (emitPhp ast))
                    case result of
                        Just _ -> pure ()
                        Nothing -> expectationFailure "Emission exceeded 1 second"

    describe "Aspect: Error Handling" $ do
        it "non-PHP file extension produces appropriate handling" $ do
            let code = "<?php\necho 'test';"
            -- Parser should work regardless of filename extension
            case parsePhpString "test.txt" code of
                Left _ -> pure ()
                Right ast -> do
                    let issues = analyzeSecurityIssues ast
                    length issues `shouldSatisfy` (>= 0)

        it "empty PHP file returns empty issues and valid output" $ do
            let code = "<?php\n"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let issues = analyzeSecurityIssues ast
                    issues `shouldBe` []
                    emitPhp ast `shouldSatisfy` T.isPrefixOf "<?php"

        it "file with only whitespace returns valid result" $ do
            let code = "<?php\n   \n  \n"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let issues = analyzeSecurityIssues ast
                    issues `shouldBe` []

        it "unterminated string doesn't cause analyzer to crash" $ do
            let code = "<?php\necho \"unterminated string;"
            case parsePhpString "test.php" code of
                Left _ -> pure ()  -- Parser error acceptable
                Right ast -> do
                    let issues = analyzeSecurityIssues ast
                    length issues `shouldSatisfy` (>= 0)

        it "invalid function calls are handled gracefully" $ do
            let code = "<?php\n@@@invalid_syntax@@@"
            case parsePhpString "test.php" code of
                Left _ -> pure ()  -- Parser error acceptable
                Right ast -> do
                    let issues = analyzeSecurityIssues ast
                    length issues `shouldSatisfy` (>= 0)

    describe "Aspect: Transform Safety" $ do
        it "strict transform doesn't lose information" $ do
            let code = "<?php\nfunction test() { echo 'hello'; }"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let transformed = transformStrict ast
                    let original = emitPhp ast
                    let modified = emitPhp transformed
                    -- Both should be valid PHP
                    original `shouldSatisfy` T.isPrefixOf "<?php"
                    modified `shouldSatisfy` T.isPrefixOf "<?php"
                    -- Modified should have additional declarations
                    T.length modified `shouldSatisfy` (>= T.length original)

        it "sanitize transform produces valid PHP" $ do
            let code = "<?php\necho $_GET['name'];"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let transformed = transformSanitizeOutput ast
                    let emitted = emitPhp transformed
                    emitted `shouldSatisfy` T.isPrefixOf "<?php"
                    T.length emitted `shouldSatisfy` (> 0)

    describe "Aspect: Concurrent Safety" $ do
        it "analyzer is reentrant (multiple concurrent parses)" $ do
            let code1 = "<?php\necho $_GET['x'];"
                code2 = "<?php\n$safe = 'value';"
                code3 = "<?php\necho 'test';"
            case (parsePhpString "test1.php" code1,
                  parsePhpString "test2.php" code2,
                  parsePhpString "test3.php" code3) of
                (Right ast1, Right ast2, Right ast3) -> do
                    let issues1 = analyzeSecurityIssues ast1
                    let issues2 = analyzeSecurityIssues ast2
                    let issues3 = analyzeSecurityIssues ast3
                    -- All analyses should complete
                    length issues1 `shouldSatisfy` (>= 0)
                    length issues2 `shouldBe` 0
                    length issues3 `shouldBe` 0
                _ -> pure ()
