-- | Transformation test suite - Sanitization and hardening
-- SPDX-License-Identifier: MPL-2.0
module TransformSpec (spec) where

import Test.Hspec
import qualified Data.Text as T

import Sanctify.Parser
import Sanctify.AST
import Sanctify.Emit
import Sanctify.Transform.Sanitize
import Sanctify.Transform.Strict
import Sanctify.Transform.TypeHints (transformAddTypeHints)

spec :: Spec
spec = do
    describe "Strict Types Transform" $ do
        it "adds declare(strict_types=1) when missing" $ do
            let code = "<?php\nfunction test() {}"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let transformed = transformStrict ast
                    phpDeclareStrict transformed `shouldBe` True

        it "preserves existing declare(strict_types=1)" $ do
            let code = "<?php\ndeclare(strict_types=1);\nfunction test() {}"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let transformed = transformStrict ast
                    phpDeclareStrict transformed `shouldBe` True
                    -- Should not duplicate the declaration

        it "adds ABSPATH check to WordPress files" $ do
            let code = "<?php\nfunction my_plugin_init() {}"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let transformed = transformWordPressSecurity ast
                    let emitted = emitPhp transformed
                    emitted `shouldSatisfy` T.isInfixOf "ABSPATH"

    describe "Output Escaping Transform" $ do
        it "wraps echo with esc_html for plain text" $ do
            let code = "<?php\necho $_GET['name'];"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let transformed = transformSanitizeOutput ast
                    let emitted = emitPhp transformed
                    emitted `shouldSatisfy` T.isInfixOf "esc_html"

        it "wraps echo with esc_attr for attributes" $ do
            let code = "<?php\necho '<input value=\"' . $_GET['val'] . '\">';"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let transformed = transformSanitizeOutput ast
                    let emitted = emitPhp transformed
                    -- Should detect attribute context
                    emitted `shouldSatisfy` T.isInfixOf "esc_attr"

        it "does not wrap already escaped output" $ do
            let code = "<?php\necho esc_html($_GET['name']);"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let original = emitPhp ast
                    let transformed = transformSanitizeOutput ast
                    let emitted = emitPhp transformed
                    -- Should not double-escape
                    emitted `shouldBe` original

    describe "Input Sanitization Transform" $ do
        it "wraps $_GET access with sanitize_text_field" $ do
            let code = "<?php\n$name = $_GET['name'];"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let transformed = transformSanitizeInput ast
                    let emitted = emitPhp transformed
                    emitted `shouldSatisfy` T.isInfixOf "sanitize_text_field"

        it "wraps $_POST email with sanitize_email" $ do
            let code = "<?php\n$email = $_POST['user_email'];"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let transformed = transformSanitizeInput ast
                    let emitted = emitPhp transformed
                    emitted `shouldSatisfy` T.isInfixOf "sanitize_email"

        it "does not wrap already sanitized input" $ do
            let code = "<?php\n$name = sanitize_text_field($_GET['name']);"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let original = emitPhp ast
                    let transformed = transformSanitizeInput ast
                    let emitted = emitPhp transformed
                    emitted `shouldBe` original

    describe "SQL Preparation Transform" $ do
        it "wraps $wpdb->query with prepare" $ do
            let code = "<?php\n$wpdb->query(\"SELECT * FROM posts WHERE id = \" . $_GET['id']);"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let transformed = transformSQLPrepare ast
                    let emitted = emitPhp transformed
                    emitted `shouldSatisfy` T.isInfixOf "$wpdb->prepare"
                    emitted `shouldSatisfy` T.isInfixOf "%d"

        it "does not wrap already prepared queries" $ do
            let code = "<?php\n$wpdb->query($wpdb->prepare(\"SELECT * FROM posts WHERE id = %d\", $id));"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let original = emitPhp ast
                    let transformed = transformSQLPrepare ast
                    let emitted = emitPhp transformed
                    emitted `shouldBe` original

    describe "Redirect Safety Transform" $ do
        it "adds exit after wp_redirect" $ do
            let code = "<?php\nwp_redirect('/login');"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let transformed = transformRedirectSafety ast
                    let emitted = emitPhp transformed
                    emitted `shouldSatisfy` T.isInfixOf "wp_redirect"
                    emitted `shouldSatisfy` T.isInfixOf "exit"

        it "adds exit after header Location redirect" $ do
            let code = "<?php\nheader('Location: /login');"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let transformed = transformRedirectSafety ast
                    let emitted = emitPhp transformed
                    emitted `shouldSatisfy` T.isInfixOf "exit"

        it "does not add duplicate exit" $ do
            let code = "<?php\nwp_redirect('/login');\nexit;"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let original = emitPhp ast
                    let transformed = transformRedirectSafety ast
                    let emitted = emitPhp transformed
                    -- Should not add second exit
                    T.count "exit" emitted `shouldBe` T.count "exit" original

    describe "Type Hint Addition" $ do
        it "infers and adds return type from return statement" $ do
            let code = "<?php\nfunction getId() { return 42; }"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let transformed = transformAddTypeHints ast
                    let emitted = emitPhp transformed
                    emitted `shouldSatisfy` T.isInfixOf ": int"

        it "infers parameter type from usage" $ do
            let code = "<?php\nfunction double($x) { return $x * 2; }"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let transformed = transformAddTypeHints ast
                    let emitted = emitPhp transformed
                    -- Should infer numeric type from multiplication
                    emitted `shouldSatisfy` \t -> T.isInfixOf "int" t || T.isInfixOf "float" t

        it "does not override existing type hints" $ do
            let code = "<?php\nfunction test(string $name): void {}"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let original = emitPhp ast
                    let transformed = transformAddTypeHints ast
                    let emitted = emitPhp transformed
                    emitted `shouldBe` original

    describe "Crypto Modernization" $ do
        it "replaces rand() with random_int()" $ do
            let code = "<?php\n$random = rand(1, 100);"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let transformed = transformModernizeCrypto ast
                    let emitted = emitPhp transformed
                    emitted `shouldSatisfy` T.isInfixOf "random_int"
                    emitted `shouldNotSatisfy` T.isInfixOf "rand"

        it "replaces md5() with SHAKE3-256" $ do
            let code = "<?php\n$hash = md5($password);"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let transformed = transformModernizeCrypto ast
                    let emitted = emitPhp transformed
                    emitted `shouldSatisfy` T.isInfixOf "hash('sha3-256'"

        it "replaces sha1() with BLAKE3" $ do
            let code = "<?php\n$hash = sha1($data);"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let transformed = transformModernizeCrypto ast
                    let emitted = emitPhp transformed
                    emitted `shouldSatisfy` T.isInfixOf "sodium_crypto_generichash"

    describe "Transformation Idempotence" $ do
        it "applying transforms twice produces same result" $ do
            let code = "<?php\necho $_GET['name'];"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let once = transformSanitizeOutput ast
                    let twice = transformSanitizeOutput once
                    emitPhp once `shouldBe` emitPhp twice
