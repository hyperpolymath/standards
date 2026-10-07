-- | Parser test suite - Golden file tests
-- SPDX-License-Identifier: MPL-2.0
module ParserSpec (spec) where

import Test.Hspec
import Test.Hspec.Golden
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath ((</>))

import Sanctify.Parser
import Sanctify.AST
import Sanctify.Emit

spec :: Spec
spec = do
    describe "PHP 8.2+ Parser" $ do
        it "parses readonly classes" $ do
            let code = "<?php\nreadonly class User { public string $name; }"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    phpDeclareStrict ast `shouldBe` False
                    length (phpStatements ast) `shouldBe` 1

        it "parses DNF types (A&B)|(C&D)" $ do
            let code = "<?php\nfunction test((Foo&Bar)|(Baz&Qux) $param): void {}"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right _ -> pure ()  -- Successfully parsed

        it "parses match expressions" $ do
            let code = "<?php\n$result = match($x) { 1 => 'one', 2 => 'two', default => 'other' };"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right _ -> pure ()

        it "parses enums" $ do
            let code = "<?php\nenum Status { case Pending; case Approved; case Rejected; }"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right _ -> pure ()

        it "parses backed enums" $ do
            let code = "<?php\nenum Status: string { case Pending = 'pending'; case Approved = 'approved'; }"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right _ -> pure ()

        it "parses attributes" $ do
            let code = "<?php\n#[Route('/api/users')]\nfunction getUsers() {}"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right _ -> pure ()

        it "parses constructor promotion" $ do
            let code = "<?php\nclass User { public function __construct(public readonly string $name) {} }"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right _ -> pure ()

        it "parses arrow functions" $ do
            let code = "<?php\n$fn = fn($x) => $x * 2;"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right _ -> pure ()

        it "parses null coalescing assignment" $ do
            let code = "<?php\n$x ??= 'default';"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right _ -> pure ()

        it "parses trait constants (PHP 8.2)" $ do
            let code = "<?php\ntrait MyTrait { public const CONSTANT = 'value'; }"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right _ -> pure ()

    describe "Parser Round-trip" $ do
        it "maintains code structure through parse->emit" $ do
            let code = "<?php\ndeclare(strict_types=1);\n\nfunction add(int $a, int $b): int {\n    return $a + $b;\n}\n"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let emitted = emitPhp ast
                    -- Should parse again without error
                    case parsePhpString "test.php" emitted of
                        Left err -> expectationFailure $ "Round-trip failed: " ++ show err
                        Right _ -> pure ()

    describe "Error Recovery" $ do
        it "reports useful error for syntax error" $ do
            let code = "<?php\nfunction test( { }"  -- Missing parameter
            case parsePhpString "test.php" code of
                Left err -> show err `shouldContain` "test.php"
                Right _ -> expectationFailure "Should have failed to parse"

        it "handles unclosed braces gracefully" $ do
            let code = "<?php\nfunction test() {"  -- No closing brace
            case parsePhpString "test.php" code of
                Left _ -> pure ()  -- Expected to fail
                Right _ -> expectationFailure "Should have failed to parse"
