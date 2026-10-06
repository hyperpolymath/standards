-- | Test suite for Sanctify PHP
-- SPDX-License-Identifier: MPL-2.0
module Main (main) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import Sanctify.Parser
import Sanctify.AST
import Sanctify.Analysis.DeadCode
import Sanctify.Ruleset

main :: IO ()
main = hspec $ do
    describe "Sanctify.Analysis.DeadCode" $ do
        deadCodeSpecs
    describe "Sanctify.Ruleset" $ do
        rulesetSpecs

deadCodeSpecs :: Spec
deadCodeSpecs = do
    describe "analyzeDeadCode" $ do
        it "detects unused variables" $ do
            let code = T.unlines
                    [ "<?php"
                    , "$unused = 42;"
                    , "$used = 'hello';"
                    , "echo $used;"
                    ]
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ "Parse error: " ++ show err
                Right file -> do
                    let issues = analyzeDeadCode file
                        unusedVars = filter ((== UnusedVariable) . dcType) issues
                    length unusedVars `shouldBe` 1
                    dcIdentifier (head unusedVars) `shouldBe` "unused"

        it "detects unreachable code after return" $ do
            let code = T.unlines
                    [ "<?php"
                    , "function test() {"
                    , "    return 1;"
                    , "    $x = 2;"
                    , "}"
                    ]
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ "Parse error: " ++ show err
                Right file -> do
                    let issues = analyzeDeadCode file
                        unreachable = filter ((== UnreachableCode) . dcType) issues
                    length unreachable `shouldBe` 1

        it "detects unreachable code after throw" $ do
            let code = T.unlines
                    [ "<?php"
                    , "function test() {"
                    , "    throw new Exception('error');"
                    , "    $cleanup = true;"
                    , "}"
                    ]
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ "Parse error: " ++ show err
                Right file -> do
                    let issues = analyzeDeadCode file
                        unreachable = filter ((== UnreachableCode) . dcType) issues
                    length unreachable `shouldBe` 1

        it "detects unused function parameters" $ do
            let code = T.unlines
                    [ "<?php"
                    , "function greet($name, $age) {"
                    , "    return 'Hello, ' . $name;"
                    , "}"
                    ]
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ "Parse error: " ++ show err
                Right file -> do
                    let issues = analyzeDeadCode file
                        unusedParams = filter ((== UnusedParameter) . dcType) issues
                    length unusedParams `shouldBe` 1
                    dcIdentifier (head unusedParams) `shouldBe` "age"

        it "returns no issues for clean code" $ do
            let code = T.unlines
                    [ "<?php"
                    , "function add($a, $b) {"
                    , "    $sum = $a + $b;"
                    , "    return $sum;"
                    , "}"
                    ]
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ "Parse error: " ++ show err
                Right file -> do
                    let issues = analyzeDeadCode file
                    issues `shouldBe` []

        it "handles variables in closures correctly" $ do
            let code = T.unlines
                    [ "<?php"
                    , "$outer = 10;"
                    , "$fn = function() use ($outer) {"
                    , "    return $outer * 2;"
                    , "};"
                    , "$result = $fn();"
                    , "echo $result;"
                    ]
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ "Parse error: " ++ show err
                Right file -> do
                    let issues = analyzeDeadCode file
                        unusedVars = filter ((== UnusedVariable) . dcType) issues
                    -- $outer and $result should be marked as used, $fn should be used
                    unusedVars `shouldBe` []

        it "detects multiple unused variables" $ do
            let code = T.unlines
                    [ "<?php"
                    , "$a = 1;"
                    , "$b = 2;"
                    , "$c = 3;"
                    , "echo $a;"
                    ]
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ "Parse error: " ++ show err
                Right file -> do
                    let issues = analyzeDeadCode file
                        unusedVars = filter ((== UnusedVariable) . dcType) issues
                    length unusedVars `shouldBe` 2

    describe "findUnusedVariables" $ do
        it "filters only variable-related issues" $ do
            let code = T.unlines
                    [ "<?php"
                    , "function test($unused) {"
                    , "    $dead = 1;"
                    , "    return 0;"
                    , "    $unreachable = 2;"
                    , "}"
                    ]
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ "Parse error: " ++ show err
                Right file -> do
                    let issues = findUnusedVariables file
                    -- Should have unused parameter and unused variable, but not unreachable
                    all (\i -> dcType i `elem` [UnusedVariable, UnusedParameter]) issues
                        `shouldBe` True

    describe "findUnreachableCode" $ do
        it "filters only unreachable code issues" $ do
            let code = T.unlines
                    [ "<?php"
                    , "$unused = 1;"
                    , "return;"
                    , "$dead = 2;"
                    ]
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ "Parse error: " ++ show err
                Right file -> do
                    let issues = findUnreachableCode file
                    all (\i -> dcType i == UnreachableCode) issues `shouldBe` True
                    length issues `shouldBe` 1

rulesetSpecs :: Spec
rulesetSpecs = do
    describe "predefined rulesets" $ do
        it "defaultRuleset has security rules enabled" $ do
            let rs = defaultRuleset
            isRuleEnabled (RuleId "SEC001") rs `shouldBe` True
            isRuleEnabled (RuleId "SEC002") rs `shouldBe` True

        it "minimalRuleset disables low-severity rules" $ do
            let rs = minimalRuleset
            -- Dead code rules should be disabled in minimal
            isRuleEnabled (RuleId "DEAD001") rs `shouldBe` False

        it "strictRuleset elevates severity levels" $ do
            let rs = strictRuleset
                cfg = getRuleConfig (RuleId "SEC010") rs
            -- Missing strict_types should be elevated from Info
            rcSeverity cfg `shouldSatisfy` (> SeverityInfo)

        it "securityRuleset only enables security category" $ do
            let rs = securityRuleset
            isRuleEnabled (RuleId "SEC001") rs `shouldBe` True
            isRuleEnabled (RuleId "DEAD001") rs `shouldBe` False
            isRuleEnabled (RuleId "TYPE001") rs `shouldBe` False

        it "wordpressRuleset enables WP rules" $ do
            let rs = wordpressRuleset
            isRuleEnabled (RuleId "WP001") rs `shouldBe` True
            isRuleEnabled (RuleId "WP002") rs `shouldBe` True

    describe "ruleset operations" $ do
        it "enableRule enables a disabled rule" $ do
            let rs = disableRule (RuleId "SEC001") defaultRuleset
            isRuleEnabled (RuleId "SEC001") rs `shouldBe` False
            let rs' = enableRule (RuleId "SEC001") rs
            isRuleEnabled (RuleId "SEC001") rs' `shouldBe` True

        it "disableRule disables an enabled rule" $ do
            let rs = disableRule (RuleId "SEC001") defaultRuleset
            isRuleEnabled (RuleId "SEC001") rs `shouldBe` False

        it "setRuleSeverity changes rule severity" $ do
            let rs = setRuleSeverity (RuleId "SEC001") SeverityWarning defaultRuleset
                cfg = getRuleConfig (RuleId "SEC001") rs
            rcSeverity cfg `shouldBe` SeverityWarning

        it "mergeRulesets overrides rules from base" $ do
            let base = defaultRuleset
                override = disableRule (RuleId "SEC001") $
                           createRuleset "override" "test" []
                merged = mergeRulesets base override
            isRuleEnabled (RuleId "SEC001") merged `shouldBe` False

    describe "rule definitions" $ do
        it "allRules contains security rules" $ do
            let secRules = rulesByCategory CategorySecurity
            length secRules `shouldSatisfy` (> 0)
            all ((== CategorySecurity) . riCategory) secRules `shouldBe` True

        it "allRules contains dead code rules" $ do
            let deadRules = rulesByCategory CategoryDeadCode
            length deadRules `shouldSatisfy` (>= 4)

        it "getRuleInfo returns correct info" $ do
            case getRuleInfo (RuleId "SEC001") of
                Nothing -> expectationFailure "Rule SEC001 not found"
                Just info -> do
                    riCategory info `shouldBe` CategorySecurity
                    riAutoFixable info `shouldBe` True

        it "getRuleInfo returns Nothing for unknown rule" $ do
            getRuleInfo (RuleId "UNKNOWN999") `shouldBe` Nothing

    describe "createRuleset" $ do
        it "creates ruleset with specified rules enabled" $ do
            let rs = createRuleset "test" "Test ruleset"
                        [RuleId "SEC001", RuleId "SEC002"]
            isRuleEnabled (RuleId "SEC001") rs `shouldBe` True
            isRuleEnabled (RuleId "SEC002") rs `shouldBe` True

    describe "getPredefinedRuleset" $ do
        it "returns strict ruleset" $ do
            case getPredefinedRuleset "strict" of
                Nothing -> expectationFailure "strict ruleset not found"
                Just rs -> rsName rs `shouldBe` "strict"

        it "returns Nothing for unknown ruleset" $ do
            getPredefinedRuleset "nonexistent" `shouldBe` Nothing

        it "is case-insensitive" $ do
            getPredefinedRuleset "STRICT" `shouldSatisfy` (/= Nothing)
            getPredefinedRuleset "WordPress" `shouldSatisfy` (/= Nothing)
