-- | Security analysis test suite - Vulnerability detection
-- SPDX-License-Identifier: MPL-2.0
module SecuritySpec (spec) where

import Test.Hspec
import qualified Data.Text as T

import Sanctify.Parser
import Sanctify.Analysis.Security
import Sanctify.Analysis.Advanced
import Sanctify.WordPress.Security
import Sanctify.AST (Located)

spec :: Spec
spec = do
    describe "SQL Injection Detection" $ do
        it "detects direct query with variable concatenation" $ do
            let code = "<?php\n$sql = \"SELECT * FROM users WHERE id = \" . $_GET['id'];\nmysqli_query($conn, $sql);"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let issues = analyzeSecurityIssues ast
                    any (\i -> issueType i == SQLInjection) issues `shouldBe` True

        it "detects WordPress $wpdb query without prepare" $ do
            let code = "<?php\n$wpdb->query(\"UPDATE posts SET views = views + 1 WHERE id = \" . $_GET['id']);"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let issues = analyzeSecurityIssues ast
                    length (filter (\i -> issueType i == SQLInjection) issues) `shouldSatisfy` (> 0)

        it "allows safe $wpdb->prepare usage" $ do
            let code = "<?php\n$wpdb->query($wpdb->prepare(\"UPDATE posts SET views = %d WHERE id = %d\", $views, $id));"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let issues = analyzeSecurityIssues ast
                    filter (\i -> issueType i == SQLInjection) issues `shouldBe` []

    describe "XSS Detection" $ do
        it "detects echo of unsanitized $_GET" $ do
            let code = "<?php\necho $_GET['name'];"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let issues = analyzeSecurityIssues ast
                    any (\i -> issueType i == CrossSiteScripting) issues `shouldBe` True

        it "detects echo of $_POST without escaping" $ do
            let code = "<?php\necho \"<div>\" . $_POST['content'] . \"</div>\";"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let issues = analyzeSecurityIssues ast
                    length (filter (\i -> issueType i == CrossSiteScripting) issues) `shouldSatisfy` (> 0)

        it "allows properly escaped output" $ do
            let code = "<?php\necho esc_html($_GET['name']);"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let issues = analyzeSecurityIssues ast
                    filter (\i -> issueType i == CrossSiteScripting) issues `shouldBe` []

    describe "Command Injection Detection" $ do
        it "detects shell_exec with user input" $ do
            let code = "<?php\nshell_exec(\"ls \" . $_GET['dir']);"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let issues = analyzeSecurityIssues ast
                    any (\i -> issueType i == CommandInjection) issues `shouldBe` True

        it "detects exec with user input" $ do
            let code = "<?php\nexec(\"ping \" . $_POST['host']);"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let issues = analyzeSecurityIssues ast
                    any (\i -> issueType i == CommandInjection) issues `shouldBe` True

    describe "Advanced Security - ReDoS" $ do
        it "detects catastrophic backtracking pattern (.*)* " $ do
            let code = "<?php\npreg_match('/(.*)*/i', $input);"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let issues = concatMap checkReDoS (allExprs ast)
                    length issues `shouldSatisfy` (> 0)

        it "detects nested quantifiers (.+)+" $ do
            let code = "<?php\npreg_match('/(.+)+/i', $input);"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let issues = concatMap checkReDoS (allExprs ast)
                    length issues `shouldSatisfy` (> 0)

    describe "Advanced Security - SSRF" $ do
        it "detects file_get_contents with user-controlled URL" $ do
            let code = "<?php\n$content = file_get_contents($_GET['url']);"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let issues = concatMap checkSSRF (allExprs ast)
                    length issues `shouldSatisfy` (> 0)

        it "detects wp_remote_get with user input" $ do
            let code = "<?php\nwp_remote_get($_POST['api_url']);"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let issues = concatMap checkSSRF (allExprs ast)
                    length issues `shouldSatisfy` (> 0)

    describe "WordPress Security" $ do
        it "detects missing nonce verification in form handler" $ do
            let code = "<?php\nif ($_POST['action'] == 'save') { update_option('key', $_POST['value']); }"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let issues = analyzeWordPressSecurity ast
                    any (\i -> wpIssueType i == MissingNonce) issues `shouldBe` True

        it "detects missing capability check" $ do
            let code = "<?php\nfunction admin_handler() { update_option('sensitive', $_POST['val']); }"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let issues = analyzeWordPressSecurity ast
                    any (\i -> wpIssueType i == MissingCapability) issues `shouldBe` True

        it "allows proper nonce and capability checks" $ do
            let code = "<?php\nif (current_user_can('manage_options') && wp_verify_nonce($_POST['_nonce'])) { update_option('key', sanitize_text_field($_POST['value'])); }"
            case parsePhpString "test.php" code of
                Left err -> expectationFailure $ show err
                Right ast -> do
                    let issues = analyzeWordPressSecurity ast
                    filter (\i -> wpIssueType i `elem` [MissingNonce, MissingCapability]) issues `shouldBe` []

-- Helper to extract all expressions from AST for testing
allExprs :: PhpFile -> [Located Expr]
allExprs file = concatMap stmtExprs (phpStatements file)
  where
    stmtExprs (Located _ (StmtExpr e)) = [e]
    stmtExprs (Located _ (StmtReturn (Just e))) = [e]
    stmtExprs (Located _ (StmtIf cond then_ else_)) =
        cond : concatMap stmtExprs then_ ++ maybe [] (concatMap stmtExprs) else_
    stmtExprs _ = []
