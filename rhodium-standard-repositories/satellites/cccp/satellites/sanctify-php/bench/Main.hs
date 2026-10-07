-- | Benchmark suite for sanctify-php
-- SPDX-License-Identifier: MPL-2.0
module Main where

import Criterion.Main
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath ((</>))

import Sanctify.Parser
import Sanctify.Analysis.Security
import Sanctify.Analysis.Advanced
import Sanctify.Transform.Sanitize
import Sanctify.Transform.Strict
import Sanctify.Emit

main :: IO ()
main = do
    -- Read fixture files for benchmarking
    sqlCode <- TIO.readFile ("test" </> "fixtures" </> "vulnerable-sql.php")
    xssCode <- TIO.readFile ("test" </> "fixtures" </> "vulnerable-xss.php")
    wpCode <- TIO.readFile ("test" </> "fixtures" </> "wordpress-unsafe.php")

    -- Generate synthetic PHP for throughput testing
    let smallPhp = generatePhp 10
    let mediumPhp = generatePhp 100
    let largePhp = generatePhp 500

    defaultMain
        [ bgroup "Parser"
            [ bench "small PHP (10 lines)" $ nf parseSmall smallPhp
            , bench "medium PHP (100 lines)" $ nf parseMedium mediumPhp
            , bench "large PHP (500 lines)" $ nf parseLarge largePhp
            , bench "sql-injection fixture" $ nf parseFixture sqlCode
            , bench "xss fixture" $ nf parseFixture xssCode
            , bench "wordpress fixture" $ nf parseFixture wpCode
            ]

        , bgroup "Security Analysis"
            [ bench "small PHP analysis" $ nf analyzeSmall smallPhp
            , bench "medium PHP analysis" $ nf analyzeMedium mediumPhp
            , bench "large PHP analysis" $ nf analyzeLarge largePhp
            , bench "sql-injection analysis" $ nf analyzeFixture sqlCode
            , bench "xss analysis" $ nf analyzeFixture xssCode
            ]

        , bgroup "Transformation"
            [ bench "strict transform (small)" $ nf transformSmallStrict smallPhp
            , bench "strict transform (medium)" $ nf transformMediumStrict mediumPhp
            , bench "sanitize transform (small)" $ nf transformSmallSanitize smallPhp
            , bench "sanitize transform (medium)" $ nf transformMediumSanitize mediumPhp
            ]

        , bgroup "Emission (Code Generation)"
            [ bench "emit small PHP" $ nf emitSmall smallPhp
            , bench "emit medium PHP" $ nf emitMedium mediumPhp
            , bench "emit large PHP" $ nf emitLarge largePhp
            ]

        , bgroup "Full Pipeline"
            [ bench "parse + analyze + emit (small)" $ nf fullPipelineSmall smallPhp
            , bench "parse + analyze + emit (medium)" $ nf fullPipelineMedium mediumPhp
            , bench "parse + analyze + emit (large)" $ nf fullPipelineLarge largePhp
            ]
        ]

-- Helpers for small benchmarks
parseSmall :: T.Text -> Either String ()
parseSmall code = case parsePhpString "test.php" code of
    Left _ -> Left "parse error"
    Right _ -> Right ()

parseMedium :: T.Text -> Either String ()
parseMedium code = case parsePhpString "test.php" code of
    Left _ -> Left "parse error"
    Right _ -> Right ()

parseLarge :: T.Text -> Either String ()
parseLarge code = case parsePhpString "test.php" code of
    Left _ -> Left "parse error"
    Right _ -> Right ()

parseFixture :: T.Text -> Either String ()
parseFixture code = case parsePhpString "fixture.php" code of
    Left _ -> Left "parse error"
    Right _ -> Right ()

analyzeSmall :: T.Text -> Either String Int
analyzeSmall code = case parsePhpString "test.php" code of
    Left _ -> Left "parse error"
    Right ast -> Right (length (analyzeSecurityIssues ast))

analyzeMedium :: T.Text -> Either String Int
analyzeMedium code = case parsePhpString "test.php" code of
    Left _ -> Left "parse error"
    Right ast -> Right (length (analyzeSecurityIssues ast))

analyzeLarge :: T.Text -> Either String Int
analyzeLarge code = case parsePhpString "test.php" code of
    Left _ -> Left "parse error"
    Right ast -> Right (length (analyzeSecurityIssues ast))

analyzeFixture :: T.Text -> Either String Int
analyzeFixture code = case parsePhpString "fixture.php" code of
    Left _ -> Left "parse error"
    Right ast -> Right (length (analyzeSecurityIssues ast))

transformSmallStrict :: T.Text -> Either String ()
transformSmallStrict code = case parsePhpString "test.php" code of
    Left _ -> Left "parse error"
    Right ast -> Right (let _ = transformStrict ast in ())

transformMediumStrict :: T.Text -> Either String ()
transformMediumStrict code = case parsePhpString "test.php" code of
    Left _ -> Left "parse error"
    Right ast -> Right (let _ = transformStrict ast in ())

transformSmallSanitize :: T.Text -> Either String ()
transformSmallSanitize code = case parsePhpString "test.php" code of
    Left _ -> Left "parse error"
    Right ast -> Right (let _ = transformSanitizeOutput ast in ())

transformMediumSanitize :: T.Text -> Either String ()
transformMediumSanitize code = case parsePhpString "test.php" code of
    Left _ -> Left "parse error"
    Right ast -> Right (let _ = transformSanitizeOutput ast in ())

emitSmall :: T.Text -> Either String ()
emitSmall code = case parsePhpString "test.php" code of
    Left _ -> Left "parse error"
    Right ast -> Right (let _ = emitPhp ast in ())

emitMedium :: T.Text -> Either String ()
emitMedium code = case parsePhpString "test.php" code of
    Left _ -> Left "parse error"
    Right ast -> Right (let _ = emitPhp ast in ())

emitLarge :: T.Text -> Either String ()
emitLarge code = case parsePhpString "test.php" code of
    Left _ -> Left "parse error"
    Right ast -> Right (let _ = emitPhp ast in ())

fullPipelineSmall :: T.Text -> Either String T.Text
fullPipelineSmall code = case parsePhpString "test.php" code of
    Left _ -> Left "parse error"
    Right ast ->
        let transformed = transformSanitizeOutput (transformStrict ast)
            emitted = emitPhp transformed
        in Right emitted

fullPipelineMedium :: T.Text -> Either String T.Text
fullPipelineMedium code = case parsePhpString "test.php" code of
    Left _ -> Left "parse error"
    Right ast ->
        let transformed = transformSanitizeOutput (transformStrict ast)
            emitted = emitPhp transformed
        in Right emitted

fullPipelineLarge :: T.Text -> Either String T.Text
fullPipelineLarge code = case parsePhpString "test.php" code of
    Left _ -> Left "parse error"
    Right ast ->
        let transformed = transformSanitizeOutput (transformStrict ast)
            emitted = emitPhp transformed
        in Right emitted

-- Generate synthetic PHP code for benchmarking
generatePhp :: Int -> T.Text
generatePhp lineCount =
    T.pack $ unlines $
        [ "<?php"
        , "// SPDX-License-Identifier: MPL-2.0"
        , "// Generated benchmark fixture"
        ] ++ replicate (lineCount - 3) "echo 'line';"
