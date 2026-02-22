-- | Report generation for sanctify-php
-- SPDX-License-Identifier: AGPL-3.0-or-later
module Sanctify.Report
    ( -- * Report types
      Report(..)
    , FileReport(..)
    , IssueSummary(..)

      -- * Report generation
    , generateReport
    , generateFileReport

      -- * Output formats
    , renderJson
    , renderSarif
    , renderHtml
    , renderText
    , renderMarkdown

      -- * Statistics
    , reportStatistics
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import GHC.Generics (Generic)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime, getCurrentTime)

import Sanctify.Analysis.Security
import Sanctify.WordPress.Constraints
import Sanctify.Config

-- | Complete analysis report
data Report = Report
    { reportTimestamp     :: UTCTime
    , reportVersion       :: Text
    , reportConfig        :: Config
    , reportFiles         :: [FileReport]
    , reportSummary       :: IssueSummary
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON)

-- | Report for a single file
data FileReport = FileReport
    { fileReportPath           :: FilePath
    , fileReportSecurityIssues :: [SecurityIssue]
    , fileReportWpIssues       :: [WordPressIssue]
    , fileReportAutoFixed      :: Int
    , fileReportManualReview   :: Int
    , fileReportTransformed    :: Bool
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON)

-- | Summary of all issues
data IssueSummary = IssueSummary
    { summaryTotalFiles     :: Int
    , summaryFilesWithIssues :: Int
    , summaryBySeverity     :: Map Text Int
    , summaryByType         :: Map Text Int
    , summaryAutoFixed      :: Int
    , summaryManualReview   :: Int
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON)

-- | Generate a complete report
generateReport :: Config -> [FileReport] -> IO Report
generateReport cfg files = do
    timestamp <- getCurrentTime
    pure Report
        { reportTimestamp = timestamp
        , reportVersion = "0.1.0"
        , reportConfig = cfg
        , reportFiles = files
        , reportSummary = summarizeReports files
        }

-- | Generate a file report
generateFileReport :: FilePath -> [SecurityIssue] -> [WordPressIssue] -> Int -> Int -> Bool -> FileReport
generateFileReport = FileReport

-- | Summarize multiple file reports
summarizeReports :: [FileReport] -> IssueSummary
summarizeReports files = IssueSummary
    { summaryTotalFiles = length files
    , summaryFilesWithIssues = length $ filter hasIssues files
    , summaryBySeverity = countBySeverity files
    , summaryByType = countByType files
    , summaryAutoFixed = sum $ map fileReportAutoFixed files
    , summaryManualReview = sum $ map fileReportManualReview files
    }
  where
    hasIssues :: FileReport -> Bool
    hasIssues f = not (null (fileReportSecurityIssues f)) || not (null (fileReportWpIssues f))

    countBySeverity :: [FileReport] -> Map Text Int
    countBySeverity = foldr addSeverities Map.empty
      where
        addSeverities fr m = foldr addOne m (map issueSeverity (fileReportSecurityIssues fr))
        addOne sev = Map.insertWith (+) (T.pack $ show sev) 1

    countByType :: [FileReport] -> Map Text Int
    countByType = foldr addTypes Map.empty
      where
        addTypes fr m = foldr addOne m (map issueType (fileReportSecurityIssues fr))
        addOne t = Map.insertWith (+) (T.pack $ show t) 1

-- | Render report as JSON
renderJson :: Report -> ByteString
renderJson = Aeson.encode

-- | Render report as SARIF (Static Analysis Results Interchange Format)
renderSarif :: Report -> ByteString
renderSarif report = Aeson.encode $ Aeson.object
    [ Key.fromString "$schema" .= ("https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json" :: Text)
    , Key.fromString "version" .= ("2.1.0" :: Text)
    , Key.fromString "runs" .= [Aeson.object
        [ Key.fromString "tool" .= Aeson.object
            [ Key.fromString "driver" .= Aeson.object
                [ Key.fromString "name" .= ("sanctify-php" :: Text)
                , Key.fromString "version" .= reportVersion report
                , Key.fromString "informationUri" .= ("https://github.com/hyperpolymath/sanctify-php" :: Text)
                , Key.fromString "rules" .= sarifRules
                ]
            ]
        , Key.fromString "results" .= concatMap fileToResults (reportFiles report)
        ]]
    ]
  where
    sarifRules :: [Aeson.Value]
    sarifRules =
        [ ruleObject "SQL001" "SQL Injection" "critical"
        , ruleObject "XSS001" "Cross-Site Scripting" "high"
        , ruleObject "CMD001" "Command Injection" "critical"
        , ruleObject "WP001" "Missing Escaping" "high"
        , ruleObject "WP002" "Missing Sanitization" "high"
        , ruleObject "WP003" "Missing Nonce" "medium"
        , ruleObject "WP004" "Direct Database Query" "high"
        , ruleObject "WP005" "Missing ABSPATH Check" "medium"
        ]

    ruleObject :: Text -> Text -> Text -> Aeson.Value
    ruleObject rid name level = Aeson.object
        [ Key.fromString "id" .= rid
        , Key.fromString "name" .= name
        , Key.fromString "shortDescription" .= Aeson.object [Key.fromString "text" .= name]
        , Key.fromString "defaultConfiguration" .= Aeson.object [Key.fromString "level" .= level]
        ]

    fileToResults :: FileReport -> [Aeson.Value]
    fileToResults fr =
        map (securityToSarif (fileReportPath fr)) (fileReportSecurityIssues fr) ++
        map (wpToSarif (fileReportPath fr)) (fileReportWpIssues fr)

    securityToSarif :: FilePath -> SecurityIssue -> Aeson.Value
    securityToSarif path issue = Aeson.object
        [ Key.fromString "ruleId" .= issueToRuleId (issueType issue)
        , Key.fromString "level" .= severityToLevel (issueSeverity issue)
        , Key.fromString "message" .= Aeson.object [Key.fromString "text" .= issueDescription issue]
        , Key.fromString "locations" .= [locationObject path (issueLocation issue)]
        ]

    wpToSarif :: FilePath -> WordPressIssue -> Aeson.Value
    wpToSarif path issue = Aeson.object
        [ Key.fromString "ruleId" .= wpIssueToRuleId (wpIssueType issue)
        , Key.fromString "level" .= severityToLevel (wpSeverity issue)
        , Key.fromString "message" .= Aeson.object [Key.fromString "text" .= wpDescription issue]
        , Key.fromString "locations" .= [locationObject path (wpLocation issue)]
        ]

    locationObject :: FilePath -> SourcePos -> Aeson.Value
    locationObject path pos = Aeson.object
        [ Key.fromString "physicalLocation" .= Aeson.object
            [ Key.fromString "artifactLocation" .= Aeson.object
                [ Key.fromString "uri" .= path
                ]
            , Key.fromString "region" .= Aeson.object
                [ Key.fromString "startLine" .= posLine pos
                , Key.fromString "startColumn" .= posColumn pos
                ]
            ]
        ]

    issueToRuleId :: IssueType -> Text
    issueToRuleId SqlInjection = "SQL001"
    issueToRuleId CrossSiteScripting = "XSS001"
    issueToRuleId CommandInjection = "CMD001"
    issueToRuleId _ = "SEC999"

    wpIssueToRuleId :: WpIssueType -> Text
    wpIssueToRuleId MissingEscaping = "WP001"
    wpIssueToRuleId MissingSanitization = "WP002"
    wpIssueToRuleId MissingNonce = "WP003"
    wpIssueToRuleId DirectDatabaseQuery = "WP004"
    wpIssueToRuleId DirectFileAccess = "WP005"
    wpIssueToRuleId _ = "WP999"

    severityToLevel :: Severity -> Text
    severityToLevel Critical = "error"
    severityToLevel High = "error"
    severityToLevel Medium = "warning"
    severityToLevel Low = "note"
    severityToLevel Info = "note"

-- | Render report as HTML
renderHtml :: Report -> Text
renderHtml report = T.unlines
    [ "<!DOCTYPE html>"
    , "<html><head>"
    , "<title>Sanctify-PHP Report</title>"
    , "<style>"
    , "body { font-family: system-ui, sans-serif; max-width: 1200px; margin: 0 auto; padding: 20px; }"
    , ".critical { color: #dc3545; } .high { color: #fd7e14; } .medium { color: #ffc107; } .low { color: #28a745; }"
    , "table { width: 100%; border-collapse: collapse; } th, td { padding: 8px; text-align: left; border-bottom: 1px solid #ddd; }"
    , ".summary { display: flex; gap: 20px; margin-bottom: 20px; }"
    , ".summary-card { background: #f8f9fa; padding: 15px; border-radius: 8px; }"
    , "</style>"
    , "</head><body>"
    , "<h1>Sanctify-PHP Security Report</h1>"
    , "<div class=\"summary\">"
    , "  <div class=\"summary-card\"><strong>" <> T.pack (show (summaryTotalFiles (reportSummary report))) <> "</strong><br>Files Analyzed</div>"
    , "  <div class=\"summary-card\"><strong>" <> T.pack (show (summaryFilesWithIssues (reportSummary report))) <> "</strong><br>Files with Issues</div>"
    , "  <div class=\"summary-card\"><strong>" <> T.pack (show (summaryAutoFixed (reportSummary report))) <> "</strong><br>Auto-Fixed</div>"
    , "  <div class=\"summary-card\"><strong>" <> T.pack (show (summaryManualReview (reportSummary report))) <> "</strong><br>Manual Review</div>"
    , "</div>"
    , "<h2>Issues by File</h2>"
    , "<table>"
    , "<tr><th>File</th><th>Security</th><th>WordPress</th><th>Auto-Fixed</th></tr>"
    , T.concat $ map renderFileRow (reportFiles report)
    , "</table>"
    , "</body></html>"
    ]
  where
    renderFileRow :: FileReport -> Text
    renderFileRow fr = T.concat
        [ "<tr><td>", T.pack (fileReportPath fr), "</td>"
        , "<td>", T.pack (show (length (fileReportSecurityIssues fr))), "</td>"
        , "<td>", T.pack (show (length (fileReportWpIssues fr))), "</td>"
        , "<td>", T.pack (show (fileReportAutoFixed fr)), "</td></tr>"
        ]

-- | Render report as plain text
renderText :: Report -> Text
renderText report = T.unlines
    [ "Sanctify-PHP Security Report"
    , "============================"
    , ""
    , "Summary:"
    , "  Files analyzed: " <> T.pack (show (summaryTotalFiles (reportSummary report)))
    , "  Files with issues: " <> T.pack (show (summaryFilesWithIssues (reportSummary report)))
    , "  Auto-fixed: " <> T.pack (show (summaryAutoFixed (reportSummary report)))
    , "  Manual review needed: " <> T.pack (show (summaryManualReview (reportSummary report)))
    , ""
    , "Issues by Severity:"
    , T.unlines $ map (\(s, c) -> "  " <> s <> ": " <> T.pack (show c)) $ Map.toList (summaryBySeverity (reportSummary report))
    , ""
    , "File Details:"
    , T.unlines $ map renderFileText (reportFiles report)
    ]
  where
    renderFileText :: FileReport -> Text
    renderFileText fr = T.unlines $
        [ "", "  " <> T.pack (fileReportPath fr) <> ":" ] ++
        map (\i -> "    [" <> T.pack (show (issueSeverity i)) <> "] " <> issueDescription i) (fileReportSecurityIssues fr) ++
        map (\i -> "    [WP:" <> T.pack (show (wpIssueType i)) <> "] " <> wpDescription i) (fileReportWpIssues fr)

-- | Render report as Markdown
renderMarkdown :: Report -> Text
renderMarkdown report = T.unlines
    [ "# Sanctify-PHP Security Report"
    , ""
    , "## Summary"
    , ""
    , "| Metric | Count |"
    , "|--------|-------|"
    , "| Files Analyzed | " <> T.pack (show (summaryTotalFiles (reportSummary report))) <> " |"
    , "| Files with Issues | " <> T.pack (show (summaryFilesWithIssues (reportSummary report))) <> " |"
    , "| Auto-Fixed | " <> T.pack (show (summaryAutoFixed (reportSummary report))) <> " |"
    , "| Manual Review | " <> T.pack (show (summaryManualReview (reportSummary report))) <> " |"
    , ""
    , "## Issues by Severity"
    , ""
    , T.unlines $ map (\(s, c) -> "- **" <> s <> "**: " <> T.pack (show c)) $ Map.toList (summaryBySeverity (reportSummary report))
    , ""
    , "## File Details"
    , ""
    , T.unlines $ map renderFileMd (reportFiles report)
    ]
  where
    renderFileMd :: FileReport -> Text
    renderFileMd fr = T.unlines $
        [ "### `" <> T.pack (fileReportPath fr) <> "`"
        , ""
        ] ++
        (if null (fileReportSecurityIssues fr) then []
         else ["**Security Issues:**", ""] ++
              map (\i -> "- [" <> T.pack (show (issueSeverity i)) <> "] " <> issueDescription i) (fileReportSecurityIssues fr) ++
              [""]) ++
        (if null (fileReportWpIssues fr) then []
         else ["**WordPress Issues:**", ""] ++
              map (\i -> "- [" <> T.pack (show (wpIssueType i)) <> "] " <> wpDescription i) (fileReportWpIssues fr))

-- | Get statistics from report
reportStatistics :: Report -> Map Text Int
reportStatistics report = Map.unions
    [ summaryBySeverity (reportSummary report)
    , summaryByType (reportSummary report)
    , Map.fromList
        [ ("total_files", summaryTotalFiles (reportSummary report))
        , ("files_with_issues", summaryFilesWithIssues (reportSummary report))
        , ("auto_fixed", summaryAutoFixed (reportSummary report))
        , ("manual_review", summaryManualReview (reportSummary report))
        ]
    ]
