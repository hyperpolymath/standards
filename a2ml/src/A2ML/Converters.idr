module A2ML.Converters

import A2ML.TypedCore
import A2ML.Parser
import Data.String
import Data.List
import Data.Maybe

%default total

-- ============================================================================
-- Markdown Converter
-- ============================================================================

mutual
  markdownBlocks : List Block -> String
  markdownBlocks [] = ""
  markdownBlocks (block :: rest) =
    markdownBlock block ++ markdownBlocks rest

  markdownBlock : Block -> String
  markdownBlock (Section (MkSec sid title body)) =
    let level = length sid.raw
        heading = replicate level '#' ++ " " ++ title ++ "\n\n"
    in heading ++ markdownBlocks body
  markdownBlock (Para text) = text ++ "\n\n"
  markdownBlock (Bullet items) =
    concatMap (\item => "- " ++ item ++ "\n") items ++ "\n"
  markdownBlock (Figure f) =
    "![" ++ f.caption ++ "](#" ++ f.id.raw ++ ")\n\n"
  markdownBlock (Table t) =
    "**Table " ++ t.id.raw ++ ":** " ++ t.caption ++ "\n\n"
  markdownBlock (Refs refs) =
    "## References\n\n" ++
    concatMap (\r => "- " ++ r.label ++ "\n") refs ++ "\n"
  markdownBlock (Opaque p) =
    let lang = fromMaybe "" p.lang
    in "```" ++ lang ++ "\n" ++ p.bytes ++ "\n```\n\n"

||| Convert A2ML document to CommonMark Markdown
export
toMarkdown : Doc -> String
toMarkdown (MkDoc blocks) = markdownBlocks blocks

-- ============================================================================
-- HTML Converter
-- ============================================================================

||| Escape HTML special characters
escapeHtml : String -> String
escapeHtml str = pack (concatMap escape (unpack str))
  where
    escape : Char -> List Char
    escape '<' = unpack "&lt;"
    escape '>' = unpack "&gt;"
    escape '&' = unpack "&amp;"
    escape '"' = unpack "&quot;"
    escape '\'' = unpack "&#39;"
    escape c = [c]

mutual
  htmlBlocks : List Block -> String
  htmlBlocks [] = ""
  htmlBlocks (block :: rest) = htmlBlock block ++ htmlBlocks rest

  htmlBlock : Block -> String
  htmlBlock (Section (MkSec sid title body)) =
    let level = min 6 (length sid.raw)  -- HTML has h1-h6
        tag = "h" ++ show level
        id_attr = " id=\"" ++ escapeHtml sid.raw ++ "\""
    in "<" ++ tag ++ id_attr ++ ">" ++
       escapeHtml title ++
       "</" ++ tag ++ ">\n" ++
       htmlBlocks body
  htmlBlock (Para text) =
    "<p>" ++ escapeHtml text ++ "</p>\n"
  htmlBlock (Bullet items) =
    "<ul>\n" ++
    concatMap (\item => "  <li>" ++ escapeHtml item ++ "</li>\n") items ++
    "</ul>\n"
  htmlBlock (Figure f) =
    let id_attr = " id=\"" ++ escapeHtml f.id.raw ++ "\""
    in "<figure" ++ id_attr ++ ">\n" ++
       "  <figcaption>" ++ escapeHtml f.caption ++ "</figcaption>\n" ++
       "</figure>\n"
  htmlBlock (Table t) =
    let id_attr = " id=\"" ++ escapeHtml t.id.raw ++ "\""
    in "<table" ++ id_attr ++ ">\n" ++
       "  <caption>" ++ escapeHtml t.caption ++ "</caption>\n" ++
       "</table>\n"
  htmlBlock (Refs refs) =
    "<section class=\"references\">\n" ++
    "  <h2>References</h2>\n" ++
    "  <ol>\n" ++
    concatMap (\r => "    <li>" ++ escapeHtml r.label ++ "</li>\n") refs ++
    "  </ol>\n" ++
    "</section>\n"
  htmlBlock (Opaque p) =
    let lang = fromMaybe "" p.lang
        id_attr = case p.id of
                    Just id => " id=\"" ++ escapeHtml id.raw ++ "\""
                    Nothing => ""
    in "<pre" ++ id_attr ++ "><code class=\"language-" ++ escapeHtml lang ++ "\">" ++
       escapeHtml p.bytes ++
       "</code></pre>\n"

||| Convert A2ML document to HTML5
export
toHtml : Doc -> String
toHtml (MkDoc blocks) =
  "<!DOCTYPE html>\n" ++
  "<html lang=\"en\">\n" ++
  "<head>\n" ++
  "  <meta charset=\"UTF-8\">\n" ++
  "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n" ++
  "  <title>A2ML Document</title>\n" ++
  "  <style>\n" ++
  "    body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; }\n" ++
  "    body { max-width: 800px; margin: 2rem auto; padding: 0 1rem; }\n" ++
  "    code { background: #f0f0f0; padding: 0.2em 0.4em; border-radius: 3px; }\n" ++
  "    pre { background: #f0f0f0; padding: 1rem; border-radius: 5px; overflow-x: auto; }\n" ++
  "  </style>\n" ++
  "</head>\n" ++
  "<body>\n" ++
  htmlBlocks blocks ++
  "</body>\n" ++
  "</html>\n"

-- ============================================================================
-- Djot Converter
-- ============================================================================

mutual
  djotBlocks : List Block -> String
  djotBlocks [] = ""
  djotBlocks (block :: rest) = djotBlock block ++ djotBlocks rest

  djotBlock : Block -> String
  djotBlock (Section (MkSec sid title body)) =
    let level = length sid.raw
        heading = replicate level '#' ++ " " ++ title ++ "\n" ++
                  "{#" ++ sid.raw ++ "}\n\n"
    in heading ++ djotBlocks body
  djotBlock (Para text) = text ++ "\n\n"
  djotBlock (Bullet items) =
    concatMap (\item => "- " ++ item ++ "\n") items ++ "\n"
  djotBlock (Figure f) =
    "!{#" ++ f.id.raw ++ "}[" ++ f.caption ++ "]\n\n"
  djotBlock (Table t) =
    "{#" ++ t.id.raw ++ "}\n" ++
    "**" ++ t.caption ++ "**\n\n"
  djotBlock (Refs refs) =
    "## References\n\n" ++
    concatMap (\r => ": " ++ r.label ++ "\n") refs ++ "\n"
  djotBlock (Opaque p) =
    let lang = fromMaybe "" p.lang
        id_attr = case p.id of
                    Just id => "{#" ++ id.raw ++ "}\n"
                    Nothing => ""
    in id_attr ++ "``` " ++ lang ++ "\n" ++ p.bytes ++ "\n```\n\n"

||| Convert A2ML document to Djot markup
export
toDjot : Doc -> String
toDjot (MkDoc blocks) = djotBlocks blocks

-- ============================================================================
-- LaTeX Converter
-- ============================================================================

||| Escape LaTeX special characters
escapeTex : String -> String
escapeTex str = pack (concatMap escape (unpack str))
  where
    escape : Char -> List Char
    escape '\\' = unpack "\\textbackslash{}"
    escape '{' = unpack "\\{"
    escape '}' = unpack "\\}"
    escape '$' = unpack "\\$"
    escape '&' = unpack "\\&"
    escape '%' = unpack "\\%"
    escape '#' = unpack "\\#"
    escape '_' = unpack "\\_"
    escape '^' = unpack "\\^{}"
    escape '~' = unpack "\\~{}"
    escape c = [c]

mutual
  latexBlocks : List Block -> String
  latexBlocks [] = ""
  latexBlocks (block :: rest) = latexBlock block ++ latexBlocks rest

  latexBlock : Block -> String
  latexBlock (Section (MkSec sid title body)) =
    let level = length sid.raw
        command = case level of
                    1 => "\\section"
                    2 => "\\subsection"
                    3 => "\\subsubsection"
                    4 => "\\paragraph"
                    _ => "\\subparagraph"
        label = "\\label{" ++ sid.raw ++ "}"
    in command ++ "{" ++ escapeTex title ++ "}" ++ label ++ "\n" ++
       latexBlocks body ++ "\n"
  latexBlock (Para text) = escapeTex text ++ "\n\n"
  latexBlock (Bullet items) =
    "\\begin{itemize}\n" ++
    concatMap (\item => "  \\item " ++ escapeTex item ++ "\n") items ++
    "\\end{itemize}\n\n"
  latexBlock (Figure f) =
    "\\begin{figure}[h]\n" ++
    "  \\centering\n" ++
    "  % Insert figure here\n" ++
    "  \\caption{" ++ escapeTex f.caption ++ "}\n" ++
    "  \\label{fig:" ++ f.id.raw ++ "}\n" ++
    "\\end{figure}\n\n"
  latexBlock (Table t) =
    "\\begin{table}[h]\n" ++
    "  \\centering\n" ++
    "  \\caption{" ++ escapeTex t.caption ++ "}\n" ++
    "  \\label{tab:" ++ t.id.raw ++ "}\n" ++
    "  % Insert table here\n" ++
    "\\end{table}\n\n"
  latexBlock (Refs refs) =
    "\\section*{References}\n" ++
    "\\begin{enumerate}\n" ++
    concatMap (\r => "  \\item " ++ escapeTex r.label ++ "\n") refs ++
    "\\end{enumerate}\n\n"
  latexBlock (Opaque p) =
    let lang = fromMaybe "" p.lang
    in "\\begin{lstlisting}[language=" ++ lang ++ "]\n" ++
       p.bytes ++ "\n" ++
       "\\end{lstlisting}\n\n"

||| Convert A2ML document to LaTeX
export
toLatex : Doc -> String
toLatex (MkDoc blocks) =
  "\\documentclass{article}\n" ++
  "\\usepackage[utf8]{inputenc}\n" ++
  "\\usepackage{hyperref}\n" ++
  "\\usepackage{listings}\n" ++
  "\n" ++
  "\\begin{document}\n" ++
  "\n" ++
  latexBlocks blocks ++
  "\n" ++
  "\\end{document}\n"

-- ============================================================================
-- Plain Text Converter
-- ============================================================================

numberedRefs : Nat -> List Ref -> String
numberedRefs _ [] = ""
numberedRefs n (ref :: rest) =
  show n ++ ". " ++ ref.label ++ "\n" ++ numberedRefs (S n) rest

mutual
  plainTextBlocks : List Block -> String
  plainTextBlocks [] = ""
  plainTextBlocks (block :: rest) =
    plainTextBlock block ++ plainTextBlocks rest

  plainTextBlock : Block -> String
  plainTextBlock (Section (MkSec _ title body)) =
    title ++ "\n" ++
    replicate (length title) '=' ++ "\n\n" ++
    plainTextBlocks body
  plainTextBlock (Para text) = text ++ "\n\n"
  plainTextBlock (Bullet items) =
    concatMap (\item => "* " ++ item ++ "\n") items ++ "\n"
  plainTextBlock (Figure f) =
    "[Figure: " ++ f.caption ++ "]\n\n"
  plainTextBlock (Table t) =
    "[Table: " ++ t.caption ++ "]\n\n"
  plainTextBlock (Refs refs) =
    "References\n" ++
    "==========\n\n" ++
    numberedRefs 1 refs ++ "\n"
  plainTextBlock (Opaque p) = p.bytes ++ "\n\n"

||| Convert A2ML document to plain text (no formatting)
export
toPlainText : Doc -> String
toPlainText (MkDoc blocks) = plainTextBlocks blocks

-- ============================================================================
-- Converter Selection
-- ============================================================================

||| Supported output formats
public export
data OutputFormat
  = Markdown
  | Html
  | Djot
  | Latex
  | PlainText

||| Convert document to specified format
export
convert : OutputFormat -> Doc -> String
convert Markdown = toMarkdown
convert Html = toHtml
convert Djot = toDjot
convert Latex = toLatex
convert PlainText = toPlainText

||| Parse format string to OutputFormat
export
parseFormat : String -> Maybe OutputFormat
parseFormat "md" = Just Markdown
parseFormat "markdown" = Just Markdown
parseFormat "html" = Just Html
parseFormat "htm" = Just Html
parseFormat "djot" = Just Djot
parseFormat "tex" = Just Latex
parseFormat "latex" = Just Latex
parseFormat "txt" = Just PlainText
parseFormat "text" = Just PlainText
parseFormat _ = Nothing

-- ============================================================================
-- Round-trip Testing
-- ============================================================================

||| Test if A2ML → Markdown → A2ML preserves structure
export
partial
testMarkdownRoundTrip : Doc -> Bool
testMarkdownRoundTrip doc =
  let md = toMarkdown doc
  in case parseDocument md of
    Success doc' _ => length (blocks doc) == length (blocks doc')
    _ => False

mutual
  extractContent : Doc -> String
  extractContent (MkDoc blocks) = extractBlocks blocks

  extractBlocks : List Block -> String
  extractBlocks [] = ""
  extractBlocks (block :: rest) = extractBlock block ++ extractBlocks rest

  extractBlock : Block -> String
  extractBlock (Section (MkSec _ title body)) = title ++ extractBlocks body
  extractBlock (Para text) = text
  extractBlock (Bullet items) = concatMap id items
  extractBlock (Figure f) = f.caption
  extractBlock (Table t) = t.caption
  extractBlock (Refs refs) = concatMap (\r => r.label) refs
  extractBlock (Opaque p) = p.bytes

||| Test if A2ML → Plain Text → A2ML preserves content
export
partial
testPlainTextRoundTrip : Doc -> Bool
testPlainTextRoundTrip doc =
  let txt = toPlainText doc
      content1 = extractContent doc
      content2 = txt
  in length content1 == length content2
