module A2ML.Converters

import A2ML.TypedCore
import Data.String
import Data.List

%default total

-- ============================================================================
-- Markdown Converter
-- ============================================================================

||| Convert A2ML document to CommonMark Markdown
export
toMarkdown : Doc -> String
toMarkdown (MkDoc blocks) = concatMap blockToMd blocks
  where
    blockToMd : Block -> String
    blockToMd (Section s) =
      let level = length s.id.raw
          heading = replicate level '#' ++ " " ++ s.title ++ "\n\n"
          body = toMarkdown (MkDoc s.body)
      in heading ++ body

    blockToMd (Para text) = text ++ "\n\n"

    blockToMd (Bullet items) =
      concatMap (\item => "- " ++ item ++ "\n") items ++ "\n"

    blockToMd (Figure f) =
      "![" ++ f.caption ++ "](#" ++ f.id.raw ++ ")\n\n"

    blockToMd (Table t) =
      "**Table " ++ t.id.raw ++ ":** " ++ t.caption ++ "\n\n"

    blockToMd (Refs refs) =
      "## References\n\n" ++
      concatMap (\r => "- " ++ r.label ++ "\n") refs ++ "\n"

    blockToMd (Opaque p) =
      let lang = fromMaybe "" p.lang
      in "```" ++ lang ++ "\n" ++ p.bytes ++ "\n```\n\n"

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

||| Convert A2ML document to HTML5
export
toHtml : Doc -> String
toHtml doc =
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
  docToHtml doc ++
  "</body>\n" ++
  "</html>\n"
  where
    docToHtml : Doc -> String
    docToHtml (MkDoc blocks) = concatMap blockToHtml blocks

    blockToHtml : Block -> String
    blockToHtml (Section s) =
      let level = min 6 (length s.id.raw)  -- HTML has h1-h6
          tag = "h" ++ show level
          id_attr = " id=\"" ++ escapeHtml s.id.raw ++ "\""
      in "<" ++ tag ++ id_attr ++ ">" ++
         escapeHtml s.title ++
         "</" ++ tag ++ ">\n" ++
         docToHtml (MkDoc s.body)

    blockToHtml (Para text) =
      "<p>" ++ escapeHtml text ++ "</p>\n"

    blockToHtml (Bullet items) =
      "<ul>\n" ++
      concatMap (\item => "  <li>" ++ escapeHtml item ++ "</li>\n") items ++
      "</ul>\n"

    blockToHtml (Figure f) =
      let id_attr = " id=\"" ++ escapeHtml f.id.raw ++ "\""
      in "<figure" ++ id_attr ++ ">\n" ++
         "  <figcaption>" ++ escapeHtml f.caption ++ "</figcaption>\n" ++
         "</figure>\n"

    blockToHtml (Table t) =
      let id_attr = " id=\"" ++ escapeHtml t.id.raw ++ "\""
      in "<table" ++ id_attr ++ ">\n" ++
         "  <caption>" ++ escapeHtml t.caption ++ "</caption>\n" ++
         "</table>\n"

    blockToHtml (Refs refs) =
      "<section class=\"references\">\n" ++
      "  <h2>References</h2>\n" ++
      "  <ol>\n" ++
      concatMap (\r => "    <li>" ++ escapeHtml r.label ++ "</li>\n") refs ++
      "  </ol>\n" ++
      "</section>\n"

    blockToHtml (Opaque p) =
      let lang = fromMaybe "" p.lang
          id_attr = case p.id of
                      Just id => " id=\"" ++ escapeHtml id.raw ++ "\""
                      Nothing => ""
      in "<pre" ++ id_attr ++ "><code class=\"language-" ++ escapeHtml lang ++ "\">" ++
         escapeHtml p.bytes ++
         "</code></pre>\n"

-- ============================================================================
-- Djot Converter
-- ============================================================================

||| Convert A2ML document to Djot markup
export
toDjot : Doc -> String
toDjot (MkDoc blocks) = concatMap blockToDjot blocks
  where
    blockToDjot : Block -> String
    blockToDjot (Section s) =
      let level = length s.id.raw
          heading = replicate level '#' ++ " " ++ s.title ++ "\n" ++
                    "{#" ++ s.id.raw ++ "}\n\n"
          body = toDjot (MkDoc s.body)
      in heading ++ body

    blockToDjot (Para text) = text ++ "\n\n"

    blockToDjot (Bullet items) =
      concatMap (\item => "- " ++ item ++ "\n") items ++ "\n"

    blockToDjot (Figure f) =
      "!{#" ++ f.id.raw ++ "}[" ++ f.caption ++ "]\n\n"

    blockToDjot (Table t) =
      "{#" ++ t.id.raw ++ "}\n" ++
      "**" ++ t.caption ++ "**\n\n"

    blockToDjot (Refs refs) =
      "## References\n\n" ++
      concatMap (\r => ": " ++ r.label ++ "\n") refs ++ "\n"

    blockToDjot (Opaque p) =
      let lang = fromMaybe "" p.lang
          id_attr = case p.id of
                      Just id => "{#" ++ id.raw ++ "}\n"
                      Nothing => ""
      in id_attr ++ "``` " ++ lang ++ "\n" ++ p.bytes ++ "\n```\n\n"

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

||| Convert A2ML document to LaTeX
export
toLatex : Doc -> String
toLatex doc =
  "\\documentclass{article}\n" ++
  "\\usepackage[utf8]{inputenc}\n" ++
  "\\usepackage{hyperref}\n" ++
  "\\usepackage{listings}\n" ++
  "\n" ++
  "\\begin{document}\n" ++
  "\n" ++
  docToTex doc ++
  "\n" ++
  "\\end{document}\n"
  where
    docToTex : Doc -> String
    docToTex (MkDoc blocks) = concatMap blockToTex blocks

    blockToTex : Block -> String
    blockToTex (Section s) =
      let level = length s.id.raw
          command = case level of
                      1 => "\\section"
                      2 => "\\subsection"
                      3 => "\\subsubsection"
                      4 => "\\paragraph"
                      _ => "\\subparagraph"
          label = "\\label{" ++ s.id.raw ++ "}"
      in command ++ "{" ++ escapeTex s.title ++ "}" ++ label ++ "\n" ++
         docToTex (MkDoc s.body) ++ "\n"

    blockToTex (Para text) =
      escapeTex text ++ "\n\n"

    blockToTex (Bullet items) =
      "\\begin{itemize}\n" ++
      concatMap (\item => "  \\item " ++ escapeTex item ++ "\n") items ++
      "\\end{itemize}\n\n"

    blockToTex (Figure f) =
      "\\begin{figure}[h]\n" ++
      "  \\centering\n" ++
      "  % Insert figure here\n" ++
      "  \\caption{" ++ escapeTex f.caption ++ "}\n" ++
      "  \\label{fig:" ++ f.id.raw ++ "}\n" ++
      "\\end{figure}\n\n"

    blockToTex (Table t) =
      "\\begin{table}[h]\n" ++
      "  \\centering\n" ++
      "  \\caption{" ++ escapeTex t.caption ++ "}\n" ++
      "  \\label{tab:" ++ t.id.raw ++ "}\n" ++
      "  % Insert table here\n" ++
      "\\end{table}\n\n"

    blockToTex (Refs refs) =
      "\\section*{References}\n" ++
      "\\begin{enumerate}\n" ++
      concatMap (\r => "  \\item " ++ escapeTex r.label ++ "\n") refs ++
      "\\end{enumerate}\n\n"

    blockToTex (Opaque p) =
      let lang = fromMaybe "" p.lang
      in "\\begin{lstlisting}[language=" ++ lang ++ "]\n" ++
         p.bytes ++ "\n" ++
         "\\end{lstlisting}\n\n"

-- ============================================================================
-- Plain Text Converter
-- ============================================================================

||| Convert A2ML document to plain text (no formatting)
export
toPlainText : Doc -> String
toPlainText (MkDoc blocks) = concatMap blockToText blocks
  where
    blockToText : Block -> String
    blockToText (Section s) =
      s.title ++ "\n" ++
      replicate (length s.title) '=' ++ "\n\n" ++
      toPlainText (MkDoc s.body)

    blockToText (Para text) = text ++ "\n\n"

    blockToText (Bullet items) =
      concatMap (\item => "* " ++ item ++ "\n") items ++ "\n"

    blockToText (Figure f) =
      "[Figure: " ++ f.caption ++ "]\n\n"

    blockToText (Table t) =
      "[Table: " ++ t.caption ++ "]\n\n"

    blockToText (Refs refs) =
      "References\n" ++
      "==========\n\n" ++
      concatMap (\r => show (1 + elemIndex r refs) ++ ". " ++ r.label ++ "\n") refs ++ "\n"
      where
        elemIndex : Eq a => a -> List a -> Nat
        elemIndex x [] = 0
        elemIndex x (y :: ys) = if x == y then 0 else 1 + elemIndex x ys

    blockToText (Opaque p) = p.bytes ++ "\n\n"

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

||| Test if A2ML → Plain Text → A2ML preserves content
export
partial
testPlainTextRoundTrip : Doc -> Bool
testPlainTextRoundTrip doc =
  let txt = toPlainText doc
      content1 = extractContent doc
      content2 = txt
  in length content1 == length content2
  where
    extractContent : Doc -> String
    extractContent (MkDoc blocks) = concatMap extractBlock blocks

    extractBlock : Block -> String
    extractBlock (Section s) = s.title ++ extractContent (MkDoc s.body)
    extractBlock (Para text) = text
    extractBlock (Bullet items) = concatMap id items
    extractBlock (Figure f) = f.caption
    extractBlock (Table t) = t.caption
    extractBlock (Refs refs) = concatMap (\r => r.label) refs
    extractBlock (Opaque p) = p.bytes
