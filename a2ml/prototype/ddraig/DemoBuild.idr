module DemoBuild

import System
import System.File
import Ddraig

main : IO ()
main = do
  let inputPath = "prototype/ddraig/content/demo.md"
  let outputPath = "docs/demo.html"
  contentE <- readFile inputPath
  case contentE of
    Left err => do
      putStrLn ("Failed to read demo input: " ++ show err)
      exitFailure
    Right content => do
      let (fm, body) = parseFrontmatter content
      let htmlBody = parseMarkdown body
      let htmlOut = applyTemplate fm htmlBody
      writeResult <- writeFile outputPath htmlOut
      case writeResult of
        Left err => do
          putStrLn ("Failed to write demo output: " ++ show err)
          exitFailure
        Right _ => putStrLn ("Wrote: " ++ outputPath)
