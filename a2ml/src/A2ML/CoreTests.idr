module A2ML.CoreTests

import A2ML.TypedCore
import A2ML.Surface
import A2ML.Translator

-- Simple sanity tests for v0.2

mkSampleDoc : Doc
mkSampleDoc = MkDoc [
  Section (MkSec (MkId "sec:abstract") "Abstract" [Para "Hello"]),
  Figure (MkFig (MkId "fig:one") "Fig" (Just (MkId "sec:abstract"))),
  Table (MkTbl (MkId "tbl:one") "Table caption"),
  Refs [MkRef "[1] Ref"]
  ]

mkSurface : SDoc
mkSurface = MkSDoc [
  SHeading 1 "Intro",
  SParagraph [SText "Hello"],
  SList [[SText "Item"]],
  SDirective "fig" [("id", "fig:one"), ("ref", "sec:intro")] [SParagraph [SText "Caption"]],
  SDirective "table" [("id", "tbl:one")] [SParagraph [SText "Table caption"]],
  SDirective "refs" [] [SParagraph [SText "[1] Ref"]]
  ]

main : IO ()
main = do
  let errs = validateDoc mkSampleDoc
  case errs == [] of
    True => putStrLn "validateDoc: ok"
    False => putStrLn ("validateDoc: failed: " ++ show errs)

  let core = translate mkSurface
  case uniqueIdsB core of
    True => putStrLn "translate: ok"
    False => putStrLn "translate: duplicate ids"
