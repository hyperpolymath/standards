module A2ML.Translator

import A2ML.TypedCore
import A2ML.Surface

-- Minimal translation: headings become sections with generated IDs,
-- paragraphs and lists map directly. Directives map based on name.

export
translate : SDoc -> Doc
translate (MkSDoc bs) = MkDoc (map toBlock bs)
  where
    mkId : String -> Id
    mkId t = MkId t

    inlineToText : List Inline -> String
    inlineToText = concatMap toString
      where
        toString : Inline -> String
        toString (SText s) = s
        toString (SEmph s) = s
        toString (SStrong s) = s
        toString (SLink label _) = label

    listToText : List (List Inline) -> List String
    listToText xs = map inlineToText xs

    attrLookup : String -> List (String, String) -> Maybe String
    attrLookup _ [] = Nothing
    attrLookup k ((k2, v) :: rest) = if k == k2 then Just v else attrLookup k rest

    collectParagraphs : List SBlock -> List String
    collectParagraphs [] = []
    collectParagraphs (SParagraph xs :: rest) = inlineToText xs :: collectParagraphs rest
    collectParagraphs (_ :: rest) = collectParagraphs rest

    toBlock : SBlock -> Block
    toBlock (SHeading _ t) = Section (MkSec (MkId ("sec:" ++ t)) t [])
    toBlock (SParagraph t) = Para (inlineToText t)
    toBlock (SList xs) = Bullet (listToText xs)
    toBlock (SDirective name attrs body) =
      case name of
        "fig" =>
          let mid = attrLookup "id" attrs
              mref = attrLookup "ref" attrs
              caption = case collectParagraphs body of
                [] => ""
                (c :: _) => c
          in Figure (MkFig (mkId (maybe "fig:unnamed" id mid)) caption (map mkId mref))
        "table" =>
          let mid = attrLookup "id" attrs
              caption = case collectParagraphs body of
                [] => ""
                (c :: _) => c
          in Table (MkTbl (mkId (maybe "tbl:unnamed" id mid)) caption)
        "refs" =>
          let labels = collectParagraphs body
              refs = map MkRef labels
          in Refs refs
        "abstract" =>
          Section (MkSec (mkId "sec:abstract") "Abstract" (map toBlock body))
        _ =>
          Section (MkSec (mkId ("sec:" ++ name)) name (map toBlock body))
