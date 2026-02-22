module A2ML.Surface

%default total

public export
data Inline
  = SText String
  | SEmph String
  | SStrong String
  | SLink String String

public export
data SBlock
  = SHeading Int String
  | SParagraph (List Inline)
  | SList (List (List Inline))
  | SDirective String (List (String, String)) (List SBlock)

public export
record SDoc where
  constructor MkSDoc
  blocks : List SBlock
