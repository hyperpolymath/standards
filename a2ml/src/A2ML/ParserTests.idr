module A2ML.ParserTests

import A2ML.Parser
import A2ML.Surface
import A2ML.Translator
import A2ML.TypedCore
import A2ML.Proofs

-- Test the Idris2 parser with a simple input
testInput : String
testInput = """
# A2ML Overview

@abstract:
A2ML is a typed, attested markup format.
@end

## Claims
- Required sections must exist
- References must resolve
"""

main : IO ()
main = do
  putStrLn "Testing Idris2 A2ML Parser..."
  case parse testInput of
    Left err => putStrLn "Parse error"
    Right sdoc => do
      putStrLn "✓ Parsed successfully"
      let doc = translate sdoc
      putStrLn "✓ Translated to typed core"

      -- Test decidable proofs
      case uniqueIdsDec doc of
        Yes prf => putStrLn "✓ Unique IDs: proven"
        No contra => putStrLn "✗ Unique IDs: failed"

      case refsResolveDec doc of
        Yes prf => putStrLn "✓ Refs resolve: proven"
        No contra => putStrLn "✗ Refs resolve: failed"

      case hasAbstractDec doc of
        Yes prf => putStrLn "✓ Has abstract: proven"
        No contra => putStrLn "✗ Has abstract: failed"

      putStrLn "\nAll tests complete!"
