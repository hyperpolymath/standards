module A2ML.ParserTests

import A2ML.Parser
import A2ML.TypedCore
import A2ML.Proofs
import Decidable.Equality

-- NOTE (2026-07-29): this module previously exercised a pipeline that does not
-- exist: `parse : String -> Either _ SDoc`, plus `uniqueIdsDec`,
-- `refsResolveDec` and `hasAbstractDec`. None of those are defined anywhere in
-- the core, so this file could never compile.
--
-- It now tests the API that IS implemented: `parseDocument` into the typed
-- core, then the real decision procedures from A2ML.Proofs.
--
-- Real gap this uncovered, worth its own work: `A2ML.Surface.SDoc` and
-- `A2ML.Translator.translate : SDoc -> Doc` both exist, but NOTHING produces an
-- SDoc — there is no surface parser. Until one exists, the Surface/Translator
-- half of the pipeline is unreachable.

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
  case parseDocument testInput of
    Failure err _ => putStrLn ("Parse error: " ++ err)
    Success doc _ => do
      putStrLn "Parsed successfully"
      let ids = collectIds doc
          refs = collectRefs doc

      case uniqueDec ids of
        Yes _ => putStrLn "Unique IDs: proven"
        No _  => putStrLn "Unique IDs: failed"

      case allInDec refs ids of
        Yes _ => putStrLn "Refs resolve: proven"
        No _  => putStrLn "Refs resolve: failed"

      putStrLn "All tests complete!"
