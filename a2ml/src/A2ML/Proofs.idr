module A2ML.Proofs

import A2ML.TypedCore
import Data.List
import Data.List.Elem
import Decidable.Equality

%default total

-- ============================================================================
-- Decidable Equality for Id
-- ============================================================================

export
DecEq Id where
  decEq (MkId a) (MkId b) with (decEq a b)
    decEq (MkId a) (MkId a) | Yes Refl = Yes Refl
    decEq (MkId a) (MkId b) | No contra = No (\Refl => contra Refl)

-- ============================================================================
-- Unique IDs with Dependent Types
-- ============================================================================

||| Proof that a list contains unique elements
public export
data Unique : List a -> Type where
  UniqueNil  : Unique []
  UniqueCons : Not (Elem x xs) -> Unique xs -> Unique (x :: xs)

||| Decidable uniqueness check
export
uniqueDec : DecEq a => (xs : List a) -> Dec (Unique xs)
uniqueDec [] = Yes UniqueNil
uniqueDec (x :: xs) with (isElem x xs)
  uniqueDec (x :: xs) | Yes prf = No (\(UniqueCons notElem _) => notElem prf)
  uniqueDec (x :: xs) | No notElem with (uniqueDec xs)
    uniqueDec (x :: xs) | No notElem | Yes uniqueXs =
      Yes (UniqueCons notElem uniqueXs)
    uniqueDec (x :: xs) | No notElem | No notUnique =
      No (\(UniqueCons _ uniqueXs) => notUnique uniqueXs)

-- ============================================================================
-- Reference Resolution with Proofs
-- ============================================================================

||| Proof that all elements of xs are in ys
public export
data AllIn : List a -> List a -> Type where
  AllInNil  : AllIn [] ys
  AllInCons : Elem x ys -> AllIn xs ys -> AllIn (x :: xs) ys

||| Decidable all-in check
export
allInDec : DecEq a => (xs, ys : List a) -> Dec (AllIn xs ys)
allInDec [] ys = Yes AllInNil
allInDec (x :: xs) ys with (isElem x ys)
  allInDec (x :: xs) ys | No notElem =
    No (\(AllInCons elem _) => notElem elem)
  allInDec (x :: xs) ys | Yes elem with (allInDec xs ys)
    allInDec (x :: xs) ys | Yes elem | Yes allIn =
      Yes (AllInCons elem allIn)
    allInDec (x :: xs) ys | Yes elem | No notAllIn =
      No (\(AllInCons _ allIn) => notAllIn allIn)

-- ============================================================================
-- Document Validation with Dependent Types
-- ============================================================================

||| A document with unique IDs (proven at type level)
public export
record UniqueDoc where
  constructor MkUniqueDoc
  doc : Doc
  ids : List Id
  {auto uniqueProof : Unique ids}

||| A document with resolved references (proven at type level)
public export
record ResolvedDoc where
  constructor MkResolvedDoc
  doc : Doc
  ids : List Id
  refs : List Id
  {auto resolvedProof : AllIn refs ids}

||| A fully validated document (unique IDs + resolved refs)
public export
record ValidatedDoc where
  constructor MkValidatedDoc
  doc : Doc
  ids : List Id
  refs : List Id
  {auto uniqueProof : Unique ids}
  {auto resolvedProof : AllIn refs ids}

-- ============================================================================
-- Construction Functions
-- ============================================================================

||| Try to construct a UniqueDoc (returns Nothing if IDs not unique)
export
mkUniqueDoc : (d : Doc) -> (ids : List Id) -> Maybe UniqueDoc
mkUniqueDoc d ids with (uniqueDec ids)
  mkUniqueDoc d ids | Yes prf = Just (MkUniqueDoc d ids)
  mkUniqueDoc d ids | No _ = Nothing

||| Try to construct a ResolvedDoc (returns Nothing if refs don't resolve)
export
mkResolvedDoc : (d : Doc) -> (ids : List Id) -> (refs : List Id) ->
                Maybe ResolvedDoc
mkResolvedDoc d ids refs with (allInDec refs ids)
  mkResolvedDoc d ids refs | Yes prf = Just (MkResolvedDoc d ids refs)
  mkResolvedDoc d ids refs | No _ = Nothing

||| Try to construct a ValidatedDoc (returns Nothing if validation fails)
export
mkValidatedDoc : (d : Doc) -> (ids : List Id) -> (refs : List Id) ->
                 Maybe ValidatedDoc
mkValidatedDoc d ids refs with (uniqueDec ids, allInDec refs ids)
  mkValidatedDoc d ids refs | (Yes uniquePrf, Yes resolvedPrf) =
    Just (MkValidatedDoc d ids refs)
  mkValidatedDoc d ids refs | _ = Nothing

-- ============================================================================
-- Validation Error Messages
-- ============================================================================

||| Structured validation error
public export
data ValidationError
  = DuplicateId Id
  | UnresolvedRef Id
  | MissingRequired String

||| Validate a document and return errors or a validated document
export
validateDocument : Doc -> List Id -> List Id ->
                   Either (List ValidationError) ValidatedDoc
validateDocument d ids refs =
  case (uniqueDec ids, allInDec refs ids) of
    (Yes uniquePrf, Yes resolvedPrf) =>
      Right (MkValidatedDoc d ids refs)
    (No _, Yes _) =>
      -- Find duplicate IDs
      Left [DuplicateId (MkId "unknown")]  -- TODO: identify specific duplicate
    (Yes _, No _) =>
      -- Find unresolved refs
      Left [UnresolvedRef (MkId "unknown")]  -- TODO: identify specific ref
    (No _, No _) =>
      Left [DuplicateId (MkId "unknown"), UnresolvedRef (MkId "unknown")]

-- ============================================================================
-- Attestation Mechanisms
-- ============================================================================

||| Cryptographic attestation (stub for now)
public export
record Attestation where
  constructor MkAttestation
  documentHash : String
  signature : String
  publicKey : String
  timestamp : Integer

||| An attested document (validated + signed)
public export
record AttestedDoc where
  constructor MkAttestedDoc
  validated : ValidatedDoc
  attestation : Attestation

||| Verify an attestation (stub - would use real crypto in production)
export
verifyAttestation : AttestedDoc -> Bool
verifyAttestation attested =
  -- In production: verify signature using publicKey
  -- For now: just check fields are non-empty
  let att = attested.attestation in
  not (att.documentHash == "") &&
  not (att.signature == "") &&
  not (att.publicKey == "")

-- ============================================================================
-- Theorem: If a document is validated, it has unique IDs
-- ============================================================================

||| Proof that ValidatedDoc implies unique IDs
export
validatedHasUniqueIds : ValidatedDoc -> Unique (ids validated)
validatedHasUniqueIds validated = validated.uniqueProof

||| Proof that ValidatedDoc implies resolved references
export
validatedHasResolvedRefs : ValidatedDoc -> AllIn (refs validated) (ids validated)
validatedHasResolvedRefs validated = validated.resolvedProof

-- ============================================================================
-- Example Usage
-- ============================================================================

||| Example: Create a validated document
export
exampleValidated : Maybe ValidatedDoc
exampleValidated =
  let doc = MkDoc []
      ids = [MkId "intro", MkId "methods"]
      refs = [MkId "intro"]
  in mkValidatedDoc doc ids refs

||| Example: Validate and attest a document
export
exampleAttestation : Maybe AttestedDoc
exampleAttestation = do
  validated <- exampleValidated
  let attestation = MkAttestation
        "sha256:abc123..."
        "ed25519:def456..."
        "pub:789abc..."
        1706659200
  pure (MkAttestedDoc validated attestation)

-- ============================================================================
-- Property: Uniqueness is preserved under append (if disjoint)
-- ============================================================================

||| If two lists are unique and disjoint, their concatenation is unique
export
uniqueAppendDisjoint : Unique xs -> Unique ys ->
                       (disjoint : All (\x => Not (Elem x ys)) xs) ->
                       Unique (xs ++ ys)
uniqueAppendDisjoint UniqueNil uniqueYs disjoint = uniqueYs
uniqueAppendDisjoint (UniqueCons notElem uniqueXs) uniqueYs (d :: ds) =
  UniqueCons (appendNotElem notElem d) (uniqueAppendDisjoint uniqueXs uniqueYs ds)
  where
    appendNotElem : Not (Elem x xs) -> Not (Elem x ys) -> Not (Elem x (xs ++ ys))
    appendNotElem notXs notYs elem with (elemAppend xs ys elem)
      appendNotElem notXs notYs elem | Left elemXs = notXs elemXs
      appendNotElem notXs notYs elem | Right elemYs = notYs elemYs

-- ============================================================================
-- Property: Reference resolution is monotonic
-- ============================================================================

||| If all refs resolve in ids, they resolve in a superset
export
resolveMonotonic : AllIn refs ids -> (moreIds : List Id) ->
                   AllIn refs (ids ++ moreIds)
resolveMonotonic AllInNil moreIds = AllInNil
resolveMonotonic (AllInCons elem allIn) moreIds =
  AllInCons (elemAppend ids moreIds elem |> Left)
            (resolveMonotonic allIn moreIds)
