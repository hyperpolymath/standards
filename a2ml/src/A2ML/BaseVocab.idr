-- SPDX-License-Identifier: AGPL-3.0-or-later
||| A2ML v1.1 base record vocabulary (SPEC.adoc Section 7).
|||
||| This is the anti-desync primitive: one neutral shape for "a thing that can be
||| identified, located, attributed, and integrity-checked". The module defines the
||| typed-core nodes for the base record and *decidable* base-validity checks.
||| It names no downstream consumer.
module A2ML.BaseVocab

import A2ML.TypedCore
import Decidable.Equality

%default total

-- ============================================================================
-- Provenance kind: a CLOSED enumeration (SPEC Section 7.5)
-- ============================================================================

||| Exactly one of human / ai / mechanical. A validator MUST reject any other
||| value in checked mode.
public export
data Kind = Human | Ai | Mechanical

public export
Eq Kind where
  Human      == Human      = True
  Ai         == Ai         = True
  Mechanical == Mechanical = True
  _          == _          = False

||| Parse the closed surface tokens. Returns Nothing for anything off-enum.
export
parseKind : String -> Maybe Kind
parseKind "human"      = Just Human
parseKind "ai"         = Just Ai
parseKind "mechanical" = Just Mechanical
parseKind _            = Nothing

export
renderKind : Kind -> String
renderKind Human      = "human"
renderKind Ai         = "ai"
renderKind Mechanical = "mechanical"

-- ============================================================================
-- Located region and structural address (SPEC Section 7.4 / 7.2)
-- ============================================================================

public export
record Position where
  constructor MkPosition
  line : Nat    -- 1-based
  col  : Nat    -- 1-based

public export
record SourceSpan where
  constructor MkSourceSpan
  file  : String
  start : Position
  end   : Position

-- ============================================================================
-- Provenance (SPEC Section 7.5)
-- ============================================================================

public export
record Provenance where
  constructor MkProvenance
  author : String
  tool   : String          -- name@version
  kind   : Kind
  agent  : Maybe String    -- SHOULD be present when kind = Ai

-- ============================================================================
-- The base record (SPEC Section 7.2)
-- ============================================================================

public export
record BaseRecord where
  constructor MkBaseRecord
  id            : Id
  sourceSpan    : SourceSpan
  canonicalNode : Maybe String   -- OPTIONAL: stable structural address
  hash          : String         -- "sha256:<64 lower-hex>"
  provenance    : Provenance
  timestamp     : String         -- RFC 3339 UTC
  artefactRef   : String
  profileDecl   : Maybe String   -- OPTIONAL: QualifiedId echo of @profile

-- ============================================================================
-- Well-formedness helpers
-- ============================================================================

nonEmpty : String -> Bool
nonEmpty s = s /= ""

hexLower : Char -> Bool
hexLower c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')

||| Structural prefix strip on character lists (total; recurses on the prefix).
dropPrefix : List Char -> List Char -> Maybe (List Char)
dropPrefix []        ys        = Just ys
dropPrefix (_ :: _)  []        = Nothing
dropPrefix (x :: xs) (y :: ys) = if x == y then dropPrefix xs ys else Nothing

||| A canonical SHA-256 digest: "sha256:" followed by 64 lowercase hex chars
||| (SPEC Section 9.2). The prefix is the hash-agility hook (SPEC Section 12.4).
export
isSha256 : String -> Bool
isSha256 s =
  case dropPrefix (unpack "sha256:") (unpack s) of
    Nothing   => False
    Just rest => length rest == 64 && all hexLower rest

||| Lexicographic <= on positions (line, then col).
posLeq : Position -> Position -> Bool
posLeq a b = (a.line < b.line) || (a.line == b.line && a.col <= b.col)

||| A span is well-formed when both endpoints are 1-based and start <= end.
export
spanWellFormed : SourceSpan -> Bool
spanWellFormed sp =
  sp.start.line >= 1 && sp.start.col >= 1 &&
  sp.end.line   >= 1 && sp.end.col   >= 1 &&
  posLeq sp.start sp.end

-- ============================================================================
-- Decidable base-validity (SPEC Section 7.2 / 7.6)
-- ============================================================================

||| All REQUIRED fields present and well-formed. canonical_node, agent, and
||| profile_decl are OPTIONAL and do not affect base validity.
export
baseValidB : BaseRecord -> Bool
baseValidB r =
  nonEmpty r.id.raw &&
  spanWellFormed r.sourceSpan &&
  isSha256 r.hash &&
  nonEmpty r.provenance.author &&
  nonEmpty r.provenance.tool &&
  nonEmpty r.timestamp &&
  nonEmpty r.artefactRef

||| Base validity as a proposition.
public export
BaseValid : BaseRecord -> Type
BaseValid r = baseValidB r = True

||| Decidable base-validity. Total, via DecEq on Bool.
export
baseValidDec : (r : BaseRecord) -> Dec (BaseValid r)
baseValidDec r = decEq (baseValidB r) True

||| SHOULD-level advisory (SPEC Section 7.5): when kind = Ai, agent SHOULD be
||| present. This is not a hard-validity failure; surface it as a warning.
export
aiAgentAdvisory : BaseRecord -> Bool
aiAgentAdvisory r =
  case r.provenance.kind of
    Ai => case r.provenance.agent of
            Just _  => True
            Nothing => False
    _  => True

-- ============================================================================
-- Structured diagnostics
-- ============================================================================

public export
data BaseRecordIssue
  = EmptyId
  | MalformedSpan
  | BadHash String
  | EmptyAuthor
  | EmptyTool
  | EmptyTimestamp
  | EmptyArtefactRef
  | AiMissingAgent      -- advisory (SHOULD)

||| Enumerate every issue with a base record (errors first, advisory last).
export
baseRecordIssues : BaseRecord -> List BaseRecordIssue
baseRecordIssues r =
  (if nonEmpty r.id.raw            then [] else [EmptyId]) ++
  (if spanWellFormed r.sourceSpan  then [] else [MalformedSpan]) ++
  (if isSha256 r.hash              then [] else [BadHash r.hash]) ++
  (if nonEmpty r.provenance.author then [] else [EmptyAuthor]) ++
  (if nonEmpty r.provenance.tool   then [] else [EmptyTool]) ++
  (if nonEmpty r.timestamp         then [] else [EmptyTimestamp]) ++
  (if nonEmpty r.artefactRef       then [] else [EmptyArtefactRef]) ++
  (if aiAgentAdvisory r            then [] else [AiMissingAgent])
