-- SPDX-License-Identifier: AGPL-3.0-or-later
||| A2ML v1.1 profile mechanism (SPEC.adoc Section 8).
|||
||| A profile is a NON-EXECUTABLE validation bundle. It may only raise the
||| validation bar (required sections, allowed/required directives, reference
||| classes, minimum attestation level); it can never grant capability or relax
||| a base invariant. This module gives the typed-core nodes for profile ids and
||| profile definitions, plus a *decidable* conformance check of a document's
||| profile-relevant facts against a profile definition.
module A2ML.Profiles

import A2ML.TypedCore
import A2ML.BaseVocab
import Decidable.Equality

%default total

-- ============================================================================
-- Modes (SPEC Section 4) and profile ids (SPEC Section 8.1)
-- ============================================================================

public export
data Mode = Lax | Checked | Attested

public export
modeLevel : Mode -> Nat
modeLevel Lax      = 0
modeLevel Checked  = 1
modeLevel Attested = 2

||| A QualifiedId of the form ns/name (e.g. a2ml/state).
public export
record ProfileId where
  constructor MkProfileId
  ns   : String
  name : String

public export
Eq ProfileId where
  (MkProfileId a b) == (MkProfileId c d) = a == c && b == d

export
renderProfileId : ProfileId -> String
renderProfileId p = p.ns ++ "/" ++ p.name

-- ============================================================================
-- The five constrainable axes (SPEC Section 8.2)
-- ============================================================================

||| Allowed-directive policy. "base" is the v1.0 core directive set (see
||| `baseDirs`); v1.1 additions are opted in via BasePlus or ClosedAllow.
public export
data DirectivePolicy
  = AllowAll                    -- no restriction
  | BasePlus    (List String)   -- base directives PLUS the named extras
  | ClosedAllow (List String)   -- EXACTLY the named directives
  | Deny        (List String)   -- base directives MINUS the named ones

||| A required directive with cardinality. maxCount = Nothing means unbounded.
public export
record RequiredDirective where
  constructor MkRequiredDirective
  directive : String
  minCount  : Nat
  maxCount  : Maybe Nat

public export
data RefTargetKind = TgtSection | TgtFigure | TgtTable | TgtAny

||| Constrain @ref targets by class (the QualifiedId namespace of the ref id).
public export
record ReferenceClass where
  constructor MkReferenceClass
  klass    : String          -- namespace prefix the class governs
  target   : RefTargetKind
  required : Bool            -- if True, at least one ref of this class MUST occur

||| A profile definition (SPEC Section 8.4). Self-describing: a definition is
||| itself base A2ML.
public export
record ProfileDef where
  constructor MkProfileDef
  pid                : ProfileId
  version            : String
  requiredSections   : List String
  directivePolicy    : DirectivePolicy
  requiredDirectives : List RequiredDirective
  referenceClasses   : List ReferenceClass
  minAttestation     : Mode

-- ============================================================================
-- Profile-relevant projection of a document
-- ============================================================================

||| The facts a profile checks against. A translator produces this from the
||| (v1.1) typed core: the new nodes (@record, @cite, @profile) are folded in via
||| `directives` / `profiles`, which the v1.0 `Doc` cannot itself express.
public export
record DocFacts where
  constructor MkDocFacts
  sectionTitles : List String     -- titles of every Section, recursively
  directives    : List String     -- directive-name occurrences (with repeats)
  refClasses    : List String     -- namespace prefixes of @ref ids used
  profiles      : List ProfileId   -- declared @profile ids
  mode          : Mode             -- validator mode in effect

||| The v1.0 core directive set. v1.1 additions (record, cite, rationale,
||| origin, source_span, canonical_node) are NOT base and must be opted in.
public export
baseDirs : List String
baseDirs =
  ["abstract", "refs", "fig", "table", "opaque", "requires", "ref", "profile"]

-- ============================================================================
-- Decidable conformance (SPEC Section 8)
-- ============================================================================

hasAll : List String -> List String -> Bool
hasAll required present = all (\x => elem x present) required

countOf : String -> List String -> Nat
countOf _ []        = 0
countOf x (y :: ys) = (if x == y then 1 else 0) + countOf x ys

satisfiesCardinality : RequiredDirective -> List String -> Bool
satisfiesCardinality rd ds =
  let n = countOf rd.directive ds in
  (n >= rd.minCount) &&
  (case rd.maxCount of
     Nothing => True
     Just m  => n <= m)

directiveAllowed : DirectivePolicy -> String -> Bool
directiveAllowed AllowAll            _ = True
directiveAllowed (BasePlus extra)    d = elem d baseDirs || elem d extra
directiveAllowed (ClosedAllow allow) d = elem d allow
directiveAllowed (Deny denied)       d = elem d baseDirs && not (elem d denied)

allDirectivesAllowed : DirectivePolicy -> List String -> Bool
allDirectivesAllowed pol ds = all (directiveAllowed pol) ds

refClassSatisfied : ReferenceClass -> List String -> Bool
refClassSatisfied rc used = not rc.required || elem rc.klass used

||| Boolean conformance of document facts to a profile definition.
export
conformsB : ProfileDef -> DocFacts -> Bool
conformsB pd f =
  hasAll pd.requiredSections f.sectionTitles &&
  allDirectivesAllowed pd.directivePolicy f.directives &&
  all (\rd => satisfiesCardinality rd f.directives) pd.requiredDirectives &&
  all (\rc => refClassSatisfied rc f.refClasses) pd.referenceClasses &&
  (modeLevel f.mode >= modeLevel pd.minAttestation)

||| Conformance as a proposition.
public export
Conforms : ProfileDef -> DocFacts -> Type
Conforms pd f = conformsB pd f = True

||| Decidable conformance. Total, via DecEq on Bool.
export
conformsDec : (pd : ProfileDef) -> (f : DocFacts) -> Dec (Conforms pd f)
conformsDec pd f = decEq (conformsB pd f) True

-- ============================================================================
-- Structured diagnostics
-- ============================================================================

public export
data ProfileViolation
  = MissingSection         String
  | DisallowedDirective    String
  | CardinalityViolation   String Nat   -- directive, actual count
  | MissingRequiredRefClass String
  | AttestationTooLow      Mode Mode    -- have, need

||| Enumerate every profile violation. Empty iff `conformsB` is True.
export
checkProfile : ProfileDef -> DocFacts -> List ProfileViolation
checkProfile pd f =
  let missSecs   = map MissingSection
                     (filter (\s => not (elem s f.sectionTitles))
                             pd.requiredSections)
      badDirs    = map DisallowedDirective
                     (filter (\d => not (directiveAllowed pd.directivePolicy d))
                             f.directives)
      cardIssues = map (\rd => CardinalityViolation rd.directive
                                                    (countOf rd.directive f.directives))
                     (filter (\rd => not (satisfiesCardinality rd f.directives))
                             pd.requiredDirectives)
      refIssues  = map (\rc => MissingRequiredRefClass rc.klass)
                     (filter (\rc => not (refClassSatisfied rc f.refClasses))
                             pd.referenceClasses)
      attIssue   = if modeLevel f.mode >= modeLevel pd.minAttestation
                     then []
                     else [AttestationTooLow f.mode pd.minAttestation]
  in missSecs ++ badDirs ++ cardIssues ++ refIssues ++ attIssue

-- ============================================================================
-- Fact extraction from the v1.0 core (section titles)
-- ============================================================================

||| Recursively collect every Section title. Marked partial for the same reason
||| as TypedCore.collectIds: the walk recurses through the section-body wrapper.
||| Decidability of conformance does not depend on this being total.
export
partial
sectionTitlesOf : Doc -> List String
sectionTitlesOf (MkDoc blocks) = concatMap go blocks
  where
    go : Block -> List String
    go (Section s) = s.title :: sectionTitlesOf (MkDoc s.body)
    go _           = []

-- ============================================================================
-- Composition: union of constraints (SPEC Section 8.1)
-- ============================================================================

||| A document conforms to a SET of active profiles iff it conforms to each.
||| (Profiles compose by taking the stricter of every overlapping axis; the
||| conjunction below realises that union.)
export
conformsAll : List ProfileDef -> DocFacts -> Bool
conformsAll pds f = all (\pd => conformsB pd f) pds
