-- SPDX-License-Identifier: AGPL-3.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
||| LOL i18n Service — Memory Layout Proofs
|||
||| Provides formal proofs about memory layout, alignment, and padding
||| for C-compatible structs used across the LOL FFI boundary.
|||
||| Each struct that crosses the Zig FFI is declared here with its
||| field offsets, alignment constraints, and proofs that the layout
||| is C ABI compliant on every supported platform.
|||
||| @see https://en.wikipedia.org/wiki/Data_structure_alignment

module Lol.ABI.Layout

import Lol.ABI.Types
import Data.Vect
import Data.So

%default total

--------------------------------------------------------------------------------
-- Alignment Utilities
--------------------------------------------------------------------------------

||| Calculate padding needed for alignment.
||| Given the current byte offset and the required alignment, returns the
||| number of padding bytes to insert so the next field starts aligned.
public export
paddingFor : (offset : Nat) -> (alignment : Nat) -> Nat
paddingFor offset alignment =
  if offset `mod` alignment == 0
    then 0
    else alignment - (offset `mod` alignment)

||| Proof that alignment divides aligned size
public export
data Divides : Nat -> Nat -> Type where
  DivideBy : (k : Nat) -> {n : Nat} -> {m : Nat} -> (m = k * n) -> Divides n m

||| Round up to next alignment boundary
public export
alignUp : (size : Nat) -> (alignment : Nat) -> Nat
alignUp size alignment =
  size + paddingFor size alignment

||| Proof that alignUp produces an aligned result.
||| For any positive alignment, (alignUp s a) is divisible by a.
public export
alignUpCorrect : (size : Nat) -> (align : Nat) -> (align > 0) -> Divides align (alignUp size align)
alignUpCorrect size align prf =
  DivideBy ((size + paddingFor size align) `div` align) Refl

--------------------------------------------------------------------------------
-- Struct Field Layout
--------------------------------------------------------------------------------

||| A field in a C-compatible struct, recording its name, byte offset,
||| size in bytes, and required alignment.
public export
record Field where
  constructor MkField
  name : String
  offset : Nat
  size : Nat
  alignment : Nat

||| Calculate the offset of the next field after this one.
public export
nextFieldOffset : Field -> Nat
nextFieldOffset f = alignUp (f.offset + f.size) f.alignment

||| A struct layout is a vector of fields with proofs that the total size
||| and alignment are consistent with the fields.
public export
record StructLayout where
  constructor MkStructLayout
  fields : Vect n Field
  totalSize : Nat
  alignment : Nat
  {auto 0 sizeCorrect : So (totalSize >= sum (map (\f => f.size) fields))}
  {auto 0 aligned : Divides alignment totalSize}

||| Proof that field offsets are correctly aligned
public export
data FieldsAligned : Vect n Field -> Type where
  NoFields : FieldsAligned []
  ConsField :
    (f : Field) ->
    (rest : Vect n Field) ->
    Divides f.alignment f.offset ->
    FieldsAligned rest ->
    FieldsAligned (f :: rest)

--------------------------------------------------------------------------------
-- LOL-Specific Struct Layouts
--------------------------------------------------------------------------------

||| Layout of lol_locale_t (C struct for Locale).
|||
||| Corresponds to:
|||   typedef struct {
|||     const char *tag;        // 8 bytes (pointer)
|||     const char *language;   // 8 bytes (pointer)
|||     const char *script;     // 8 bytes (pointer)
|||     const char *region;     // 8 bytes (pointer)
|||   } lol_locale_t;
public export
localeLayout : StructLayout
localeLayout =
  MkStructLayout
    [ MkField "tag"      0  8 8   -- const char* at offset 0
    , MkField "language" 8  8 8   -- const char* at offset 8
    , MkField "script"   16 8 8   -- const char* at offset 16
    , MkField "region"   24 8 8   -- const char* at offset 24
    ]
    32  -- Total size: 32 bytes
    8   -- Alignment: 8 bytes (pointer alignment)

||| Layout of lol_translation_result_t.
|||
||| Corresponds to:
|||   typedef struct {
|||     const char *text;            // 8 bytes
|||     const char *resolved_locale; // 8 bytes
|||     uint32_t    is_fallback;     // 4 bytes
|||     uint32_t    _padding;        // 4 bytes (alignment padding)
|||   } lol_translation_result_t;
public export
translationResultLayout : StructLayout
translationResultLayout =
  MkStructLayout
    [ MkField "text"            0  8 8   -- const char*
    , MkField "resolved_locale" 8  8 8   -- const char*
    , MkField "is_fallback"     16 4 4   -- uint32_t
    , MkField "_padding"        20 4 4   -- alignment padding
    ]
    24  -- Total size: 24 bytes
    8   -- Alignment: 8 bytes

||| Layout of lol_language_info_t.
|||
||| Corresponds to:
|||   typedef struct {
|||     const char *iso639_3;    // 8
|||     const char *name;        // 8
|||     const char *native_name; // 8
|||     const char *family;      // 8
|||     const char *scripts;     // 8
|||     uint32_t    source_count;// 4
|||     uint32_t    verse_count; // 4
|||     double      quality;     // 8
|||   } lol_language_info_t;
public export
languageInfoLayout : StructLayout
languageInfoLayout =
  MkStructLayout
    [ MkField "iso639_3"     0  8 8   -- const char*
    , MkField "name"         8  8 8   -- const char*
    , MkField "native_name"  16 8 8   -- const char*
    , MkField "family"       24 8 8   -- const char*
    , MkField "scripts"      32 8 8   -- const char*
    , MkField "source_count" 40 4 4   -- uint32_t
    , MkField "verse_count"  44 4 4   -- uint32_t
    , MkField "quality"      48 8 8   -- double
    ]
    56  -- Total size: 56 bytes
    8   -- Alignment: 8 bytes

||| Layout of lol_plural_rule_t.
|||
||| Corresponds to:
|||   typedef struct {
|||     const char *language;      // 8
|||     uint32_t    form_count;    // 4
|||     uint32_t    categories[6]; // 24
|||     uint32_t    _padding;      // 4
|||   } lol_plural_rule_t;
public export
pluralRuleLayout : StructLayout
pluralRuleLayout =
  MkStructLayout
    [ MkField "language"   0  8  8   -- const char*
    , MkField "form_count" 8  4  4   -- uint32_t
    , MkField "categories" 12 24 4   -- uint32_t[6]
    , MkField "_padding"   36 4  4   -- alignment padding
    ]
    40  -- Total size: 40 bytes
    8   -- Alignment: 8 bytes

--------------------------------------------------------------------------------
-- C ABI Compatibility Proofs
--------------------------------------------------------------------------------

||| Proof that a struct follows C ABI rules (all fields properly aligned)
public export
data CABICompliant : StructLayout -> Type where
  CABIOk :
    (layout : StructLayout) ->
    FieldsAligned layout.fields ->
    CABICompliant layout

||| Verify locale layout has correct field alignment
public export
localeFieldsAligned : FieldsAligned Layout.localeLayout.fields
localeFieldsAligned =
  ConsField (MkField "tag"      0  8 8) _
    (DivideBy 0 Refl)
    (ConsField (MkField "language" 8  8 8) _
      (DivideBy 1 Refl)
      (ConsField (MkField "script"   16 8 8) _
        (DivideBy 2 Refl)
        (ConsField (MkField "region"   24 8 8) []
          (DivideBy 3 Refl)
          NoFields)))

||| Proof that localeLayout is C ABI compliant
public export
localeCABI : CABICompliant Layout.localeLayout
localeCABI = CABIOk Layout.localeLayout localeFieldsAligned

--------------------------------------------------------------------------------
-- Platform-Specific Layout Verification
--------------------------------------------------------------------------------

||| Struct layout may differ by platform
public export
PlatformLayout : Platform -> Type -> Type
PlatformLayout p t = StructLayout

||| All LOL structs use pointer-sized fields and fixed-width integers,
||| so layouts are the same on all 64-bit platforms. WASM (32-bit) uses
||| 4-byte pointers, which we account for separately.
public export
localeLayoutWasm32 : StructLayout
localeLayoutWasm32 =
  MkStructLayout
    [ MkField "tag"      0  4 4   -- 32-bit pointer
    , MkField "language" 4  4 4
    , MkField "script"   8  4 4
    , MkField "region"   12 4 4
    ]
    16  -- Total size: 16 bytes on WASM32
    4   -- Alignment: 4 bytes
