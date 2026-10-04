#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# make-fixtures.sh — deterministically build the FFP conformance vectors.
#
# The fixtures are deliberately minimal, uncompressed, ASCII-only PDFs with a
# classic cross-reference table, so that (a) they are readable in a terminal and
# in review, and (b) the reference probe (a small awk reader, not a real PDF
# parser) can classify them without a PDF library.
#
# Usage:
#   bash make-fixtures.sh            write vectors/*.pdf
#   bash make-fixtures.sh --check    rebuild into a temp dir and diff against the
#                                    committed vectors; non-zero on drift
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VECTORS="$HERE/vectors"
MODE="write"
[ "${1:-}" = "--check" ] && MODE="check"

OUTDIR="$VECTORS"
TMP=""
if [ "$MODE" = "check" ]; then
  TMP="$(mktemp -d)"; OUTDIR="$TMP/vectors"; mkdir -p "$OUTDIR"
  trap 'rm -rf "$TMP"' EXIT
else
  mkdir -p "$OUTDIR"
fi

# ---------------------------------------------------------------------------
# Object assembly. Objects are written sequentially; offsets are the byte
# position of each object header, which is what the xref table records.
# ---------------------------------------------------------------------------
declare -A OFF
MAXID=0
CUR=""

new_doc() {
  CUR="$1"
  OFF=(); MAXID=0
  printf '%%PDF-1.7\n' > "$CUR"
}

obj() { # obj <id> <body>
  local id="$1" body="$2"
  OFF[$id]=$(wc -c < "$CUR" | tr -d ' ')
  printf '%s 0 obj\n%s\nendobj\n' "$id" "$body" >> "$CUR"
  [ "$id" -gt "$MAXID" ] && MAXID="$id"
}

stream_obj() { # stream_obj <id> <dict-without-Length> <content>
  local id="$1" dict="$2" content="$3"
  local len=${#content}
  obj "$id" "$dict /Length $len >>
stream
$content
endstream"
}

finish_doc() { # finish_doc <root-id>
  local root="$1" i
  local xref_off
  xref_off=$(wc -c < "$CUR" | tr -d ' ')
  {
    printf 'xref\n0 %d\n' $((MAXID + 1))
    printf '0000000000 65535 f \n'
    for ((i = 1; i <= MAXID; i++)); do
      if [ -n "${OFF[$i]:-}" ]; then
        printf '%010d 00000 n \n' "${OFF[$i]}"
      else
        printf '0000000000 65535 f \n'
      fi
    done
    printf 'trailer\n<< /Size %d /Root %d 0 R >>\nstartxref\n%s\n%%%%EOF\n' \
      $((MAXID + 1)) "$root" "$xref_off"
  } >> "$CUR"
}

# ---------------------------------------------------------------------------
# Shared fragments.
# ---------------------------------------------------------------------------
PAGE="<< /Type /Page /Parent 2 0 R /MediaBox [0 0 595 842] /Resources << /Font << /Helv 8 0 R >> >>"
FONT='<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>'
FORM_STREAM='BT /Helv 10 Tf 0 0 Td (Jewell) Tj ET'

# field_tx <name> <value-or-empty>  (value appears as /V only when non-empty)
field_tx() {
  local name="$1" value="$2"
  local body="<< /Type /Annot /Subtype /Widget /FT /Tx /T ($name) /Rect [50 700 300 730] /P 3 0 R /DA (/Helv 10 Tf 0 g)"
  [ -n "$value" ] && body="$body /V ($value)"
  printf '%s >>' "$body"
}

# field_tx_ap <name> <value> <appearance-stream-object-number>
# As field_tx, but with a normal appearance. /AP is a key *inside* the widget
# dictionary: an indirect object holds exactly one object, so appending a second
# dictionary after the widget's closing `>>` would be malformed PDF. The
# reference probe tolerates that spelling because it matches text, but a real
# parser (and therefore any conforming product detector) cannot.
field_tx_ap() {
  local name="$1" value="$2" ap="$3"
  local body; body="$(field_tx "$name" "$value")"
  printf '%s /AP << /N %s 0 R >> >>' "${body% >>}" "$ap"
}

# xmp <filledBy-value> <tool> <appearancesGenerated>
# filledBy-value may be empty (namespace present, no property).
xmp_packet() {
  local filled_by="$1" tool="$2" apgen="$3"
  local props=""
  [ -n "$filled_by" ] && props="$props ffp:filledBy=\"$filled_by\""
  [ -n "$tool" ] && props="$props ffp:tool=\"$tool\""
  [ -n "$apgen" ] && props="$props ffp:appearancesGenerated=\"$apgen\""
  printf '%s' "<?xpacket begin=\"\" id=\"W5M0MpCehiHzreSzNTczkc9d\"?>
<x:xmpmeta xmlns:x=\"adobe:ns:meta/\">
  <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">
    <rdf:Description rdf:about=\"\" xmlns:ffp=\"https://hyperpolymath.dev/ns/form-fill-provenance/1.0/\"$props/>
  </rdf:RDF>
</x:xmpmeta>
<?xpacket end=\"w\"?>"
}

# ---------------------------------------------------------------------------
# The vectors. Each builder writes one fixture; add a row to VECTORS below.
# ---------------------------------------------------------------------------
v_no_form() {
  obj 1 '<< /Type /Catalog /Pages 2 0 R >>'
  obj 2 '<< /Type /Pages /Kids [3 0 R] /Count 1 >>'
  obj 3 "$PAGE >>"
  obj 8 "$FONT"
  finish_doc 1
}

v_blank_form() {
  obj 1 '<< /Type /Catalog /Pages 2 0 R /AcroForm 4 0 R >>'
  obj 2 '<< /Type /Pages /Kids [3 0 R] /Count 1 >>'
  obj 3 "$PAGE /Annots [5 0 R 6 0 R] >>"
  obj 4 '<< /Fields [5 0 R 6 0 R] /DA (/Helv 0 Tf 0 g) >>'
  obj 5 "$(field_tx surname '')"
  obj 6 "$(field_tx given '')"
  obj 8 "$FONT"
  finish_doc 1
}

v_blank_form_need_appearances() {
  obj 1 '<< /Type /Catalog /Pages 2 0 R /AcroForm 4 0 R >>'
  obj 2 '<< /Type /Pages /Kids [3 0 R] /Count 1 >>'
  obj 3 "$PAGE /Annots [5 0 R 6 0 R] >>"
  obj 4 '<< /Fields [5 0 R 6 0 R] /NeedAppearances true /DA (/Helv 0 Tf 0 g) >>'
  obj 5 "$(field_tx surname '')"
  obj 6 "$(field_tx given '')"
  obj 8 "$FONT"
  finish_doc 1
}

v_blank_form_empty_values() {
  obj 1 '<< /Type /Catalog /Pages 2 0 R /AcroForm 4 0 R >>'
  obj 2 '<< /Type /Pages /Kids [3 0 R] /Count 1 >>'
  obj 3 "$PAGE /Annots [5 0 R 6 0 R] >>"
  obj 4 '<< /Fields [5 0 R 6 0 R] /DA (/Helv 0 Tf 0 g) >>'
  obj 5 '<< /Type /Annot /Subtype /Widget /FT /Tx /T (surname) /Rect [50 700 300 730] /P 3 0 R /V () >>'
  obj 6 '<< /Type /Annot /Subtype /Widget /FT /Tx /T (given) /Rect [50 650 300 680] /P 3 0 R /V (   ) >>'
  obj 8 "$FONT"
  finish_doc 1
}

v_machine_filled_suspected() {
  obj 1 '<< /Type /Catalog /Pages 2 0 R /AcroForm 4 0 R >>'
  obj 2 '<< /Type /Pages /Kids [3 0 R] /Count 1 >>'
  obj 3 "$PAGE /Annots [5 0 R 6 0 R] >>"
  obj 4 '<< /Fields [5 0 R 6 0 R] /NeedAppearances true /DA (/Helv 0 Tf 0 g) >>'
  obj 5 "$(field_tx surname Jewell)"
  obj 6 "$(field_tx given Jonathan)"
  obj 8 "$FONT"
  finish_doc 1
}

v_machine_filled_declared() {
  local packet; packet="$(xmp_packet machine blocky-writer false)"
  obj 1 '<< /Type /Catalog /Pages 2 0 R /AcroForm 4 0 R /Metadata 9 0 R >>'
  obj 2 '<< /Type /Pages /Kids [3 0 R] /Count 1 >>'
  obj 3 "$PAGE /Annots [5 0 R 6 0 R] >>"
  obj 4 '<< /Fields [5 0 R 6 0 R] /NeedAppearances true /DA (/Helv 0 Tf 0 g) >>'
  obj 5 "$(field_tx surname Jewell)"
  obj 6 "$(field_tx given Jonathan)"
  obj 8 "$FONT"
  stream_obj 9 '<< /Type /Metadata /Subtype /XML' "$packet"
  finish_doc 1
}

v_machine_filled_declared_generated() {
  local packet; packet="$(xmp_packet machine blocky-writer true)"
  obj 1 '<< /Type /Catalog /Pages 2 0 R /AcroForm 4 0 R /Metadata 9 0 R >>'
  obj 2 '<< /Type /Pages /Kids [3 0 R] /Count 1 >>'
  obj 3 "$PAGE /Annots [5 0 R 6 0 R] >>"
  obj 4 '<< /Fields [5 0 R 6 0 R] /DA (/Helv 0 Tf 0 g) >>'
  obj 5 "$(field_tx_ap surname Jewell 10)"
  obj 6 "$(field_tx_ap given Jonathan 11)"
  obj 8 "$FONT"
  stream_obj 9 '<< /Type /Metadata /Subtype /XML' "$packet"
  stream_obj 10 '<< /Type /XObject /Subtype /Form /BBox [0 0 250 30]' "$FORM_STREAM"
  stream_obj 11 '<< /Type /XObject /Subtype /Form /BBox [0 0 250 30]' "$FORM_STREAM"
  finish_doc 1
}

v_machine_filled_suspected_partial() {
  obj 1 '<< /Type /Catalog /Pages 2 0 R /AcroForm 4 0 R >>'
  obj 2 '<< /Type /Pages /Kids [3 0 R] /Count 1 >>'
  obj 3 "$PAGE /Annots [5 0 R 6 0 R] >>"
  obj 4 '<< /Fields [5 0 R 6 0 R] /NeedAppearances true /DA (/Helv 0 Tf 0 g) >>'
  obj 5 "$(field_tx surname Jewell)"
  obj 6 "$(field_tx given '')"
  obj 8 "$FONT"
  finish_doc 1
}

v_machine_filled_suspected_mixed_ap() {
  obj 1 '<< /Type /Catalog /Pages 2 0 R /AcroForm 4 0 R >>'
  obj 2 '<< /Type /Pages /Kids [3 0 R] /Count 1 >>'
  obj 3 "$PAGE /Annots [5 0 R 6 0 R] >>"
  obj 4 '<< /Fields [5 0 R 6 0 R] /NeedAppearances true /DA (/Helv 0 Tf 0 g) >>'
  obj 5 "$(field_tx_ap surname Jewell 10)"
  obj 6 "$(field_tx given Jonathan)"
  obj 8 "$FONT"
  stream_obj 10 '<< /Type /XObject /Subtype /Form /BBox [0 0 250 30]' "$FORM_STREAM"
  finish_doc 1
}

v_viewer_filled() {
  obj 1 '<< /Type /Catalog /Pages 2 0 R /AcroForm 4 0 R >>'
  obj 2 '<< /Type /Pages /Kids [3 0 R] /Count 1 >>'
  obj 3 "$PAGE /Annots [5 0 R 6 0 R] >>"
  obj 4 '<< /Fields [5 0 R 6 0 R] /DA (/Helv 0 Tf 0 g) >>'
  obj 5 "$(field_tx_ap surname Jewell 10)"
  obj 6 "$(field_tx_ap given Jonathan 11)"
  obj 8 "$FONT"
  stream_obj 10 '<< /Type /XObject /Subtype /Form /BBox [0 0 250 30]' "$FORM_STREAM"
  stream_obj 11 '<< /Type /XObject /Subtype /Form /BBox [0 0 250 30]' "$FORM_STREAM"
  finish_doc 1
}

v_filled_with_ap_need_appearances() {
  obj 1 '<< /Type /Catalog /Pages 2 0 R /AcroForm 4 0 R >>'
  obj 2 '<< /Type /Pages /Kids [3 0 R] /Count 1 >>'
  obj 3 "$PAGE /Annots [5 0 R 6 0 R] >>"
  obj 4 '<< /Fields [5 0 R 6 0 R] /NeedAppearances true /DA (/Helv 0 Tf 0 g) >>'
  obj 5 "$(field_tx_ap surname Jewell 10)"
  obj 6 "$(field_tx_ap given Jonathan 11)"
  obj 8 "$FONT"
  stream_obj 10 '<< /Type /XObject /Subtype /Form /BBox [0 0 250 30]' "$FORM_STREAM"
  stream_obj 11 '<< /Type /XObject /Subtype /Form /BBox [0 0 250 30]' "$FORM_STREAM"
  finish_doc 1
}

v_declared_unknown_value() {
  local packet; packet="$(xmp_packet robot blocky-writer false)"
  obj 1 '<< /Type /Catalog /Pages 2 0 R /AcroForm 4 0 R /Metadata 9 0 R >>'
  obj 2 '<< /Type /Pages /Kids [3 0 R] /Count 1 >>'
  obj 3 "$PAGE /Annots [5 0 R 6 0 R] >>"
  obj 4 '<< /Fields [5 0 R 6 0 R] /NeedAppearances true /DA (/Helv 0 Tf 0 g) >>'
  obj 5 "$(field_tx surname Jewell)"
  obj 6 "$(field_tx given Jonathan)"
  obj 8 "$FONT"
  stream_obj 9 '<< /Type /Metadata /Subtype /XML' "$packet"
  finish_doc 1
}

v_button_machine_filled() {
  obj 1 '<< /Type /Catalog /Pages 2 0 R /AcroForm 4 0 R >>'
  obj 2 '<< /Type /Pages /Kids [3 0 R] /Count 1 >>'
  obj 3 "$PAGE /Annots [6 0 R 7 0 R 9 0 R] >>"
  obj 4 '<< /Fields [5 0 R 9 0 R] /NeedAppearances true /DA (/Helv 0 Tf 0 g) >>'
  # 5 is a button field with two widget kids; /FT and /V live on the parent,
  # and both kids carry the state appearances. 9 is an empty text field.
  obj 5 '<< /FT /Btn /Ff 32768 /T (sex) /V /Yes /Kids [6 0 R 7 0 R] >>'
  obj 6 '<< /Type /Annot /Subtype /Widget /Parent 5 0 R /Rect [50 600 70 620] /P 3 0 R /AS /Yes /AP << /N << /Off 10 0 R /Yes 11 0 R >> >> >>'
  obj 7 '<< /Type /Annot /Subtype /Widget /Parent 5 0 R /Rect [80 600 100 620] /P 3 0 R /AS /Off /AP << /N << /Off 10 0 R /Yes 11 0 R >> >> >>'
  obj 8 "$FONT"
  obj 9 "$(field_tx surname '')"
  stream_obj 10 '<< /Type /XObject /Subtype /Form /BBox [0 0 20 20]' 'q Q'
  stream_obj 11 '<< /Type /XObject /Subtype /Form /BBox [0 0 20 20]' 'q Q'
  finish_doc 1
}

v_unreadable() {
  CUR="$1"; MAXID=0; OFF=()
  printf '%%PDF-1.7\nthis is not a structurally valid PDF: no xref, no trailer\n' > "$CUR"
}

# name → builder function
declare -A BUILDERS=(
  [no-form]=v_no_form
  [blank-form]=v_blank_form
  [blank-form-need-appearances]=v_blank_form_need_appearances
  [blank-form-empty-values]=v_blank_form_empty_values
  [machine-filled-suspected]=v_machine_filled_suspected
  [machine-filled-declared]=v_machine_filled_declared
  [machine-filled-declared-generated]=v_machine_filled_declared_generated
  [machine-filled-suspected-partial]=v_machine_filled_suspected_partial
  [machine-filled-suspected-mixed-ap]=v_machine_filled_suspected_mixed_ap
  [viewer-filled]=v_viewer_filled
  [filled-with-ap-need-appearances]=v_filled_with_ap_need_appearances
  [declared-unknown-value]=v_declared_unknown_value
  [button-machine-filled]=v_button_machine_filled
  [unreadable]=v_unreadable
)

for name in "${!BUILDERS[@]}"; do
  new_doc "$OUTDIR/$name.pdf"
  "${BUILDERS[$name]}" "$OUTDIR/$name.pdf"
done

if [ "$MODE" = "check" ]; then
  if diff -rq "$VECTORS" "$OUTDIR" >/dev/null 2>&1 \
     || diff -rq "$VECTORS" "$OUTDIR"; then
    echo "make-fixtures --check: committed vectors match the generator"
    exit 0
  fi
  echo "make-fixtures --check: DRIFT — regenerate with: bash make-fixtures.sh" >&2
  exit 1
fi

echo "wrote $(ls "$OUTDIR" | wc -l | tr -d ' ') fixtures to ${OUTDIR#"$HERE"/}"
