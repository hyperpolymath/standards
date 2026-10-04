# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# probe.awk — the FFP reference probe.
#
# WHAT THIS IS
#   An executable statement of the DETECTION rules, over the small, uncompressed
#   PDF syntax the conformance vectors are written in. It is the oracle the
#   vectors are checked against, and the thing a production detector (e.g.
#   presswerk's Rust implementation) is compared to.
#
# WHAT THIS IS NOT
#   A general PDF parser. It does not decode streams, follow object streams or
#   cross-reference streams, decrypt, or accept every legal spelling of every
#   construct. Its limits are named in spec/conformance/README.adoc. A
#   production detector MUST use a real PDF library; it MUST still produce this
#   line for every vector in vectors/.
#
# OUTPUT (one line, stable order)
#   classification=<c> form=<present|absent|unknown> filled=<n>/<total> appearances=<state> evidence=<codes>
#
#   FFP_PROBE_RECORD=1 additionally prints the DETECTION record as JSON.

{ buf = buf $0 "\n" }

END {
  n = length(buf)
  ok = 1
  if (buf !~ /%PDF-/) ok = 0
  if (ok) {
    index_objects()
    root = trailer_root()
    if (root == "" || !(root in OBJ)) ok = 0
  }
  if (!ok) { die_unreadable() } else {
  catalog = OBJ[root]

  decl = ""; tool = ""; toolver = ""

  # --- Step 2: declared marker -------------------------------------------
  meta = dget(catalog, "Metadata")
  if (meta != "") {
    payload = stream_payload(deref_text(meta))
    if (payload == "") {
      ev["FFP-E-XMP-UNREADABLE"] = 1
    } else if (payload ~ /form-fill-provenance\/1\.0/) {
      val = xmp_value(payload, "filledBy")
      if (val == "machine") { ev["FFP-E-DECL-MACHINE"] = 1; decl = "machine" }
      else { ev["FFP-E-DECL-UNRECOGNISED"] = 1; decl = val }
      tool = xmp_value(payload, "tool")
      toolver = xmp_value(payload, "toolVersion")
    }
  }

  # --- Step 3: fillable fields -------------------------------------------
  form = "absent"; total = 0; filled = 0; apmissing = 0
  acro = dget(catalog, "AcroForm")
  if (acro == "") {
    ev["FFP-E-NO-ACROFORM"] = 1
  } else {
    adict = deref_text(acro)
    if (adict == "") {
      ev["FFP-E-NO-ACROFORM"] = 1
    } else {
      if (dget(adict, "NeedAppearances") == "true") ev["FFP-E-NEED-APPEARANCES"] = 1
      fields = dget(adict, "Fields")
      ids = array_ids(fields)
      cnt = split_ids(ids, fids)
      for (i = 1; i <= cnt; i++) walk_field(fids[i], "", "", "", 0)
      if (total > 0) form = "present"
      if (total == 0) ev["FFP-E-NO-ACROFORM"] = 1
    }
  }

  # --- Steps 5/6: appearance state and classification ---------------------
  if (form == "absent" || filled == 0) {
    appr = "not-applicable"
    if (form == "present") ev["FFP-E-NO-VALUES"] = 1
  } else if (apmissing > 0) {
    appr = "incomplete"; ev["FFP-E-AP-INCOMPLETE"] = 1
  } else {
    appr = "generated"
  }

  if (form == "absent") cls = "no-form"
  else if (filled == 0) cls = "blank-form"
  else if (decl == "machine") cls = "machine-filled"
  else if (("FFP-E-NEED-APPEARANCES" in ev) && ("FFP-E-AP-INCOMPLETE" in ev)) cls = "machine-filled-suspected"
  else cls = "filled-unknown"

  printf "classification=%s form=%s filled=%d/%d appearances=%s evidence=%s\n", \
    cls, form, filled, total, appr, evcodes()
  if (ENVIRON["FFP_PROBE_RECORD"] == "1")
    print record_json(cls, form, appr, decl, tool, toolver)
  }
}

# ---------------------------------------------------------------------------
# Object index and reference resolution
# ---------------------------------------------------------------------------
function index_objects(   pos, m, s, e, id, body, rest) {
  pos = 1
  while (pos <= n) {
    rest = substr(buf, pos)
    if (!match(rest, /[0-9]+[ \t\r\n]+0[ \t\r\n]+obj/)) break
    m = pos + RSTART - 1
    id = substr(rest, RSTART); sub(/[ \t\r\n].*$/, "", id)
    s = m + RLENGTH
    e = index(substr(buf, s), "endobj")
    if (e == 0) e = n - s + 1
    body = substr(buf, s, e - 1)
    gsub(/^[ \t\r\n]+/, "", body); gsub(/[ \t\r\n]+$/, "", body)
    OBJ[id] = body
    pos = s + e + 5
  }
}

function trailer_root(   s) {
  if (!match(buf, /\/Root[ \t\r\n]+[0-9]+/)) return ""
  s = substr(buf, RSTART, RLENGTH)
  sub(/^\/Root[ \t\r\n]+/, "", s)
  return s
}

function deref_text(v,   parts, id) {
  v = trim(v)
  if (v ~ /^[0-9]+[ \t\r\n]+0[ \t\r\n]+R$/) {
    split(v, parts, /[ \t\r\n]+/)
    id = parts[1]
    return (id in OBJ) ? OBJ[id] : ""
  }
  return v
}

function stream_payload(t,   s, e) {
  if (t !~ /stream/) return ""
  s = index(t, "stream") + 6
  if (substr(t, s, 1) == "\r") s++
  if (substr(t, s, 1) == "\n") s++
  e = index(substr(t, s), "endstream")
  if (e == 0) return ""
  return substr(t, s, e - 1)
}

# ---------------------------------------------------------------------------
# Minimal object grammar
# ---------------------------------------------------------------------------
function trim(s) { gsub(/^[ \t\r\n]+/, "", s); gsub(/[ \t\r\n]+$/, "", s); return s }

function dget(dict, key,   pos, rest, start, after, prev, nxt, tok) {
  pos = 1
  while (pos <= length(dict)) {
    rest = substr(dict, pos)
    tok = "/" key
    if (!match(rest, tok)) return ""
    start = pos + RSTART - 1
    after = start + 1 + length(key)
    prev = (start > 1) ? substr(dict, start - 1, 1) : " "
    nxt = substr(dict, after, 1)
    if (prev !~ /[A-Za-z0-9]/ && (nxt == "" || nxt ~ /[ \t\r\n\/\[\]<>\(\)]/))
      return value_at(dict, after)
    pos = after
  }
  return ""
}

function value_at(text, pos,   c, c2, endp, tok, p2, e2, t2, p3, e3) {
  while (pos <= length(text) && substr(text, pos, 1) ~ /[ \t\r\n]/) pos++
  if (pos > length(text)) return ""
  c = substr(text, pos, 1); c2 = substr(text, pos + 1, 1)
  if (c == "<" && c2 == "<") { endp = scan_balanced(text, pos); return trim(substr(text, pos, endp - pos)) }
  if (c == "[")              { endp = scan_balanced(text, pos); return trim(substr(text, pos, endp - pos)) }
  if (c == "(")              { endp = scan_string(text, pos);   return trim(substr(text, pos, endp - pos)) }
  if (c == "<") { endp = index(substr(text, pos + 1), ">"); if (endp == 0) return ""; return trim(substr(text, pos, endp + 1)) }
  if (c == "/") {                       # a name: "/" is part of the token
    endp = pos + 1
    while (endp <= length(text) && substr(text, endp, 1) !~ /[ \t\r\n\/\[\]<>\(\)]/) endp++
    return trim(substr(text, pos, endp - pos))
  }

  endp = pos
  while (endp <= length(text) && substr(text, endp, 1) !~ /[ \t\r\n\/\[\]<>\(\)]/) endp++
  tok = substr(text, pos, endp - pos)
  if (tok ~ /^[0-9]+$/) {
    p2 = endp
    while (p2 <= length(text) && substr(text, p2, 1) ~ /[ \t\r\n]/) p2++
    e2 = p2
    while (e2 <= length(text) && substr(text, e2, 1) !~ /[ \t\r\n\/\[\]<>\(\)]/) e2++
    t2 = substr(text, p2, e2 - p2)
    if (t2 == "0") {
      p3 = e2
      while (p3 <= length(text) && substr(text, p3, 1) ~ /[ \t\r\n]/) p3++
      e3 = p3
      while (e3 <= length(text) && substr(text, e3, 1) !~ /[ \t\r\n\/\[\]<>\(\)]/) e3++
      if (substr(text, p3, e3 - p3) == "R") tok = substr(text, pos, e3 - pos)
    }
  }
  return trim(tok)
}

function scan_balanced(text, p,   i, ch, stack, top, e) {
  i = p; stack = ""
  while (i <= length(text)) {
    ch = substr(text, i, 1)
    if (ch == "(") { i = scan_string(text, i); continue }
    if (ch == "<" && substr(text, i + 1, 1) == "<") { stack = stack "d"; i += 2; continue }
    if (ch == ">" && substr(text, i + 1, 1) == ">") {
      top = substr(stack, length(stack), 1)
      if (top == "d") stack = substr(stack, 1, length(stack) - 1)
      i += 2
      if (stack == "") return i
      continue
    }
    if (ch == "[") { stack = stack "a"; i++; continue }
    if (ch == "]") {
      top = substr(stack, length(stack), 1)
      if (top == "a") stack = substr(stack, 1, length(stack) - 1)
      i++
      if (stack == "") return i
      continue
    }
    if (ch == "<") { e = index(substr(text, i + 1), ">"); i = (e == 0) ? length(text) + 1 : i + e + 1; continue }
    i++
  }
  return length(text) + 1
}

function scan_string(text, p,   i, ch, depth) {
  i = p; depth = 0
  while (i <= length(text)) {
    ch = substr(text, i, 1)
    if (ch == "\\") { i += 2; continue }
    if (ch == "(") depth++
    if (ch == ")") { depth--; if (depth == 0) return i + 1 }
    i++
  }
  return length(text) + 1
}

# array_ids: "[5 0 R 6 0 R]" -> "5 6"
function array_ids(v,   s, out, tok, num, k) {
  s = trim(v)
  if (substr(s, 1, 1) != "[") return ""
  s = trim(substr(s, 2, length(s) - 2))
  out = ""
  while (s != "") {
    tok = value_at(s, 1)
    if (tok == "") break
    num = tok
    sub(/[ \t\r\n].*$/, "", num)          # keep the object number only
    out = (out == "") ? num : out " " num
    k = index(s, tok)
    if (k == 0) break
    s = trim(substr(s, k + length(tok)))
  }
  return out
}

function split_ids(ids, arr,   cnt) {
  cnt = split(ids, arr, " ")
  return cnt
}

# ---------------------------------------------------------------------------
# Field walking
# ---------------------------------------------------------------------------
function walk_field(id, inh_ft, inh_ff, inh_v, depth,   f, ft, ff, v, kids, kidsids, kcnt, kparts, kf, first, i) {
  if (depth > 16) return
  if (!(id in OBJ)) return
  f = OBJ[id]

  ft = dget(f, "FT"); if (ft == "") ft = inh_ft
  ff = dget(f, "Ff"); if (ff == "") ff = inh_ff
  v  = dget(f, "V");  if (v  == "") v  = inh_v
  kids = dget(f, "Kids")

  if (kids == "") {
    register_field(f, id, "", ft, ff, v)
    return
  }

  kidsids = array_ids(kids)
  kcnt = split_ids(kidsids, kparts)
  first = (kcnt >= 1) ? kparts[1] : ""
  kf = (first in OBJ) ? OBJ[first] : ""
  if (dget(kf, "Subtype") == "/Widget") {
    register_field(f, id, kidsids, ft, ff, v)
  } else {
    for (i = 1; i <= kcnt; i++) walk_field(kparts[i], ft, ff, v, depth + 1)
  }
}

# register_field <field-text> <field-id> <widget-ids-or-empty> <ft> <ff> <v>
function register_field(f, fid, wids, ft, ff, v,   fnum, wcnt, wparts, i, wid, w, ap, nn, stream, state, substream) {
  if (ft !~ /^\/(Tx|Ch|Btn)$/) return
  fnum = substr(ff, 2) + 0
  if (ft == "/Btn" && int(fnum / 65536) % 2 == 1) return

  total++
  if (!is_meaningful(ft, v)) return
  filled++

  if (wids == "") wids = fid
  wcnt = split_ids(wids, wparts)
  for (i = 1; i <= wcnt; i++) {
    wid = wparts[i]
    if (!(wid in OBJ)) { apmissing++; continue }
    w = OBJ[wid]
    ap = dget(w, "AP")
    if (ap == "") { apmissing++; continue }
    nn = dget(ap, "N")
    if (nn == "") { apmissing++; continue }
    stream = deref_text(nn)
    if (stream ~ /\/Length/ || stream ~ /\/Subtype[ \t\r\n]+\/Form/) continue
    state = dget(w, "AS")
    if (state == "") state = dget(f, "AS")
    if (state == "" && substr(trim(v), 1, 1) == "/") state = trim(v)
    if (state == "") { apmissing++; continue }
    substream = dget(stream, substr(state, 2))
    if (substream == "") apmissing++
  }
}

function is_meaningful(ft, v,   name) {
  v = trim(v)
  if (v == "") return 0
  if (ft == "/Btn") {
    name = trim(v)
    return (name != "/Off" && substr(name, 1, 1) == "/")
  }
  if (substr(v, 1, 1) == "[") return array_any_meaningful(v)
  return string_meaningful(v)
}

# array_any_meaningful: true iff at least one element of [..] is meaningful.
function array_any_meaningful(v,   s, tok, k, n2) {
  s = trim(v)
  if (substr(s, 1, 1) != "[") return 0
  s = trim(substr(s, 2, length(s) - 2))
  while (s != "") {
    tok = value_at(s, 1)
    if (tok == "") return 0
    if (string_meaningful(tok)) return 1
    k = index(s, tok)
    if (k == 0) return 0
    s = trim(substr(s, k + length(tok)))
  }
  return 0
}

function string_meaningful(v,   s) {
  v = trim(v)
  if (v == "") return 0
  if (substr(v, 1, 1) == "(") {
    s = substr(v, 2, length(v) - 2)
    gsub(/\\[()\\]/, "", s)
    gsub(/[ \t\r\n]/, "", s)
    return (length(s) > 0)
  }
  if (substr(v, 1, 1) == "<") {
    s = substr(v, 2, length(v) - 2)
    gsub(/[ \t\r\n0]/, "", s)
    return (length(s) > 0)
  }
  return 0
}

# ---------------------------------------------------------------------------
# XMP
# ---------------------------------------------------------------------------
function xmp_value(payload, prop,   s) {
  if (match(payload, prop "[ \t\r\n]*=[ \t\r\n]*\"[^\"]*\"")) {
    s = substr(payload, RSTART, RLENGTH)
    sub(/^[^"]*"/, "", s)
    sub(/"$/, "", s)
    return s
  }
  if (match(payload, "<[A-Za-z0-9_]*:" prop ">[^<]*<")) {
    s = substr(payload, RSTART, RLENGTH)
    sub(/^<[^>]*>/, "", s)
    sub(/<.*$/, "", s)
    return s
  }
  return ""
}

# ---------------------------------------------------------------------------
# Output
# ---------------------------------------------------------------------------
function evcodes(   k, i, j, cnt, keys, out) {
  cnt = 0
  for (k in ev) keys[cnt++] = k
  for (i = 1; i < cnt; i++) {
    k = keys[i]; j = i - 1
    while (j >= 0 && keys[j] > k) { keys[j + 1] = keys[j]; j-- }
    keys[j + 1] = k
  }
  out = ""
  for (i = 0; i < cnt; i++) out = (out == "") ? keys[i] : out "," keys[i]
  return out
}

function record_json(cls, form, appr, decl, tool, toolver,   codes, cnt, i, arr, out, dv) {
  if (decl == "") dv = "null"
  else dv = sprintf("{\"filledBy\":\"%s\",\"tool\":%s,\"toolVersion\":%s}", decl,
                    (tool == "" ? "null" : "\"" tool "\""),
                    (toolver == "" ? "null" : "\"" toolver "\""))
  codes = evcodes()
  cnt = split(codes, arr, ",")
  out = ""
  for (i = 1; i <= cnt; i++) {
    if (arr[i] == "") continue
    out = (out == "") ? "\"" arr[i] "\"" : out ",\"" arr[i] "\""
  }
  return sprintf("{\"ffp\":\"1.0\",\"classification\":\"%s\",\"form\":\"%s\",\"filled_fields\":%d,\"total_fields\":%d,\"appearances\":\"%s\",\"declared\":%s,\"evidence\":[%s]}",
                 cls, form, filled, total, appr, dv, out)
}

function die_unreadable() {
  printf "classification=unreadable form=unknown filled=0/0 appearances=unknown evidence=FFP-E-UNREADABLE\n"
  if (ENVIRON["FFP_PROBE_RECORD"] == "1")
    print "{\"ffp\":\"1.0\",\"classification\":\"unreadable\",\"form\":\"unknown\",\"filled_fields\":0,\"total_fields\":0,\"appearances\":\"unknown\",\"declared\":null,\"evidence\":[\"FFP-E-UNREADABLE\"]}"
}
