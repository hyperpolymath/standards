#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Proves fill-placeholders.sh does NOT reproduce the damage it replaces.
#
# Case 1 is the important one: it is the exact corruption found across 90
# repositories — the left-hand side of a `sed` substitution being filled in,
# which permanently breaks the script's ability to apply templates.
set -euo pipefail
S="$(cd "$(dirname "$0")/.." && pwd)/fill-placeholders.sh"
W="$(mktemp -d)"; trap 'rm -rf "$W"' EXIT
cd "$W"
printf '{"PROJECT_NAME":"Conative Gating","DATE":"2026-08-05","DEPS":"zig"}\n' > map.json

mkdir -p scripts templates
cat > scripts/apply-common-files.sh <<'EOF'
sed "s/{{PROJECT_NAME}}/$name/g" "$T/.editorconfig.template" > "$repo/.editorconfig"
sed -e "s/{{PROJECT_NAME}}/$name/g" -e "s/{{DATE}}/$DATE/g" "$T/x" > "$y"
sed -e "s|{{PROJECT_NAME}}|$project_name|g" "$T/z" > "$w"
EOF
printf '# {{PROJECT_NAME}} readme\nBuilt on {{DATE}}.\n' > README.md
printf 'Replace {{PROJECT_NAME}}, {{DEPS}} with actuals\n' > QUICKSTART.adoc
printf 'Outstanding tokens:\n- {{PROJECT_NAME}}\n' > REQUIRES_INITIALISATION.md
printf 'name = "{{PROJECT_NAME}}"\n' > templates/x.template
printf 'run *{{ARGS}}:\n    echo {{ARGS}}\n' > Justfile

"$S" . --map map.json --apply >/dev/null

pass=0; fail=0
ck(){ if eval "$2"; then pass=$((pass+1)); echo "  ok    $1"; else fail=$((fail+1)); echo "  FAIL  $1"; fi; }

ck "sed LHS placeholders survive (the 90-repo corruption)" \
   'grep -q "s/{{PROJECT_NAME}}/" scripts/apply-common-files.sh'
ck "sed LHS {{DATE}} survives" \
   'grep -q "s/{{DATE}}/" scripts/apply-common-files.sh'
ck "alternate sed delimiter | also protected" \
   'grep -q "s|{{PROJECT_NAME}}|" scripts/apply-common-files.sh'
ck "ordinary prose IS substituted" \
   'grep -q "Conative Gating readme" README.md'
ck "ordinary date IS substituted" \
   'grep -q "Built on 2026-08-05" README.md'
ck "QUICKSTART left alone (its subject is the token)" \
   'grep -q "Replace {{PROJECT_NAME}}, {{DEPS}}" QUICKSTART.adoc'
ck "REQUIRES_INITIALISATION left alone" \
   'grep -q -- "- {{PROJECT_NAME}}" REQUIRES_INITIALISATION.md'
ck "template source keeps its tokens" \
   'grep -q "{{PROJECT_NAME}}" templates/x.template'
ck "just's own {{ARGS}} never touched" \
   'grep -qc "{{ARGS}}" Justfile'

# --- slug slots: a value can be correct AND wrong depending on where it lands.
# These reproduce findings from boj-server-mk2, project-ovine and squeakwell,
# where the human display name was rendered into Guix channel names, BibTeX
# cite keys and URLs — all syntactically invalid.
W2="$(mktemp -d)"; cd "$W2"
printf '{"PROJECT_NAME":"BoJ Server Mk2"}\n' > map.json
printf "(name '{{PROJECT_NAME}})\nurl \"https://github.com/x/{{PROJECT_NAME}}\"\n@software{{{PROJECT_NAME}}_2026,\n" > guix.scm
printf 'The {{PROJECT_NAME}} project builds things.\n' > README.md
"$S" . --map map.json --apply >/dev/null 2>&1 || true

ck "display name REFUSED in a Guix (name ...) slot" \
   'grep -q "{{PROJECT_NAME}}" guix.scm'
ck "display name still substituted in ordinary prose" \
   'grep -q "The BoJ Server Mk2 project" README.md'

printf '{"PROJECT_NAME":"BoJ Server Mk2","PROJECT_SLUG":"boj-server-mk2"}\n' > map.json
printf "(name '{{PROJECT_NAME}})\nurl \"https://github.com/x/{{PROJECT_NAME}}\"\n" > guix.scm
"$S" . --map map.json --apply >/dev/null 2>&1

ck "with PROJECT_SLUG supplied, slug slots get the SLUG" \
   'grep -q "(name .boj-server-mk2)" guix.scm'
ck "and the URL gets the slug too" \
   'grep -q "github.com/x/boj-server-mk2" guix.scm'

# just's own interpolations must never be touched, mapped or not
printf 'build -t {{project}}:latest\nrun *{{ARGS}}:\n' > Justfile
"$S" . --map map.json --apply >/dev/null 2>&1
ck "just {{project}} interpolation survives" 'grep -q "{{project}}:latest" Justfile'

echo; echo "  ${pass} passed, ${fail} failed"
[ "$fail" = "0" ]
