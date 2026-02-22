#!/usr/bin/env bash
set -euo pipefail

DDRAIG_REPO="/var/mnt/eclipse/repos/ddraig-ssg"

if [ ! -d "$DDRAIG_REPO" ]; then
  echo "ddraig-ssg repo not found at $DDRAIG_REPO" >&2
  exit 1
fi

mkdir -p build
TMP_SRC="build/ddraig-src"
mkdir -p "$TMP_SRC"
cp -f "$DDRAIG_REPO/src/Ddraig.idr" "$TMP_SRC/"
cp -f prototype/ddraig/DemoBuild.idr "$TMP_SRC/"
idris2 --codegen node --source-dir "$TMP_SRC" "$TMP_SRC/DemoBuild.idr" -o build/ddraig-demo.js
OUT_JS="build/exec/build/ddraig-demo.js"
if [ ! -f "$OUT_JS" ]; then
  echo "Expected output not found: $OUT_JS" >&2
  exit 1
fi
cp -f "$OUT_JS" build/ddraig-demo.js
node build/ddraig-demo.js
