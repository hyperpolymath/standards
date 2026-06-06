#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# UX Test Orchestrator — runs a repo through all platform containers
#
# Usage: ./test-repo.sh /path/to/repo [platform...]
#   Platforms: fedora, ubuntu, alpine, debian (default: all)
#
# Output: JSON reports in /tmp/ux-test-results/<repo>/

set -euo pipefail

REPO="${1:?Usage: $0 /path/to/repo [fedora|ubuntu|alpine|debian]}"
REPO="$(realpath "$REPO")"
REPO_NAME="$(basename "$REPO")"
HARNESS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RESULTS_DIR="/tmp/ux-test-results/$REPO_NAME"

shift || true
PLATFORMS=("${@:-fedora ubuntu alpine debian}")
if [ ${#PLATFORMS[@]} -eq 0 ]; then
    PLATFORMS=(fedora ubuntu alpine debian)
fi

mkdir -p "$RESULTS_DIR"

echo "=== UX Test Orchestrator ==="
echo "Repo: $REPO_NAME ($REPO)"
echo "Platforms: ${PLATFORMS[*]}"
echo "Results: $RESULTS_DIR/"
echo ""

# Build container images (if not cached)
for platform in "${PLATFORMS[@]}"; do
    IMAGE="ux-test-$platform:latest"
    CONTAINERFILE="$HARNESS_DIR/Containerfile.$platform"

    if [ ! -f "$CONTAINERFILE" ]; then
        echo "SKIP: No Containerfile for $platform"
        continue
    fi

    echo "--- Building $IMAGE ---"
    podman build -q -t "$IMAGE" -f "$CONTAINERFILE" "$HARNESS_DIR" || {
        echo "FAIL: Could not build $IMAGE"
        continue
    }
done

echo ""

# Run tests on each platform
for platform in "${PLATFORMS[@]}"; do
    IMAGE="ux-test-$platform:latest"
    REPORT="$RESULTS_DIR/$platform.json"

    echo "--- Testing on $platform ---"
    podman run --rm \
        -v "$REPO:/repo:ro,Z" \
        "$IMAGE" > "$REPORT" 2>&1 || true

    # Extract summary from JSON if valid
    if python3 -c "import json; json.load(open('$REPORT'))" 2>/dev/null; then
        SUMMARY=$(python3 -c "
import json
r = json.load(open('$REPORT'))
s = r.get('summary', {})
print(f\"  pass={s.get('pass',0)} fail={s.get('fail',0)} warn={s.get('warn',0)}\")
" 2>/dev/null || echo "  (could not parse summary)")
        echo "$SUMMARY"
    else
        # Report might have mixed stderr/stdout — try to extract JSON
        if grep -q '"summary"' "$REPORT" 2>/dev/null; then
            # Extract just the JSON part (after last line starting with ===)
            sed -n '/^{/,/^}/p' "$REPORT" > "${REPORT}.clean" && mv "${REPORT}.clean" "$REPORT"
            echo "  (extracted JSON from mixed output)"
        else
            echo "  (raw output — no JSON summary)"
        fi
    fi
    echo ""
done

# Summary across all platforms
echo "=== Cross-Platform Summary ==="
echo ""

for platform in "${PLATFORMS[@]}"; do
    REPORT="$RESULTS_DIR/$platform.json"
    if [ -f "$REPORT" ]; then
        echo "$platform:"
        # Show failures only
        grep '"fail"' "$REPORT" 2>/dev/null | head -10 || echo "  (no failures or unparseable)"
    fi
done

echo ""
echo "Full reports: $RESULTS_DIR/"
echo ""
echo "To generate hypatia rules from failures:"
echo "  just generate-ux-rules $RESULTS_DIR"
