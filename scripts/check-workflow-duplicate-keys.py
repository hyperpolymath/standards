#!/usr/bin/env python3
# SPDX-License-Identifier: MPL-2.0
"""Reject GitHub Actions workflows containing duplicate YAML keys.

WHY THIS EXISTS AS A SEPARATE CHECK
-----------------------------------
GitHub Actions rejects a workflow with duplicate keys outright. The run is
recorded as `failure` with NO jobs, NO log and NO check run — a red mark on
the board with nothing behind it to read, and `gh pr checks` shows no row at
all.

Nothing else in the toolchain can see this, because `yaml.safe_load` SILENTLY
KEEPS THE LAST duplicate and reports success. The file "parses". Every
ordinary validation — linters, formatters, our own sweep scripts — is
structurally blind to it.

Measured 2026-08-05: nine workflows in `hypatia` were in this state, including
a CodeQL workflow with 18 failures, 12 startup_failures and ZERO successes in
its lifetime. The repository had never once been scanned by its own scanner.

USAGE
-----
    check-workflow-duplicate-keys.py [PATH ...]      # default: .github/workflows

Exit 0 when clean, 1 when any duplicate is found.
"""
import glob
import os
import sys

import yaml


class StrictLoader(yaml.SafeLoader):
    """A SafeLoader that refuses duplicate mapping keys instead of silently
    keeping the last one."""


def _no_duplicates(loader, node, deep=False):
    mapping = {}
    dupes = []
    for key_node, value_node in node.value:
        key = loader.construct_object(key_node, deep=deep)
        if key in mapping:
            dupes.append((key, key_node.start_mark.line + 1))
        mapping[key] = loader.construct_object(value_node, deep=deep)
    if dupes:
        detail = ", ".join(f"{k!r} (line {ln})" for k, ln in dupes)
        raise yaml.YAMLError(f"duplicate key(s): {detail}")
    return mapping


StrictLoader.add_constructor(
    yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, _no_duplicates
)


def check(path):
    """Return a problem string, or None when the file is fine."""
    try:
        with open(path, encoding="utf-8") as fh:
            yaml.load(fh, StrictLoader)
    except yaml.YAMLError as exc:
        return str(exc).replace("\n", " ")[:160]
    except OSError as exc:
        return f"unreadable: {exc}"
    return None


def main(argv):
    targets = argv[1:] or [".github/workflows"]
    files = []
    for t in targets:
        if os.path.isdir(t):
            for ext in ("yml", "yaml"):
                files.extend(sorted(glob.glob(os.path.join(t, f"*.{ext}"))))
        else:
            files.append(t)

    failed = 0
    for f in files:
        problem = check(f)
        if problem:
            print(f"::error file={f}::{problem}")
            print(f"FAIL {f}: {problem}")
            failed += 1

    if failed:
        print(f"\n{failed} of {len(files)} workflow file(s) contain duplicate keys.")
        print("GitHub Actions rejects these before any job is created — they")
        print("fail with no log and no check run. yaml.safe_load does NOT")
        print("catch this; it keeps the last duplicate and reports success.")
        return 1

    print(f"duplicate-key check: {len(files)} workflow file(s) clean")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
