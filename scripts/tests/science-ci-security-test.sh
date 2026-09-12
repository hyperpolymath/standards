#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
set -euo pipefail
ruby "$(dirname "$0")/science-ci-security-test.rb"
