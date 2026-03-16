# SPDX-License-Identifier: PMPL-1.0-or-later
# justfile for pandoc-a2ml

# Run all tests
test: test-reader test-writer

# Test the A2ML reader (Lua custom reader)
test-reader:
    @pandoc -f a2ml.lua sample.a2ml -t html | \
        diff expected.html - >/dev/null 2>&1 && \
        echo "Reader test passed" || \
        echo "Reader test FAILED"

# Test the A2ML writer (Lua custom writer)
test-writer:
    @pandoc -f a2ml.lua sample.a2ml -t a2ml-writer.lua | \
        diff expected.a2ml - >/dev/null 2>&1 && \
        echo "Writer test passed" || \
        echo "Writer test FAILED"
