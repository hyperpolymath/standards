.PHONY: test

test: test-yaml test-nickel

test-yaml:
	@pandoc -f k9.lua sample.k9 -t html >/dev/null 2>&1 && \
		echo "YAML reader test passed" || \
		echo "YAML reader test FAILED"

test-nickel:
	@pandoc -f k9.lua sample.k9.ncl -t html >/dev/null 2>&1 && \
		echo "Nickel reader test passed" || \
		echo "Nickel reader test FAILED"
