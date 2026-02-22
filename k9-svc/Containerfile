# SPDX-License-Identifier: AGPL-3.0-or-later
# Containerfile - K9 SVC Runtime Container
#
# Build:  podman build -t k9-svc:latest .
# Run:    podman run --rm -it k9-svc:latest status
# Mount:  podman run --rm -it -v ./components:/k9/components k9-svc:latest validate-all

FROM docker.io/library/rust:slim-bookworm AS builder

# Install build dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Install Nickel
RUN cargo install nickel-lang-cli --locked

# Install Just
RUN cargo install just --locked

# ─────────────────────────────────────────────────────────────
# Runtime Stage
# ─────────────────────────────────────────────────────────────
FROM docker.io/library/debian:bookworm-slim

LABEL org.opencontainers.image.title="K9 SVC Runtime"
LABEL org.opencontainers.image.description="Self-Validating Component runtime environment"
LABEL org.opencontainers.image.version="1.0.0-alpha"
LABEL org.opencontainers.image.vendor="hyperpolymath"
LABEL org.opencontainers.image.licenses="AGPL-3.0-or-later"
LABEL org.opencontainers.image.source="https://github.com/hyperpolymath/k9-svc"

# Install runtime dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    file \
    && rm -rf /var/lib/apt/lists/*

# Copy binaries from builder
COPY --from=builder /usr/local/cargo/bin/nickel /usr/local/bin/nickel
COPY --from=builder /usr/local/cargo/bin/just /usr/local/bin/just

# Create non-root user for security
RUN useradd -m -s /bin/sh k9user
USER k9user
WORKDIR /home/k9user/k9

# Copy K9 SVC files
COPY --chown=k9user:k9user must justfile pedigree.ncl register.ncl leash.ncl ./
COPY --chown=k9user:k9user mime/ ./mime/
COPY --chown=k9user:k9user examples/ ./examples/

# Verify the triad is functional
RUN ./must status && \
    nickel typecheck pedigree.ncl && \
    nickel typecheck register.ncl && \
    nickel typecheck leash.ncl

# Default: show status
ENTRYPOINT ["./must"]
CMD ["status"]

# Volume for mounting external components
VOLUME ["/home/k9user/k9/components"]

# Expose nothing by default (K9 is not a server)
# Components can override if they need ports
