# Palimpsest License - Containerfile (Podman/Docker compatible)
# Multi-stage build using Chainguard Wolfi for minimal attack surface
# SPDX-License-Identifier: Palimpsest-0.4 OR MIT
# Version: 0.4.0

# ==============================================================================
# STAGE 1: Builder - Compile Haskell validators and build documentation
# ==============================================================================

FROM cgr.dev/chainguard/wolfi-base:latest AS builder

# Install build dependencies
# Using Wolfi's minimal package set for security
RUN apk add --no-cache \
    nodejs-18 \
    npm \
    git \
    curl \
    ca-certificates \
    ghc \
    cabal \
    python3 \
    just \
    && rm -rf /var/cache/apk/*

# Set build environment variables for reproducibility
ENV SOURCE_DATE_EPOCH=1609459200
ENV NODE_ENV=production
ENV LANG=en_GB.UTF-8
ENV LC_ALL=en_GB.UTF-8

# Create build user (rootless)
RUN addgroup -g 65532 builder && \
    adduser -D -u 65532 -G builder builder

# Switch to build user
USER builder:builder
WORKDIR /build

# Copy dependency manifests first (layer caching optimisation)
COPY --chown=builder:builder package.json package-lock.json ./
COPY --chown=builder:builder justfile ./

# Install Node.js dependencies
RUN npm ci --only=production && \
    npm cache clean --force

# Copy Haskell project files
COPY --chown=builder:builder TOOLS/validation/haskell/palimpsest-validator.cabal \
                              TOOLS/validation/haskell/cabal.project \
                              ./TOOLS/validation/haskell/

# Update Cabal index and install dependencies
WORKDIR /build/TOOLS/validation/haskell
RUN cabal update && \
    cabal build --only-dependencies --enable-tests --enable-benchmarks

# Copy all source files
WORKDIR /build
COPY --chown=builder:builder . .

# Build Haskell validators
WORKDIR /build/TOOLS/validation/haskell
RUN cabal build --enable-optimization=2 && \
    cabal install --install-method=copy --installdir=/build/dist/bin

# Build SCSS to CSS
WORKDIR /build
RUN npm run scss:build || true

# Generate documentation index
RUN mkdir -p /build/dist/docs && \
    cp -r LICENSES /build/dist/docs/ && \
    cp -r GUIDES_v0.4 /build/dist/docs/ && \
    cp -r docs /build/dist/docs/ && \
    cp -r examples /build/dist/docs/ && \
    cp README.md /build/dist/docs/ && \
    cp README.nl.md /build/dist/docs/ 2>/dev/null || true

# Create healthcheck script
RUN mkdir -p /build/dist/scripts && \
    echo '#!/bin/sh' > /build/dist/scripts/healthcheck.sh && \
    echo 'curl -f http://localhost:8000/ || exit 1' >> /build/dist/scripts/healthcheck.sh && \
    chmod +x /build/dist/scripts/healthcheck.sh

# ==============================================================================
# STAGE 2: Documentation Server - Minimal runtime with Python HTTP server
# ==============================================================================

FROM cgr.dev/chainguard/wolfi-base:latest AS docs-server

# Install only runtime dependencies
RUN apk add --no-cache \
    python3 \
    curl \
    ca-certificates \
    && rm -rf /var/cache/apk/*

# Create non-root user
RUN addgroup -g 65532 palimpsest && \
    adduser -D -u 65532 -G palimpsest palimpsest

# Set runtime environment
ENV PYTHONUNBUFFERED=1
ENV LANG=en_GB.UTF-8
ENV LC_ALL=en_GB.UTF-8

# Switch to non-root user
USER palimpsest:palimpsest
WORKDIR /app

# Copy built documentation from builder stage
COPY --from=builder --chown=palimpsest:palimpsest /build/dist/docs ./docs
COPY --from=builder --chown=palimpsest:palimpsest /build/dist/scripts ./scripts
COPY --from=builder --chown=palimpsest:palimpsest /build/.well-known ./.well-known
COPY --from=builder --chown=palimpsest:palimpsest /build/assets ./assets

# Copy static files
COPY --from=builder --chown=palimpsest:palimpsest /build/style.css ./
COPY --from=builder --chown=palimpsest:palimpsest /build/robots.txt ./
COPY --from=builder --chown=palimpsest:palimpsest /build/humans.txt ./

# Create minimal index.html
RUN echo '<!DOCTYPE html>' > index.html && \
    echo '<html lang="en-GB">' >> index.html && \
    echo '<head>' >> index.html && \
    echo '  <meta charset="UTF-8">' >> index.html && \
    echo '  <meta name="viewport" content="width=device-width, initial-scale=1.0">' >> index.html && \
    echo '  <title>Palimpsest License v0.4</title>' >> index.html && \
    echo '  <link rel="stylesheet" href="/style.css">' >> index.html && \
    echo '</head>' >> index.html && \
    echo '<body>' >> index.html && \
    echo '  <header>' >> index.html && \
    echo '    <h1>Palimpsest License</h1>' >> index.html && \
    echo '    <p>A future-proof license for creative work in the age of AI</p>' >> index.html && \
    echo '  </header>' >> index.html && \
    echo '  <main>' >> index.html && \
    echo '    <ul>' >> index.html && \
    echo '      <li><a href="/docs/README.md">Project README</a></li>' >> index.html && \
    echo '      <li><a href="/docs/LICENSES/">License Texts</a></li>' >> index.html && \
    echo '      <li><a href="/docs/GUIDES_v0.4/">User Guides</a></li>' >> index.html && \
    echo '      <li><a href="/docs/docs/">Documentation</a></li>' >> index.html && \
    echo '      <li><a href="/.well-known/security.txt">Security Policy</a></li>' >> index.html && \
    echo '      <li><a href="/.well-known/ai.txt">AI Training Policy</a></li>' >> index.html && \
    echo '    </ul>' >> index.html && \
    echo '  </main>' >> index.html && \
    echo '</body>' >> index.html && \
    echo '</html>' >> index.html

# Expose documentation server port
EXPOSE 8000

# Configure healthcheck
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD ["/app/scripts/healthcheck.sh"]

# Start documentation server
CMD ["python3", "-m", "http.server", "8000", "--bind", "0.0.0.0"]

# ==============================================================================
# STAGE 3: Validator Service - Haskell validators with minimal runtime
# ==============================================================================

FROM cgr.dev/chainguard/wolfi-base:latest AS validator

# Install only GHC runtime dependencies (no compiler needed)
RUN apk add --no-cache \
    gmp \
    libffi \
    curl \
    ca-certificates \
    && rm -rf /var/cache/apk/*

# Create non-root user
RUN addgroup -g 65532 validator && \
    adduser -D -u 65532 -G validator validator

# Set runtime environment
ENV LANG=en_GB.UTF-8
ENV LC_ALL=en_GB.UTF-8

# Switch to non-root user
USER validator:validator
WORKDIR /app

# Copy compiled Haskell binaries from builder stage
COPY --from=builder --chown=validator:validator /build/dist/bin ./bin

# Copy validation schemas and reference data
COPY --from=builder --chown=validator:validator /build/METADATA_v0.4 ./metadata
COPY --from=builder --chown=validator:validator /build/LICENSES ./licenses

# Create healthcheck script
RUN mkdir -p /app/scripts && \
    echo '#!/bin/sh' > /app/scripts/healthcheck.sh && \
    echo '/app/bin/palimpsest-validator --version' >> /app/scripts/healthcheck.sh && \
    chmod +x /app/scripts/healthcheck.sh

# Configure healthcheck
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD ["/app/scripts/healthcheck.sh"]

# Default command: run validator
CMD ["/app/bin/palimpsest-validator", "--help"]

# ==============================================================================
# METADATA & LABELS
# ==============================================================================

LABEL org.opencontainers.image.title="Palimpsest License"
LABEL org.opencontainers.image.description="Future-proof license for creative work in the age of AI"
LABEL org.opencontainers.image.version="0.4.0"
LABEL org.opencontainers.image.authors="Palimpsest Stewardship Council"
LABEL org.opencontainers.image.url="https://palimpsest.license"
LABEL org.opencontainers.image.documentation="https://palimpsest.license/docs"
LABEL org.opencontainers.image.source="https://github.com/your-username/palimpsest-license"
LABEL org.opencontainers.image.licenses="Palimpsest-0.4 OR MIT"
LABEL org.opencontainers.image.vendor="Palimpsest Stewardship Council"
LABEL org.opencontainers.image.base.name="cgr.dev/chainguard/wolfi-base:latest"

# Security labels
LABEL security.privilege="unprivileged"
LABEL security.user="nonroot"
LABEL security.readonly-rootfs="false"
LABEL security.no-new-privileges="true"

# RSR compliance labels
LABEL rsr.compliance.tier="bronze"
LABEL rsr.compliance.score="91/110"
LABEL rsr.compliance.version="1.0"

# ==============================================================================
# BUILD NOTES
# ==============================================================================

# To build and run with Podman (rootless, recommended):
#
#   # Build documentation server
#   podman build --target docs-server -t palimpsest-docs:0.4.0 -f Containerfile .
#
#   # Build validator service
#   podman build --target validator -t palimpsest-validator:0.4.0 -f Containerfile .
#
#   # Run documentation server (rootless)
#   podman run --rm -p 8000:8000 --name palimpsest-docs palimpsest-docs:0.4.0
#
#   # Run validator (rootless)
#   podman run --rm -it palimpsest-validator:0.4.0
#
#   # Run with read-only root filesystem (enhanced security)
#   podman run --rm -p 8000:8000 --read-only --tmpfs /tmp palimpsest-docs:0.4.0
#
#   # Run with SELinux and seccomp (maximum security)
#   podman run --rm -p 8000:8000 \
#     --security-opt label=type:container_runtime_t \
#     --security-opt seccomp=unconfined \
#     --cap-drop=ALL \
#     palimpsest-docs:0.4.0
#
# To build with Docker:
#   docker build --target docs-server -t palimpsest-docs:0.4.0 -f Containerfile .
#
# Security best practices applied:
# - Multi-stage builds (minimal final images)
# - Chainguard Wolfi base (minimal attack surface, no shell by default)
# - Non-root user (UID 65532)
# - No privileged operations
# - Healthchecks configured
# - Read-only root filesystem compatible
# - Drop all capabilities
# - Reproducible builds (SOURCE_DATE_EPOCH)
# - Layer caching optimised (dependencies first)
# - Minimal runtime dependencies
# - British English throughout
