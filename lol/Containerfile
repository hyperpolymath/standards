# SPDX-License-Identifier: MIT AND LicenseRef-Palimpsest-0.8
# SPDX-FileCopyrightText: 2024-2025 Ehsaneddin Asgari and Contributors
#
# 1000Langs Containerfile (Podman-native)
# =======================================
# OCI-compliant container image for the parallel corpus crawler

# Build stage
FROM docker.io/library/node:20-alpine AS builder

LABEL org.opencontainers.image.title="1000Langs"
LABEL org.opencontainers.image.description="Super-parallel corpus crawler for multilingual NLP research"
LABEL org.opencontainers.image.source="https://github.com/Hyperpolymath/1000Langs"
LABEL org.opencontainers.image.licenses="MIT AND LicenseRef-Palimpsest-0.8"
LABEL org.opencontainers.image.vendor="Hyperpolymath"

WORKDIR /build

# Install build dependencies
RUN apk add --no-cache \
    python3 \
    make \
    g++ \
    git

# Copy package files
COPY package.json package-lock.json* rescript.json ./

# Install dependencies
RUN npm ci --ignore-scripts

# Copy source files
COPY src/ ./src/
COPY test/ ./test/

# Build ReScript
RUN npm run build

# Prune dev dependencies
RUN npm prune --production

# ============================================================================
# Production stage
# ============================================================================
FROM docker.io/library/node:20-alpine AS production

LABEL org.opencontainers.image.title="1000Langs"
LABEL org.opencontainers.image.description="Super-parallel corpus crawler for multilingual NLP research"
LABEL org.opencontainers.image.source="https://github.com/Hyperpolymath/1000Langs"
LABEL org.opencontainers.image.licenses="MIT AND LicenseRef-Palimpsest-0.8"
LABEL org.opencontainers.image.vendor="Hyperpolymath"

# Security: Create non-root user
RUN addgroup -g 1000 -S langs && \
    adduser -u 1000 -S langs -G langs

WORKDIR /app

# Copy built artifacts and production dependencies
COPY --from=builder --chown=langs:langs /build/node_modules ./node_modules
COPY --from=builder --chown=langs:langs /build/src/*.mjs ./dist/
COPY --from=builder --chown=langs:langs /build/src/**/*.mjs ./dist/

# Copy static assets
COPY --chown=langs:langs meta/ ./meta/
COPY --chown=langs:langs config/ ./config/

# Create data directories
RUN mkdir -p /app/data /app/output /app/logs && \
    chown -R langs:langs /app

# Security hardening
USER langs

# Environment
ENV NODE_ENV=production
ENV DATA_DIR=/app/data
ENV OUTPUT_DIR=/app/output
ENV LOG_DIR=/app/logs

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD node -e "console.log('healthy')" || exit 1

# Volumes for persistent data
VOLUME ["/app/data", "/app/output", "/app/logs"]

# Entry point
ENTRYPOINT ["node", "dist/Lang1000.res.mjs"]
CMD ["--help"]
