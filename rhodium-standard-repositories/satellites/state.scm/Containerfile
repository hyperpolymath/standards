# SPDX-License-Identifier: MIT AND LicenseRef-Palimpsest-0.8
# Container image for STATE development and usage
#
# Build: podman build -t state:latest .
# Run:   podman run -it --rm state:latest
# Dev:   podman run -it --rm -v $(pwd):/workspace:Z state:latest

FROM docker.io/library/debian:bookworm-slim

LABEL org.opencontainers.image.title="STATE"
LABEL org.opencontainers.image.description="Stateful Context Tracking Engine for AI Conversation Continuity"
LABEL org.opencontainers.image.version="2.0.0"
LABEL org.opencontainers.image.licenses="MIT AND LicenseRef-Palimpsest-0.8"
LABEL org.opencontainers.image.source="https://github.com/Hyperpolymath/STATE.scm"

# Install Guile and dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    guile-3.0 \
    guile-3.0-dev \
    graphviz \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -m -s /bin/bash state
USER state
WORKDIR /home/state

# Copy library files
COPY --chown=state:state lib/ ./lib/
COPY --chown=state:state STATE.scm ./
COPY --chown=state:state examples/ ./examples/

# Set up Guile load path
ENV GUILE_LOAD_PATH="/home/state/lib:$GUILE_LOAD_PATH"

# Default command: start REPL with STATE loaded
CMD ["guile", "-L", "lib", "-l", "lib/state.scm"]
