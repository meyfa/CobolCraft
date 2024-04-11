# --- Build stage ---
FROM debian:bookworm-slim AS build

# Install packages required for building
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && \
    apt-get install -y gcc g++ make gnucobol curl default-jre-headless && \
    rm -rf /var/lib/apt/lists/*

# Copy source files
COPY Makefile .
COPY main.cob .
COPY src ./src
COPY cpp ./cpp
COPY CBL_GC_SOCKET ./CBL_GC_SOCKET
COPY blobs ./blobs

# Build
RUN make

# --- Runtime stage ---
FROM debian:bookworm-slim

# Install runtime packages
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && \
    apt-get install -y gnucobol tini && \
    rm -rf /var/lib/apt/lists/*

# Copy the build results
COPY --from=build Makefile .
COPY --from=build cobolcraft .
COPY --from=build *.so .
COPY --from=build blobs ./blobs
COPY --from=build data/generated/reports/*.json ./data/generated/reports/

# Include runtime dependencies
ENV COB_PRE_LOAD=CBL_GC_SOCKET:COBOLCRAFT_UTIL

# Run the server within Tini (to handle signals properly)
ENTRYPOINT ["/usr/bin/tini", "--"]
CMD ["./cobolcraft"]
