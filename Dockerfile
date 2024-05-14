# --- Build stage ---
# Need to use ubuntu instead of debian to get a recent Java version
FROM ubuntu:jammy AS build

# Install packages required for building
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && \
    apt-get install -y gcc g++ make gnucobol zlib1g-dev curl openjdk-21-jre-headless && \
    rm -rf /var/lib/apt/lists/*

# Perform data extraction first to allow Docker to cache this layer
COPY Makefile .
RUN make data

# Copy source files and build
COPY main.cob .
COPY src ./src
COPY cpp ./cpp
COPY CBL_GC_SOCKET ./CBL_GC_SOCKET
COPY blobs ./blobs
RUN make -j $(nproc)

# --- Runtime stage ---
FROM ubuntu:jammy

# Install runtime packages
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && \
    apt-get install -y gnucobol zlib1g tini && \
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
