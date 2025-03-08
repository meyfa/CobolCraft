FROM docker.io/library/ubuntu:oracular AS base

WORKDIR /app


FROM base AS build

# Install build dependencies
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y \
        gcc g++ make gnucobol3 zlib1g-dev curl openjdk-21-jre-headless && \
    rm -rf /var/lib/apt/lists/*

# Perform data extraction first to allow Docker to cache this layer
COPY Makefile .
RUN make data

# Copy source files and build
COPY cpp ./cpp
COPY codegen ./codegen
COPY src ./src
COPY main.cob .
RUN make -j $(nproc) GCVERSION=32


FROM base AS deploy

# Install runtime dependencies
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y \
        libcob4 zlib1g tini && \
    rm -rf /var/lib/apt/lists/*

# Copy the build results
COPY --from=build /app/cobolcraft .
COPY --from=build /app/data/generated/reports/*.json ./data/generated/reports/
COPY --from=build /app/data/generated/data ./data/generated/data

# Run the server within Tini (to handle signals properly)
ENTRYPOINT ["/usr/bin/tini", "--"]
CMD ["/app/cobolcraft"]
