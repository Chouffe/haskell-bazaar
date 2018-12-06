# Use same lts version as stack.yaml
FROM fpco/stack-build:lts-9.21

RUN mkdir -p /opt/build
WORKDIR /opt/build

COPY stack.yaml stack.yaml
COPY package.yaml package.yaml

# Install GHC
RUN stack setup

# Build dependencies
RUN stack build --only-dependencies && \
    stack build --test --only-dependencies

# Build dev tools
RUN stack build ghcid hlint

# Copy over all source code
COPY . .

EXPOSE 8002

# Run repl as default command
CMD ["stack", "repl"]
