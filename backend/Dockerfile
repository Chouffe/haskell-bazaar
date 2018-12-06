# Multi Stage Docker file that reuses the build-env image used for building haskell code
FROM fpco/stack-build:lts-9.21 as builder

RUN mkdir -p /opt/build
WORKDIR /opt/build

COPY stack.yaml stack.yaml
COPY package.yaml package.yaml

# Install GHC
RUN stack setup

# Build dependencies
RUN stack build --only-dependencies && \
    stack build --test --only-dependencies

# Copy over all source code
COPY . .

RUN stack build

# From a small base image
# TODO try with an alpine image instead
FROM ubuntu:18.10

# Installing postgresql-client and cleaning up after the install
RUN apt-get update \
 && apt-get install -y postgresql-client curl \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN mkdir -p /opt/app

WORKDIR /opt/app

# Copying binary from previous stage
# TODO: use a variable instead of hardcoding the path?
COPY --from=builder \
  /opt/build/.stack-work/install/x86_64-linux/lts-9.21/8.0.2/bin/haskell-bazaar-exe \
  haskell-bazaar-exe

# Copying static / config files
COPY static static
COPY config config

EXPOSE 8001

# Running the command by default
CMD ["/opt/app/haskell-bazaar-exe"]
