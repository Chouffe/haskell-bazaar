# Multi Stage Docker file that reuses the build-env image used for building haskell code


# From the build-env docker image
FROM build-env as builder
# Copying source code from host to the docker image
COPY . /opt/build/
RUN stack build --system-ghc

# From a small base image
FROM ubuntu:18.10

# Installing postgresql-client and cleaning up after the install
RUN apt-get update \
 && apt-get install -y postgresql-client \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN mkdir -p /opt/app
WORKDIR /opt/app

# Copying binary from previous stage
# TODO: use a variable instead of hardcoding the path?
COPY --from=builder /opt/build/.stack-work/install/x86_64-linux/lts-9.21/8.0.2/bin/ .

# Copying static files and config files
COPY static /opt/myapp/static
COPY config /opt/myapp/config

# Running the command by default
CMD ["/opt/app/haskell-bazaar-exe"]
