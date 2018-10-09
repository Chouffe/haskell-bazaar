FROM build-env as builder
COPY . /opt/build/
RUN stack build --system-ghc

FROM fpco/stack-build:lts-9.21
RUN apt-get update && apt-get install -y
RUN mkdir -p /opt/app
WORKDIR /opt/app
# Copying from previous stage
COPY --from=builder /opt/build/.stack-work/install/x86_64-linux/lts-9.21/8.0.2/bin/ .
# COPY static /opt/myapp/static
# COPY config /opt/myapp/config
CMD ["/opt/app/haskell-bazaar-exe"]
