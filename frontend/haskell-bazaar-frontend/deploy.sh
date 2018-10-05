#!/bin/bash

# Compiling Clojurescript -> JS
lein clean
lein cljsbuild once prod

# Syncing to S3 with public read access
aws s3 \
  sync resources/public/ s3://haskell-bazaar/ \
  --exclude 'js/compiled/out/*' \
  --acl public-read
