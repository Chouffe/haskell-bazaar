#!/bin/bash

BRANCH=`git rev-parse --abbrev-ref HEAD`
GIT_SHA=`git rev-parse HEAD`

# Compiling Clojurescript -> JS
echo "Building artifacts"
lein clean
lein cljsbuild once prod

echo "Saving artifacts"
# TODO: package the build with Branch and GIT_SHA in a S3 Bucket

# Syncing to S3 with public read access
echo "Deploying"
aws s3 \
  sync resources/public/ s3://haskell-bazaar/ \
  --exclude 'js/compiled/out/*' \
  --acl public-read
