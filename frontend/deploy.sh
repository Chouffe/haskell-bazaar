#!/bin/bash

# Config
TAG=`git rev-parse --short HEAD`
BUILD_DIR=build/
ARTIFACT_NAME=$TAG.tar.gz
S3_BUCKET=haskell-bazaar-builds/frontend/

mkdir -p $BUILD_DIR

echo "Compressing artifacts"
tar \
  --exclude 'js/compiled/out/*/' \
  -cvzf $BUILD_DIR/$ARTIFACT_NAME \
  resources/public/

# Saving artifacts to S3
echo "Saving Artifacts"
aws s3 cp \
  $BUILD_DIR/$ARTIFACT_NAME \
  s3://$S3_BUCKET

echo "Cleaning up local artifacts"
rm $BUILD_DIR/$ARTIFACT_NAME

# Syncing to S3 with public read access
echo "Deploying"
aws s3 \
  sync resources/public/ s3://haskell-bazaar/ \
  --exclude 'js/compiled/out/*' \
  --acl public-read
