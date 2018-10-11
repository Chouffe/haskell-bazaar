#/bin/bash

# Config
BRANCH=`git rev-parse --abbrev-ref HEAD`
GIT_SHA=`git rev-parse HEAD`
BUILD_DIR=build/
ARTIFACT_NAME=docker-image.haskell-bazaar-server.$BRANCH.$GIT_SHA.tar.gz
S3_BUCKET=haskell-bazaar-builds/backend/

# Create the build directory
mkdir -p $BUILD_DIR

# Building a docker image
echo "Building docker image"
docker build \
  -t haskell-bazaar-server:latest \
  -f scripts/Dockerfile \
  .

echo "Persisting docker image to disk"
docker save haskell-bazaar-server:latest \
  | gzip \
  > $BUILD_DIR/$ARTIFACT_NAME

echo "Saving Artifacts"
aws s3 cp \
  $BUILD_DIR/$ARTIFACT_NAME \
  s3://$S3_BUCKET

echo "Cleaning up local Artifacts"
rm $BUILD_DIR/$ARTIFACT_NAME
