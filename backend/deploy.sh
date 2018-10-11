#/bin/bash

# One needs to run the script with the following env variables:
#  - $GITLAB_ACCESS_TOKEN: access token for cloning the repo
#  - $DEPLOY_GIT_SHA: git sha you wish to deploy

# on the machine that you wish to deploy code on

REPO_NAME=haskell-bazaar
S3_BUCKET=haskell-bazaar-builds/backend

# Remove the previous code
echo "Cloning git repository"
cd ~
rm -rf $REPO_NAME
git clone https://oauth2:$GITLAB_ACCESS_TOKEN@gitlab.com/chouffe/haskell-bazaar.git $REPO_NAME

cd $REPO_NAME
git checkout $DEPLOY_GIT_SHA
DEPLOY_BRANCH=`git rev-parse --abbrev-ref HEAD`

# Getting the name of the artifact to deploy
DEPLOY_DOCKER_ARTIFACT_NAME=docker-image.haskell-bazaar-server.$DEPLOY_BRANCH.$DEPLOY_GIT_SHA.tar.gz
echo "Name of the artifact: $DEPLOY_DOCKER_ARTIFACT_NAME"

echo "Downloading build artifact from S3"
aws s3 cp s3://$S3_BUCKET/$DEPLOY_DOCKER_ARTIFACT_NAME .

echo "Loading docker image"
docker-load < $DEPLOY_DOCKER_ARTIFACT_NAME

echo "Bringing down the stack"
docker-compose -f docker-compose.yml -f production.yml down

echo "Bringing up the stack"
docker-compose -f docker-compose.yml -f production.yml up -d

echo "Done"
