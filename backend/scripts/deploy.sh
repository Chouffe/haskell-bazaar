#!/bin/bash

DOCKER_STACK_CONFIG_FILENAME=docker_stack_config.yml
DOCKER_STACK_NAME=haskell-bazaar
GIT_REPO_NAME=haskell-bazaar

# TODO: clone only from git sha
cd ~
echo "\n\n"
echo "Removing former code source"
rm -rf $GIT_REPO_NAME

echo "\n\n"
# TODO: clone from github instead
echo "Cloning git repository"
git clone https://oauth2:$GITLAB_ACCESS_TOKEN@gitlab.com/chouffe/$GIT_REPO_NAME.git $GIT_REPO_NAME

cd $GIT_REPO_NAME
git checkout $DEPLOY_GIT_SHA

# Pulling docker image
echo "\n\n"
echo "Pulling docker image"
docker image pull $DOCKER_IMG

# Building docker stack config
# echo "\n\n"
# echo "Generating docker stack config file"
# docker-compose -f docker-compose.yml -f production.yml config > $DOCKER_STACK_CONFIG_FILENAME

echo "\n\n"
echo "Taging docker image to latest"
docker image tag $DOCKER_IMG $DOCKER_NAME:latest

echo "\n\n"
echo "Deploying stack"
docker stack deploy -c production.yml $DOCKER_STACK_NAME
