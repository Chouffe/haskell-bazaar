GIT_SHA := $$(git rev-parse HEAD)

# Docker Tagging
NAME    := chouffe/haskell-bazaar
TAG     := $$(git rev-parse --short HEAD)
IMG     := ${NAME}:${TAG}
LATEST  := ${NAME}:latest


login:
	@docker login -u ${DOCKER_USER} -p ${DOCKER_PASSWORD}

build-dev:
	@docker image build \
		-t haskell-bazaar-dev:latest \
		-f Dockerfile.dev \
		.

build:
	@docker image build -t ${IMG} .
	@docker image tag ${IMG} ${LATEST}

dev:
	@docker-compose up -d

test:
	@docker container exec haskell-bazaar-api stack test

push:
	@docker push ${NAME}

deploy:
	ssh \
		-i ${SSH_PEM_FILE} ${SSH_USER}@${SSH_HOST} \
		GITLAB_ACCESS_TOKEN=${GITLAB_ACCESS_TOKEN} \
		DEPLOY_GIT_SHA=${GIT_SHA} \
		DOCKER_NAME=${NAME} \
		DOCKER_IMG=${IMG} \
		DOCKER_LATEST=${LATEST} \
		'bash -s' < scripts/deploy.sh

# Local develpment commands
psql:
	@docker container exec -it \
		haskell-bazaar-postgres psql -U haskellbazaar

repl:
	@docker container exec -it \
		haskell-bazaar-api stack repl

ghcid:
	@docker container exec \
		haskell-bazaar-api stack exec -- ghcid --command "stack ghci"

lint:
	@docker container exec \
		haskell-bazaar-api stack exec -- hlint .

db-seed:
	docker exec \
		-i haskell-bazaar-postgres psql \
		-U haskellbazaar \
		< scripts/db/seed.sql


.PHONY: build \
	build-dev \
	test \
	deploy \
	lint \
	ghcid \
        psql \
	db-seed
