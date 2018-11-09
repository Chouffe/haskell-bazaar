build:
	stack build

run: build
	stack exec haskell-bazaar-exe

test:
	stack test

clean:
	rm -rf .stack-work

ghcid-devel:
	ghcid \
	   --command "stack ghci"

lint:
	stack exec -- hlint .

docker-build-env:
	docker build \
		-t build-env:latest \
		-f scripts/Dockerfile.build \
		.

docker-build:
	docker build \
		-t haskell-bazaar-server:latest \
		-f scripts/Dockerfile \
		.

docker-dev-run:
	docker-compose \
		-f docker-compose.yml \
		up

docker-prod-run:
	docker-compose \
		-f docker-compose.yml \
		-f production.yml \
		up

docker-build-save:
	scripts/build.sh

docker-deploy:
	ssh \
		-i ${SSH_PEM_FILE} \
		${SSH_USER}@${SSH_HOST} \
		GITLAB_ACCESS_TOKEN=${GITLAB_ACCESS_TOKEN} \
		DEPLOY_GIT_SHA="$(shell git rev-parse HEAD)" \
		'bash -s' < scripts/deploy.sh

db-seed:
	docker exec \
		-i haskell-bazaar-postgres psql \
		-U haskellbazaar \
		< scripts/db/seed.sql

.PHONY: build run test clean lint ghcid-devel docker-build-dev docker-build docker-prod-run docker-build-save docker-deploy source-env
