build-dev:
	docker build \
		-t haskell-bazaar-dev:latest \
		-f Dockerfile.dev \
		.

build:
	docker build \
		-t chouffe/haskell-bazaar:latest \
		-f Dockerfile \
		.

psql:
	docker container exec -it \
		haskell-bazaar-postgres psql -U haskellbazaar


test:
	docker container exec haskell-bazaar-api stack test

repl:
	docker container exec -it \
		haskell-bazaar-api stack repl

ghcid:
	docker container exec \
		haskell-bazaar-api stack exec -- ghcid --command "stack ghci"


lint:
	docker container exec \
		haskell-bazaar-api stack exec -- hlint .

docker-build-save:
	scripts/build.sh

docker-deploy:
	ssh \
		-i ${SSH_PEM_FILE} \
		${SSH_USER}@${SSH_HOST} \
		GITLAB_ACCESS_TOKEN=${GITLAB_ACCESS_TOKEN} \
		DEPLOY_GIT_SHA="$(shell git rev-parse HEAD)" \
		'bash -s' < scripts/deploy.sh

db-backup:
	docker exec \
		-it haskell-bazaar-postgres \
		pg_dump -U haskellbazaar > ./backups/$(shell date +%Y_%m_%d).backup.sql

# TODO add date as an argument
db-restore:
	docker exec \
		-i haskell-bazaar-postgres psql \
		-U haskellbazaar \
		< ./backups/$(shell date +%Y_%m_%d).backup.sql

db-seed:
	docker exec \
		-i haskell-bazaar-postgres psql \
		-U haskellbazaar \
		< scripts/db/seed.sql


.PHONY: build \
	test \
	lint \
	ghcid \
	db-backup \
	docker-build \
	docker-build-save \
	docker-deploy
