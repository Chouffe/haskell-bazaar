build-common:
	docker build \
		-t build-env:latest \
		-f Dockerfile.build \
		.

build:
	docker build \
		-t haskell-bazaar-server:latest \
		-f Dockerfile \
		.

psql:
	docker container exec -it haskell-bazaar-postgres psql -U haskellbazaar


test:
	docker container run -it --rm \
		-v $(shell pwd):/opt/build/. \
		--link haskell-bazaar-postgres \
		--network backend_default \
	        build-env stack --allow-different-user test

repl:
	docker container run -it --rm \
		--link haskell-bazaar-postgres \
		-p 8002:8002 \
		--network backend_default \
		-v $(shell pwd):/opt/build/. \
	        build-env stack --allow-different-user repl

ghcid:
	docker container run -it --rm \
		build-env ghcid --command "stack ghci"


lint:
	docker container run -it --rm \
		build-env stack exec -- hlint .

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
