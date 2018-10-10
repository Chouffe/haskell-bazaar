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

.PHONY: build run test clean lint ghcid-devel docker-build-dev docker-build docker-prod-run
