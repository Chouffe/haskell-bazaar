BUILD_DIR := "build"
GIT_SHA   := $$(git rev-parse HEAD)
TAG       := $$(git rev-parse --short HEAD)

clean:
	@rm -rf ${BUILD_DIR}

build:
	@docker container run \
		-v $(shell pwd):/clj \
		haskell-bazaar_frontend \
		lein cljsbuild once prod

deploy:
	@deploy.sh

repl:
	@docker container attach haskell-bazaar-repl


.PHONY: clean \
	build \
	test \
	lint \
	deploy
