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

.PHONY: build run test clean lint ghcid-devel
