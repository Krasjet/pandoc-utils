.PHONY: test build ghci ghci-test ghcid release doc

test:
	@stack test

build:
	@stack build

ghci:
	@stack ghci

ghci-test:
	@stack ghci --test

ghcid:
	@ghcid \
		--command "stack ghci --test" \
		--color=always \
		--test "main --color"

doc:
	@stack haddock --open pandoc-utils

release:
	@cabal sdist
	@cabal haddock --haddock-for-hackage --enable-doc --haddock-hyperlinked-source
