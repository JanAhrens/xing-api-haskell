.PHONY: all dependencies clean configure haddock
CABAL_FLAGS = --enable-tests -f demos -f minimal-demo

all:
	@echo "make [targets]"
	@echo ""
	@echo "Available targets:"
	@echo "  dependencies  Install all required haskell packages"
	@echo "  configure     Setup cabal"
	@echo "  clean         Remove all build information"
	@echo "  haddock       Build the haddock documentation and start a HTTP server serving it"

dependencies:
	cabal install $(CABAL_FLAGS) --only-dependencies

clean:
	cabal clean
	rm -f client_session_key.aes

configure: clean
	cabal configure $(CABAL_FLAGS)

haddock: configure
	cabal haddock
	cd dist/doc/html/xing-api/; python -mSimpleHTTPServer 3000
