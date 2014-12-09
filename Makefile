.PHONY: all dependencies clean configure build test haddock
TEST_FLAGS = --enable-tests
CABAL_FLAGS = $(TEST_FLAGS) -f demos -f minimal-demo

all:
	@echo "make [targets]"
	@echo ""
	@echo "Available targets:"
	@echo "  dependencies  Install all required haskell packages"
	@echo "  configure     Setup cabal"
	@echo "  clean         Remove all build information"
	@echo "  test          Run the testsuite"
	@echo "  haddock       Build the haddock documentation and start a HTTP server serving it"

dependencies:
	cabal install $(CABAL_FLAGS) --only-dependencies --jobs=4

clean:
	cabal clean
	rm -f client_session_key.aes

configure: clean
	cabal configure $(CABAL_FLAGS)

test: clean
	cabal configure $(TEST_FLAGS)
	cabal build
	cabal test

haddock: configure
	cabal haddock
	cd dist/doc/html/xing-api/; python -mSimpleHTTPServer 3000
