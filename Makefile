haddock:
	cabal haddock
	cd dist/doc/html/xing-api/; python -mSimpleHTTPServer 3000
