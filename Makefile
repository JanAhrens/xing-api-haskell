all:
	cabal clean && cabal configure && cabal build

demo:
	runhaskell -Wall CliDemo.hs
