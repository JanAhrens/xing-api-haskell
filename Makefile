all:
	cabal clean && cabal configure && cabal build

demo:
	runhaskell -Wall -Werror CliDemo.hs
