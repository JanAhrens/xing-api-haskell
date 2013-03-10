all: cli-demo yesod-demo

build:
	cabal clean && cabal configure && cabal build

cli-demo:
	runhaskell -idemos:. -Wall -Werror demos/CliDemo.hs

yesod-demo:
	runhaskell -idemos:. -Wall -Werror demos/helloXING.hs

clean:
	rm -rf client_session_key.aes dist/ tmp/
