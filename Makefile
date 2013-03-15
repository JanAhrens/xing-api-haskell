.PHONY: cli-demo yesod-demo yesod-auth-demo
all: cli-demo yesod-demo

build:
	cabal clean && cabal configure && cabal build

cli-demo:
	runhaskell -idemos:. -Wall -Werror demos/CliDemo.hs

yesod-demo:
	runhaskell -idemos:. -Wall -Werror demos/helloXING.hs

yesod-auth-demo:
	runhaskell -idemos:. -Wall -Werror demos/yesodAuth.hs

clean:
	rm -rf client_session_key.aes dist/ tmp/
