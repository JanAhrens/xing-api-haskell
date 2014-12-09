# xing-api

[![Build Status](https://api.travis-ci.org/JanAhrens/xing-api-haskell.png)](https://travis-ci.org/JanAhrens/xing-api-haskell)

This Haskell library is a wrapper for the [XING API](https://dev.xing.com/).

It's currently under development and is **not** considered **stable**.
This library is a private project and isn't associated with XING AG.

## Usage

To use this [minimal working example](demos/minimal.hs?raw=true) you'll need to get
a consumer key and secret from the [XING developer](https://dev.xing.com) site.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Web.XING
import qualified Data.ByteString.Char8 as BS

oauthConfig :: OAuth
oauthConfig = consumer "YOUR_CONSUMER_KEY" "YOUR_CONSUMER_SECRET"

main :: IO ()
main = withManager $ \manager -> do
  (accessToken, _) <- handshake manager
  idCard <- getIdCard oauthConfig manager accessToken
  liftIO $ putStrLn $ show idCard
  where
    handshake manager = do
      (requestToken, url) <- getRequestToken oauthConfig manager
      verifier <- liftIO $ do
        BS.putStrLn url
        BS.putStr "PIN: "
        BS.getLine
      getAccessToken requestToken verifier oauthConfig manager
```

## GHCI session

If you want to test this library using GHCI, you can use this snippet as a starting point.

    $ ghci -XOverloadedStrings
    :module +Network.HTTP.Conduit
    manager <- newManager def

    :module +Web.XING
    let oauth = consumer "CONSUMER_KEY" "CONSUMER_SECRET"

    :module +Control.Monad.Trans.Resource
    (requestToken, url) <- runResourceT (getRequestToken oauth manager)
    url

    (accessToken, userId) <- runResourceT (getAccessToken requestToken "THE_PIN_YOU_GOT_FROM_XING" oauth manager)
    userId

## Development environment

The simplest way to setup a development environment is to use Docker.

> sudo docker build .

This repository includes several demo programs.
To use them, you have to obtain an API consumer key by registering your
application at [the XING developer portal](https://dev.xing.com/applications).
Put the consumer key and secret in the `demos/Config.hs` file (the test consumer key is enough).
The `Config.hs.template` file is recommended to be used as a template.

    cp Config.hs.template Config.hs
