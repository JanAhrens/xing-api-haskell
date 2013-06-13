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

This section walks you through the process of setting up your development environment.

### TL;DR

Use Vagrant if you don't want to setup your own environment:

    wget http://files.vagrantup.com/packages/7e400d00a3c5a0fdf2809c8b5001a035415a607b/vagrant_1.2.2_x86_64.deb
    sudo dpkg -i vagrant_1.2.2_x86_64.deb
    vagrant plugin install vagrant-berkshelf
    vagrant up
    vagrant ssh
    cd /vagrant/demos
    cp Config.hs.template Config.hs
    vim Config.hs # get a consumer key from https://dev.xing.com/applications
    cd /vagrant
    cabal install # will download the internet ..
    make cli-demo

### The longer version

This project uses [Vagrant](http://www.vagrantup.com/) to provide a simple development environment.
Vagrant most likely provides a binary for your OS on it's [downloads page](http://downloads.vagrantup.com/) (don't
use the RubyGem, it's [not maintained](http://docs.vagrantup.com/v2/installation/index.html)).

The provisioning of the Vagrant images will be done with [Chef](http://opscode.com/). The depending Chef cookbooks are
managed using [Berkshelf](http://berkshelf.com/). To use Berkshelf with Chef, you have to install the
[Vagrant Berkshelf plugin](https://github.com/RiotGames/vagrant-berkshelf) using
`vagrant plugin install vagrant-berkshelf`.

To create a VM based on [Ubuntu 12.04.2 LTS](http://releases.ubuntu.com/precise/), executing `vagrant up` should be
enough. This will download the required Ubuntu image and provision the system using Chef.

Once the system is ready, you need to install the required Haskell libraries:

  1. Open a SSH connection to the fresh Ubuntu VM using `vagrant ssh`
  2. Go to the `/vagrant` directory `cd /vagrant`
  3. Execute `cabal install`.

This repository includes several demo programs.
To use them, you have to obtain an API consumer key by registering your
application at [the XING developer portal](https://dev.xing.com/applications).
Put the consumer key and secret in the `demos/Config.hs` file (the test consumer key is enough).
The `Config.hs.template` file is recommended to be used as a template.

    cp Config.hs.template Config.hs
