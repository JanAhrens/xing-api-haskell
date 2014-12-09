FROM haskell:7.8
RUN cabal update

# Add the cabal file first, to take advantage of the Docker build cache
ADD ./xing-api.cabal /opt/
RUN cd /opt && cabal install --enable-tests --only-dependencies --jobs=4

# The Docker container needs make to execute the Makefile
RUN apt-get update && apt-get install -y make && apt-get clean

# Everything after this line will get executed with every "docker build"
ADD . /opt
RUN cd /opt && cabal install

ENV PATH /root/.cabal/bin:$PATH

RUN cd /opt && make test

WORKDIR /opt
