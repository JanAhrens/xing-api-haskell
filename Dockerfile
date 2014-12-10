FROM haskell:7.8
RUN cabal update

# Add the cabal file first, to take advantage of the Docker build cache
RUN mkdir /opt/app
ADD ./xing-api.cabal /opt/app/
RUN cd /opt/app && cabal install --enable-tests --only-dependencies --jobs=4

# The Docker container needs make to execute the Makefile
# I choose not to use "make dependencies" for the previous line, because that would mean that this would have to rerun
# every time the Makefile changes.
RUN apt-get update && apt-get install -y make && apt-get clean

# Everything after this line will get executed with every "docker build"
ADD . /opt/app/

ENV PATH /root/.cabal/bin:$PATH

RUN cd /opt/app && make test && cabal copy && cabal register

WORKDIR /opt/app
VOLUME ["/opt/app"]
