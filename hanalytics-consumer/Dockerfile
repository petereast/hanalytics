FROM haskell:8

MAINTAINER Peter East <petereast+docker@pm.me>

WORKDIR /opt

RUN stack update

# Add just the .cabal file to capture dependencies
COPY ./package.yaml /opt/package.yaml

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN stack setup
RUN stack init
RUN stack build --only-dependencies

# Add and Install Application Code
COPY . /opt
RUN stack build

CMD ["stack", "run"]
