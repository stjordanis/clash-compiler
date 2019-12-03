# Clash CI Image

This directory contains the `Dockerfile` and scripts used by the CI for
`clash-compiler`.

## Building and Running in Docker

The CI image can be built (tagged with both "latest" and the current date) by
running the following in a POSIX compatible shell:

```bash
$ ./build-and-publish-docker-image.sh
```

The script will optionally ask if you want to push the newly built tags to
the [container registry](https://hub.docker.com/repository/docker/clashlang/clash-ci).
Using pre-built images from the registry can be done by running

```bash
$ docker pull clashlang/clash-ci:latest
$ docker run --rm -it clashlang/clash-ci:latest /bin/bash
```

## Overview

The docker image runs in two stages. The first stage fetches and builds the
necessary synthesis tools for running `clash-testsuite` VHDL and Verilog tests.
In the second stage, `cabal` and `ghc` packages are pre-fetched for quicker
installation in the CI runner.

The project currently uses [Gitlab CI](../.gitlab-ci.yml) to perform testing
and publishing of Clash. Specific jobs set the desired Cabal and GHC versions
by exporting environment variables, e.g.

```bash
export CABAL=cabal-install-3.0
export GHC=ghc-8.8.1
```

These variables are then used in the scripts to keep them version agnostic.

