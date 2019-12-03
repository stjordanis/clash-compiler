#!/usr/bin/env bash

set -xeo pipefail

apt-get install -yq cabal-install-$CABAL_VERSION ghc-$GHC_VERSION

cabal --version
ghc --version

envsubst < .ci/cabal.project.local > cabal.project.local

if [[ "$GHC_VERSION" == "head" ]]; then
	cat <<- EOF >> cabal.project.local
	repository head.hackage.ghc.haskell.org
	  url: https://ghc.gitlab.haskell.org/head.hackage/
	  secure: True
	  key-threshold: 3
	  root-keys:
	    7541f32a4ccca4f97aea3b22f5e593ba2c0267546016b992dfadcd2fe944e55d
	    26021a13b401500c8eb2761ca95c61f2d625bfef951b939a8124ed12ecf07329
	    f76d08be13e9a61a377a85e2fb63f4c5435d40f8feb3e12eb05905edb8cdea89
	EOF
fi

cat cabal.project.local

# run new-update first to generate the cabal config file that we can then modify
# retry 5 times, as hackage servers are not perfectly reliable
NEXT_WAIT_TIME=0

until cabal v2-update || [[ $NEXT_WAIT_TIME == 5 ]]; do
        sleep $(( NEXT_WAIT_TIME++ ))
done

cabal v2-build all

CLASH_HADDOCK_PKGS="clash-lib clash-prelude clash-cosim clash-cores"

for pkg in $CLASH_HADDOCK_PKGS; do
	if [[ ! -e "dist-newstyle/build/*/ghc-*/${pkg}-*/doc/html/${pkg}/index.html" ]]; then
		echo "Haddock generation failed for package ${pkg}"
		exit 1
	fi
done

