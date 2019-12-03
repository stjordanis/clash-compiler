#!/usr/bin/env bash

set -xeo pipefail

# Create a ghc environment file needed for the doctests
# On cabal>=2.4.1.0 and ghc<8.4.4 this isn't done automatically.
cabal --write-ghc-environment-files=always v2-build all

# Check that clash-dev compiles
sed "s/^ghci/ghc -fno-code/" clash-dev > clash-dev-test
sh clash-dev-test

# Check whether version numbers in snap / clash-{prelude,lib,ghc,cores} are the same
cabal_files="clash-prelude/clash-prelude.cabal clash-lib/clash-lib.cabal clash-ghc/clash-ghc.cabal clash-cores/clash-cores.cabal"
snapcraft_file="bindist/linux/snap/snap/snapcraft.yaml"
versions=$(grep "^[vV]ersion" $cabal_files $snapcraft_file | grep -Eo '[0-9]+(\.[0-9]+)+')

if [[ $(echo $versions | tr ' ' '\n' | wc -l) == 5 ]]; then
	if [[ $(echo $versions | tr ' ' '\n' | uniq | wc -l) != 1 ]]; then
		echo "Expected all distributions to have the same version number. Found: $versions"
		exit 1;
	fi
else
	echo "Expected to find version number in all distributions. Found: $versions";
	exit 1;
fi

# Check tag is valid for git tags.
if [[ ! -z "$CI_COMMIT_TAG" ]]; then
	version=$(echo $versions | tr ' ' '\n' | head -n 1)

	if [[ ${CI_COMMIT_TAG:1} != $version ]]; then
		if [[ ${CI_COMMIT_TAG:0:1} == "v" ]]; then
			cat <<- EOF
			Tag name and distribution release number should match:
			  Tag version:         $CI_COMMIT_TAG
			  Distribution number: v$version
			EOF

			exit 1
		else
			echo "\$CI_COMMIT_TAG should start with 'v', found: $CI_COMMIT_TAG"
			exit 1
		fi
	fi
fi

# Run package tests and clash-testsuite

cabal v2-test clash-cores clash-cosim clash-prelude clash-lib
cabal v2-run clash-testsuite -- --hide-successes -p "/.VHDL./ || /.Verilog./"

