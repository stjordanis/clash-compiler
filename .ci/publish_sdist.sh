#!/usr/bin/env bash

set -xeuo pipefail

PASSWORD=$(echo $HACKAGE_PASSWORD | base64 --decode --ignore-garbage)

SDIST=$(find $1-*.tar.gz | grep -v docs)
DDIST=$(find $1-*.tar.gz | grep docs)

set +u

if [[ "$HACKAGE_RELEASE" == "yes" ]]; then
	# Release tag set, upload as release.
	$CABAL upload --publish --username=${HACKAGE_USERNAME} --password=${PASSWORD} ${SDIST}
	$CABAL upload --publish --documentation --username=${HACKAGE_USERNAME} --password=${PASSWORD} ${DDIST}
elif [[ "$HACKAGE_RELEASE" == "no" ]]; then
	# Upload as release candidate
	$CABAL upload --username=${HACKAGE_USERNAME} --password=${PASSWORD} ${SDIST} 
	$CABAL upload --documentation --username=${HACKAGE_USERNAME} --password=${PASSWORD} ${DDIST} 
else
	echo "Unrecognized \$HACAKGE_RELEASE: $HACAKGE_RELEASE"
	exit 1;
fi

