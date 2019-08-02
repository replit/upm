#!/usr/bin/env bash

set -e
set -o pipefail

# Normalize the package names to lowercase and using hyphens instead
# of underscores. I believe they actually come like this already, but
# let's be extra sure since we rely on it in the Go code.
curl -s https://pypi.org/simple/ \
    | grep -o '/simple/[^/]\+/' \
    | grep -o '[^/]\+/$' \
    | grep -o '[^/]\+' \
    | sed 's/_/-/g' \
    | tr A-Z a-z \
         > resources/python/pypi
