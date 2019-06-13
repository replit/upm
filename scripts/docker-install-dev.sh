#!/bin/sh

set -e
set -o pipefail

apk add --no-cache curl git go make musl-dev python3
pip3 --disable-pip-version-check install poetry

rm /tmp/docker-install-dev.sh
