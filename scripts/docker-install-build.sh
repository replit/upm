#!/bin/sh

set -e
set -o pipefail

apk add --no-cache git go make musl-dev

rm /tmp/docker-install-build.sh
