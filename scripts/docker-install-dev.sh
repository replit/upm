#!/bin/sh

set -e
set -o pipefail

apk add --no-cache curl emacs git go make musl-dev python2 python3
pip3 --disable-pip-version-check install poetry
curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python3
ln -s "$HOME/.cask/bin/cask" /usr/local/bin/

rm /tmp/docker-install-dev.sh
