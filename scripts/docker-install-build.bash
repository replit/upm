#!/usr/bin/env bash

set -e
set -o pipefail

cd /tmp

packages="

gcc
git
make
wget
r-base-dev

"

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y $packages
rm -rf /var/lib/apt/lists/*

# We can't use Go from the Ubuntu repositories because it's not new
# enough to have support for Go modules.
wget -nv https://dl.google.com/go/go1.11.11.linux-amd64.tar.gz
tar -C /usr/local -xzf *.tar.gz
rm *.tar.gz

rm /tmp/docker-install-build.bash
