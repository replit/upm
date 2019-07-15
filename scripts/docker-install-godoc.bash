#!/usr/bin/env bash

set -e
set -o pipefail

cd /tmp

packages="

# to download go and watchexec
wget

# needed for 'go get'
git

"

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y $(grep -v "^#" <<< "$packages")
rm -rf /var/lib/apt/lists/*

# We can't use Go from the Ubuntu repositories because it's not new
# enough to have support for Go modules.
wget -nv https://dl.google.com/go/go1.11.11.linux-amd64.tar.gz
tar -C /usr/local -xzf *.tar.gz
rm *.tar.gz

# Watchexec is not available from the Ubuntu repositories.
wget -nv https://github.com/watchexec/watchexec/releases/download/1.10.2/watchexec-1.10.2-x86_64-unknown-linux-gnu.tar.gz
tar -xzvf watchexec-*.tar.gz
mv watchexec-*/watchexec /usr/bin/
rm -rf watchexec-*

go get golang.org/x/tools/cmd/godoc

rm "$0"
