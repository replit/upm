#!/usr/bin/env bash

set -e
set -o pipefail

cd /tmp

packages="

gcc
git
make
software-properties-common
wget
r-base-dev

"

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y $packages

add-apt-repository -y ppa:longsleep/golang-backports
DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends golang-go

rm -rf /var/lib/apt/lists/*
rm /tmp/docker-install-build.bash
