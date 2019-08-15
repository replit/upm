#!/usr/bin/env bash

set -e
set -o pipefail

cd /upm

updpkgsums
makepkg --printsrcinfo > .SRCINFO
makepkg --syncdeps --install --noconfirm
upm --version
