#!/usr/bin/env bash

set -e
set -o pipefail

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y curl gnupg
rm -rf /var/lib/apt/lists/*

curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -

tee -a /etc/apt/sources.list.d/yarn.list >/dev/null <<"EOF"
deb https://dl.yarnpkg.com/debian/ stable main
EOF

# bsdmainutils for the column utility. jq for prettifying JSON output
# if you want to actually read it as a human.

packages="

bsdmainutils
curl
emacs-nox
git
jq
nodejs
npm
python
python-pip
python3
python3-pip
python3-venv
ruby
ruby-bundler
sqlite3
yarn

"

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y $packages
rm -rf /var/lib/apt/lists/*

pip2 --disable-pip-version-check install poetry
pip3 --disable-pip-version-check install poetry
curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python3
ln -s "$HOME/.cask/bin/cask" /usr/local/bin/

# Install Wasmer/WAPM
curl https://github.com/wasmerio/wasmer/releases/download/0.10.1/wasmer-linux-amd64.tar.gz -o /tmp/wasmer.tar.gz
mkdir /usr/local/wasmer
tar -xvf /tmp/wasmer.tar.gz -C /usr/local/wasmer --strip-components=1
ln -s /usr/local/wasmer/wasmer /usr/local/bin/wasmer
ln -s /usr/local/wasmer/wapm /usr/local/bin/wapm
rm /tmp/wasmer.tar.gz

ln -s "$HOME/.wasmer/bin/wasmer" /usr/local/bin/
ln -s "$HOME/.wasmer/bin/wapm" /usr/local/bin/

# https://github.com/docker-library/rails/issues/10#issuecomment-169957222
bundle config --global silence_root_warning 1

rm /tmp/docker-install-languages.bash
