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
maven
nodejs
npm
python
python-pip
python3
python3-pip
python3-venv
r-base
r-base-dev
r-recommended
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

# https://github.com/docker-library/rails/issues/10#issuecomment-169957222
bundle config --global silence_root_warning 1

rm /tmp/docker-install-languages.bash
