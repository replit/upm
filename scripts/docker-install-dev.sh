#!/bin/sh

set -e
set -o pipefail

packages="

curl
emacs
git
go
make
musl-dev
nodejs
python2
python3
ruby
ruby-bundler
ruby-json
ruby-rdoc
sqlite
util-linux
yarn

"

apk add --no-cache $packages
pip3 --disable-pip-version-check install pipreqs poetry
gem install gems
curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python3
ln -s "$HOME/.cask/bin/cask" /usr/local/bin/

# https://github.com/docker-library/rails/issues/10#issuecomment-169957222
bundle config --global silence_root_warning 1

rm /tmp/docker-install-dev.sh
