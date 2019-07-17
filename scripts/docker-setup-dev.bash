#!/usr/bin/env bash

set -e
set -o pipefail

cd /tmp

# Turns out 'go mod' is hardcoded to disallow go.mod in /tmp
# specifically, so we have to move to a subdirectory. See
# <https://github.com/golang/vgo/blob/9d567625acf4c5e156b9890bf6feb16eb9fa5c51/vendor/cmd/go/internal/modload/init.go#L157-L166>.
mkdir tmp-go
mv go.* tmp-go/

pushd tmp-go >/dev/null
go mod download
popd >/dev/null

rm -rf tmp-go

tee -a "$HOME/.bashrc" >/dev/null <<"EOF"
# Alias to build and run the latest version of UPM.
alias u='make -s -C /upm upm && upm'

# Alias to force rebuilding UPM.
alias ub='make -s -C /upm -B upm && upm'

# Alias to generate and cd to a temporary directory, for testing UPM.
alias mt='cd "$(mktemp -d)"'

# Alias for listing directories.
alias l='ls -lAhF'
EOF

rm "$0"
