#!/usr/bin/env bash

set -e
set -o pipefail

cd /tmp

tee -a "$HOME/.bashrc" >/dev/null <<"EOF"
# Build and run the latest version of UPM.
u() {
    make -s -C /upm upm && upm "$@"
}

# Force rebuilding UPM.
ub() {
     make -s -C /upm -B upm && upm "$@"
}

# Generate and cd to a temporary directory, for testing UPM.
mt() {
     cd "$(mktemp -d)"
}

# List directories.
unalias l
unalias ls
l() {
    ls -lAhF --color=always "$@"
}
EOF

rm "$0"
