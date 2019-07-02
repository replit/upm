#!/usr/bin/env bash

set -e
set -o pipefail

tee -a "$HOME/.bashrc" >/dev/null <<"EOF"
export PATH="/upm/cmd/upm:$PATH"
cd /upm

# Alias to build and run the latest version of UPM.
alias u='make -s -C /upm upm && upm'

# Alias to force rebuilding UPM.
alias ub='make -s -C /upm -B upm && upm'

# Alias to generate and cd to a temporary directory, for testing UPM.
alias mt='cd "$(mktemp -d)"'

# Alias for listing directories.
alias l='ls -lA'
EOF

rm /tmp/docker-setup-env.bash
