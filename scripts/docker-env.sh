#!/bin/sh

export PATH="/upm/cmd/upm:$PATH"
cd /upm

# Alias to force rebuilding UPM.
alias b='make -s -C /upm clean upm'

# Alias to build and run the latest version of UPM.
alias u='make -s -C /upm upm && upm'

# Alias to generate and cd to a temporary directory, for testing UPM.
alias mt='cd "$(mktemp -d)"'

# Alias for listing directories.
alias l='ls -lA'
