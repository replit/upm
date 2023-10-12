#!/usr/bin/env sh

set -e
set -o xtrace

install_script_cache_key="install-nix-2.16.1"
install_script="/tmp/$install_script_cache_key"

# Install Nix. Install the base version if the currently installed version is
# not what we expect.
if [ \
	! -f ~/.nix-profile/etc/profile.d/nix.sh -o \
	"$(~/.nix-profile/bin/nix --version)" != "nix (Nix) 2.16.1" \
	]; then
	curl -L https://releases.nixos.org/nix/nix-2.16.1/install | sh
fi

source ~/.nix-profile/etc/profile.d/nix.sh

set +e
