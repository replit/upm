#!/usr/bin/env sh

install_script_cache_key="install-nix-2.16.1"
install_script="/tmp/$install_script_cache_key"

if [ cache has_key "$install_script_cache_key" ]; then
	cache restore "$install_script_cache_key"
else
	curl -L https://releases.nixos.org/nix/nix-2.16.1/install -o "$install_script"
	chmod +x "$install_script"
	cache store "$install_script_cache_key" "$install_script"
fi

# Install Nix. Install the base version if the currently installed version is
# not what we expect.
if [ \
	! -f ~/.nix-profile/etc/profile.d/nix.sh -o \
	"$(~/.nix-profile/bin/nix --version)" != "nix (Nix) 2.16.1" \
	]; then
	exec "$install_script"
fi
