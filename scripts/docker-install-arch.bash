#!/usr/bin/env bash

set -e
set -o pipefail

pacman -Sy --needed --noconfirm base-devel pacman-contrib

# https://www.reddit.com/r/archlinux/comments/6qu4jt/how_to_run_makepkg_in_docker_container_yes_as_root/dl1t5m9/
useradd builder -m
passwd -d builder
printf 'builder ALL=(ALL) ALL\n' | tee -a /etc/sudoers

rm "$0"
