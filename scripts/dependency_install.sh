#!/usr/bin/env bash
set -euo pipefail

INSTALL_ALL=""
CHANNEL=$HOME/git/nixos-unstable

if ! [ -d $CHANNEL ]; then
    echo "No nix channel, assuming not setup for local work and exiting"
    exit 0
fi

# Whether to just do minEnv or all
if hostname | grep -qE "^brh"; then
    INSTALL_ALL=1
fi

#############################################
# Create ssh dir with appropriate permissions
#
create_ssh() {
    mkdir -p "$HOME"/.ssh
    chmod 0700 "$HOME"/.ssh
}

nix_install() {
    if ! [ -d /nix ]; then
        echo "NixPkg not installed on this machine."
        return
    fi

    ATTRS="minEnv"

    if [ -n "$INSTALL_ALL" ]; then
        ATTRS="$ATTRS bigEnv "
    fi

    export NIXPKGS_ALLOW_UNFREE=1
    # Build first before installing, so we can see the progress bar
    set -x
    nix build -Lvf $CHANNEL --no-link --keep-going -j$(nproc) $ATTRS
    nix build -Lvf ~/git/nixos-24.05 ledger

    if [ -z ${BUILD_ONLY:=} ]; then
        nix-env -f $CHANNEL -k -riA $ATTRS
        nix-env -f ~/git/nixos-24.05 -k -iA ledger
    fi
}

browserpass_install() {
    # This file needs to be symlinked manually; the NixOS package can't do it.
    passfile="$(nix build -Lvf $CHANNEL --no-link browserpass --print-out-paths)/lib/browserpass/hosts/chromium/com.github.browserpass.native.json"
    (
      cd ~/.config/BraveSoftware/Brave-Browser/NativeMessagingHosts
      rm -rf *
      ln -s $passfile
    )
}

create_ssh
nix_install
browserpass_install
