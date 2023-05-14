#!/usr/bin/env bash
set -euo pipefail

INSTALL_ALL=""

# Whether to just do minEnv or all
if hostname | grep -qE "^brh"; then
    INSTALL_ALL=1
fi

if [[ -d /mnt/wsl ]]; then
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
        ATTRS="$ATTRS nixStable bigEnv spacemacs "
    fi

    if [ -d $HOME/git/plaid2qif ]; then
        # Also install plaid2qif from my nix user repo
        ATTRS="$ATTRS plaid2qif"
    fi

    export NIXPKGS_ALLOW_UNFREE=1
    # Build first before installing, so we can see the progress bar
    set -x
    channel=$HOME/git/nix-channel
    nix --extra-experimental-features nix-command build -Lvf $channel --no-link --keep-going -j$(nproc) $ATTRS

    if [ -z ${BUILD_ONLY:=} ]; then
        nix-env -f $channel -k -riA $ATTRS
    fi
}

create_ssh
nix_install
