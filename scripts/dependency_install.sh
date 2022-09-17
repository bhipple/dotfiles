#!/usr/bin/env bash
set -euo pipefail

INSTALL_ALL=""

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
        # Also install plaid2qif from my nix user repo
        ATTRS="$ATTRS bigEnv spacemacs bhipple.plaid2qif"
    fi

    export NIXPKGS_ALLOW_UNFREE=1
    # Build first before installing, so we can see the progress bar
    set -x
    channel='<nixos>'
    channel=$HOME/git/nix-channel
    nix build -Lvf $channel --no-link -j$(nproc) $ATTRS
    nix-env -f $channel -j$(nproc) -k -riA $ATTRS

}

create_ssh
nix_install
