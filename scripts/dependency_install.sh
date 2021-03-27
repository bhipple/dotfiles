#!/usr/bin/env bash
set -euo pipefail

INSTALL_ALL=""

# Whether to just do minEnv or all
if hostname | grep -qE "^brh"; then
    INSTALL_ALL=1
fi

##########################################
# Change shell to zsh, if not already done
#
change_to_zsh() {
    if [ "$(echo "$SHELL" | grep -c "zsh")" -eq "0" ]; then
        echo "Setting shell to zsh"
        chsh -s "$(which zsh)"
    else
        echo "zsh is already the default shell"
    fi
}

#############################################
# Create ssh dir with appropriate permissions
#
create_ssh() {
    mkdir -p "$HOME"/.ssh
    chmod 0700 "$HOME"/.ssh
}

nix_install() {
    if ! [ -d ~/src/nixpkgs ]; then
        mkdir -p ~/src
        cd ~/src
        git clone https://github.com/NixOS/nixpkgs
        git remote add bhipple https://github.com/bhipple/nixpkgs
        git fetch --all
    fi

    if ! [ -d /nix ]; then
        echo "NixPkg not installed on this machine."
        if groups | grep -q sudo; then
            curl https://nixos.org/nix/install | sh
        else
            echo "Can't install NixPkg without sudo."
            return
        fi
    fi

    ATTRS="minEnv"

    if [ -n "$INSTALL_ALL" ]; then
        # Also install plaid2qif from my nix user repo
        ATTRS="$ATTRS bigEnv nur.repos.bhipple.plaid2qif"
    fi

    cd ~/src/nixpkgs
    (set -x; nix-env -f . -j$(nproc) -k -riA $ATTRS)

    if [ -n "$INSTALL_ALL" ]; then
        cd ~/src/nixpkgs
        nix-env -f . -iA spacemacs
    fi
}

# See here for list of docsets:
# https://kapeli.com/dash#docsets
dasht_setup() {
    if [ -z "$INSTALL_ALL" ]; then return; fi

    dasht-docsets-install \
        "^Bash$" \
        "^Boost$" \
        "^C++$" \
        "^CMake$" \
        "^Docker$" \
        "^Emacs_Lisp$" \
        "^Groovy$" \
        "^Jinja$" \
        "^NumPy$" \
        "^Pandas$" \
        "^Python_3$" \
        "^Rust$" \
        "^SaltStack$" \

}

change_to_zsh
create_ssh
nix_install
dasht_setup
