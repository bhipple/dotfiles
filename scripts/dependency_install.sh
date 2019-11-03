#!/usr/bin/env bash
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
    if ! [ -d /nix ]; then
        echo "NixPkg not installed on this machine."
        if groups | grep -q sudo; then
            curl https://nixos.org/nix/install | sh
        else
            echo "Can't install NixPkg without sudo."
            return
        fi
    fi

    CHANNEL="nixos"
    ATTRS="$CHANNEL.minEnv $CHANNEL.pyEnv"

    if hostname | grep -qE "^brh"; then
        # Also install plaid2qif from my nix user repo
        ATTRS="$ATTRS $CHANNEL.bigEnv $CHANNEL.nur.repos.bhipple.plaid2qif"
    fi

    (set -x; nix-env -j8 -k -riA $ATTRS)
}

# See here for list of docsets:
# https://kapeli.com/dash#docsets
dasht_setup() {
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
