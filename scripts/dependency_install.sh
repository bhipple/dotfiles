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
    if [ -d /nix ]; then
        nix-env -j 4 -i minEnv bigEnv
    else
        echo "NixPkg not installed on this machine. Installing."
        curl https://nixos.org/nix/install | sh
        . "$HOME"/.nix-profile/etc/profile.d/nix.sh
    fi
}

fzf_install() {
    if ! [ -f ~/.fzf.zsh ]; then
        echo "Installing fzf"
        ./fzf/install --key-bindings --completion --no-update-rc
    else
        echo "fzf already installed"
    fi
}

change_to_zsh
create_ssh
nix_install
fzf_install
