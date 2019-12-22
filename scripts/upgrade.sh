#!/usr/bin/env zsh
set -euo pipefail
# Script for helping with bumps and upgrades

source ~/dotfiles/zsh/functions.zsh

EMACS_OVERLAY="https://github.com/nix-community/emacs-overlay"

emacs_overlay() {
    rev=$(git ls-remote "$EMACS_OVERLAY" refs/heads/master | awk '{print $1}')
    hash=$(nix-prefetch-url --unpack "$EMACS_OVERLAY/archive/$rev.tar.gz")
    echo "Change emacs-overlay with rev=$rev and sha256=$hash"
}

submodules() {
    for sub in $(git submodule | grep -v spacemacs | awk '{print $2}'); do
        echo "Updating $sub"
        (cd "$sub" && git pull origin master)
    done
}

zgen() {
    rm ~/.zgen/init.zsh
    zgen update
}

################################################################################
# MAIN
confirm emacs_overlay "Would you like to update the emacs-overlay version?"
confirm submodules    "Would you like to update the git submodules?"
confirm zgen          "Would you like to update the zgen zsh plugins?"
