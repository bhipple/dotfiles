#!/usr/bin/env zsh
# Script for helping with bumps and upgrades
. ~/dotfiles/zsh/functions.zsh

set -euo pipefail

EMACS_OVERLAY="https://github.com/nix-community/emacs-overlay"

emacs_overlay() {
    rev=$(git ls-remote "$EMACS_OVERLAY" refs/heads/master | awk '{print $1}')
    hash=$(nix-prefetch-url --unpack "$EMACS_OVERLAY/archive/$rev.tar.gz")
    # TODO: Just sed these in
    echo "Change emacs-overlay with rev=$rev and sha256=$hash"
}

submodules() {
    for sub in $(git submodule | grep -v spacemacs | awk '{print $2}'); do
        echo "Updating $sub"
        (cd "$sub" && git pull origin master)
    done
}

zgen() {
    rm -f ~/.zgen/init.zsh
    zgen update
}

################################################################################
# MAIN
echo "Starting upgrade ..."
confirm emacs_overlay "Would you like to update the emacs-overlay version?"
confirm submodules    "Would you like to update the git submodules?"
confirm zgen          "Would you like to update the zgen zsh plugins?"
