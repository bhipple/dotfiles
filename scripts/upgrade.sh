#!/usr/bin/env zsh
# Script for helping with bumps and upgrades
. ~/dotfiles/zsh/functions.zsh

set -euo pipefail

EMACS_OVERLAY="https://github.com/nix-community/emacs-overlay"

emacs_overlay() {
    rev=$(git ls-remote "$EMACS_OVERLAY" refs/heads/master | awk '{print $1}')
    hash=$(nix-prefetch-url --unpack "$EMACS_OVERLAY/archive/$rev.tar.gz")
    echo "Updating emacs-overlay.nix to $rev and $hash"
    sed -i "s|^  rev = .*|  rev = \"$rev\";|"           ~/dotfiles/config/nixpkgs/overlays/emacs-overlay.nix
    sed -i "s|^  sha256 = .*|  sha256 = \"$hash\";|"    ~/dotfiles/config/nixpkgs/overlays/emacs-overlay.nix
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
