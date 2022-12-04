#!/usr/bin/env zsh
# Script for helping with bumps and upgrades

# For confirm
. ~/dotfiles/zsh/functions.zsh

set -euo pipefail

submodules() {
    for sub in $(git submodule | grep -v spacemacs | grep -v nur-packages | awk '{print $2}'); do
        echo "Updating $sub"
        (cd "$sub" && git pull origin master)
    done
}

zgen_up() {
    zsh -lc 'zgen update'
}

################################################################################
# MAIN
echo "Starting upgrade ..."
confirm submodules    "Would you like to update the git submodules?"
confirm zgen_up       "Would you like to update the zgen zsh plugins?"
