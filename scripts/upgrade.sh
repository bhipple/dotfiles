#!/usr/bin/env zsh
# Script for helping with bumps and upgrades

# For confirm
. ~/dotfiles/zsh/functions.zsh
# For zgen
#. ~/dotfiles/zsh/plugins.zsh

set -euo pipefail

submodules() {
    for sub in $(git submodule | grep -v spacemacs | grep -v nur-packages | awk '{print $2}'); do
        echo "Updating $sub"
        (cd "$sub" && git pull origin master || git pull)
    done
}

zgenom_up() {
    zgenom update
}

################################################################################
# MAIN
echo "Starting upgrade ..."
confirm submodules    "Would you like to update the git submodules?"
confirm zgenom_up     "Would you like to update the zgenom zsh plugins?"
