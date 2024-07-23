#!/usr/bin/env zsh
set -euo pipefail

add_dir() {
    if ! grep -q $1 ~/.local/share/nvim/project_nvim/project_history; then
        echo "Adding $1"
        echo $1 >> ~/.local/share/nvim/project_nvim/project_history
    fi
}

for d in $(realpath ~/git/* ~/git/*/* ~/dotfiles ~/dotfiles_local ~/ledger ~/personal /etc/nixos); do
    if [ -d $d/.git ]; then
        add_dir $d
    fi
done
