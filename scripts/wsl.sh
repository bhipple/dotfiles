#!/usr/bin/env zsh
set -euxo pipefail

if ! [[ -d /mnt/wsl ]]; then
    exit
fi

echo "Linking WSL scripts"

if [[ -d /mnt/c/Users/bhipp/.glaze-wm ]]; then
    if [[ -L /mnt/c/Users/bhipp/.glaze-wm/config.yaml ]]; then continue; fi
    rm -f /mnt/c/Users/bhipp/.glaze-wm/config.yaml
    ln -s ~/dotfiles/wsl/glaze-wm/config.yaml /mnt/c/Users/bhipp/.glaze-wm/config.yaml 
fi
