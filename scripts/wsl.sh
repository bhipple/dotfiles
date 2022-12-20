#!/usr/bin/env zsh
set -euxo pipefail

if ! [[ -d /mnt/wsl ]]; then
    exit
fi

echo "Linking WSL scripts"

for f in /mnt/c/Users/*/.glaze-wm/config.yaml; do
    cp ~/dotfiles/wsl/glaze-wm/config.yaml $f
done

for f in /mnt/c/Users/*/AppData/Local/Packages/Microsoft.WindowsTerminal_*/LocalState/settings.json; do
    cp ~/dotfiles/wsl/windows-terminal/settings.json $f
done
