#!/usr/bin/env zsh
set -euxo pipefail

if ! [[ -d /mnt/wsl ]]; then
    exit
fi

echo "Linking WSL scripts"

for d in /mnt/c/Users/*/.glaze-wm; do
    cp ~/dotfiles/wsl/glaze-wm/config.yaml $d/
done

for f in /mnt/c/Users/*/AppData/Local/Packages/Microsoft.WindowsTerminal_*/LocalState/settings.json; do
    cp ~/dotfiles/wsl/windows-terminal/settings.json $f
done
