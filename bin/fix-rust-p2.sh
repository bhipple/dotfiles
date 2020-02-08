#!/usr/bin/env zsh
set -euo pipefail
cd ~/src/nixpkgs || exit 1
git checkout -f feature/fetchcargo-tar-gz

sed -i 's|^, legacyCargoFetcher.*|, legacyCargoFetcher ? false|' pkgs/build-support/rust/default.nix

for f in $(rg -l 'cargoSha256 = "' **/*.nix); do
    if rg -q 'legacyCargoFetcher = false;' $f; then
        continue
    fi

    sed -i '/cargoSha256 = "/i   # Delete this on next update\n  legacyCargoFetcher = true;\n' $f
done
