#!/usr/bin/env zsh
set -euo pipefail
# Once staging hits master, run this and send the treewide PR to master. This
# should not cause any hash changes.
cd ~/src/nixpkgs || exit 1
git checkout -f feature/fetchcargo-tar-gz

fix() {
    sed -i 's|^, legacyCargoFetcher.*|, legacyCargoFetcher ? false|' pkgs/build-support/rust/default.nix

    for f in $(rg -l 'cargoSha256 = "' **/*.nix); do
        # Leave the opt-outs for the first big wave, then delete them in another follow up (there are only 8).
        if rg -q 'legacyCargoFetcher = false;' $f; then
            continue
        fi

        sed -i '/cargoSha256 = "/i   # Delete this on next update\n  legacyCargoFetcher = true;\n' $f
    done
}

fix
