#!/usr/bin/env zsh
set -euo pipefail
# Once staging hits master, run this and send the treewide PR to master. This
# should not cause any hash changes.
cd ~/src/nixpkgs || exit 1
git checkout -f master

COMMENT="Delete this on next update; see #79975 for details"

################################################################################
# Handling the special cases
for f in pkgs/applications/blockchains/parity/parity.nix pkgs/development/tools/parsing/tree-sitter/default.nix; do
    sed -i "/^  inherit cargoSha256;/a \  # $COMMENT\n  legacyCargoFetcher = true;\n" $f
done

sed -i '/cargoSha256/i \    legacyCargoFetcher = true;' pkgs/games/ja2-stracciatella/default.nix;

################################################################################
# Handling the rest of them
fix() {
    # Change the default at the infra level to false!
    sed -i 's|^, legacyCargoFetcher.*|, legacyCargoFetcher ? false|' pkgs/build-support/rust/default.nix

    for f in $(rg -l 'cargoSha256 = "' **/*.nix | grep -Ev '(parity/beta.nix|parity/default.nix|tree-sitter/default.nix|ja2-stracciatella/default.nix)'); do
        # Leave the opt-outs for the first big wave, then delete them in another follow up (there are only 8).
        if rg -q 'legacyCargoFetcher = false;' $f; then
            continue
        fi

        sed -i "/cargoSha256 = \"/i \  # $COMMENT\n  legacyCargoFetcher = true;\n" $f
    done
}

fix
