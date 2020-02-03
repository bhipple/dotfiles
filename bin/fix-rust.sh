#!/usr/bin/env zsh
set -euo pipefail

# Helper script for migrating a NixPkgs Rust pkg to the new cargoSha256 verification.
if [ -z "$1" ]; then
    echo "USAGE: $0 <attribute>"
    echo "EXAMPLE: $0 ripgrep"
    exit 1
fi

cd ~/src/nixpkgs || exit 1

ATTR=$1
FNAME=$(EDITOR=ls nix edit -f . $ATTR)

section() {
    echo "********************************************************************************"
    echo "$ATTR: $1"
    echo "********************************************************************************"
}

main() {
    if ! grep -q legacyCargoFetcher $FNAME; then
        echo "Need to add legacyCargoFetcher = false; to $FNAME"
        exit 1
    fi

    section "Nuking cargoSha256 reference for $FNAME, then rebuilding"
    sed -i 's|cargoSha256.*|cargoSha256 = "0000000000000000000000000000000000000000000000000000";|' $FNAME;
    nix-build -A $ATTR 2>&1 | tee /tmp/nix-rust-logfile-$ATTR || true
    actual=$(grep 'got:.*sha256:.*' /tmp/nix-rust-logfile-$ATTR | cut -d':' -f3-)
    echo "Build of $ATTR failed; cargoSha256 should be $actual"
    sed -i "s|cargoSha256.*|cargoSha256 = \"$actual\";|" $FNAME

    section "Rebuilding with updated hash, expecting a pass:"
    nix-build -A $ATTR || exit 1

    section "Finished successfully!"
}

main
