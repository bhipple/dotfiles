#!/usr/bin/env zsh
set -o pipefail

# Helper script for migrating a NixPkgs Rust pkg to the new cargoSha256 verification.
if [ -z "$1" ]; then
    echo "USAGE: $0 <attribute>"
    exit 1
fi

cd ~/src/nixpkgs || exit 1
ATTR=$1
FNAME=$(EDITOR=ls nix edit -f . $ATTR)

main() {
    if ! grep -q verifyCargoDeps $FNAME; then
        echo "Need to add verifyCargoDeps = true; to $FNAME"
        exit 1
    fi

    if ! nix-build -A $ATTR 2>&1 | tee /tmp/nix-rust-logfile; then
        actual=$(grep 'got:.*sha256:.*' /tmp/nix-rust-logfile | cut -d':' -f3-)
        echo "Build failed; cargoSha256 should be $actual"
        sed -i "s|cargoSha256.*|cargoSha256 = \"$actual\";|" $FNAME

        echo "Trying again..."
        nix-build -A $ATTR || exit 1
    fi

    echo "Finished successfully!"
}

main
