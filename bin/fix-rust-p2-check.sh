#!/usr/bin/env bash
set -euxo pipefail
git clean -ffdx
git checkout -f master

ATTRS=(
    # The fetchCargo packages that were already using the new implementation
    broot
    fido2luks
    gitAndTools.git-workspace
    hexdino
    mdsh
    nix-du
    tre-command
    wagyu
    # Plus a spot-check of ones using the old implementation
    cargo
    exa
    nix-index
    ripgrep
    rustc
    rx
    termplay
    texlab
    # These ones have some special conditions to note accounted for in the sed
    ja2-stracciatella
    parity
    tree-sitter
    # This one has a cargo build hook that needs write access to the vendor dir!
    spotifyd
)

# Re-run the fix-rust-p2 script and verify no hash changes
mkdir -p old new
nix build -L -f . ${ATTRS[@]}

mv result* old/
fix-rust-p2.sh
nix build -L -f . ${ATTRS[@]}
mv result* new/

set +x
tree old new
echo "********************************************************************************"
echo "Diff"
if ! diff -r old new; then
    echo "There's a diff!"
else
    echo "No hash diff generated. Success!"
fi
