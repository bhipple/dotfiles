#!/usr/bin/env bash
set -euxo pipefail
cd ~/src/nixpkgs || exit 1

ATTRS=(
    # All of the new fetchCargo packages
    broot
    fido2luks
    gitAndTools.git-workspace
    hexdino
    mdsh
    nix-du
    tre-command
    wagyu
    # Plus a spot-check of older ones
    cargo
    exa
    ripgrep
    rustc
    rx
    # These ones have some special inherit cargoSha256 abstraction to note
    # parity FIXME
    tree-sitter
)

# Re-run the fix-rust-p2 script and verify no hash changes
rm -rf old new
rm -f result* || true
git checkout -f feature/fetchcargo-tar-gz || true
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
