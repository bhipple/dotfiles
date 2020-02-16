#!/usr/bin/env bash
set -euo pipefail

SELECTED=$(rg -l 'Delete this on next' | rg -o '[^/]*/default.nix' | sed 's|/default.nix||'  | head -100 | tail -80 | head -20)
echo "Processing $SELECTED"

git checkout -- .
git checkout -b upgrade/rust-batch || git checkout upgrade/rust-batch

for ATTR in $SELECTED; do
    git checkout -- .
    fix-rust.sh $ATTR || continue
    git diff --exit-code || continue

MSG="""\
$ATTR: upgrade cargo fetcher and cargoSha256

Infra upgrade as part of #79975; no functional change expected.
"""

    git commit -am"$MSG"

    git show
done
