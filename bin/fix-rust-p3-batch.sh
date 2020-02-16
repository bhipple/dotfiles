#!/usr/bin/env bash
set -euo pipefail
git checkout -f master
git pull origin master

OPEN=$(curl -s https://github.com/NixOS/nixpkgs/pulls/bhipple)

ATTRS=""
CANDIDATES=$(rg -l 'Delete this on next' | rg -o '[^/]*/default.nix' | sed 's|/default.nix||' | head -50)
for candidate in $CANDIDATES; do
    if echo "$OPEN" | grep -q "$candidate: "; then
        echo "PR already open for $candidate"
    elif grep "$candidate\$" failure-log; then
         echo "Previous failure detected"
    elif hydra $candidate | grep Succeeded; then
        echo "Added $candidate to list"
        ATTRS="$ATTRS $candidate"
    else
        echo "$candidate is failing on master or unable to be found on Hydra; skipping for now"
        echo "[$(date)] Failing upstream on Hydra or other issue: $candidate" >> failure-log
    fi
done

echo "Processing $ATTRS"

for attr in $ATTRS; do
    fix-rust-p3.sh $attr || echo "$attr failed, go take a look!"
    echo "[$(date)] Failed to build $attr" >> failure-log
done

echo "DONE"
