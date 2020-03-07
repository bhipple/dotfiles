#!/usr/bin/env zsh
set -euo pipefail

for f in $(rg -l "Delete this on next update"); do
    if rg -q "broken = true;" $f; then continue; fi
    if rg -q "sourceRoot =" $f; then continue; fi
    if rg -q "cargoPatches =" $f; then continue; fi
    if rg -q "Cargo.lock" $f; then continue; fi
    echo $f
done
