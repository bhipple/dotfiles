#!/usr/bin/env zsh
set -euo pipefail
ATTR=$1

cd ~/src/nixpkgs || exit 1
git fetch origin
git checkout -b u/fix-$ATTR || git checkout u/fix-$ATTR
git reset --hard origin/master

fix-rust.sh $ATTR
git diff

MSG="""\
$ATTR: upgrade cargo fetcher implementation and cargoSha256

Infra upgrade as part of #79975; no functional change expected.
"""

git commit -am"$MSG"

git push $USER
hub pull-request
