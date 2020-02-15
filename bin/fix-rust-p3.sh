#!/usr/bin/env zsh
set -euo pipefail
ATTR=$1

cd ~/src/nixpkgs || exit 1
git fetch origin
git checkout -b u/fix-$ATTR || git checkout u/fix-$ATTR
git reset --hard origin/master

fix-rust.sh $ATTR
git diff

# Covers almost all of them, fortunately
HAPPY="""\
$ATTR: upgrade cargo fetcher implementation and cargoSha256

Infra upgrade as part of #79975; no functional change expected.
"""

# Missed a few, alas :(
# Fortunately, they're broken on the "legacy" implementation only, so we can
# just upgrade.
SAD="""\
$ATTR: fix build by migrating off legacy fetchCargo implementation

Currently broken; see #79975 for details. Would also be fixed by #80153
eventually, but since we want to upgrade either way we might as well do so now.
"""

git commit -am"$HAPPY"
#git commit -am"$SAD"

git push $USER
hub pull-request
