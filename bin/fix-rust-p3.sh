#!/usr/bin/env zsh
set -euo pipefail
ATTR=$1

cd ~/src/nixpkgs || exit 1
git fetch origin
git checkout -b u/rust-cargo-$ATTR || git checkout u/rust-cargo-$ATTR
git reset --hard origin/master

fix-rust.sh $ATTR
git diff

# Covers almost all of them, fortunately
HAPPY="""\
$ATTR: upgrade cargo fetcher and cargoSha256

Infra upgrade as part of #79975; no functional change expected.
"""

# Missed a few, alas :(
# Fortunately, they're broken on the "legacy" implementation only, so we can
# just upgrade.
SAD="""\
$ATTR: fix build by migrating off legacy fetchCargo

Currently broken; see #79975 for details. Would also be fixed by #80153
eventually, but since we want to upgrade either way we might as well do so now.

https://https://hydra.nixos.org/job/nixpkgs/trunk/$ATTR.x86_64-linux
"""

git commit -am"$HAPPY"
#git commit -am"$SAD"

################################################################################
# Auto-PR via hub
git push $USER
rm -f hub-log
hub pull-request | tee hub-log

PR=$(grep nixpkgs hub-log | sed 's|.*pull/||')
TOKEN=$(grep oauth_token ~/.config/hub | awk '{print $NF}')

# Tell ofBorg to build it!
curl -s -H "Authorization: token $TOKEN" -XPOST \
    -d "{\"body\": \"@grahamcofborg build $ATTR\"}" https://api.github.com/repos/NixOS/nixpkgs/issues/$PR/comments
