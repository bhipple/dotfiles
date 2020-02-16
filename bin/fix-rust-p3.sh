#!/usr/bin/env zsh
set -euo pipefail
ATTR=$1

git fetch origin
git checkout -b u/rust-cargo-$ATTR || git checkout u/rust-cargo-$ATTR
git reset --hard origin/master

fix-rust.sh $ATTR
git diff

MSG="""\
$ATTR: upgrade cargo fetcher and cargoSha256

Infra upgrade as part of #79975; ran \`nixpkgs-review wip\` successfully.
"""

git commit -am"$MSG"

################################################################################
# Auto-PR via hub
git push -f $USER
rm -f hub-log

# TODO: Before running again, rate limit this with something like:
# while [ $(curl https://events.nix.ci/stats.php | jq -r .evaluator.messages.waiting) -gt 1 ]; do sleep 30; done
hub pull-request --no-edit | tee hub-log

PR=$(grep nixpkgs hub-log | sed 's|.*pull/||')
TOKEN=$(grep oauth_token ~/.config/hub | awk '{print $NF}')

# Tell ofBorg to build it!
# Per comment from Graham C, this is unnecessary
# curl -s -H "Authorization: token $TOKEN" -XPOST \
#     -d "{\"body\": \"@grahamcofborg build $ATTR\"}" https://api.github.com/repos/NixOS/nixpkgs/issues/$PR/comments
