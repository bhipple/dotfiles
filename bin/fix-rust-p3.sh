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

while [ $(curl -s https://events.nix.ci/stats.php | jq -r .evaluator.messages.waiting) -gt 1 ]; do
    echo "Waiting for the OfBorg queue to die down before submitting another PR" | tee -a wait-log.txt
    sleep 30
done

hub pull-request --no-edit | tee hub-log

# Tell ofBorg to build it!
# Per comment from Graham C, this is unnecessary
# PR=$(grep nixpkgs hub-log | sed 's|.*pull/||')
# TOKEN=$(grep oauth_token ~/.config/hub | awk '{print $NF}')
# curl -s -H "Authorization: token $TOKEN" -XPOST \
#     -d "{\"body\": \"@grahamcofborg build $ATTR\"}" https://api.github.com/repos/NixOS/nixpkgs/issues/$PR/comments
