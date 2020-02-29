#!/usr/bin/env zsh
set -euo pipefail
ATTR=$1

git fetch origin
git checkout -b u/rust-cargo-$ATTR || git checkout u/rust-cargo-$ATTR
git reset --hard origin/master

fix-rust.sh $ATTR
git diff

SUBJECT="$ATTR: upgrade cargo fetcher and cargoSha256"
MSG="""\
$SUBJECT

Infra upgrade as part of #79975; no functional change expected.
"""

git commit -am"$SUBJECT"

################################################################################
# Auto-PR via hub
git push -f $USER
rm -f hub-log

while [ $(curl -s https://events.nix.ci/stats.php | jq -r .evaluator.messages.waiting) -gt 1 ]; do
    echo "Waiting for the OfBorg queue to die down before submitting another PR" | tee -a wait-log.txt
    sleep 30
done

hub pull-request -m"$MSG" --no-edit
