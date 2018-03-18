#!/bin/sh
# Creates ctags, gtags, and haskell fast-tags
set -e
PATH="/usr/local/bin:$PATH"
dir="$(git rev-parse --git-dir)"
trap 'rm -f "$dir/$$.tags"' EXIT

# CTags
git ls-files | \
          ctags --tag-relative -L - -f"$dir/$$.tags" --languages=-javascript,sql
          mv "$dir/$$.tags" "$dir/tags"

# GNU Global Tags
git ls-files | \
    gtags --file=- --skip-unreadable "$dir/$$.gtags"
mv "$dir/$$.gtags/*" "$dir/.."

# Haskell Fast-Tags
if hash fast-tags > /dev/null 2>&1; then
    git ls-files '*.hs' | xargs fast-tags -o .hs-tags
fi
