#!/usr/bin/env sh
# Creates ctags after commits
set -e
PATH="/usr/local/bin:$PATH"
dir="$(git rev-parse --git-dir)"
trap 'rm -f "$dir/$$.tags"' EXIT

# CTags
git ls-files | \
          ctags - -f"$dir/$$.tags"
          mv "$dir/$$.tags" "$dir/tags"
