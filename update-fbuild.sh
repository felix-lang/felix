#!/bin/sh

# TO UPDATE FBUILD:
# - Run this script.
#   - If it fails, run `git merge --abort`, followed by running this script again
#     with --recursive.
# - Commit the changes with something like `git commit -m "Update Fbuild"`.

set -e
git fetch https://github.com/felix-lang/fbuild.git
if [ "$1" = "--recursive" ]; then
    git merge -X subtree=fbuild --squash fbuild/master
else
    git merge -s subtree --squash fbuild/master
fi
