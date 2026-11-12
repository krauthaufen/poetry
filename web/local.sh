#!/bin/bash
# After build.sh, for looking at the page on this machine: a few corpus characters as
# builds/b<nnn>.json and the private builds as links (nothing is copied out of
# builds-private; dist is not in the repository).
here="$(cd "$(dirname "$0")" && pwd)"
mkdir -p "$here/dist/builds"
for n in 011 024 047 121; do cp "$here"/../corpus/$n-*.json "$here/dist/builds/b$n.json" 2>/dev/null; done
for f in "$here"/../builds-private/*.json; do [ -e "$f" ] && ln -sfn "$f" "$here/dist/builds/$(basename "$f")"; done
ls "$here/dist/builds"
