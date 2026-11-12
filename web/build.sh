#!/bin/bash
# The front end as static files in dist/: ./build.sh, then serve dist/.
set -e
here="$(cd "$(dirname "$0")" && pwd)"
lib="${TOYLIB:-$here/../../fablecube-toy/lib}"
[ -e "$here/node_modules" ] || ln -s "$lib/node_modules" "$here/node_modules"
"$here/build-sim.sh"
WORK="$here" PROJECT=Poetry.Web.fsproj APP_SOURCES="$here/App.fs" "$lib/build.sh"
