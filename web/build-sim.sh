#!/bin/bash
# The simulator as JavaScript modules for the worker (public/sim), and the game data beside
# it (public/data, a link to data/game).
set -e
here="$(cd "$(dirname "$0")" && pwd)"
cd "$here/.."
dotnet fable src/PoEtry -o web/public/sim --extension .js 2>&1 | grep -E " error |Fable compilation finished" || true
[ -e web/public/data ] || ln -s ../../data/game web/public/data
