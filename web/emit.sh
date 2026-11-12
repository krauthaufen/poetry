#!/bin/bash
# Only FableCube's step (about 12 s): writes View.g.fs, for looking at what the view became.
here="$(cd "$(dirname "$0")" && pwd)"
lib="${TOYLIB:-$here/../../fablecube-toy/lib}"
export WORK="$here" PROJECT=Poetry.Web.fsproj APP_SOURCES="$here/Shaders.fs $here/App.fs"
source "$lib/setup.sh"
dotnet "$compiler" emit Generated.View Entry.frame View.g.fs --split ${SPLIT:-8} --ref "$refs" $sources > .emit.log 2>&1 || { grep -v "^   at " .emit.log | head -20; exit 1; }
grep -E "^(left out|runs as compiled)" .emit.log || true
# is the render control given cells, or constants (then it is rebuilt whenever they change)?
grep -o "x_[0-9]* : (Pure.Camera FSharp.Data.Adaptive.aval) = ([A-Za-z.]*" View.g.fs | sed 's/.*= (/camera: /' | sort | uniq -c
