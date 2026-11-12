#!/bin/bash
# after emit.sh: how many inputs is the render control's one big cell made of (0: it is taken apart properly)
./emit.sh >/dev/null 2>&1
n=$(grep -c "Pure.Scene< App.Msg > list)))) [0-9]*) values_" View.g.fs)
echo "scene lists zipped into one cell: $n; camera: $(grep -o 'Pure.Camera FSharp.Data.Adaptive.aval) = ([A-Za-z.]*' View.g.fs | sed 's/.*= (//' | sort -u | tr '\n' ' ')"
