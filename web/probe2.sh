#!/bin/bash
./emit.sh >/dev/null 2>&1
echo "render control built as a plain value: $(grep -c 'let x_[0-9]* : Pure.Node< App.Msg > = (Pure.Node.\(Element.*Pure.Node.\)\?RenderView' View.g.fs)"
