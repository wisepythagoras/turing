#!/bin/bash

# This command should count all source code.
find . -regextype sed -regex '.*\.\(zig\|loom\)$' \
    -type f \
    -not -name 'main.js' \
    -not -path './zig-out/*' \
    -not -path './.zig-cache/*' \
    -exec wc -l {} +
