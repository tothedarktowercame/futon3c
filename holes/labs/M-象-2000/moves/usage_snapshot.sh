#!/bin/sh
set -eu
here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
used=$("$here/../../../../scripts/zai-usage" 2>&1 | sed -nE 's/.*monthly[^0-9]*([0-9]+([.][0-9]+)?)%.*/\1/p' | head -n 1)
test -n "$used"
printf '%s monthly_used_percent=%s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$used" >> "$here/usage.log"
