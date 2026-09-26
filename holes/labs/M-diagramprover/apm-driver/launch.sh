#!/bin/bash
# The ONLY sanctioned way to launch the APM driver. systemd-run gives a
# durable parent (survives ssh logout; nohup does NOT survive session
# cgroup cleanup — learned 2026-08-04 night) and the fixed unit name is a
# natural mutex: a second launch fails instead of racing the first.
# Usage: ./launch.sh --once [--problem <id>|--new]   or   ./launch.sh --continuous
set -euo pipefail
cd "$(dirname "$0")"
exec systemd-run --user --unit apm-driver \
  --nice="${APM_NICE:-10}" \
  --setenv=PATH="$HOME/.elan/bin:/usr/bin:/bin" \
  --setenv=APM_ZAI_SEAT="${APM_ZAI_SEAT:-zai-1}" \
  --setenv=APM_CODEX_SEAT="${APM_CODEX_SEAT:-codex-12}" \
  --working-directory="$(pwd)" \
  python3 run.py "$@"
