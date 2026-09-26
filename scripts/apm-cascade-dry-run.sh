#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 3 || $# -gt 4 ]]; then
  echo "usage: $0 SNAPSHOT-PATH CAP OUT-PATH [ROUTE,...]" >&2
  exit 2
fi

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_dir="$(cd "${script_dir}/.." && pwd)"
printf -v exact_command '%q ' "$0" "$@"

cd "${repo_dir}"
APM_CASCADE_DRY_RUN_COMMAND="${exact_command% }" \
  clojure -Sdeps '{:paths ["src" "resources" "library" "dev"]}' \
  -M -m futon3c.apm.cascade-dry-run "$@"
