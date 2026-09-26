#!/usr/bin/env bash
set -euo pipefail

prefix="futon-lifecycle-test-$$"
desc_unit="${prefix}-descendants"
memory_unit="${prefix}-memory"
tmp_dir="$(mktemp -d)"
desc_runner=""

cleanup() {
  systemctl --user stop "${desc_unit}.scope" "${memory_unit}.scope" >/dev/null 2>&1 || true
  systemctl --user reset-failed "${desc_unit}.scope" "${memory_unit}.scope" >/dev/null 2>&1 || true
  [[ -z "$desc_runner" ]] || kill "$desc_runner" >/dev/null 2>&1 || true
  rm -rf "$tmp_dir"
}
trap cleanup EXIT INT TERM

pass() { printf 'PASS: %s\n' "$1"; }
fail() { printf 'FAIL: %s\n' "$1" >&2; exit 1; }

serving_pids() {
  pgrep -f '[c]lojure\.main -m futon3c\.dev($| )' || true
}

wait_for() {
  local description="$1" command="$2" attempts=100
  while (( attempts > 0 )); do
    if eval "$command"; then return 0; fi
    sleep 0.05
    attempts=$((attempts - 1))
  done
  fail "timeout waiting for $description"
}

initial_pids="$(serving_pids)"
initial_count="$(grep -c . <<<"$initial_pids" || true)"
[[ "$initial_count" -eq 1 ]] || fail "precondition: expected one serving futon3c JVM, found $initial_count"
agency_pid="$initial_pids"

# A setsid child changes session but cannot escape its systemd cgroup.
systemd-run --user --scope --quiet --unit="$desc_unit" \
  -p KillMode=control-group -p MemoryAccounting=yes \
  bash -c 'setsid sleep 300 >/dev/null 2>&1 & echo $! > "$1"; wait' \
  _ "${tmp_dir}/child.pid" &
desc_runner=$!
wait_for "detached child pid" "[[ -s '${tmp_dir}/child.pid' ]]"
detached_pid="$(<"${tmp_dir}/child.pid")"
kill -0 "$detached_pid" 2>/dev/null || fail "detached child did not start"
systemctl --user stop "${desc_unit}.scope"
wait_for "old detached child death" "! kill -0 '$detached_pid' 2>/dev/null"
wait "$desc_runner" 2>/dev/null || true
desc_runner=""
# Reusing the unit name proves the prior scope has fully drained before respawn.
wait_for "old scope collection" \
  "[[ \$(systemctl --user show '${desc_unit}.scope' -p LoadState --value 2>/dev/null) == not-found ]]"
systemd-run --user --scope --quiet --unit="$desc_unit" bash -c 'true'
pass "detached children cannot survive scope respawn"

# MemoryMax is deliberately tiny. The allocator must die inside its own scope.
set +e
timeout 15 systemd-run --user --scope --quiet --unit="$memory_unit" \
  -p MemoryAccounting=yes -p MemoryMax=32M \
  -p ManagedOOMMemoryPressure=kill -p ManagedOOMMemoryPressureLimit=60% \
  -p MemorySwapMax=0 -p OOMPolicy=kill \
  python3 -c 'chunks=[]; exec("while True:\n chunks.append(bytearray(1024 * 1024))")'
memory_rc=$?
set -e
[[ "$memory_rc" -eq 137 ]] \
  || fail "memory workload was not cgroup-killed as expected (exit $memory_rc)"
kill -0 "$agency_pid" 2>/dev/null || fail "Agency JVM died with the memory test scope"
pass "memory pressure kills the workload scope without killing Agency"

final_pids="$(serving_pids)"
final_count="$(grep -c . <<<"$final_pids" || true)"
[[ "$final_count" -eq 1 && "$final_pids" == "$agency_pid" ]] \
  || fail "serving JVM changed: before=$agency_pid after=${final_pids:-none}"
pass "one and only one serving futon3c JVM remains (pid $agency_pid)"
