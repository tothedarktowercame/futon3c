#!/usr/bin/env bash
# Sourceable resource-class launcher for transient Futon workload scopes.

futon_scope_usage() {
  echo "usage: run-agent-scope {cr|cx|cz|click|test} [label] -- command [args...]" >&2
}

futon_scope_sanitize() {
  printf '%s' "$1" | tr -c '[:alnum:]_.-' '-'
}

futon_scope_limits() {
  case "$1" in
    cr)    printf '%s\n' 6G 8G 256 ;;
    cx)    printf '%s\n' 4G 6G 256 ;;
    cz)    printf '%s\n' 3G 4G 192 ;;
    click) printf '%s\n' 3G 4G 256 ;;
    test)  printf '%s\n' 4G 6G 512 ;;
    *) return 1 ;;
  esac
}

futon_scope_pane_label() {
  local pane="${TMUX_PANE:-none}"
  if [[ "$pane" != "none" ]] && command -v tmux >/dev/null 2>&1; then
    tmux display-message -p -t "$pane" '#{session_name}:#{window_name}.#{pane_index}' \
      2>/dev/null || printf '%s' "$pane"
  else
    printf '%s' "$pane"
  fi
}

futon_run_scoped() {
  local class="${1:-}" label high max tasks pane unit limits
  shift || true
  if ! limits="$(futon_scope_limits "$class")"; then
    futon_scope_usage
    return 2
  fi
  mapfile -t _futon_limits <<<"$limits"
  high="${_futon_limits[0]}"
  max="${_futon_limits[1]}"
  tasks="${_futon_limits[2]}"

  label="$class"
  if [[ "${1:-}" != "--" ]]; then
    label="${1:-$class}"
    shift || true
  fi
  if [[ "${1:-}" != "--" ]]; then
    futon_scope_usage
    return 2
  fi
  shift
  if [[ $# -eq 0 ]]; then
    futon_scope_usage
    return 2
  fi

  pane="$(futon_scope_pane_label)"
  unit="futon-agent-$(futon_scope_sanitize "$class")-$(futon_scope_sanitize "$label")-$(date +%s)-$$"
  exec systemd-run --user --scope --quiet --collect \
    --unit="$unit" --slice=futon-agents.slice \
    --description="Futon $class workload label=$(futon_scope_sanitize "$label") pane=$pane" \
    -p CPUAccounting=yes -p MemoryAccounting=yes -p IOAccounting=yes \
    -p TasksAccounting=yes -p "MemoryHigh=$high" -p "MemoryMax=$max" \
    -p MemorySwapMax=1G -p "TasksMax=$tasks" -p OOMPolicy=kill \
    -p ManagedOOMMemoryPressure=kill -p ManagedOOMMemoryPressureLimit=60% \
    -- "$@"
}
