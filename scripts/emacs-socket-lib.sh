#!/usr/bin/env bash

futon_emacs_runtime_dir() {
  printf '%s\n' "${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/emacs"
}

futon_emacs_socket_path() {
  local socket_name="${1:-}"
  [[ -n "${socket_name}" ]] || return 1
  printf '%s/%s\n' "$(futon_emacs_runtime_dir)" "${socket_name}"
}

futon_emacs_socket_exists() {
  local socket_name="${1:-}"
  [[ -n "${socket_name}" ]] || return 1
  [[ -S "$(futon_emacs_socket_path "${socket_name}")" ]]
}

futon_emacs_preference_mode() {
  local mode="${FUTON3C_EMACS_PREFERENCE:-interactive}"
  case "${mode}" in
    interactive|headless)
      printf '%s\n' "${mode}"
      ;;
    *)
      printf '%s\n' "interactive"
      ;;
  esac
}

futon_find_ancestor_emacs_socket() {
  # Walk up the process tree and stop at the FIRST Emacs ancestor.
  # If it is a daemon (`--daemon=NAME`), return NAME. If it is a
  # non-daemon Emacs (e.g. a GUI server started without `--daemon=`),
  # return failure so the caller falls through to the `server` check
  # rather than walking past it into a parent daemon's process tree.
  local pid="$$"
  while [[ "${pid}" -gt 1 ]]; do
    local stat_file="/proc/${pid}/stat"
    [[ -f "${stat_file}" ]] || break
    local ppid
    ppid="$(sed 's/.*) //' "${stat_file}" 2>/dev/null | awk '{print $2}')" || break
    [[ -n "${ppid:-}" && "${ppid}" -gt 0 ]] 2>/dev/null || break
    local cmdline="/proc/${ppid}/cmdline"
    if [[ -f "${cmdline}" ]]; then
      local cmd argv0
      cmd="$(tr '\0' ' ' < "${cmdline}" 2>/dev/null)" || true
      argv0="${cmd%% *}"
      if [[ "${argv0##*/}" == "emacs" ]]; then
        if [[ "${cmd}" =~ --daemon=([^ ]+) ]]; then
          printf '%s\n' "${BASH_REMATCH[1]}"
          return 0
        fi
        return 1
      fi
    fi
    pid="${ppid}"
  done
  return 1
}

futon_list_emacs_sockets() {
  local runtime_dir
  runtime_dir="$(futon_emacs_runtime_dir)"
  [[ -d "${runtime_dir}" ]] || return 0
  local path
  for path in "${runtime_dir}"/*; do
    [[ -S "${path}" ]] || continue
    basename "${path}"
  done
}

futon_preferred_emacs_socket() {
  local socket_name ops_socket ancestor_socket runtime_dir mode
  mode="$(futon_emacs_preference_mode)"

  if [[ -n "${FUTON3C_EMACS_SOCKET:-}" ]] && futon_emacs_socket_exists "${FUTON3C_EMACS_SOCKET}"; then
    printf '%s\n' "${FUTON3C_EMACS_SOCKET}"
    return 0
  fi

  ops_socket="${FUTON_OPS_SERVER:-futon-ops}"

  if [[ "${mode}" == "headless" ]]; then
    if [[ -n "${FUTON3C_HUD_SOCKET:-}" ]] && futon_emacs_socket_exists "${FUTON3C_HUD_SOCKET}"; then
      printf '%s\n' "${FUTON3C_HUD_SOCKET}"
      return 0
    fi

    if futon_emacs_socket_exists "${ops_socket}"; then
      printf '%s\n' "${ops_socket}"
      return 0
    fi
  fi

  if [[ -n "${EMACS_SOCKET_NAME:-}" ]] && futon_emacs_socket_exists "${EMACS_SOCKET_NAME}"; then
    printf '%s\n' "${EMACS_SOCKET_NAME}"
    return 0
  fi

  ancestor_socket="$(futon_find_ancestor_emacs_socket || true)"
  if [[ -n "${ancestor_socket}" ]] && futon_emacs_socket_exists "${ancestor_socket}"; then
    printf '%s\n' "${ancestor_socket}"
    return 0
  fi

  if futon_emacs_socket_exists "server"; then
    printf '%s\n' "server"
    return 0
  fi

  if [[ "${mode}" == "interactive" ]]; then
    if [[ -n "${FUTON3C_HUD_SOCKET:-}" ]] && futon_emacs_socket_exists "${FUTON3C_HUD_SOCKET}"; then
      printf '%s\n' "${FUTON3C_HUD_SOCKET}"
      return 0
    fi

    if futon_emacs_socket_exists "${ops_socket}"; then
      printf '%s\n' "${ops_socket}"
      return 0
    fi
  fi

  for socket_name in futon; do
    if futon_emacs_socket_exists "${socket_name}"; then
      printf '%s\n' "${socket_name}"
      return 0
    fi
  done

  runtime_dir="$(futon_emacs_runtime_dir)"
  if [[ -d "${runtime_dir}" ]]; then
    for socket_name in $(futon_list_emacs_sockets); do
      printf '%s\n' "${socket_name}"
      return 0
    done
  fi

  return 1
}

futon_start_preferred_emacs_daemon() {
  local ops_socket="${FUTON_OPS_SERVER:-futon-ops}"

  if futon_emacs_socket_exists "${ops_socket}"; then
    printf '%s\n' "${ops_socket}"
    return 0
  fi

  if [[ -x "${HOME}/bin/fopsd" ]]; then
    FUTON_OPS_SERVER="${ops_socket}" "${HOME}/bin/fopsd" >/dev/null 2>&1 || return 1
    if futon_emacs_socket_exists "${ops_socket}"; then
      printf '%s\n' "${ops_socket}"
      return 0
    fi
  fi

  if futon_emacs_socket_exists "futon"; then
    printf '%s\n' "futon"
    return 0
  fi

  if command -v emacs >/dev/null 2>&1; then
    emacs --daemon=futon >/dev/null 2>&1 || return 1
    if futon_emacs_socket_exists "futon"; then
      printf '%s\n' "futon"
      return 0
    fi
  fi

  return 1
}
