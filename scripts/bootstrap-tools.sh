#!/usr/bin/env bash
set -euo pipefail

# Futon3c tool bootstrap for superpod / module-based clusters.
#
# Installs:
# - Babashka (bb) into:   .tools/bin/bb
# - Clojure CLI tools into: .tools/clojure/ (bin/clojure, bin/clj, lib/...)
#
# This avoids relying on cluster module availability for clojure/bb.

usage() {
  cat <<'EOF'
Usage: scripts/bootstrap-tools.sh [options]

Options:
  --prefix DIR             Install under DIR (default: <repo>/.tools)
  --bb-version VER         Babashka version (default: latest)
  --cljtools-version VER   Clojure CLI tools version tag (default: latest)
  --no-bb                  Skip babashka install
  --no-clojure             Skip clojure tools install
  --force                  Reinstall even if already present
  -h, --help               Show this help

Notes:
  - Run on a login node with outbound network if compute nodes are restricted.
  - After install, you can run:
      ./bb scripts/alfworld_runner.clj 10 verbose
      make dev
EOF
}

repo_root="$(
  cd "$(dirname "${BASH_SOURCE[0]}")/.." >/dev/null 2>&1
  pwd
)"

prefix="${repo_root}/.tools"
bb_version="latest"
cljtools_version="latest"
install_bb=true
install_clojure=true
force=false

while [[ $# -gt 0 ]]; do
  case "$1" in
    --prefix)
      prefix="${2:-}"
      shift 2
      ;;
    --bb-version)
      bb_version="${2:-}"
      shift 2
      ;;
    --cljtools-version)
      cljtools_version="${2:-}"
      shift 2
      ;;
    --no-bb)
      install_bb=false
      shift
      ;;
    --no-clojure)
      install_clojure=false
      shift
      ;;
    --force)
      force=true
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      echo >&2
      usage >&2
      exit 2
      ;;
  esac
done

need_cmd() {
  local cmd="$1"
  if ! command -v "$cmd" >/dev/null 2>&1; then
    echo "Missing required command: $cmd" >&2
    exit 1
  fi
}

need_cmd bash
need_cmd curl
need_cmd tar
need_cmd sha256sum
need_cmd python3

mkdir -p "${prefix}/bin" "${prefix}/clojure"

detect_arch() {
  local arch
  arch="$(uname -m)"
  case "$arch" in
    x86_64) echo "amd64" ;;
    aarch64|arm64) echo "aarch64" ;;
    *)
      echo "Unsupported arch: ${arch}" >&2
      exit 1
      ;;
  esac
}

github_latest_tag() {
  local repo="$1" # e.g. babashka/babashka
  curl -fsSL "https://api.github.com/repos/${repo}/releases/latest" \
    | python3 -c 'import sys,json; print(json.load(sys.stdin)["tag_name"])'
}

install_babashka() {
  local arch ver tag url tmp tgz bb_bin current
  arch="$(detect_arch)"
  bb_bin="${prefix}/bin/bb"

  if [[ "${bb_version}" == "latest" ]]; then
    tag="$(github_latest_tag babashka/babashka)"
    ver="${tag#v}"
  else
    ver="${bb_version#v}"
    tag="v${ver}"
  fi

  if [[ -x "${bb_bin}" && "${force}" == "false" ]]; then
    current="$("${bb_bin}" --version 2>/dev/null | awk '{print $2}' | sed 's/^v//')"
    if [[ "${current}" == "${ver}" ]]; then
      echo "[bootstrap] bb already installed: ${bb_bin} (v${current})"
      return 0
    fi
  fi

  case "$arch" in
    amd64)   url="https://github.com/babashka/babashka/releases/download/${tag}/babashka-${ver}-linux-amd64-static.tar.gz" ;;
    aarch64) url="https://github.com/babashka/babashka/releases/download/${tag}/babashka-${ver}-linux-aarch64-static.tar.gz" ;;
    *) echo "Unsupported babashka arch: ${arch}" >&2; exit 1 ;;
  esac

  tmp="$(mktemp -d)"
  tgz="${tmp}/bb.tgz"
  echo "[bootstrap] Downloading babashka ${tag}..."
  curl -fL -o "${tgz}" "${url}"
  tar -xzf "${tgz}" -C "${prefix}/bin" bb
  chmod +x "${bb_bin}"
  echo "[bootstrap] Installed bb: ${bb_bin} ($("${bb_bin}" --version))"
}

install_clojure_tools() {
  local clj_prefix clj_bin ver tag script_url tmp installer
  clj_prefix="${prefix}/clojure"
  clj_bin="${clj_prefix}/bin/clojure"

  if [[ "${cljtools_version}" == "latest" ]]; then
    # Tags are like "1.12.4.1602"
    ver="$(github_latest_tag clojure/brew-install)"
  else
    ver="${cljtools_version}"
  fi

  if [[ -x "${clj_bin}" && "${force}" == "false" ]]; then
    echo "[bootstrap] clojure already installed: ${clj_bin}"
    return 0
  fi

  script_url="https://download.clojure.org/install/linux-install-${ver}.sh"
  tmp="$(mktemp -d)"
  installer="${tmp}/linux-install-clj.sh"

  echo "[bootstrap] Downloading Clojure CLI tools ${ver} installer..."
  curl -fsSL -o "${installer}" "${script_url}"
  chmod +x "${installer}"

  echo "[bootstrap] Installing Clojure CLI tools into: ${clj_prefix}"
  bash "${installer}" --prefix "${clj_prefix}"

  echo "[bootstrap] Installed clojure: ${clj_bin}"
  "${clj_bin}" -Sdescribe >/dev/null 2>&1 || true
}

if [[ "${install_bb}" == "true" ]]; then
  install_babashka
fi

if [[ "${install_clojure}" == "true" ]]; then
  install_clojure_tools
fi

cat <<EOF

[bootstrap] Done.

Quick usage:
  - babashka: ${prefix}/bin/bb
      ./bb scripts/alfworld_runner.clj 10 verbose
  - clojure:  ${prefix}/clojure/bin/clojure
      make dev

If you want them on PATH in this shell:
  export PATH="${prefix}/bin:${prefix}/clojure/bin:\$PATH"
EOF

