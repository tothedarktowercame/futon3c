#!/usr/bin/env bash
# Trace a sandbox futon3c boot and fail on accesses to another /home/joe root.
set -euo pipefail

code_root=""
agency_port=7270
drawbridge_port=6968
settle_seconds=120
output_dir="${TMPDIR:-/tmp}/futon3c-foreign-root-check-$$"

usage() {
  echo "usage: $0 --code-root DIR [--agency-port PORT] [--drawbridge-port PORT] [--seconds N] [--output DIR]" >&2
}
while (($#)); do
  case "$1" in
    --code-root) code_root=$2; shift 2 ;;
    --agency-port) agency_port=$2; shift 2 ;;
    --drawbridge-port) drawbridge_port=$2; shift 2 ;;
    --seconds) settle_seconds=$2; shift 2 ;;
    --output) output_dir=$2; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *) usage; echo "unknown argument: $1" >&2; exit 2 ;;
  esac
done
[[ -n "$code_root" ]] || { usage; exit 2; }
code_root=$(realpath "$code_root")
[[ -d "$code_root/futon3c" ]] || { echo "missing $code_root/futon3c" >&2; exit 2; }
for port in "$agency_port" "$drawbridge_port"; do
  case "$port" in 7070|6768|7073|7074) echo "refusing live port $port" >&2; exit 2;; esac
done
command -v strace >/dev/null || { echo "strace is required" >&2; exit 2; }

mkdir -p "$output_dir" "$code_root/storage/futon3c/mission-control" \
  "$code_root/storage/futon3c/portfolio-inference" "$code_root/notes" \
  "$code_root/futon2/data"
trace_prefix="$output_dir/trace"
runtime_log="$output_dir/runtime.log"
sentinel="$output_dir/live-write"

repos=""
for repo in futon3c futon3b futon3a futon5 futon3 futon4 futon6 futon2 futon0 futon1b; do
  repos+="${repos:+,}$repo=$code_root/$repo"
done

stop_group() {
  if [[ -n "${leader:-}" ]] && kill -0 "$leader" 2>/dev/null; then
    kill -TERM -- "-$leader" 2>/dev/null || true
    sleep 2
    kill -KILL -- "-$leader" 2>/dev/null || true
  fi
}
trap stop_group EXIT INT TERM

watch_writes() {
  while :; do
    for file in "$trace_prefix".*; do
      [[ -f "$file" ]] || continue
      while IFS= read -r line; do
        [[ "$line" == *"/home/joe/"* ]] || continue
        [[ "$line" == *"$code_root"* ]] && continue
        if [[ "$line" =~ O_WRONLY|O_RDWR|O_CREAT|O_TRUNC|rename\(|unlink\( ]]; then
          printf '%s\n' "$line" >"$sentinel"
          stop_group
          return
        fi
      done <"$file"
    done
    kill -0 "$leader" 2>/dev/null || return
    sleep 0.25
  done
}

(
  cd "$code_root/futon3c"
  export GIT_OPTIONAL_LOCKS=0
  export FUTON3C_HEAP="${FUTON3C_HEAP:-1536m}"
  export FUTON3C_PORT="$agency_port" FUTON3C_DRAWBRIDGE_PORT="$drawbridge_port"
  export FUTON3C_IRC_PORT=0 FUTON1A_PORT=0 FUTON1B_EMBED=0 FUTON5_PORT=0 FUTON3C_WEBARXANA_PORT=0
  export FUTON_CODE_ROOT="$code_root" FUTON_WM_CODE_ROOT="$code_root"
  export FUTON_REPO_BASE="$code_root" FUTON3C_REPO_BASE="$code_root" FUTON3C_REPOS="$repos"
  export FUTON3C_ROOT="$code_root/futon3c" FUTON3_ROOT="$code_root/futon3"
  export FUTON3A_ROOT="$code_root/futon3a" FUTON5_ROOT="$code_root/futon5" FUTON6_ROOT="$code_root/futon6"
  export FUTON2_DATA_ROOT="$code_root/futon2/data" FUTON_WM_NOTES_ROOT="$code_root/notes"
  export FUTON_STORAGE_ROOT="$code_root/storage"
  export FUTON3C_WARRANT_INDEX="$code_root/storage/test-registry/warrant-index.sqlite"
  export FUTON3C_MISSION_CONTROL_SNAPSHOT_PATH="$code_root/storage/futon3c/mission-control/sessions.edn"
  export FUTON3C_PORTFOLIO_INFERENCE_SNAPSHOT_PATH="$code_root/storage/futon3c/portfolio-inference/sessions.edn"
  export FUTON_PATTERN_LIBRARY_ROOT="$code_root/futon3/library" FUTON3_LIBRARY_DIR="$code_root/futon3/library"
  export FUTON3C_INSTALLATION_WATCH_ROOT="$code_root" FUTON3C_WORKTREE_ROOT="$code_root/futon3c"
  export FUTON3C_PLANETMATH_ROOT="$code_root/planetmath"
  exec setsid strace -ff -o "$trace_prefix" -e trace=openat,open,connect,rename,unlink make dev
) >"$runtime_log" 2>&1 &
leader=$!
watch_writes & watcher=$!

deadline=$((SECONDS + 180))
until curl -fsS --max-time 2 "http://127.0.0.1:$agency_port/health" >/dev/null 2>&1; do
  [[ ! -f "$sentinel" ]] || break
  kill -0 "$leader" 2>/dev/null || break
  ((SECONDS < deadline)) || { echo "health timeout" >&2; stop_group; break; }
  sleep 1
done
if [[ ! -f "$sentinel" ]] && kill -0 "$leader" 2>/dev/null; then
  end=$((SECONDS + settle_seconds))
  while ((SECONDS < end)); do
    [[ ! -f "$sentinel" ]] || break
    kill -0 "$leader" 2>/dev/null || break
    sleep 1
  done
fi
stop_group
wait "$watcher" 2>/dev/null || true
trap - EXIT INT TERM

if [[ -f "$sentinel" ]]; then
  echo "ABORTED on live write:" >&2
  cat "$sentinel" >&2
fi

python3 - "$code_root" "$output_dir" "$trace_prefix" <<'PY'
import collections, glob, os, re, sys
root, out, prefix = sys.argv[1:]
paths = collections.Counter(); writes = collections.Counter()
quoted = re.compile(r'"(/home/joe/[^"\\]*)"')
write_mark = re.compile(r'O_WRONLY|O_RDWR|O_CREAT|O_TRUNC|rename\(|unlink\(')
# Read-only host toolchain inputs.  They are not application roots and are
# intentionally shared by relocated checkouts; writes remain prohibited.
allowed_read_prefixes = (
    ('/home/joe/.m2/', 'Maven dependency cache'),
    ('/home/joe/.clojure/', 'Clojure CLI user configuration and cache'),
    ('/home/joe/.cache/', 'user-level toolchain cache'),
    ('/home/joe/.deps.clj/', 'deps.clj tool cache'),
    ('/home/joe/.config/git/', 'Git user configuration fragments'),
)
allowed_read_exact = {
    '/home/joe/.gitconfig': 'Git user configuration',
}
for name in glob.glob(prefix + '.*'):
    for line in open(name, errors='replace'):
        for path in quoted.findall(line):
            if path == root or path.startswith(root + '/'):
                continue
            if '/.git/objects/' in path and not write_mark.search(line):
                continue
            allowed_read = (path in allowed_read_exact or
                            any(path.startswith(p) for p, _ in allowed_read_prefixes))
            if allowed_read and not write_mark.search(line):
                continue
            paths[path] += 1
            if write_mark.search(line): writes[path] += 1
with open(os.path.join(out, 'prohibited.tsv'), 'w') as f:
    f.write('count\tpath\twrites\n')
    for path, count in sorted(paths.items()):
        f.write(f'{count}\t{path}\t{writes[path]}\n')
print(f'prohibited_unique={len(paths)} prohibited_calls={sum(paths.values())} write_paths={len(writes)}')
sys.exit(1 if paths else 0)
PY
