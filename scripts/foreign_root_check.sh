#!/usr/bin/env bash
# Trace a sandbox futon3c boot and fail on accesses to another path below HOME.
set -euo pipefail

code_root=""
home_root=$(realpath "$HOME")
agency_port=7270
drawbridge_port=6968
settle_seconds=120
output_dir="${TMPDIR:-/tmp}/futon3c-foreign-root-check-$$"
alt_objects_root=""
sandbox_ports=()
codex_state_root="$home_root/.codex"
seat_worktree_root="$home_root/worktrees/futon3c/vscode-port"

usage() {
  echo "usage: $0 --code-root DIR [--sandbox-port PORT ...] [--alt-objects-root DIR] [--agency-port PORT] [--drawbridge-port PORT] [--seconds N] [--output DIR]" >&2
}
while (($#)); do
  case "$1" in
    --code-root) code_root=$2; shift 2 ;;
    --alt-objects-root) alt_objects_root=$2; shift 2 ;;
    --sandbox-port) sandbox_ports+=("$2"); shift 2 ;;
    --agency-port) agency_port=$2; shift 2 ;;
    --drawbridge-port) drawbridge_port=$2; shift 2 ;;
    --seconds) settle_seconds=$2; shift 2 ;;
    --output) output_dir=$2; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *) usage; echo "unknown argument: $1" >&2; exit 2 ;;
  esac
done
sandbox_ports+=("$agency_port" "$drawbridge_port")
sandbox_ports_csv=$(IFS=,; echo "${sandbox_ports[*]}")
[[ -n "$code_root" ]] || { usage; exit 2; }
code_root=$(realpath "$code_root")
[[ -z "$alt_objects_root" ]] || alt_objects_root=$(realpath "$alt_objects_root")
[[ -d "$code_root/futon3c" ]] || { echo "missing $code_root/futon3c" >&2; exit 2; }
for port in "$agency_port" "$drawbridge_port"; do
  case "$port" in 7070|6768|7073|7074) echo "refusing live port $port" >&2; exit 2;; esac
done
command -v strace >/dev/null || { echo "strace is required" >&2; exit 2; }

mkdir -p "$output_dir" "$code_root/storage/futon3c/mission-control" \
  "$code_root/storage/futon3c/portfolio-inference" "$code_root/notes" \
  "$code_root/storage/test-registry" "$code_root/futon2/data" \
  "$code_root/futon3c/data/jvm-incidents" "$code_root/storage/dispatch-payloads" \
  "$code_root/.claude/projects" \
  "$code_root/storage/futon3c" "$code_root/storage/arxana-together"
mkdir -p "$code_root/.emacs-server"
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
        [[ "$line" == *"$home_root/"* ]] || continue
        [[ "$line" == *"$code_root"* ]] && continue
        if [[ "$line" == *"$codex_state_root"* &&
              "$line" != *"$codex_state_root/auth.json"* ]]; then
          continue
        fi
        [[ "$line" == *"$seat_worktree_root"* ]] && continue
        if [[ "$line" =~ ^(open|openat|openat2)\(.*(O_WRONLY|O_RDWR|O_CREAT|O_TRUNC) ]] ||
           [[ "$line" =~ ^(mkdir|mkdirat|rmdir|unlink|unlinkat|rename|renameat|renameat2|link|linkat|symlink|symlinkat|truncate|creat|chmod|fchmodat|utimensat)\( ]]; then
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
  export FUTON3C_DISABLE_EMACSCLIENT=true
  export FUTON3C_IRC_PORT=0 FUTON1A_PORT=0 FUTON1B_EMBED=0 FUTON5_PORT=0 FUTON3C_WEBARXANA_PORT=0
  export FUTON_CODE_ROOT="$code_root" FUTON_WM_CODE_ROOT="$code_root"
  export FUTON_WM_WORKTREE_ROOT="$code_root/worktrees"
  export XDG_CACHE_HOME="$code_root/.cache" XDG_STATE_HOME="$code_root/.local/state"
  export XDG_CONFIG_HOME="$code_root/.config" NPM_CONFIG_CACHE="$code_root/.npm-cache"
  export FUTON_REPO_BASE="$code_root" FUTON3C_REPO_BASE="$code_root" FUTON3C_REPOS="$repos"
  export FUTON3C_ROOT="$code_root/futon3c" FUTON3_ROOT="$code_root/futon3"
  export FUTON3A_ROOT="$code_root/futon3a" FUTON5_ROOT="$code_root/futon5" FUTON6_ROOT="$code_root/futon6"
  export FUTON2_DATA_ROOT="$code_root/futon2/data" FUTON_WM_NOTES_ROOT="$code_root/notes"
  export FUTON1A_URL="http://127.0.0.1:7271" FUTON_SUBSTRATE_URL="http://127.0.0.1:7271"
  export FUTON1B_URL="http://127.0.0.1:7273" FUTON_MEMORY_BASE="http://127.0.0.1:7273"
  export FUTON_STORAGE_ROOT="$code_root/storage"
  export FUTON3A_PATTERN_INDEX_DB="$code_root/storage/futon3a/pattern-index.sqlite"
  export MEME_DB_PATH="$code_root/futon3a/meme.db"
  export GIRAFFE_ROOT="$code_root/storage/giraffe"
  export FUTON3C_GIRAFFE_REREAD_ENV_FILE="$code_root/.config/futon3c/giraffe-reread.env"
  export FUTON3C_GIRAFFE_REREAD_STATE_ROOT="$code_root/.local/state/futon3c/giraffe-reread"
  export FUTON3C_GIRAFFE_REREAD_REQUISITIONS_ROOT="$code_root/giraffe-reread-requisitions"
  export FUTON3C_AGENTS_MD_ROOT="$code_root"
  export FUTON3C_BLACKBOARD_PROJECT=false
  export FUTON3C_EMACS_SOCKET="$code_root/.emacs-server/nonexistent"
  export FUTON3C_WARRANT_INDEX="$code_root/storage/test-registry/warrant-index.sqlite"
  export FUTON3C_INCIDENTS_ROOT="$code_root/futon3c/data/jvm-incidents"
  export FUTON3C_CLAUDE_PROJECTS_ROOT="$code_root/.claude/projects"
  export FUTON3C_ARXANA_TOGETHER_LOG_DIR="$code_root/storage/arxana-together"
  export FUTON3C_DISCIPLINE_EVENTS_PATH="$code_root/futon3c/data/discipline-events.edn"
  export FUTON3C_EVIDENCE_SPOOL_DB="$code_root/storage/futon3c/evidence-spool.db"
  export FUTON3C_COMMIT_INGEST_CURSORS="$code_root/storage/futon3c/commit-ingest-cursors.edn"
  export FUTON3C_DISPATCH_PAYLOAD_STORE="$code_root/storage/dispatch-payloads"
  export FUTON3C_FILE_CHANGE_SHADOW_LEDGER="$code_root/storage/futon3c/giraffe-shadow-demands.jsonl"
  export FUTON3C_MISSION_CONTROL_SNAPSHOT_PATH="$code_root/storage/futon3c/mission-control/sessions.edn"
  export FUTON3C_PORTFOLIO_INFERENCE_SNAPSHOT_PATH="$code_root/storage/futon3c/portfolio-inference/sessions.edn"
  export FUTON_PATTERN_LIBRARY_ROOT="$code_root/futon3/library" FUTON3_LIBRARY_DIR="$code_root/futon3/library"
  export FUTON3C_INSTALLATION_WATCH_ROOT="$code_root" FUTON3C_WORKTREE_ROOT="$code_root/futon3c"
  export FUTON3C_PLANETMATH_ROOT="$code_root/planetmath"
  exec setsid strace -ff -o "$trace_prefix" -e trace=%file,connect,execve make dev
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

python3 - "$home_root" "$code_root" "$alt_objects_root" "$output_dir" "$trace_prefix" "$codex_state_root" "$seat_worktree_root" "$sandbox_ports_csv" <<'PY'
import collections, glob, os, re, sys
home, root, alt_objects_root, out, prefix, codex_state_root, seat_worktree_root, sandbox_csv = sys.argv[1:]
sandbox_ports = {int(x) for x in sandbox_csv.split(',') if x}
paths = collections.Counter(); writes = collections.Counter()
ancestor_lookups = collections.Counter()
connects = collections.Counter(); execs = collections.Counter()
quoted = re.compile(r'"(' + re.escape(home) + r'/[^"\\]*)"')
write_mark = re.compile(
    r'^(?:(?:open|openat|openat2)\(.*(?:O_WRONLY|O_RDWR|O_CREAT|O_TRUNC)|'
    r'(?:mkdir|mkdirat|rmdir|unlink|unlinkat|rename|renameat|renameat2|'
    r'link|linkat|symlink|symlinkat|truncate|creat|chmod|fchmodat|utimensat)\()'
)
# Read-only host toolchain inputs.  They are not application roots and are
# intentionally shared by relocated checkouts; writes remain prohibited.
allowed_read_prefixes = (
    (os.path.join(home, '.m2') + '/', 'Maven dependency cache'),
    (os.path.join(home, '.clojure') + '/', 'Clojure CLI user configuration and cache'),
    (os.path.join(home, '.cache') + '/', 'user-level toolchain cache'),
    (os.path.join(home, '.deps.clj') + '/', 'deps.clj tool cache'),
    (os.path.join(home, '.config/git') + '/', 'Git user configuration fragments'),
    (os.path.join(home, '.local/share/claude') + '/', 'Claude launcher toolchain'),
    (os.path.join(home, '.nvm') + '/', 'Node launcher toolchain'),
)
allowed_read_exact = {
    os.path.join(home, '.gitconfig'): 'Git user configuration',
    os.path.join(home, '.zai-key'): 'ZAI API credential',
    os.path.join(home, '.zaikey'): 'legacy ZAI API credential',
    os.path.join(home, '.m2'): 'Maven dependency cache root',
    os.path.join(home, '.clojure'): 'Clojure CLI user configuration root',
    os.path.join(home, '.cache'): 'user-level toolchain cache root',
    os.path.join(home, '.deps.clj'): 'deps.clj tool cache root',
    os.path.join(home, '.config/git'): 'Git user configuration root',
}
lines = []
for name in glob.glob(prefix + '.*'):
    lines.extend(open(name, errors='replace'))
object_roots = set()
for line in lines:
    for path in quoted.findall(line):
        marker = '/.git/objects'
        if marker in path:
            object_roots.add(path.split(marker, 1)[0] + marker)
        if alt_objects_root and '/objects' in path and (path == alt_objects_root or path.startswith(alt_objects_root + '/')):
            object_roots.add(path.split('/objects', 1)[0] + '/objects')
allowed_roots = tuple(x.rstrip('/') for x in (root, alt_objects_root, *object_roots) if x)
root_ancestors = []
cursor = os.path.dirname(root)
while cursor.startswith(home):
    root_ancestors.append(cursor)
    if cursor == home:
        break
    cursor = os.path.dirname(cursor)
git_discovery_lookups = {
    candidate
    for ancestor in root_ancestors
    for candidate in (os.path.join(ancestor, '.git'),
                      os.path.join(ancestor, '.git', 'HEAD'),
                      os.path.join(ancestor, 'HEAD'))
}
toolchain_lookup_only = {
    os.path.join(home, '.nrepl/nrepl.edn'),
    os.path.join(home, '.shadow-cljs/config.edn'),
    os.path.join(home, 'AGENTS.md'),
    os.path.join(home, 'wm-sandbox/AGENTS.md'),
}
lookup_mark = re.compile(
    r'^(?:stat|lstat|newfstatat|statx|access|faccessat|faccessat2|readlink|readlinkat)\('
    r'|^(?:open|openat|openat2)\(.*O_RDONLY.*O_DIRECTORY'
)
for line in lines:
        if line.startswith('execve('):
            executable = re.search(r'^execve\("([^"]+)"', line)
            binary = os.path.basename(executable.group(1)) if executable else '<unknown>'
            kind = 'prohibited' if binary == 'emacsclient' else 'allowed'
            execs[kind] += 1
            continue
        if line.startswith('connect('):
            unix = re.search(r'sa_family=AF_UNIX, sun_path="([^"]+)"', line)
            inet_family = 'sa_family=AF_INET' in line
            port_match = re.search(r'sin6?_port=htons\((\d+)\)', line)
            address_match = (re.search(r'inet_addr\("([^"]+)"\)', line) or
                             re.search(r'inet_pton\(AF_INET6, "([^"]+)"', line))
            if unix:
                socket = unix.group(1)
                if (socket == '/var/run/nscd/socket' or
                        socket.startswith('/run/systemd/userdb/')):
                    connects['allowed-system-socket'] += 1
                elif socket == root or socket.startswith(root + '/'):
                    connects['allowed-sandbox-socket'] += 1
                else:
                    connects['prohibited'] += 1
            elif inet_family and port_match and address_match:
                port = int(port_match.group(1)); address = address_match.group(1)
                if address in {'127.0.0.1', '::1'}:
                    connects['allowed-sandbox-port' if port in sandbox_ports else 'prohibited'] += 1
                else:
                    connects['external-network'] += 1
            else:
                connects['prohibited'] += 1
            continue
        # execve is traced only to attribute an opener; its argv strings are
        # not themselves filesystem accesses.
        for path in quoted.findall(line):
            if path == root or path.startswith(root + '/'):
                continue
            is_allowed_ancestor = any(
                path == allowed or allowed.startswith(path.rstrip('/') + '/')
                for allowed in allowed_roots)
            if is_allowed_ancestor and lookup_mark.search(line) and not write_mark.search(line):
                ancestor_lookups[path] += 1
                continue
            if ((path in git_discovery_lookups or path in toolchain_lookup_only)
                    and lookup_mark.search(line) and not write_mark.search(line)):
                ancestor_lookups[path] += 1
                continue
            if '/.git/objects/' in path and not write_mark.search(line):
                continue
            if (alt_objects_root and
                    (path == alt_objects_root or path.startswith(alt_objects_root + '/')) and
                    '/objects/' in path and not write_mark.search(line)):
                continue
            allowed_read = (path in allowed_read_exact or
                            any(path.startswith(p) for p, _ in allowed_read_prefixes))
            if allowed_read and not write_mark.search(line):
                continue
            codex_state = path == codex_state_root or path.startswith(codex_state_root + '/')
            codex_auth_write = (path == os.path.join(codex_state_root, 'auth.json') and
                                write_mark.search(line))
            seat_worktree = (path == seat_worktree_root or
                             path.startswith(seat_worktree_root + '/'))
            if (codex_state and not codex_auth_write) or seat_worktree:
                continue
            paths[path] += 1
            if write_mark.search(line): writes[path] += 1
with open(os.path.join(out, 'prohibited.tsv'), 'w') as f:
    f.write('count\tpath\twrites\n')
    for path, count in sorted(paths.items()):
        f.write(f'{count}\t{path}\t{writes[path]}\n')
with open(os.path.join(out, 'ancestor-lookups.tsv'), 'w') as f:
    f.write('count\tpath\n')
    for path, count in sorted(ancestor_lookups.items()):
        f.write(f'{count}\t{path}\n')
print(f'prohibited_unique={len(paths)} prohibited_calls={sum(paths.values())} '
      f'write_paths={len(writes)} ancestor_lookup_unique={len(ancestor_lookups)} '
      f'ancestor_lookup_calls={sum(ancestor_lookups.values())} '
      f'connect_allowed_sandbox={connects["allowed-sandbox-port"] + connects["allowed-sandbox-socket"]} '
      f'connect_allowed_system={connects["allowed-system-socket"]} '
      f'connect_external={connects["external-network"]} connect_prohibited={connects["prohibited"]} '
      f'exec_allowed={execs["allowed"]} exec_prohibited={execs["prohibited"]}')
sys.exit(1 if paths or connects['prohibited'] or execs['prohibited'] else 0)
PY
