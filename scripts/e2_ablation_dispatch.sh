#!/usr/bin/env bash
# Run one E2 command inside the apmablate OS boundary, after process-bound
# isolation checks.  The outer half may inspect Joe's source repository only
# to derive the expected historical tree hash; every validity probe and the
# receipt hash are produced by the same apmablate process that starts RUNNER.

set -euo pipefail

ACCOUNT=apmablate
ACCOUNT_HOME=/home/apmablate
SOURCE_REPO=/home/joe/code/apm-lean
RUNS_ROOT=$ACCOUNT_HOME/runs
RECEIPT_PREFIX=E2_ISOLATION_RECEIPT_JSON=

die() {
  printf 'E2 ISOLATION REFUSAL: %s\n' "$*" >&2
  exit 78
}

usage() {
  cat <<'EOF'
Usage:
  e2_ablation_dispatch.sh --problem ID --base-revision REV \
    --receipt FILE -- RUNNER [ARG ...]

The command is never started unless every isolation probe passes.  A Codex
runner should use `exec --ephemeral --ignore-user-config --ignore-rules`; auth
is read from /home/apmablate/.codex/auth.json, while user config and persistent
session state are deliberately excluded.
EOF
}

checkout_tree_id() {
  local root=$1 git_dir result
  git_dir=$(mktemp -d /tmp/e2-tree-object.XXXXXX)
  git --git-dir="$git_dir" init --bare -q
  GIT_INDEX_FILE="$git_dir/index" \
    git --git-dir="$git_dir" --work-tree="$root" add -A
  result=$(GIT_INDEX_FILE="$git_dir/index" \
    git --git-dir="$git_dir" --work-tree="$root" write-tree)
  rm -rf -- "$git_dir"
  printf '%s\n' "$result"
}

inner_main() {
  local problem=$1 revision=$2 expected_hash=$3
  shift 3
  [[ ${1-} == -- ]] || die 'inner command separator missing'
  shift
  (($# > 0)) || die 'runner command is empty'

  if [[ $(basename -- "$1") == codex ]]; then
    local arg has_exec=false has_ephemeral=false has_ignore_config=false has_ignore_rules=false
    for arg in "$@"; do
      [[ $arg == exec ]] && has_exec=true
      [[ $arg == --ephemeral ]] && has_ephemeral=true
      [[ $arg == --ignore-user-config ]] && has_ignore_config=true
      [[ $arg == --ignore-rules ]] && has_ignore_rules=true
    done
    [[ $has_exec == true && $has_ephemeral == true \
       && $has_ignore_config == true && $has_ignore_rules == true ]] \
      || die 'Codex runner must use exec --ephemeral --ignore-user-config --ignore-rules'
  fi

  local run_dir=$RUNS_ROOT/$problem
  local expected_uid actual_uid actual_hash probe_file receipt_json runner_display runner_rc
  local credential_home runtime_codex_home
  local home_denied artifacts_denied codex_denied source_denied no_future own_clean
  expected_uid=$(id -u "$ACCOUNT")
  actual_uid=$(id -u)
  [[ $actual_uid == "$expected_uid" ]] \
    || die "wrong effective UID: got $actual_uid, expected $expected_uid"
  [[ $HOME == "$ACCOUNT_HOME" ]] || die "wrong HOME: $HOME"
  credential_home=$ACCOUNT_HOME/.codex
  [[ $CODEX_HOME == "$credential_home" ]] \
    || die "wrong credential home: ${CODEX_HOME-<unset>}"
  [[ -d $run_dir ]] || die "staged tree missing: $run_dir"
  [[ ! -e $run_dir/.git ]] || die 'future git objects are reachable from staged tree'
  command -v git >/dev/null || die 'git missing; future-history probe would be vacuous'
  command -v python3 >/dev/null || die 'python3 missing; receipt cannot be canonicalized'

  probe_file=$(mktemp /tmp/e2-probes.XXXXXX)
  runtime_codex_home=$(mktemp -d /tmp/e2-codex-home.XXXXXX)
  trap 'rm -f -- "$probe_file"; rm -rf -- "$runtime_codex_home"' EXIT

  denied_probe() {
    local name=$1 command=$2 err rc passed=false
    set +e
    err=$(bash -c "$command" 2>&1 >/dev/null)
    rc=$?
    set -e
    if ((rc != 0)) && grep -qi 'permission denied' <<<"$err"; then
      passed=true
    fi
    printf '%s\t%s\t%s\t%s\n' "$name" "$passed" "$rc" "$command" >>"$probe_file"
    [[ $passed == true ]] || die "$name failed (rc=$rc; expected permission denial)"
  }

  denied_probe home-read-denied 'ls /home/joe'
  home_denied=true
  denied_probe no-analysis-artifacts \
    'ls /home/joe/code/futon3c/holes/labs/M-memory-retrieval'
  artifacts_denied=true
  denied_probe operator-store-unreadable 'ls /home/joe/.codex'
  codex_denied=true
  denied_probe source-repo-unreachable 'ls /home/joe/code/apm-lean/.git'
  source_denied=true

  set +e
  git -C "$run_dir" log --all >/dev/null 2>&1
  local git_log_rc=$?
  set -e
  [[ $git_log_rc -ne 0 ]] || die 'future commits are visible through git log --all'
  no_future=true
  printf 'no-future-commits\ttrue\t%s\tgit -C RUN_DIR log --all\n' \
    "$git_log_rc" >>"$probe_file"

  local unexpected_store
  unexpected_store=$(find "$credential_home" -mindepth 1 -maxdepth 1 \
    ! -name auth.json ! -name config.toml -printf '%f\n' | sort)
  [[ -z $unexpected_store ]] \
    || die "runner-side store is not clean: ${unexpected_store//$'\n'/,}"
  [[ -r $credential_home/auth.json ]] \
    || die 'minimal Codex auth.json is missing or unreadable'
  install -m 600 "$credential_home/auth.json" "$runtime_codex_home/auth.json"
  export CODEX_HOME=$runtime_codex_home
  own_clean=true
  printf 'own-store-clean\ttrue\t0\tephemeral CODEX_HOME contains auth.json only\n' \
    >>"$probe_file"

  if [[ -n ${E2_SABOTAGE_READABLE_PATH-} ]]; then
    [[ ! -r $E2_SABOTAGE_READABLE_PATH ]] \
      || die "sabotage probe found readable path: $E2_SABOTAGE_READABLE_PATH"
    printf 'sabotage-unreadable\ttrue\t0\ttest ! -r E2_SABOTAGE_READABLE_PATH\n' \
      >>"$probe_file"
  fi

  actual_hash=$(checkout_tree_id "$run_dir")
  [[ $actual_hash == "$expected_hash" ]] \
    || die "checkout tree mismatch for $revision: got $actual_hash expected $expected_hash"
  printf 'checkout-base-revision\ttrue\t0\tcanonical tree hash matches source revision\n' \
    >>"$probe_file"

  runner_display=$(printf '%q ' "$@")
  printf 'E2 isolation passed; starting runner as uid=%s at revision=%s\n' \
    "$actual_uid" "$revision" >&2
  set +e
  "$@"
  runner_rc=$?
  set -e

  receipt_json=$(python3 - "$probe_file" "$problem" "$revision" "$actual_uid" \
    "$expected_hash" "$home_denied" "$no_future" "$artifacts_denied" \
    "$codex_denied" "$own_clean" "$source_denied" "$runner_display" \
    "$runner_rc" <<'PY'
import datetime, hashlib, json, pathlib, sys

(probe_path, problem, revision, uid, tree_hash, home_denied, no_future,
 artifacts_denied, codex_denied, own_clean, source_denied, runner_command,
 runner_exit) = sys.argv[1:]
probes = []
for line in pathlib.Path(probe_path).read_text().splitlines():
    name, passed, exit_code, command = line.split("\t", 3)
    probes.append({"name": name, "passed": passed == "true",
                   "exit-code": int(exit_code), "command": command})
probe_bytes = json.dumps(probes, sort_keys=True, separators=(",", ":")).encode()
receipt = {
    "generated-at": datetime.datetime.now(datetime.timezone.utc).isoformat(),
    "account": "apmablate",
    "effective-uid": int(uid),
    "checkout-base-revision": revision,
    "checkout-tree-id": tree_hash,
    "targets": [{"problem": problem, "base-revision": revision}],
    "home-read-denied": home_denied == "true",
    "no-future-commits": no_future == "true",
    "no-analysis-artifacts": artifacts_denied == "true",
    "no-runner-side-store": codex_denied == "true" and own_clean == "true",
    "operator-store-unreadable": codex_denied == "true",
    "own-store-clean": own_clean == "true",
    "source-repo-unreachable": source_denied == "true",
    "probes": probes,
    "probe-result-hash": hashlib.sha256(probe_bytes).hexdigest(),
    "runner-command": runner_command.rstrip(),
    "runner-started": True,
    "runner-exit-code": int(runner_exit),
}
print(json.dumps(receipt, sort_keys=True, separators=(",", ":")))
PY
  )
  printf '%s%s\n' "$RECEIPT_PREFIX" "$receipt_json"
  return "$runner_rc"
}

outer_main() {
  local problem= revision= receipt= expected_hash log status receipt_line
  while (($#)); do
    case $1 in
      --problem) problem=${2-}; shift 2 ;;
      --base-revision) revision=${2-}; shift 2 ;;
      --receipt) receipt=${2-}; shift 2 ;;
      --) shift; break ;;
      -h|--help) usage; exit 0 ;;
      *) die "unknown argument: $1" ;;
    esac
  done
  [[ -n $problem && $problem =~ ^[A-Za-z0-9.-]+$ ]] || die 'invalid --problem'
  [[ -n $revision ]] || die '--base-revision is required'
  [[ -n $receipt ]] || die '--receipt is required'
  (($# > 0)) || die 'runner command is empty'
  git -C "$SOURCE_REPO" rev-parse --verify -q "$revision^{commit}" >/dev/null \
    || die "unknown base revision: $revision"

  sudo -n -u "$ACCOUNT" true 2>/dev/null \
    || die 'cannot enter apmablate boundary non-interactively; sudo authorization required'
  expected_hash=$(git -C "$SOURCE_REPO" rev-parse "$revision^{tree}")

  log=$(mktemp /tmp/e2-dispatch.XXXXXX)
  set +e
  sudo -n -u "$ACCOUNT" env -i \
    HOME="$ACCOUNT_HOME" CODEX_HOME="$ACCOUNT_HOME/.codex" \
    PATH="$ACCOUNT_HOME/.npm-global/bin:/usr/local/bin:/usr/bin:/bin" \
    E2_SABOTAGE_READABLE_PATH="${E2_SABOTAGE_READABLE_PATH-}" \
    bash -s -- --inner "$problem" "$revision" "$expected_hash" -- "$@" \
    <"$0" 2>&1 | tee "$log"
  status=${PIPESTATUS[0]}
  set -e
  receipt_line=$(grep -F "$RECEIPT_PREFIX" "$log" | tail -1 || true)
  if [[ -n $receipt_line ]]; then
    umask 077
    printf '%s\n' "${receipt_line#"$RECEIPT_PREFIX"}" >"$receipt"
    python3 - "$receipt" <<'PY'
import hashlib, json, pathlib, sys

receipt = json.loads(pathlib.Path(sys.argv[1]).read_text())
required = [
    "generated-at", "account", "targets", "home-read-denied",
    "no-future-commits", "no-analysis-artifacts", "no-runner-side-store",
    "source-repo-unreachable", "effective-uid", "checkout-base-revision",
    "probes", "probe-result-hash",
]
missing = [key for key in required if key not in receipt]
if missing:
    raise SystemExit(f"receipt missing required fields: {missing}")
for key in ("home-read-denied", "no-future-commits",
            "no-analysis-artifacts", "no-runner-side-store",
            "source-repo-unreachable"):
    if receipt[key] is not True:
        raise SystemExit(f"receipt validity field is not true: {key}")
probe_bytes = json.dumps(receipt["probes"], sort_keys=True,
                         separators=(",", ":")).encode()
actual = hashlib.sha256(probe_bytes).hexdigest()
if actual != receipt["probe-result-hash"]:
    raise SystemExit("probe-result hash mismatch")
PY
  fi
  rm -f -- "$log"
  return "$status"
}

if [[ ${1-} == --inner ]]; then
  shift
  inner_main "$@"
else
  outer_main "$@"
fi
