#!/usr/bin/env bash
# Set up the isolated account for the E2 memory-ablation experiment.
#
#   sudo -v && ./scripts/setup-ablation-account.sh
#
# Run ONCE, as joe. Caches sudo credentials up front so there is a single
# prompt. Idempotent: re-running skips work already done.
#
# WHY AN ISOLATED ACCOUNT AT ALL
#   The experiment re-runs already-solved Lean problems from their
#   pre-solution state, with a memory withheld, to see whether the withheld
#   memory changes how much effort the problem takes. That is only valid if
#   the runner cannot look up the answer. It can, three ways, unless stopped:
#     - a git worktree at an old revision still reaches the closing commit
#       through the shared object database;
#     - the routes taken are described in futon3c analysis artifacts;
#     - the Codex-side store under ~/.codex may carry prior context.
#   All three live under /home/joe, which is already drwxr-x--- and therefore
#   unreadable to a user outside group joe. The isolation is mostly free; this
#   script sets it up and then PROVES it by probing.
#
# WHAT IT DOES NOT DO
#   It does not dispatch anything, and it does not decide how a runner is
#   invoked as this user. That is still open (see the note it prints at the
#   end) and an ablation that quietly ran as joe would pass every probe here
#   while being completely unisolated.

set -euo pipefail

ACCT=apmablate
ACCT_HOME=/home/$ACCT
ACCT_NPM=$ACCT_HOME/.npm-global
AGENT_ID=abl-codex-1
AGENCY=http://127.0.0.1:7070
SRC=/home/joe/code/apm-lean
OUT=/home/joe/code/futon3c/holes/experiments/e2-isolation-probe.edn

# problem:base-revision — the commit immediately BEFORE each problem closed.
# Per-problem rather than one shared revision, so each run sees the tree as
# its original dispatch actually saw it, solved siblings included.
TARGETS=(
  "a95J08:61ddc05"
  "a02J05:fddc86c"
  "a01A07:81dccb3"
  "a93J07:c8e6f11"
)

say() { printf '\n\033[1m== %s\033[0m\n' "$*"; }
ok()  { printf '   \033[32mok\033[0m   %s\n' "$*"; }
bad() { printf '   \033[31mFAIL\033[0m %s\n' "$*"; }

say "Preflight"
id "$ACCT" >/dev/null 2>&1 || { echo "user $ACCT does not exist; run: sudo useradd -m -s /bin/bash $ACCT"; exit 1; }
ok "account $ACCT exists (uid $(id -u $ACCT))"
[ -d "$SRC/.git" ] || { echo "no git repo at $SRC"; exit 1; }
sudo -v
ok "sudo credentials cached"

say "Toolchain (~9.5G, copied once)"
if sudo test -d "$ACCT_HOME/.elan"; then
  ok ".elan already present, skipping"
else
  # Direct root copy rather than staging through /tmp: root can read both
  # ends, so this is one pass instead of two. The chown immediately after is
  # what makes it the account's own.
  sudo cp -a /home/joe/.elan "$ACCT_HOME/.elan"
  sudo chown -R "$ACCT:$ACCT" "$ACCT_HOME/.elan"
  ok ".elan copied and chowned"
fi

say "Mathlib dependency cache (~7.0G, shared by every run)"
if sudo test -d "$ACCT_HOME/packages"; then
  ok "packages already present, skipping"
else
  # .lake/packages only — NOT .lake/build. The latter is our own build output
  # and could contain compiled forms of the very problems being ablated.
  sudo cp -a "$SRC/.lake/packages" "$ACCT_HOME/packages"
  sudo chown -R "$ACCT:$ACCT" "$ACCT_HOME/packages"
  ok "packages copied and chowned"
fi

say "Problem trees at their pre-solution revisions"
for t in "${TARGETS[@]}"; do
  prob="${t%%:*}"; rev="${t##*:}"
  dest="$ACCT_HOME/runs/$prob"
  if sudo test -d "$dest/problems"; then
    ok "$prob already staged, skipping"
    continue
  fi
  git -C "$SRC" rev-parse --verify -q "$rev^{commit}" >/dev/null \
    || { bad "$prob: revision $rev not found"; continue; }
  tmp=$(mktemp /tmp/apm-XXXXXX.tar)
  # `git archive` emits the TREE ONLY — no object database, so the closing
  # commit is unreachable rather than merely not-checked-out.
  git -C "$SRC" archive "$rev" -o "$tmp"
  sudo mkdir -p "$dest/.lake"
  sudo tar -xf "$tmp" -C "$dest"
  sudo ln -sfn "$ACCT_HOME/packages" "$dest/.lake/packages"
  sudo chown -R "$ACCT:$ACCT" "$dest"
  rm -f "$tmp"
  ok "$prob staged at $rev (tree only, no history)"
done

say "Codex runtime for $ACCT"
# The codex binary lives at /home/joe/.nvm/.../bin/codex, inside a directory
# $ACCT cannot traverse. It needs its own install.
if sudo -u "$ACCT" test -x "$ACCT_NPM/bin/codex"; then
  ok "codex already installed for $ACCT, skipping"
elif ! command -v npm >/dev/null 2>&1; then
  bad "npm not on PATH — run: sudo apt install -y nodejs npm, then re-run this script"
else
  sudo -u "$ACCT" env HOME="$ACCT_HOME" npm i -g @openai/codex --prefix "$ACCT_NPM" >/dev/null
  sudo -u "$ACCT" test -x "$ACCT_NPM/bin/codex" \
    && ok "codex installed at $ACCT_NPM/bin/codex" \
    || bad "codex install did not produce a binary"
fi

say "Credentials — auth ONLY, deliberately not the store"
# *** The step most likely to silently destroy the experiment. ***
# `no-runner-side-store` passes today because $ACCT has no ~/.codex at all. A
# convenient `cp -a ~/.codex` would hand the runner every prior conversation —
# reintroducing exactly the leak that probe exists to catch, while the probe
# kept reporting green, because it only ever tested whether JOE's store was
# readable. So: auth.json and config.toml, and nothing else. Not
# history.jsonl (20M of prior conversations), not cache/ (~3.5G), not
# goals_*.sqlite.
if sudo test -f "$ACCT_HOME/.codex/auth.json"; then
  ok "credentials already in place, skipping"
else
  sudo -u "$ACCT" mkdir -p "$ACCT_HOME/.codex"
  sudo install -o "$ACCT" -g "$ACCT" -m 600 /home/joe/.codex/auth.json   "$ACCT_HOME/.codex/auth.json"
  sudo install -o "$ACCT" -g "$ACCT" -m 644 /home/joe/.codex/config.toml "$ACCT_HOME/.codex/config.toml"
  ok "auth.json + config.toml copied (history, cache and goals deliberately omitted)"
fi

say "Agency registration"
reg_status=$(curl -sS --max-time 5 -o /dev/null -w '%{http_code}' \
  -H 'Content-Type: application/json' -X POST \
  -d "{\"agent-id\":\"$AGENT_ID\",\"type\":\"codex\"}" \
  "$AGENCY/api/alpha/agents" 2>/dev/null || echo 000)
case "$reg_status" in
  201) ok "registered $AGENT_ID" ;;
  409) ok "$AGENT_ID already registered" ;;
  *)   bad "registration returned $reg_status — is the Agency up on $AGENCY?" ;;
esac

say "Isolation probes — these ARE the experiment's validity evidence"

# A probe must fail for the RIGHT REASON. The first version of this function
# treated any non-zero exit as "denied", which meant an unauthenticated sudo,
# a missing binary or a mistyped path all reported a clean pass. That is the
# same absence-versus-inability-to-ask confusion the experiment's own
# observables were rewritten to eliminate, and it had no business being
# reintroduced in the harness that produces their evidence.
#
# So: require the denial to be an actual permission error, and abort outright
# if sudo itself cannot authenticate.
sudo -u "$ACCT" true 2>/dev/null || {
  echo "   FATAL: cannot execute as $ACCT (sudo not authenticated?)."
  echo "   Every probe below would report a false pass. Run 'sudo -v' first."
  exit 1
}
ok "can execute as $ACCT — probes will be meaningful"

probe() {
  local err rc
  err=$(sudo -u "$ACCT" bash -c "$1" 2>&1 >/dev/null); rc=$?
  if [ $rc -eq 0 ]; then
    echo false                                  # it READ the thing: not isolated
  elif printf '%s' "$err" | grep -qi 'permission denied'; then
    echo true                                   # denied for the right reason
  else
    echo "unknown:$err" | head -1                # failed for some OTHER reason
  fi
}

HOME_DENIED=$(probe 'ls /home/joe')
ARTIFACTS_DENIED=$(probe 'ls /home/joe/code/futon3c/holes/labs/M-memory-retrieval')
CODEX_DENIED=$(probe 'ls /home/joe/.codex')
SRC_DENIED=$(probe "ls $SRC/.git")

# $ACCT's OWN store must carry no prior context. This probe did not exist
# before the runtime phase was added, and without it the credentials step
# could hand over a full conversation history while every other probe stayed
# green — they only test what JOE's account exposes, never what this one
# accumulated.
OWNSTORE_CLEAN=true
OWNSTORE_FOUND=""
for f in history.jsonl goals_1.sqlite goals_1.sqlite-wal cache; do
  if sudo -u "$ACCT" test -e "$ACCT_HOME/.codex/$f" 2>/dev/null; then
    OWNSTORE_CLEAN=false; OWNSTORE_FOUND="$OWNSTORE_FOUND $f"
  fi
done
[ "$OWNSTORE_CLEAN" = true ] \
  && ok "own codex store carries no prior context" \
  || bad "own codex store contains:$OWNSTORE_FOUND — runner may recall the answer"

# No future commits. `git log` must fail because there is no repository —
# but git must EXIST for that failure to mean anything, so check the binary
# first. A missing git would otherwise fake this probe exactly as an
# unauthenticated sudo faked the others.
NOFUTURE=true
if ! sudo -u "$ACCT" bash -lc 'command -v git' >/dev/null 2>&1; then
  NOFUTURE="unknown: git not on PATH for $ACCT — probe vacuous"
else
  for t in "${TARGETS[@]}"; do
    prob="${t%%:*}"
    if sudo -u "$ACCT" git -C "$ACCT_HOME/runs/$prob" log --all >/dev/null 2>&1; then
      NOFUTURE=false; bad "$prob has a reachable git history"
    fi
  done
fi

for pair in "operator home:$HOME_DENIED" "analysis artifacts:$ARTIFACTS_DENIED" \
            "codex store:$CODEX_DENIED" "source repo .git:$SRC_DENIED" \
            "no future commits:$NOFUTURE"; do
  n="${pair%%:*}"; v="${pair#*:}"
  case "$v" in
    true)  ok "$n unreachable" ;;
    false) bad "$n IS READABLE — run is not isolated" ;;
    *)     bad "$n INDETERMINATE — $v" ;;
  esac
done

say "Recording probe results"
mkdir -p "$(dirname "$OUT")"
cat > "$OUT" <<EOF
;; E2 isolation probe results. Generated by scripts/setup-ablation-account.sh
;; These populate the IsolationProbe structure in
;; DarkTower/MemoryAblationPreregistration.lean. Every field must be true or
;; the run is not counted.
{:generated-at "$(date -Is)"
 :account "$ACCT"
 :targets [$(for t in "${TARGETS[@]}"; do printf '{:problem "%s" :base-revision "%s"} ' "${t%%:*}" "${t##*:}"; done)]
 :home-read-denied $HOME_DENIED
 :no-future-commits $NOFUTURE
 :no-analysis-artifacts $ARTIFACTS_DENIED
 ;; two conditions, not one: joe's store unreadable AND this account's own
 ;; store free of prior context. The second was added when the runtime phase
 ;; made it possible for the runner to accumulate its own history.
 :no-runner-side-store $([ "$CODEX_DENIED" = true ] && [ "$OWNSTORE_CLEAN" = true ] && echo true || echo false)
 :operator-store-unreadable $CODEX_DENIED
 :own-store-clean $OWNSTORE_CLEAN
 :source-repo-unreachable $SRC_DENIED
 :agent-id "$AGENT_ID"
 :registration-status "$reg_status"}
EOF
ok "wrote $OUT"

say "Done"
echo "   Trees:  $ACCT_HOME/runs/{$(IFS=,; echo "${TARGETS[*]%%:*}")}"
echo "   Probes: $OUT"
echo "   Agent:  $AGENT_ID (registered: $reg_status)"
echo
echo "   To start the runner:"
echo "     sudo -u $ACCT env HOME=$ACCT_HOME FUTON_CODEX_NO_MEMCAP=1 \\"
echo "       $ACCT_NPM/bin/codex resume --ask-for-approval never \\"
echo "       --sandbox danger-full-access"
echo
echo "   --sandbox danger-full-access is acceptable here BECAUSE the OS is now"
echo "   the sandbox: full access within $ACCT's own permissions, which the"
echo "   probes above have just shown exclude every path to the answer."
echo
echo "   FUTON_CODEX_NO_MEMCAP=1 because codex-picker's systemd-run --user"
echo "   scope needs a user systemd session a non-login account lacks. To keep"
echo "   the 12G cap instead: sudo loginctl enable-linger $ACCT"
echo
echo "   VERIFY BEFORE TRUSTING A RUN: confirm the runner's process actually"
echo "   belongs to $ACCT --- ps -o user=,cmd= -C codex. A dispatch that"
echo "   quietly executed as joe would satisfy every probe in $OUT while being"
echo "   completely unisolated, because those probes describe the account, not"
echo "   the process that ran."
