#!/usr/bin/env bash
# Send a bell/whistle whose body is assembled from FILES ONLY.
#
# WHY THIS EXISTS (2026-08-18): dispatch packets are written as shell heredocs.
# A *quoted* heredoc (<<'EOF') is safe; an *unquoted* one (<<EOF) lets the shell
# expand $vars and EXECUTE anything in backticks inside the packet text. The
# temptation to unquote comes from wanting to interpolate a shared preamble.
#
# That happened: a packet describing a Lean defect was sent with <<PACKET so a
# $COMMON preamble would interpolate, and the shell ate every backticked term in
# it. Two load-bearing sentences arrived as "It integrates against a , and
# measures are unsigned" and "** was a phantom parameter**" -- i.e. the two
# requirements the packet existed to state. It failed silently; the bell was
# accepted and the agent started work on the damaged text.
#
# The fix is compositional, not disciplinary: write each fragment with a QUOTED
# heredoc into a file, then concatenate the files here. No interpolation is ever
# needed, so no unquoted heredoc is ever needed.
#
#   cat > /tmp/preamble.md <<'EOF'   ... EOF
#   cat > /tmp/specific.md <<'EOF'   ... EOF
#   scripts/bell-file.sh --from claude-2 --to codex-3 /tmp/preamble.md /tmp/specific.md
set -euo pipefail

FROM=""; TO=""; KIND="bell"; FORCE=0
while [[ $# -gt 0 ]]; do
  case "$1" in
    --from) FROM="${2:-}"; shift 2 ;;
    --to)   TO="${2:-}";   shift 2 ;;
    --kind) KIND="${2:-}"; shift 2 ;;
    --force) FORCE=1; shift ;;
    --) shift; break ;;
    -*) echo "bell-file.sh: unknown option $1" >&2; exit 2 ;;
    *) break ;;
  esac
done

if [[ -z "$FROM" || -z "$TO" || $# -lt 1 ]]; then
  cat >&2 <<'USAGE'
usage: bell-file.sh --from AGENT --to AGENT [--kind bell|whistle] FILE [FILE...]

  --from is REQUIRED: it records the mesh edge and is what lets the recipient's
  completion bell route back. Without it the caller is logged as http-caller and
  auto-bellback has no recipient.
USAGE
  exit 2
fi

for f in "$@"; do
  [[ -r "$f" ]] || { echo "bell-file.sh: unreadable packet file: $f" >&2; exit 2; }
  [[ -s "$f" ]] || { echo "bell-file.sh: empty packet file: $f" >&2; exit 2; }
done

# A packet that lost text to shell expansion shows stray artefacts where a term
# was removed. BLOCK, do not warn: the whole point is that a mangled packet must
# not reach an agent, and a warning printed above an "accepted" response is
# exactly the shape of failure this script exists to prevent -- the first
# version of this script warned and sent anyway, and promptly dispatched its own
# test fixture to a working agent. Override with --force if a match is spurious.
if [[ "$FORCE" != "1" ]] && cat "$@" | grep -nE '(\*\* was |against a ,|`` )' >/dev/null 2>&1; then
  echo "bell-file.sh: REFUSING TO SEND -- packet looks shell-mangled:" >&2
  cat "$@" | grep -nE '(\*\* was |against a ,|`` )' | head -5 >&2
  echo "bell-file.sh: write the fragment with a QUOTED heredoc (<<'EOF'), or pass --force." >&2
  exit 3
fi

cat "$@" | python3 "$(dirname "$0")/agency_send.py" --from "$FROM" --to "$TO" --kind "$KIND"
