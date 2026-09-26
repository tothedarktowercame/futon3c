#!/usr/bin/env bash
# Back up the untracked evidence base (raw data kept out of git) to an
# external destination, preserving repo-relative paths, then verify hashes.
#
#   scripts/backup_evidence.sh /media/joe/<drive>/futon3c-evidence-20260801
#
# Source of truth: holes/labs/evidence-manifest-20260801.tsv (committed).
# The copy is verified against the manifest's sha256 at the destination, so
# a drifted file (edited since manifest time) fails loudly rather than
# silently backing up something the manifest does not describe.
set -euo pipefail

DEST="${1:?usage: backup_evidence.sh <destination-dir>}"
REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
MANIFEST="$REPO_ROOT/holes/labs/evidence-manifest-20260801.tsv"

[ -f "$MANIFEST" ] || { echo "manifest not found: $MANIFEST" >&2; exit 1; }
mkdir -p "$DEST"

fail=0 n=0
while IFS=$'\t' read -r path bytes sha; do
  case "$path" in \#*|path) continue;; esac
  src="$REPO_ROOT/$path"
  if [ ! -f "$src" ]; then echo "MISSING SOURCE: $path" >&2; fail=1; continue; fi
  mkdir -p "$DEST/$(dirname "$path")"
  cp -p "$src" "$DEST/$path"
  got=$(sha256sum "$DEST/$path" | cut -d' ' -f1)
  if [ "$got" != "$sha" ]; then echo "HASH MISMATCH: $path (drifted since manifest, or copy error)" >&2; fail=1; fi
  n=$((n+1))
done < "$MANIFEST"

echo "copied+verified $n files to $DEST"
[ "$fail" -eq 0 ] || { echo "BACKUP INCOMPLETE — see errors above" >&2; exit 1; }
