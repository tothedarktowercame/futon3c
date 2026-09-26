#!/usr/bin/env python3
"""Census the pattern store against the multi-watcher source surface.

Two divergences, both silent until you look:

  ORPHAN   a pattern/library row with no declaration on the multi-watcher's
           source surface. It may have been left behind by a move or delete the
           watcher did not see.
  MISSING  a declared pattern on that source surface with no row. The ingest
           never ran or failed.

Written 2026-08-17 after the math split. The first repair pass ran 31
retractions, reported `exit 0` with `:failed 0`, and left 16 rows standing;
nothing detected it until this comparison was run by hand. `exit 0` measures the
job, not the store.

Usage:
  scripts/pattern_store_census.py                  # whole watcher surface
  scripts/pattern_store_census.py math-            # only ids with this prefix
  scripts/pattern_store_census.py math- --orphan-list /tmp/orphans.txt

Exit status is 0 when clean, 1 when either divergence is non-empty, so it can
gate a repair or run under scripts/bg.py after one.
"""
import os
import re
import sys
import urllib.parse
import urllib.request

STORE = os.environ.get("FUTON_SUBSTRATE_URL", "http://127.0.0.1:7073")
# This must describe the same source surface as bootstrap.clj's multi-watcher.
# Pattern declarations are not confined to futon3/library: experimental and
# reference files under any watched repository are ingested too.
WORKSPACE = "/home/joe/code"
WATCHED_REPOS = (
    "futon0", "futon1", "futon1a", "futon2", "futon3", "futon3a",
    "futon3b", "futon3c", "futon4", "futon5", "futon5a", "futon6",
    "futon7", "futon7a",
)
EXCLUDED_DIRS = {
    ".git", ".cpcache", ".shadow-cljs", ".lsp", ".clj-kondo",
    ".pytest_cache", ".venv", ".state", "node_modules", "target", "out",
    "__pycache__",
}
# The substrate caps limit at 5000 and answers a hard 400 above it -- not a
# clamp. See futon3c/README-problem-peripheral.md and the substrate-page-limit
# note in peripheral/problem.clj.
PAGE = 5000


def store_ids():
    url = f"{STORE}/api/alpha/entities?type=pattern/library&limit={PAGE}"
    body = urllib.request.urlopen(url, timeout=180).read().decode()
    ids = set(re.findall(r':entity/external-id "([^"]+)"', body))
    if len(ids) >= PAGE:
        print(f"WARNING: {len(ids)} rows == page limit; the census may be truncated "
              f"and a full page cannot be distinguished from a complete read.",
              file=sys.stderr)
    return ids


ID_LINE = re.compile(r"^@(?:flexiarg|arg|multiarg)\s+(\S+)", re.M)


def disk_ids():
    """Pattern ids declared anywhere on the multi-watcher source surface.

    Deriving from the path was wrong twice over, and both errors inflate the
    orphan count with patterns that exist:

      * .multiarg files were not globbed at all -- 9 of them, and
        pacspine/pacspine.multiarg alone declares 12 patterns.
      * MULTI-PATTERN files declare many ids regardless of extension.
        fulab/fulab-patterns.multiarg declares 11 (fulab/clock-in,
        fulab/pattern-dep, ...), none of them "fulab/fulab-patterns".

    1161 files declare 1281 ids. Reading paths saw 1152 and reported the
    difference as orphans. The id line is the authority -- projection.clj:213
    reads @arg, @flexiarg and @multiarg in that order -- so read what the file
    DECLARES, never what its name suggests.
    """
    out = set()
    for repo in WATCHED_REPOS:
        root = os.path.join(WORKSPACE, repo)
        for dirpath, dirnames, filenames in os.walk(root):
            # Mirror watcher.multi/noise-dir? structurally: do not even enter
            # trees the live watcher cannot ingest.
            dirnames[:] = [d for d in dirnames if d not in EXCLUDED_DIRS]
            for name in filenames:
                if not name.endswith((".flexiarg", ".multiarg")):
                    continue
                f = os.path.join(dirpath, name)
                try:
                    text = open(f, errors="replace").read()
                except OSError:
                    continue
                found = ID_LINE.findall(text)
                if found:
                    out.update(found)
                else:
                    # No id line means there is no authoritative pattern id.
                    # Report it, but do not invent an id from a path: the live
                    # projection cannot create that invented entity either.
                    rel = os.path.relpath(f, WORKSPACE)
                    print(f"  NOTE: no @flexiarg/@arg/@multiarg id line in {rel}",
                          file=sys.stderr)
    return out


def main():
    argv = sys.argv[1:]
    orphan_list = None
    if "--orphan-list" in argv:
        i = argv.index("--orphan-list")
        orphan_list = argv[i + 1]
        # Drop BOTH the flag and its value. Taking only flags out left the
        # VALUE as a positional, so `--orphan-list /tmp/x` silently became
        # prefix="/tmp/x" -- which matched nothing and reported a clean store.
        # A census that reads clean because it scoped to nothing is worse than
        # one that errors.
        argv = argv[:i] + argv[i + 2:]
    args = [a for a in argv if not a.startswith("--")]
    prefix = args[0] if args else ""

    store = {i for i in store_ids() if i.startswith(prefix)}
    disk = {i for i in disk_ids() if i.startswith(prefix)}
    orphans, missing = sorted(store - disk), sorted(disk - store)

    if prefix and not store and not disk:
        print(f"REFUSING: prefix {prefix!r} matches no store rows AND no files. "
              f"That is a scope error, not a clean store.", file=sys.stderr)
        return 2

    scope = f"prefix={prefix!r}" if prefix else "whole watcher surface"
    print(f"PATTERN STORE CENSUS ({scope})")
    print(f"  rows in store : {len(store)}")
    print(f"  declared ids on disk : {len(disk)}")
    print(f"  ORPHANS (row, no file) : {len(orphans)}")
    for o in orphans:
        print(f"     {o}")
    print(f"  MISSING (file, no row) : {len(missing)}")
    for m in missing:
        print(f"     {m}")

    if "--attachments" in sys.argv and orphans:
        # Classify orphans so a sweep can be backgrounded SAFELY. An orphan with
        # memory attachments is a MISSING PATTERN (write the file); one with none
        # is surplus (retract the row). Retracting the first kind destroys
        # reviewed edges -- math-formalization/notation-semantics-traps had six.
        #
        # Counts come from the RAW hyperedge query, not the memory projection.
        # The projection groups edges and under-reports: it said layer-cake-
        # crossover-split had 1 attachment when there were 2 (one per memory),
        # and a classifier that under-counts to ZERO would mark a live pattern
        # retract-safe. This is the same query watcher/multi.clj
        # fetch-attachment-hyperedges uses, so the classifier and the repointer
        # agree about what an attachment is.
        print("\n  ORPHAN CLASSIFICATION")
        write_these, retract_these, unaskable = [], [], []
        for o in orphans:
            url = (f"{STORE}/api/alpha/hyperedges?end="
                   f"{urllib.parse.quote(o, safe='')}&limit=50")
            try:
                body = urllib.request.urlopen(url, timeout=90).read().decode()
            except Exception as e:
                # FAIL CLOSED. "I could not ask" is not "nothing is there" --
                # the same conflation that made retract-flexiarg! report success
                # on a slow store this morning. An unaskable orphan is never
                # retract-safe.
                unaskable.append(o)
                print(f"     ?????? {o}  COULD NOT ASK: {e}")
                continue
            edges = body.count(":hx/id")
            memory_edges = body.count("memory/assert")
            (write_these if edges else retract_these).append(o)
            print(f"     {'WRITE  ' if edges else 'RETRACT'} {o}  "
                  f"edges={edges} memory/assert={memory_edges}")
        print(f"\n  -> write the pattern for {len(write_these)}, "
              f"retract {len(retract_these)}, UNASKABLE {len(unaskable)}")
        if unaskable:
            print("     Unaskable orphans are excluded from the retract list by "
                  "design. Re-run when the store answers.")
        if orphan_list:
            with open(orphan_list, "w") as fh:
                fh.write("\n".join(retract_these) + ("\n" if retract_these else ""))
            print(f"  RETRACT-SAFE ids written to {orphan_list}")
        return 1

    if orphan_list:
        with open(orphan_list, "w") as fh:
            fh.write("\n".join(orphans) + ("\n" if orphans else ""))
        print(f"  orphan ids written to {orphan_list}")

    # An orphan is not automatically debris: a pattern id can carry REVIEWED
    # memory attachments with no file behind it, in which case the repair is to
    # WRITE the pattern, not retract the row. math-formalization/
    # notation-semantics-traps had six. Check before retracting.
    if orphans:
        print("\n  Before retracting any orphan, check it for attachments:")
        print("    curl -s -XPOST $FUTON_SUBSTRATE_URL/api/alpha/memory/projection \\")
        print("      -H 'Content-Type: application/json' \\")
        print("      -d '{\"endpoints\":[\"<id>\"],\"limit\":20}'")
        print("  Attachments mean the pattern is missing, not the row surplus.")

    return 1 if (orphans or missing) else 0


if __name__ == "__main__":
    sys.exit(main())
