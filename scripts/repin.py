#!/usr/bin/env python3
"""repin.py — put every reference to the mission back on the mission.

  repin.py [--dry-run]

An edit to M-futon-seams.md moves every offset after it. Five things point at
those offsets and all five must move together:

  1. annotations.edn      — via reanchor.py, which refuses a changed quote
  2. lifecycle.edn        — the :mission-anchor of each phase, and the
                            :mission :sha256 the whole record is pinned to
  3. proto/*.edn          — every :cue
  4. every :mission-sha   — across proto, wiring, exemplar, item6
  5. mission-C.edn        — :cue and :cue-quote

I did this by hand after every edit today and got it wrong twice: once
leaving four anchors stale while reporting them moved, once with a regex that
mangled \\u2014 inside a stored quote and aborted halfway. Both were the same
mistake -- a step of a procedure done from memory. This is the procedure.

Offsets are mapped through a diff of the two revisions, not searched for. A
first version looked each cue's text up in the new file and left 10 of 40
alone as "unverifiable" -- they were short spans like a role name that occur
more than once, and a search cannot place them. difflib gives the exact
correspondence, and every mapped cue is then VERIFIED byte-identical before
it is written. An offset that cannot be verified is not an offset worth
writing.
"""
import difflib, hashlib, json, os, re, subprocess, sys


def offset_map(old, new):
    """Map each offset in OLD to its offset in NEW, or None where deleted."""
    m = [None] * (len(old) + 1)
    for tag, i1, i2, j1, j2 in difflib.SequenceMatcher(None, old, new,
                                                       autojunk=False).get_opcodes():
        if tag == "equal":
            for k in range(i2 - i1):
                m[i1 + k] = j1 + k
    m[len(old)] = len(new)
    return m

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
MISSION = os.path.join(REPO, "holes/missions/M-futon-seams.md")
LIFECYCLE = os.path.join(REPO, "holes/labs/M-futon-seams/lifecycle.edn")
LAB = os.path.join(REPO, "holes/labs/M-futon-seams")
DRY = "--dry-run" in sys.argv


def set_pin(life, new_sha):
    """Put the record's :mission :sha256 on the text, and say so."""
    pin = re.search(r'(:mission \{[^}]*?:sha256 ")([0-9a-f]{64})(")', life)
    if not pin:
        sys.exit("  lifecycle.edn: no :mission :sha256 to set — pin cannot be kept")
    if pin.group(2) == new_sha:
        print("  mission pin: already current")
        return life
    print(f"  mission pin: {pin.group(2)[:12]}… → {new_sha[:12]}…")
    return life[:pin.start(2)] + new_sha + life[pin.end(2):]


def sh(*cmd, **kw):
    return subprocess.run(cmd, capture_output=True, text=True, cwd=REPO, **kw)


def main():
    text = open(MISSION, encoding="utf-8").read()
    new_sha = hashlib.sha256(text.encode()).hexdigest()
    prev = sh("git", "show", "HEAD:holes/missions/M-futon-seams.md").stdout
    old_sha = hashlib.sha256(prev.encode()).hexdigest()
    if new_sha == old_sha:
        # No offsets moved, but the pin can still be wrong: it is the one
        # thing here that drifts without the text changing, and this early
        # return is how it stayed wrong. Check it before standing down.
        print("mission unchanged since HEAD — offsets all hold")
        life = open(LIFECYCLE, encoding="utf-8").read()
        fixed = set_pin(life, new_sha)
        if fixed != life and not DRY:
            open(LIFECYCLE, "w", encoding="utf-8").write(fixed)
        return
    print(f"mission {old_sha[:12]}… → {new_sha[:12]}…")
    omap = offset_map(prev, text)

    # 1. annotations
    r = sh(sys.executable, "scripts/reanchor.py", "--author", "all",
           *([] if DRY else ["--apply"]))
    print("  annotations: " + (r.stdout.strip().splitlines() or ["(none)"])[-1])
    if r.returncode:
        sys.exit("  reanchor refused — resolve that first")

    # 2. phase anchors, by finding each stored quote afresh
    life = open(LIFECYCLE, encoding="utf-8").read()
    moved = missing = 0
    # Collect first, apply RIGHT TO LEFT. Rewriting left to right invalidates
    # every match position after the first, which is how this corrupted
    # lifecycle.edn on 2026-09-24 -- it wrote ":start58540 :end 585516",
    # eating a space and splicing two numbers. The same silent-replacement
    # shape as reanchor.py's collision and the stale DERIVE verdict: an edit
    # applied at a position computed before an earlier edit moved it.
    edits = []
    for m in re.finditer(r'\{:id (:\w+) .*?:mission-anchor \{:start (\d+) :end (\d+) '
                         r':quote ("(?:[^"\\]|\\.)*")', life, re.S):
        pid, quote = m.group(1), json.loads(m.group(4))
        at = text.find(quote)
        if at < 0 or text.find(quote, at + 1) != -1:
            print(f"  phase {pid}: quote missing or ambiguous — left alone")
            missing += 1
            continue
        if at != int(m.group(2)):
            edits.append((m.start(2), m.end(3), f"{at} :end {at + len(quote)}"))
            moved += 1
    for a, b, repl in sorted(edits, reverse=True):
        life = life[:a] + repl + life[b:]
    print(f"  phase anchors: {moved} moved, {missing} left")

    # 2b. the pin itself. This used to ride on the blind `replace(old_sha,
    # new_sha)` in step 4, which only lands while the pin still holds HEAD's
    # sha -- edit twice without committing in between and it can never match
    # again. It had been stuck at 288ce657 (the text at 52dd90ec) through
    # every later repin while all eight anchors beside it moved correctly,
    # because a replacement that matches nothing says nothing. That is the
    # same failure verdict_check.py's own docstring was written about. Set
    # the pin from the computed sha; do not go looking for its old value.
    life = set_pin(life, new_sha)

    if not DRY:
        open(LIFECYCLE, "w", encoding="utf-8").write(life)

    # 3 and 4. cues and shas
    for root, _, files in os.walk(LAB):
        for f in files:
            if not f.endswith(".edn"):
                continue
            path = os.path.join(root, f)
            s = open(path, encoding="utf-8").read()
            before = s
            ok = bad = 0

            def shift(mm):
                nonlocal ok, bad
                a, b = int(mm.group(1)), int(mm.group(2))
                want = prev[a:b]
                a2, b2 = omap[a], omap[b]
                if a2 is not None and b2 is not None and text[a2:b2] == want:
                    ok += 1
                    return f":cue [{a2} {b2}]"
                bad += 1
                return mm.group(0)

            s = re.sub(r":cue \[(\d+) (\d+)\]", shift, s)
            s = s.replace(old_sha, new_sha)
            if s != before:
                if not DRY:
                    open(path, "w", encoding="utf-8").write(s)
                note = f"{ok} cues repinned" + (f", {bad} UNVERIFIABLE" if bad else "")
                print(f"  {os.path.relpath(path, REPO)}: {note}")

    print("\nverifying:")
    for name, cmd in [("cascade_check", ["scripts/cascade_check.py"]
                       + [os.path.join(LAB, "proto", x) for x in
                          sorted(os.listdir(os.path.join(LAB, "proto")))
                          if x.startswith("instance-")]),
                      ("mission_c_check", ["scripts/mission_c_check.py",
                                           "holes/labs/M-futon-seams/item6/mission-C.edn"]),
                      ("verdict_check", ["scripts/verdict_check.py"])]:
        r = sh(sys.executable, *cmd)
        print(f"  {name}: {'OK' if r.returncode == 0 else 'FAIL'}")
        if r.returncode:
            print("    " + r.stdout.strip().replace("\n", "\n    "))


if __name__ == "__main__":
    main()
