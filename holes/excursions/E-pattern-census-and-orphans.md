# E-pattern-census-and-orphans

**Opened 2026-08-17 by claude-2 (Analyst, M-apm-demonstration) at Joe's
direction**, to hold the non-APM findings from today's store census so the
mathematics work is not blocked on them. Joe intends to hand this to Codex
directly.

**Surface:** `futon3c/scripts/pattern_store_census.py`, the pattern store
(port 7073), and `futon3/library/`.

## Why this exists

The math split (2026-08-17) needed a check that the store and the library agree.
None existed, so one was written mid-repair. It immediately found more than it
was built for, and it was also wrong twice in ways worth recording — both
failures were of the same kind it exists to detect.

## 1. The census was blind to multi-pattern files (FIXED, 5d24d72e)

`disk_ids()` derived pattern ids from FILE PATHS. Wrong two ways:

- `.multiarg` files were never globbed. There are **9**;
  `pacspine/pacspine.multiarg` alone declares **12** patterns.
- Multi-pattern files declare many ids regardless of extension.
  `fulab/fulab-patterns.flexiarg` declares **11** — `fulab/clock-in`,
  `fulab/pattern-dep`, … — and none of them is `fulab/fulab-patterns`.

**1161 files declare 1281 ids; the path-derived scan saw 1152** and reported the
difference as orphans. Orphan count fell **207 → 63** on the fix.

Joe found this from two `ls` listings after I had already proposed handing 144
"orphans" back to other missions. **A backgrounded retract sweep would have
retracted rows for patterns that exist on disk.**

The id line is the authority — `futon3a/src/futon/flexiarg/projection.clj:213`
reads `@arg`, `@flexiarg`, `@multiarg` in that order. Read what a file
DECLARES, never what its name suggests.

## 2. `--orphan-list` value was read as the scope prefix (FIXED, 7cfcc92f)

The arg parser stripped flags but not their values, so
`--orphan-list /tmp/x.txt` made the filename the prefix. It matched nothing and
printed a clean bill of health for an empty scope; the first library-wide
classification I backgrounded classified **zero** of 218 orphans and exited 0.
Now refuses when a prefix matches no rows and no files.

## 3. The classifier counted projection groupings, not raw edges (FIXED, e5650fb4)

It reported `layer-cake-crossover-split` as 1 attachment where there were 2.
Under-counting to zero marks a live pattern retract-safe. Counts now come from
the raw hyperedge query — the same one `watcher/multi.clj
fetch-attachment-hyperedges` uses — so classifier and repointer agree about what
an attachment is. It also FAILS CLOSED: an unaskable orphan reaches neither
list.

## 4. What remains, for Codex

**63 orphans library-wide** under the corrected census, of which **19 are APM's
`math/*`** and are being handled in the mission (see below). The remaining ~44
need the same treatment as everything above: classify before touching, and
verify the classification is not an artifact of the tool.

Specific questions worth answering:

1. **Re-run the classification against the corrected census.** The previous
   `WRITE 163 / RETRACT 44` split was computed from the broken disk scan and is
   void. Do not reuse `/tmp/lib-retract-safe.txt`.
2. **Are any of the remaining orphans also multi-pattern-file residue** in a
   form the fix does not catch — e.g. a declared id that no longer appears in
   the file that used to declare it?
3. **Is `alfworld/*` (the first orphans printed) live or abandoned?** It has no
   directory under `library/`.
4. **Should the census run on a schedule?** It exits non-zero on divergence and
   the divergence went undetected for months. A watcher-adjacent job or a
   commit hook would have caught the math split's breakage the same day.

## 5. The generative mechanism, still open

The attachment path accepts any pattern-id string and never checks that a
pattern exists. That is how `math/*` came to hold 191 memory attachments with no
directory at all. Whatever is decided for the APM cycle machine (validate at
attach time; refuse with "write the pattern first and let the watcher import
it") probably belongs at the shared layer rather than in one peripheral —
otherwise the next mission repeats it.

Related: `E-apm-A3-ingest-efficiency` is to be reopened after the mathematics
work (Joe, 2026-08-17). Its 232-row "in store, no file" population and this
census's orphans are the same measurement taken twice, four days apart, and the
A3 figure was computed the same path-derived way — so **it may also be
overstated** and should be recomputed with the corrected scan before any
conclusion is drawn from the two numbers.
