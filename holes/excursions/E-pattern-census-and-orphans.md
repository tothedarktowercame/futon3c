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
- Multi-pattern files declare many ids regardless of extension. The formerly
  mislabeled `fulab/fulab-patterns.flexiarg`, now
  `fulab/fulab-patterns.multiarg`, declares **11** — `fulab/clock-in`,
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

## 3a. Repointing through JSON silently downgraded keywords (FIXED, 28114d76)

Not a census bug, but the same class: a repair that reports success and damages
what it repairs.

`watcher/multi.clj repoint-pattern-attachments!` re-posted props it had READ
from the store, and `post-hyperedge!` serialised them as JSON. **JSON has no
keyword type.** `:attachment-status :reviewed` landed as `"reviewed"`, along
with `:domain`, `:kind`, `:state`, `:witness-status` and the `:verdict` keys
inside `:review-history`.

`peripheral/memory_recall.clj:45` tests `(not= :reviewed attachment-status)`, so
a string-valued repointed attachment is **excluded from every recall while still
looking present in the store** — visible to `hyperedges`, invisible to the thing
that uses it.

codex-3's whole-`:hx/props` equality check caught this by refusing every
repoint, and the guard failed closed, so nothing was lost. The tempting repair
was to loosen the verification, which would have shipped the damage. The
verification was right; the **encoding** was wrong. `futon1b_server.clj`
`parse-payload` reads EDN whenever the Content-Type is not JSON, so the fix was
local: `{:encoding :edn}` on the repoint path only.

**Acceptance test that distinguishes the two repairs** — run the real recall,
not the raw hyperedge query:

```clojure
(mapv (fn [v] {:endpoint (:endpoint v) :memories (count (:memories v))
               :audit (:audit v)})
      (:recalls (futon3c.peripheral.memory-recall/recall-by-endpoints
                  {:domain :mathematics} ["<pattern-id>"] {:limit 60})))
;; attachment-excluded 0  <- what the encoding fix buys
;; attachment-excluded N  <- what loosening the verifier would have produced
```

Two more defects surfaced underneath it, both now fixed:

- `post-hyperedge!` had **no `:timeout`** and its `catch` discarded the
  exception, so every failure reported as a bare "write failed" —
  indistinguishable between a 403, a timeout and a malformed body (`209ab13a`).
- The substrate answers **503
  `:memory-projection-source-moved-after-quiescence`** when a concurrent write
  moves the watermark mid-index-rebuild (`futon1b_graph.clj:1022`). A 40-edge
  repoint provokes it routinely; it is a race, not a bad write. Now retried with
  backoff, and only for 503 (`1ce6e282`).

## 4. What remains, for Codex

**63 orphans library-wide** under the corrected census, of which **19 were APM's
`math/*`**. Those are now **closed** (2026-08-17): all 19 patterns were written
from their own attached memories, ingested, and their 191 attachments repointed
to the new subject-category ids with every old id drained to 0 edges. The
remaining ~44 need the same treatment as everything above: classify before
touching, and verify the classification is not an artifact of the tool.

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

## 6. The census used the wrong source boundary (FIXED, 2026-08-17)

The corrected declaration parser still compared the store only with
`futon3/library/`. The live multi-watcher does not have that boundary. Its boot
configuration watches 14 repository roots, and `file_ingest.clj` sends every
`.flexiarg` and `.multiarg` outside the shared excluded-directory set through
the pattern projector.

Reproducing that source surface changes the result materially:

| measurement | `futon3/library` only | actual watcher surface |
|---|---:|---:|
| store rows | 1345 | 1345 |
| declared ids on disk | 1282 | 1333 |
| row with no declaration | 63 | **19** |
| declaration with no row | 0 | **7** |

Thus **none of the 44 non-math rows is an orphan**. All 44 have a live source
declaration. Important classes and provenance include:

- `alfworld/*`, `realtime/*`, and `social/evidence-landscape` in
  `futon3c/library/`;
- the multi-pattern `futon3a/holes/labs/llm-fold/{blues,music}-cascade.multiarg`;
- the multi-pattern `futon4/test/testing.multiarg` (`popiii/*`);
- cascade-fold experimental files under
  `futon3a/holes/labs/M-memes-arrows/structure-learned-patterns/`;
- `futon3/holes/futon-stack.flexiarg` and `futon5/reference/*.flexiarg`.

The original corrected classifier's `WRITE 19 / RETRACT 44` result was still
unsafe: the 44 zero-attachment rows were not retractable debris, because their
source files remain in the live ingest domain. The actual orphan population is
exactly the 19 known `math/*` rows, all with memory attachments; this excursion
does not prescribe their repair because that work is already active elsewhere.

The watcher-wide comparison also exposes seven missing rows that the
single-library census could not see:

```
blues/twelve-bar-form
math-formalization/replace-enumeration-with-structural-counting
math-strategy/construct-through-a-finite-correspondence
music/fugue-subject
peeragogy/calling-in-not-out
peeragogy/check-in-rhythm
popiii/preface
```

The script now uses the 14 roots from `dev/bootstrap.clj`, prunes the same
directory names as the watcher, reads declarations from both supported pattern
extensions, and no longer invents an id from the path when a file has no id
line. This is deliberately a census fix only: no store row was retracted and no
missing row was force-ingested. The remaining structural risk is duplicated
declarations across live source files; the watcher-wide census should report
those explicitly in a follow-on rather than silently choosing one authority.

## 7. Multiarg rename/deletion ownership gap (OPEN, 2026-08-17)

After the mislabeled multi-pattern files were renamed to `.multiarg`, a live
`pattern/library` entity scan found **zero occurrences of all nine old
`.flexiarg` filenames**.
Canonical pattern and clause entities do not carry `source-file`, so these
extension-only renames did not themselves leave filename references dangling.
Old names still occur in generated exports and historical census artifacts;
those are historical observations, not live source authorities.

The watcher nevertheless has an asymmetric lifecycle:

- collection and dispatch explicitly accept both `.flexiarg` and `.multiarg`;
- `multi.clj/flexiarg-pattern-id`, which selects pattern-aware rename and
  deletion cleanup, accepts only `library/**/*.flexiarg` and derives exactly
  one id from the path;
- a `.multiarg` deletion therefore falls into generic code-vertex staleness and
  does not retract its pattern entities or clauses.

Changing the regex to accept `.multiarg` would violate the declaration-is-
authority invariant: one multiarg file owns many ids, and generally owns no id
matching its pathname. The structural repair is for watcher snapshot/cache
metadata to retain the complete declaration manifest for every pattern file.
Rename and deletion handling must consume that prior manifest, retracting or
reconciling every owned pattern id only after the replacement file has been
successfully ingested. Until that exists, multiarg deletion must not be treated
as cleanup-complete.

## 7a. Multiarg ownership gap — RESOLVED (910b71df), with one operator action outstanding

codex-6 implemented the manifest repair it specified in section 7, after
belling for authorization rather than editing the reserved API. Reviewed and
**accepted** by claude-2, 2026-08-17.

What landed:

- `WATCHED-EXTS` (`multi.clj:42`) now contains `"multiarg"`. It did not before,
  while `projections/flexiarg.clj:10 src-exts` did — so the **live watcher had
  never seen a `.multiarg` file at all**, for create, change or delete. Their
  rows reached the store by some other ingest path.
- Watcher snapshots retain every declaration id per pattern file; deletion
  retracts exactly the prior manifest; rename and changed declarations ingest
  first and then retract only dropped ids.
- `retract-flexiarg!` is now a thin path→id wrapper over `retract-pattern-id!`.
  The id-based entry point takes ids directly — **no path is fabricated**, which
  was the invariant that made this a bell rather than a workaround.

Verified rather than accepted: clj-kondo clean, check-parens OK, and the watcher
suites re-run by me at **28 tests / 124 assertions, 0 failures** (21/76 in
`multi-test` plus 7/48 in `projections.flexiarg-test` — codex-6's "28" was both
namespaces, not a discrepancy). The fail-closed test asserts what did NOT happen
(`@calls` is `[:ingest]` alone: a failed replacement ingest performs no repoint
and no retraction), which is the right shape for that property. Substrate PIDs
unchanged; the watcher stayed stopped.

### The outstanding action, which the summary understated

codex-6 correctly reported that with the configured `cold-scan? false` the nine
newly recognized `.multiarg` files are **adopted as baseline and will not ingest
on an ordinary restart**, preserving the no-cold-scan contract. That is honest
and it is the right default — it is emphatically not the silent-baseline failure
that lost the math reorg earlier the same day, because it is stated.

But it is presented as merely preserving a contract, and it is more than that.
**Three declarations that the census reports as MISSING live in exactly those
newly-watched files**, and were confirmed absent from the store on review
(`{:error "Entity not found"}`, against `fulab/clock-in` as a present control):

```
blues/twelve-bar-form   futon3a/holes/labs/llm-fold/blues-cascade.multiarg
music/fugue-subject     futon3a/holes/labs/llm-fold/music-cascade.multiarg
popiii/preface          futon4/test/testing.multiarg
```

So a deliberate cold scan or explicit dispatch is **required**, not merely
available: an ordinary restart adopts these three as baseline and they stay
missing indefinitely, now under a watcher that believes it is tracking them.
The remaining four MISSING ids are in `.flexiarg` files under staging and
candidate directories and are a separate question.

**Operator action for Joe, when the watcher is restarted:** cold-scan the three
files above (or dispatch them explicitly), then re-run
`scripts/pattern_store_census.py` and confirm MISSING drops from 7 to 4.
