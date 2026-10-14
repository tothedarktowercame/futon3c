# T-futon1b-evidence-index-deep-bypass: a backdated evidence doc written around the sidecar hook stays out of list reads until boot

**Status: WATCH (2026-10-14).** Not a live defect. Nothing writes evidence this way today. Act on the trigger below.

Repo: futon1b (`futon1b_text.clj`). Context: futon1b `f6025d1`, `13a9c6e`, `51f2451`.

## The gap

Once `reconcile!` has run, `text/complete?` holds, and every evidence list read on :7073 is served by the text sidecar alone (`futon1b-evidence/index-window`). A doc is in the sidecar only if one of these indexed it:

- `on-append!`, which both futon1b write paths call (`POST /api/alpha/evidence`, and memory/assert in `futon1b_server.clj`);
- the periodic `catch-up!` (every 5 min), which indexes everything above its (at, id) checkpoint, and since `13a9c6e` also any doc missing from the 15 minutes below it;
- `reconcile!`, which compares every store id with the index, at boot only.

So a doc that reaches the store **without** `on-append!` and with an `:evidence/at` more than 15 minutes below the checkpoint is missing from every list read, count and tag read until the next futon1b boot. Point reads by id still find it. Nothing is lost.

Ways in: a direct `:put-docs :evidence` through Drawbridge (`scripts/futon1b-eval.sh`) or pgwire, or a bulk import or backfill of old evidence that does not go through the HTTP route.

## Why it can wait

- The reconcile on 2026-10-14 17:48 checked all 569,264 store docs after a catch-up and found 0 missing.
- The margin repair has reported 0 repairs since it went live (log line below).
- Being hidden from list reads is the only effect. There is no load or heap cost, unlike the 10-11..14 duplicate-tag failure, which turned every read into a full scan.

## Trigger to act

Either of:

- futon1b logs `[fts] periodic catch-up found N doc(s) below the checkpoint missing from the index` with N > 0;
- a boot or manual `reconcile!` returns `:missing` > 0 (logged as `[fts] reconcile: {... :missing N ...}`).

Either one means some writer bypasses `on-append!`. First find and fix that writer (it should go through the HTTP route or call `futon1b-text/on-append!`), then decide on the fix below.

## Fix, when triggered

Run `reconcile!` on a schedule (e.g. nightly, off-peak), deferring like the periodic catch-up when fewer than 2 query permits are free. Cost is one `SELECT _id FROM evidence` (a whole-table id read, a few seconds at ~570k rows) plus `SELECT id FROM ev_attr`. Do not read bodies (see the unbounded-read rule in futon1b's memory incidents). Add a test in `test_evidence_tag_index.clj` alongside `catch-up-repairs-docs-below-the-checkpoint`: a bypassing doc 2 h below the checkpoint becomes visible after the scheduled pass.

Rejected alternative: making `index-window` also scan the store's recent tail on every read. It costs ~2 s per read (XTDB reads the whole evidence table for any predicate), and it would still miss the backdated case.
