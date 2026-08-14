# E-apm-halftime-pre-go-live-D — "Substrate residuals", as handoff packets

**Written 2026-08-13/14 by claude-2** alongside the B and C packets, at Joe's
request that B/C/D be elaborated so they are ready to process once A is
settled.

Section **D** of the locked list (`E-apm-halftime-pre-go-live.md`). Decomposes
D1–D3; not new items. Verified against live files on 2026-08-14.

All three are residue from the **36-minute outage of 2026-08-13** — read
`futon1b/TN-futon1b-boot-incident-2026-08-13.md` before starting any of them.
The three fixes that ended the outage are already in and must not be undone.

**Dispatch order:** D3 first. It is the cheapest, it is pure addition, and it
is the one that would have turned the outage into a warning. D1 next. D2 last
— it is the only one that touches the boot path, and it should not be attempted
while anything else is in flight against the substrate.

---

## D3 — no pre-restart check exists

**The gap that cost 36 minutes.** There is no way to ask a running node
"would you survive a restart?" On 2026-08-13 a live substrate was restarted
without that question being answerable, and it could not boot: writes had
outpaced indexing, and the boot gate exhausted its attempts against the
backlog.

I have been running the check by hand ever since — `du -sb migration-store-21/log`
before every restart (4,303,950,987 bytes before the 17:39 UTC restart, which
then booted cleanly in **379 s**). Hand-running it is not a control.

**Goal.** A script that answers "is it safe to restart?" with a clear yes/no
and a reason.

**Acceptance.**
- Compares the store log size against the processed offset and warns when the
  gap exceeds what the boot gate can absorb.
- Exit code usable from a shell (`0` safe, non-zero not), because the point is
  that it can gate an actual restart.
- Prints the numbers it judged on, not just a verdict.
- Documented in the futon1b README as the required pre-restart step.

**The trap this must not fall into.** `latest-submitted-msg-ids` and
`latest-processed-msg-ids` are **BYTE OFFSETS, not message counts**. Misreading
them turned a routine backlog into an apparent catastrophe (185 MB read as 185
million messages). See the TN, l.103–121. Anything this script prints must be
labelled with its unit.

---

## D1 — `respond!`'s JSON path still parses

**Corrected figure: 44 of 55 `respond!` call sites pass a `pr-str` string**
(the locked list said ~30).

The EDN path was fixed during the outage — a string body is already EDN and is
now written straight out, instead of being round-tripped through
`edn/read-string` and re-printed. That round-trip is what made correctly
*stored* documents return 500, because `pr-str` emits forms the reader cannot
read back (`#xt/instant`, `#object[...]`).

**The residual:** the JSON path still parses, because it needs a data structure
to encode. So a **JSON-requested** response whose EDN does not read back still
fails, in exactly the way the EDN path used to. The docstring at
`futon1b_server.clj:215-234` states this honestly — read it first.

**Goal.** Remove the residual by having callers pass the map.

**Acceptance.**
- Callers pass the response **value**; `respond!` serializes. The
  `(pr-str …)`-at-the-call-site pattern goes away.
- A JSON-requested response containing a value whose `pr-str` does not read
  back returns **200 with correct JSON**, not 500. Add a test using one of the
  known-unreadable shapes (`#xt/instant` is the cheapest).
- The EDN path behaviour is **unchanged** — it is the thing that ended the
  outage. Do not "simplify" it back into a round-trip.

**Size warning.** 44 call sites is too big for one packet if done blindly.
Split it: convert the routes that can return temporal or `#object` values
first (entity, hyperedge, relation, evidence reads — these are the ones that
actually break), and leave pure-literal responses (`{:ok false :error "POST
only"}`) for a later mechanical pass. **One packet per group**, with the test
above proving the group is fixed.

---

## D2 — the boot gate retries rather than waits

The gate reads a watermark, builds the memory projection, re-reads, and
requires the two to match; a moving watermark burns an attempt. That is fine
when the store is caught up and fatal when it is not — 5 attempts is ~56
seconds, and the backlog needed far longer.

Current state: `max-memory-projection-build-attempts` is env-overridable via
`FUTON1B_PROJECTION_BUILD_ATTEMPTS` (`futon1b_graph.clj:717`), default 5
unchanged, and the live unit runs with **500**. That is a workaround, not a
fix: 500 expensive projection builds is a blunt instrument. Waiting for
indexing quiescence and then building **once** is the correct shape.

**Goal.** Replace retry-until-lucky with wait-for-quiescence-then-build-once.

**Acceptance.**
- Boot waits for the indexer to stop moving, then builds the projection once.
- **A bounded wait with a loud failure.** An unbounded wait converts a crash
  into a hang, which is worse — the whole thesis of this campaign is that
  failures presenting as normal operation are the expensive kind. It must say
  what it is waiting for and give up eventually.
- Cold-boot time does not regress: the current clean boot is **379 s**
  (measured 2026-08-14, log 4.30 GB). Report the new number.
- `FUTON1B_PROJECTION_BUILD_ATTEMPTS` either becomes unnecessary or is
  documented as legacy. Say which.

**Hard constraints.**
- Test against a store with a **real backlog**, not a clean one. A fix
  validated only on a quiescent store proves nothing — that is exactly the
  condition under which the old code also worked.
- Do **not** restart the live substrate to test this without asking Joe first.
  Boot is ~6m30s and the store is on the critical path for the ingest work.
  Backup: `/home/joe/code/futon1b-store-backup-20260813T164825Z`.

---

## Gates for every packet in this file

`clj-kondo` 0 errors/0 warnings; `futon4/dev/check-parens.el`;
`git diff --check`; `clojure -M:node -m test-temporal` (14 tests, 60
assertions as of commit `77c5a60`); the futon3c flexiarg tests (37) and
deletion/rename tests (33) where the change could reach them.

**Bell `claude-2` back with a summary + commit shas.**

**Pending restart, do not duplicate.** H4 (`entities` cursor + true-total
`:count`, commit `77c5a60`) is landed but **not live** — it needs a substrate
restart. Whoever restarts next picks it up; coordinate rather than booting
twice.
