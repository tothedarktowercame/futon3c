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

---

## D2/D3 — FIRST PRODUCTION TEST, 2026-08-14: both need recalibration

The first real cold boot with D2's quiescence wait **failed**, and D3's
pre-restart check had said it was safe. Both behaved as *written*; both are
mis-tuned. Recorded because this is the calibration data codex-3 explicitly
could not gather ("no before/after production cold-boot time was measured").

**What happened.** After the day's writes (1,184-file reingest + an
18,378-document retraction sweep), the store log had grown ~25.0 MB since the
previous clean boot. D3 judged that safe against its 32 MB default budget.
The restart then ran **605 s** at ~111% CPU and died:

```
UNIT DIED at 605s
Execution error (ExceptionInfo) at futon1b-gates/layered-error
indexing-quiescence-timeout
```

**D2 worked as designed.** It waited, did not hang, and failed **loudly** with
a named error — exactly the required behaviour, and strictly better than the
old retry-until-lucky path, which would have spent the same wall-clock doing
*expensive* projection rebuilds instead of cheap polling. The defect is only
the default: `FUTON1B_PROJECTION_QUIESCENCE_TIMEOUT_MS` = 600,000 ms is too
short for this store after a bulk write. Recovery: restart with the env var
raised (3,600,000 ms used).

**D3 is the more serious finding: its threshold predicts the wrong thing.**
It answers *"is the backlog small enough that a boot should work?"* — but the
quantity that actually matters is **how long indexing will take**, and the two
are not the same. A green check preceded a failed boot. Reference points now
on record for this store:

| log delta since last boot | boot outcome |
|---|---|
| +4.4 MB (quiet, 14 h idle) | **28 s** |
| ~0 (post-incident, 4.30 GB log) | **379 s** |
| **+25.0 MB (after bulk reingest + sweep)** | **FAILED at 605 s** |

So the budget is not monotone in "MB of log delta" alone — 25 MB of freshly
written transactions costs far more indexing time than the raw byte figure
suggests. **D3 should either predict a time and compare it against D2's
timeout, or state plainly that it bounds bootability rather than boot time.**
Silently implying the latter is the failure mode this campaign catalogues.

*Neither is a regression: before today the same restart would have hit the old
gate's 500 attempts. The difference is that we now get a named error instead of
a mystery.*


### ROOT CAUSE, MEASURED — D2 and D3 defaults are mutually inconsistent

The failed boot's own error report (`/tmp/clojure-15553444324293308928.edn`)
carried the offsets, which makes this exact rather than inferred:

```
waited-ms                     600,093
latest-submitted-byte-offset  4,335,031,163
latest-processed-byte-offset  4,327,068,451
remaining                         7,962,712 bytes   (8.0 MB short)
```

Against the 4,308,384,676-byte log size at the previous clean 28 s boot:

| quantity | value |
|---|---|
| backlog to index | 26.6 MB |
| indexed in 600 s | 18.7 MB |
| remaining at timeout | 8.0 MB |
| **observed log replay rate** | **31.1 KB/s** |
| time the full backlog needs | **856 s (14.3 min)** |
| what D2's 600 s default covers | **18.7 MB** |
| what D3 calls safe (32 MB) needs | **1,078 s (18.0 min)** |

**So D3 green-lights a backlog that D2 will not wait for — by nearly 2×.**
This is one uncalibrated pair, not two independent bugs. The boot did not
fail because the store was unhealthy; it failed because the gate gave up 8 MB
from the end of a job whose size the other gate had already declared
acceptable.

**The fix is to derive one from the other, not to pick two numbers.**
Either D3 predicts `gap_bytes / replay_rate` and refuses when that exceeds
D2's timeout, or D2's timeout is set from D3's budget at the measured rate.
Replay rate is measurable at runtime (poll the two offsets), so this can be
adaptive rather than a constant.

*Note the rate is the interesting quantity in its own right: **31 KB/s** of
log replay means a 4.3 GB log would take ~39 hours to replay from empty. The
store only boots quickly because it replays from a checkpoint. Anything that
invalidates the checkpoint is an outage, and nothing currently measures how
close we are to that.*
