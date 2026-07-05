# T-dispatch-clock-excursion-prefix — E-*/C-* ids can't be dispatch-clocked

**Found 2026-07-05 (claude-18) while specifying the cascade-live tickets box.**

`clock-store/set-dispatch-mission!` (futon3c/src/futon3c/agency/clock_store.clj)
normalizes any id that doesn't start with `M-` by **prepending `M-`**:

```clojure
mission (if (str/starts-with? mission "M-") mission (str "M-" mission))
```

So `agency_send.py --mission E-evidence-flow` would durably clock the recipient to
the fictional `M-E-evidence-flow`. Excursions and campaigns currently enter clock
lineage only via the edit-activity autoclock (`record-edit!` handles `C-/M-/E-*.md`
doc paths correctly — the asymmetry is only in the dispatch path).

**Why it matters now:** the tickets box (unclocked missions AND excursions) makes
"claim by clocking" the advertised move; for excursions the advertised
`--mission` route silently mis-clocks. Also relevant to E-evidence-flow /
M-pheromone-field, both chartered 2026-07-05 and both likely to be claimed via
dispatch.

**Suggested fix (small):** accept ids matching `^(M|E|C)-` as-is; keep the M-
prefix default only for bare names. One guard clause + a unit case alongside the
existing clock-store tests. Touches durable lineage labels, so grep
`clock-label`/lineage consumers for M- assumptions before shipping.

**Owner: unclaimed.**

## Fixed

Commit: `5dc3f2a`

Implemented typed dispatch routing in `clock-store/set-dispatch-mission!`:

- `M-*` stays in `:mission-id`.
- `E-*` stays verbatim and routes to `:excursion-id`.
- `C-*` stays verbatim and routes to `:campaign-id`.
- bare ids still get the historical mission shorthand, e.g. `live-efe-map` →
  `M-live-efe-map`.

Chose typed routing rather than putting `E-*`/`C-*` into `:mission-id` because
`clock-lineage/canonical-target-endpoint` already maps `:excursion-id` to
`<repo>-d/excursion/<stem>` and `:campaign-id` to campaign endpoints. Keeping
typed ids in `:mission-id` would preserve display but write the wrong canonical
scheme. To keep the live EFE map honest, its live-clock read now uses the same
single-active target precedence as lineage (`excursion` → `mission` →
`campaign`).

Verified via Drawbridge:

```clojure
(do
  (futon3c.agency.clock-store/set-dispatch-mission!
   "test-ticket-fix" nil "E-evidence-flow")
  (futon3c.agency.clock-store/current-clock "test-ticket-fix" nil))
;; => {:campaign-id nil, :mission-id nil, :excursion-id "E-evidence-flow"}
```

No public per-agent removal API exists; `test-ticket-fix` is a throwaway
in-memory clock-store entry and was not durably persisted.
