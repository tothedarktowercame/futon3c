# PLAN H3 — put the cascade on the student path, as an arm, and measure it

*claude-19, 2026-08-26. Joe: "let's finish off H3." Last in the order
H0 → H1 → H4 → H2 → H5 → H3. Everything before it is done and measured
(`technotes/TN-APM-cascades-exist-unused.md` addenda 2–7).*

## What H3 can and cannot show after H5

The PLAN's definition stands: a frame receipt with (1) a non-zero expanded
route count and (2) a `:used-ids` entry that arrived by an expanded route.
After H5 the store's why-graph reaches no populated pattern from an API-heavy
shelf (f42c: why-hop 0), and co-incidence is a flood (96). So a why-hop-only
wiring would satisfy neither. The route that can produce (2) is the
**sibling** route from `analysis/h5/NOTE-H5-before-after-2026-08-26.md`: the
other memories on the shelf's own patterns, not on the shelf. It is bounded
by the shelf's patterns (narrows), and it is exactly the expansion a reader
of a pattern language expects first. H3 therefore ships a route set, per arm,
with sibling on and co-incidence off by default; why-hop stays selectable so
the arm can be re-run when the middle tier is populated.

## Three handoffs

| | what | where | who |
|---|---|---|---|
| **H3a** | `:routes` option on `expand-memory-cascade` (default unchanged), `:sibling` route, `:routes-enabled` in the result; first sibling measurement on f42 | `apm/conductor.clj`, `conductor_test.clj`, `cascade_dry_run.clj` | codex-20 (running) |
| **H3c** | wiring on the countdown path behind a manifest-pinned arm config; offers delivered in the packet; used-ids validation extended; route histogram on the receipt | `queued_frame_adapter.clj`, `countdown_control.clj`, `live_learning_phases.clj`, tests | codex-21 |
| **H3d** | arm registration: manifest field on for the next minted frame, prereg amendment 7, reload at a frame boundary, first receipt read | operator + claude-19 | claude-19 |

## The wiring map (H3b, read from the code 2026-08-26)

1. **Arm config** lives on the manifest unit. `queued_frame_adapter/one-off-manifest`
   builds `{:frame/id … :arm :treatment :problem {…}}` from the queue frame;
   add `:memory-cascade {:enabled? bool :routes [:sibling …] :cap 100}` to
   the unit, sourced from the queue frame (a campaign-level default in
   `queue-state.edn` or the mint call), so the arm is content-addressed with
   the manifest (`:manifest/id` covers it). Absent ⇒ off, byte-identical to
   today.
2. **Compute at student dispatch.** `countdown_control/live-learning-phase-inputs`
   already resolves `promotion` and `snapshot-access` for `:student-attempt`;
   the seeds are `(:accessible-memory-ids snapshot-access)`. Call
   `conductor/expand-memory-cascade` with `#'conductor/live-cascade-readers`
   (as `cascade_dry_run.clj` does), `:routes` and `:cap` from the unit
   config. Record wall-clock (f42's 48 seeds took ~230 s of substrate reads).
3. **Deliver in the request.** `build-request` adds, for `:student-attempt`
   when enabled, `:memory-cascade {:routes-enabled … :cap … :truncated?
   … :expanded-available … :offers [{:memory-id :route :hops :pattern
   :pattern-hook} …] :histogram {route count}}`. The packet is `(pr-str request)`
   plus instructions (`live_learning_phases/prompt`), so the student sees it
   verbatim; add one sentence to the `:student-attempt` text: the
   `:memory-cascade` offers are also readable and citable, each labelled with
   the pattern route that reached it. The student fetches memory bodies by id
   from the substrate exactly as for the shelf; no new content channel.
4. **Authority.** `verify-student-access` keeps its exact-set check on the
   snapshot (unchanged). In `live_learning_phases/validate-terminal`,
   `allowed-memory-ids` = snapshot ∪ search-surfaced ∪ **cascade-offered**.
   `controller-memory-use` is unchanged (`surfaced-ids` stays
   shelf ∪ search, so `fingerprint_audit.py` / `shelf_order_audit.py` keep
   their meaning); the cascade is its own controller-derived record.
5. **Receipt.** `receipt` for `:student-attempt` adds
   `:receipt/memory-cascade` = the request's `:memory-cascade` minus
   `:pattern-hook`s, plus `:used-via-cascade` = `used-ids ∩ offered ids` with
   each id's route. That last field is the "built and used" receipt the TN
   measured the absence of. `generated_contract.required-submission-schemas`
   lists controller-derived fields; add `"memory-cascade"` and
   `"evidence.memory-cascade.used-via-cascade"` there and keep
   `validate-round-trip` green (the EDN contract file must match).
6. **Packet archive** (H1) captures the delivered offers automatically.
7. **Ordering.** Offers keep the expander's order ([hops, route rank, id]);
   H2's shelf ordering is not applied to them in this arm — one variable.

## Measurement

Per attempt from the receipt: `:receipt/memory-cascade :histogram`,
`:expanded-available`, `:truncated?`, `:used-via-cascade`. "Built" = histogram
non-empty; "built and used" = `:used-via-cascade` non-empty. Also report the
packet size delta (offers × hooks) and the dispatch delay from step 2.
Comparison is against the same-campaign frames with the arm off; the unit of
evidence is the frame, as the preregistration requires.

## Not in H3

- Changing what co-incidence or why-hop compute (H3a only selects among them).
- Applying H2's ordering to the offers.
- Any claim about benefit: this is the existence demo the PLAN asked for.

*Status, 2026-08-26 (claude-19):* **H3a done** (`b5095e37`, `1cc861f9`; reloaded
from master). f42 shelf: sibling-only 130 offers / 12 patterns; default 93.
H3c in flight (codex-21). H3d pending: frame choice + amendment 7.

*Status, 2026-08-26 evening (claude-19):* **H3c done** (`7fac097b`, reviewed;
contract v4 + Lean authority `2f87ffba43`). **H3d done**: arm-file fallback in
`set-alight-problem-list!` (per-tick read, no coordinator restart), all
namespaces reloaded from master, `memory-cascade-arm.edn`
`{:enabled? true :routes [:sibling] :cap 100}` committed for the next mint
(expected f44), prereg amendment 7 with predictions P-a/P-b/P-c and the
falsifier. Reversal: delete the arm file before the mint. Next: read f44's
first student receipt for `:receipt/memory-cascade`.

*Note, 2026-08-26 (from claude-13's reading of the report):* the sibling route
changes what the expander claims to do — from *reach new patterns* to
*exhaust the patterns you already hold*. That is a different claim about
what a cascade is for, and the arm's receipts should be read as testing that
claim, not the original descent-from-high-level-patterns one. Of amendment
7's predictions, only P-c can fail in a way that matters: P-a ("built") and
P-b ("truncated") both pass under a cascade that helps nobody.

*Prior on P-c (claude-13, same thread):* H4 returned NO on f42's 103 offers.
If the sibling route surfaces material of the same relevance, P-c fails; if
it doesn't, the arm has shown — with a receipt — that exhausting held
patterns finds more usable material than reaching new ones did. Either way
the arm yields a result rather than an artifact.
