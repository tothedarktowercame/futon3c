# T-strategic-cascade-emits-disconnected-patterns — 23 malformed cascades are one defect

**Opened:** 2026-08-28 · claude-13, from Joe: *"we don't need a detailed
accounting of the historical failures so much as their transformation into future
successes — indeed that's an example of the tracer-in-the-map. The 23 of 25
broken cascades is **data** — if we had a working eat-the-tail we would be able to
propagate it forward into a well-working cascade system."*

**Status:** open. Diagnosis complete and reduced; the repair and its acceptance
corpus are stated below. The futon3c consumer side is implemented
(2026-08-31, see below); the futon2 producer-side census pin still moves in a
futon2 commit.

### futon3c consumer side, 2026-08-31

`futon3c.peripheral.strategic-cascade` now enforces the repair's option 2 at
the consumption boundary. `validate-cascade` accepts an optional
`:semilattice :unrelated` carrier of
`{:pattern-id <shown-member> :reason <nonblank>}` declarations; a declaration
without a typed reason, or naming a pattern outside the `:shown` carrier,
throws `invalid unrelated-pattern declaration` (fail closed). `outer-frontier`
emits an `:unrelated-carrier-pattern` hole for every carried pattern that has
no `:descent` edge and no typed declaration, and echoes accepted declarations
in the result `:cascade` so a reviewer can distinguish deliberate
unrelatedness from an untyped absence. Golden Phase-5 fixtures are unaffected
(all their carrier members are connected). The census pin in
futon2 `cascade_order_check_test.clj` is deliberately untouched: it belongs to
the futon2 producer and moves in a futon2 commit per this ticket's own header.

## The reduction: 23 records, one defect

The census (`futon2/test/futon2/aif/cascade_order_check_test.clj`) pins 23 of 25
generated cascades as having a pair with no greatest lower bound. Read as a
tally that is 23 incidents. Characterised, it is **one**:

| | |
|---|---|
| distinct offending pairs | **4** |
| cascades where `agent/sense-deliberate-act` is in the offending pair | **23 of 23** |
| lower-bound set in every case | **`#{}`** — empty |

    x17  ["agent/sense-deliberate-act" "iching/hexagram-05-xu"]
    x3   ["agent/budget-bounds-exploration" "agent/sense-deliberate-act"]
    x2   ["agent/sense-deliberate-act" "iching/hexagram-43-guai"]
    x1   ["agent/sense-deliberate-act" "iching/hexagram-03-zhun"]

**`agent/sense-deliberate-act` is placed in the `:patterns` carrier of every
construction and given a `:descent` edge in none of them.** It is not weakly
related; it is unrelated. An empty lower-bound set in all 23 says so precisely.

And the constructor is under-connecting in general. Edge counts across the 23:

    1 edge  × 16     2 edges × 3     3 edges × 1     0 edges × 3

Sixteen of twenty-three cascades have **a single edge** over a carrier of three
or four patterns. A one-edge cascade cannot relate most of its own members.

## Why this is the eat-the-tail case, and not a bug report

The 23 were not audited; they were **read as a tracer**. Each malformed cascade
carries a mark of the route the constructor took, and reading the marks together
gives one wrong turn rather than twenty-three incidents. That is the same move as
§3.1i — invert the failures, dedupe, and what remains is the invariant — applied
to generated output rather than to a git log.

The transformation Joe asks for is then available: **the 23 become the acceptance
corpus for the repair.** They are not history to be accounted for; they are the
inputs a fixed constructor has to handle.

## The repair

`futon3c/src/futon3c/peripheral/strategic_cascade.clj` — the `:descent` producer.

Every pattern the constructor places in the `:patterns` carrier must either

1. carry at least one `:descent` edge relating it to another carrier member, or
2. be reported as **deliberately unrelated, with a typed reason** — which is I2
   applied to dependency: a pattern with no edges is currently an untyped absence
   of relation, indistinguishable from a pattern whose relations were never
   computed.

Option 2 is not a loophole. `p4ng/app-snatch.tex` records the same situation in a
different collection — `exchange-when-both-sides-gain` declares no `@why` because
it states the game's gain and has nothing above it to stand on — and the right
answer there is to say so, not to invent an edge.

## Acceptance

- Re-run the constructor over the inputs behind the 23 recorded constructions.
  Every carrier member has an edge, or a typed reason.
- The census pin moves, and moves **deliberately**: the test in
  `cascade_order_check_test.clj` is updated in the same commit, per its own
  header.
- No new cycles. The four cyclic records are a **separate** defect in the same
  producer and are not addressed here; fixing connectivity by adding edges in
  both directions would trade one failure for the other.

## Not in scope

The four cyclic `:descent` records; `close_loop.clj`'s producer/consumer shape
mismatch (§3.1g); the 40 constructions carrying no `:semilattice` at all, which
are typed in the census and not yet explained.

## Related

- `futon2/holes/missions/M-formal-war-machine.md` §2.1d (what a cascade is),
  §3.1g (the flight's structures are typed and unchecked), §3.1i (invert the
  failures).
- `futon2/test/futon2/aif/cascade_order_check_test.clj` — the pinned census.
- `DarkTower/WarMachine/CascadeOrder.lean` — `acyclicDescent`, `hasMeets`.
