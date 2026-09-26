# TN — Contracts for the twelve unimplemented tools

**Written** 2026-08-15 by claude-2, after Joe's ruling: the apm-driver is
*inspiration only* — two witnessed chains over ~200 problems is an existence
claim, not a warrant to depend on — and the census is ground truth. Pure Clojure
port. Batched dispatches.

**Discovery before implementation.** Each tool's output contract is not a matter
of taste: it is fixed by what consumes the output. Two consumers, both already
built: `apm/cycle_harness.clj derive-trace` (which entity fields it reads) and
`apm/preregistration.clj` (which invariant fires when they are wrong). Every
contract below is read off those, not invented.

## First: four of the twelve are observe-only

`problem.clj:61-65` marks these `:observe` rather than `:action` —
`:read-registration`, `:validate-registration`, `:read-substrate`,
`:read-attempt-result`. They produce no phase output and no trace entity. They
are the *inhabitant's* instruments (an agent inspecting state), not the machine's
producers. Lower priority, and cheap.

**Eight action tools carry the weight.**

## The contracts

| tool | must produce | fields `derive-trace` reads | invariant that bites |
|---|---|---|---|
| `:emit-frame` | `:frame` entity | `:frame/id :frame/cycle :frame/scaffold-hash :frame/closing-hash :frame/keys` | **F1** `:f1-scaffold-identical-frame` — closing hash **equal to** scaffold hash means the frame did no work |
| `:emit-frame` (2nd half) | `:cprobe` containment probe | `:cprobe/id :cprobe/frame :cprobe/claimed? :cprobe/recorded? :cprobe/passed` | **F8** `:f8-unwitnessed-containment` |
| `:snapshot-store` | `:snap` entity | `:snap/id :snap/cycle :snap/memory-ids` | feeds the measured transfer channel |
| `:freeze-stratum` | `:cycle/stratum-frozen-at` (integer) | `:stratum-frozen-at` vs `:assigned-at` | **F4** — must be an integer **strictly less than** `:assigned-at` |
| `:write-disposition` | `:disp` entity | `:disp/id :disp/cycle` | **F2** `:f2-non-unique-disposition` — on a closed cycle, **exactly one** |
| `:write-use` | `:use` entity | `:use/id :use/offer` | **F3** `:f3-undispositioned-offer` — offer ids must be a **subset** of use-offer ids |
| `:promote-artifact` | `:promo` entity | `:promo/id :promo/cycle :promo/artifact-id :promo/importable :promo/need-tags` | capabilities `:promotion-importable`, `:promotion-need-taggable` |
| `:guide-solver` | `:ground-control-events` | guidance count per cycle | **P1**, the round's headline prediction |
| `:pin-resources` | — see below | — | — |

## `:pin-resources` is now probably redundant

Since `29638fde` and `7c743f77` the engine stamps `:environment-revision` and
`:harness-revision` itself, from the recorded `:assign-checkouts` and a measured
harness repository. A caller-invoked `:pin-resources` would either duplicate that
or, worse, offer a second and forgeable route to values the engine owns.

**Recommendation: delete the tool rather than implement it.** Resolve before
dispatching the register batch — an implemented-but-redundant pin is exactly the
"second route to an owned field" that this session has spent the day closing.

## Proposed batching — four packets, not twelve

Grouped so each packet is one phase, one backend surface, one acceptance test.

| packet | tools | acceptance |
|---|---|---|
| **A — register** | `:snapshot-store`, `:freeze-stratum` (+ `:pin-resources` decision) | smoke traverse reaches `:frame` with a real `:snap` and an integer `stratum-frozen-at < assigned-at`; F4 fires when it is not |
| **B — frame** | `:emit-frame` + containment probe | F1 fires on a scaffold-identical frame; F8 fires on an unwitnessed containment claim |
| **C — adjudicate/promote** | `:write-disposition`, `:write-use`, `:promote-artifact` | F2 fires on zero or two dispositions; F3 fires on an offer with no use |
| **D — guidance + observers** | `:guide-solver`, and the four `:observe` tools | guidance events are counted per cycle and reconcile with the validator's independent Agency count |

B is the one that matters most for frame-1: **F1 is the round's only structural
invariant**, and it is currently computed over a frame that nothing produces.

Each packet must state which invariant its acceptance test makes **fire**, not
merely pass. A tool whose output only ever satisfies its gate has not been shown
to be connected to it — the lesson of `:f7-missed-available-artifact`, which was
vacuous by construction, and of the retrieval-probe gap Fable found this morning.
