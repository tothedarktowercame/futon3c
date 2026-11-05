# T-evidence-pinned-to-mutable-prose-26082026 — 8 of 18 deposits died because two flexiargs were edited

**Status (triaged 2026-09-01): PARTIAL -- blast radius closed, mechanism unchanged.** futon2 42a997e quarantines the 10 unreconstructable fold turns (load-deposits: 8/10/0, reproduced at review), so nothing throws today. But fold_escrow.clj still slurps CURRENT flexiarg prose with no sha pin, so future prose edits can still kill replay-valid deposits. Close after picking one of the ticket's three candidate fixes.

Parent: `E-R8-red-ring-fill.md` slice 4. Found 2026-08-26 by claude-13. Status:
open — three candidate fixes below, none chosen.

## The finding

A fold-turn deposit pins `:prompt {:sha256 … :prose-sha256 {<pattern> <sha>…}}`.
`fold_escrow/reconstruct-prompt` rebuilds the prompt by slurping **current**
flexiarg prose from `/home/joe/code/futon3/library`. If any cited prose has
changed since deposit, the sha differs and the deposit is rejected
`:prompt-not-reconstructable`.

**Eight of eighteen deposits are currently dead this way**, including
`ft-bayesian-structure-learning-003` — the mission behind every realized outcome
the loop has ever produced.

Measured on that deposit (made 2026-07-05, 15 proses pinned):

    aif/expected-free-energy-scorecard     last commit 2026-08-23
    structure/interest-event-vocabulary    last commit 2026-08-15

Both edited after the deposit. Editing pattern prose is normal, encouraged work
— on 2026-08-26 Joe repointed four `@why` declarations as routine H5 business.

## Why it is fatal rather than noisy

`fold_escrow/load-deposits` degrades **by design**: *"Rejections go to stderr
AND the return value; valid deposits still serve."* But
`actuator_a3/deposits-by-id:149` throws on `(seq rejected)`, so one dead deposit
makes the whole corpus unreadable — and `enact.clj:255`'s
`(catch Throwable _ judgement)` swallows the throw and returns the judgement
unchanged. Silent.

## Candidate fixes (none chosen)

1. **Store the prose in the deposit**, not only its sha — reconstruction stops
   depending on the current tree. Largest deposits, strongest guarantee.
2. **Version the reference** — pin `pattern@git-sha`, read the historical blob.
3. **Downgrade the check** — drift becomes a warning carrying old and new shas,
   rejection reserved for missing or malformed records.

Separately and cheaply: decide whether `deposits-by-id`'s strictness is
load-bearing, given the layer below it already degrades.

## Cross-references

- `futon2/holes/NOTE-slice4-slice5-understood.md` — the measurement and the series argument.
- `futon2/holes/NOTE-grounded-feed-missing-input.md` — the three-layer silence.

## Completion criteria

- One of the ticket's three candidate fixes is chosen and recorded in the ticket — check: a decision line naming fix 1, 2 or 3 under `## Candidate fixes` in the ticket file, or a successor item linked **Not met.**
- The chosen fix is implemented so deposit reconstruction no longer depends on the current flexiarg tree — check: `/home/joe/code/futon2/src/futon2/aif/fold_escrow.clj` `flexiarg-prose` (line ~107) reads pinned content (stored prose or `pattern@git-sha` blob) instead of slurping `flexiarg-dir/<pattern>.flexiarg`, or the check is downgraded to a warning **Not met.**
- The quarantine of the 10 unreconstructable fold turns still loads clean after the fix — check: `futon2 git 42a997e03` behaviour intact (load-deposits 8/10/0) via the gate/test named in that commit **Met.**
- A decision on `deposits-by-id` strictness is recorded — check: ticket file or a TN states whether `actuator_a3.clj:149-151`'s throw on rejected deposits stays or goes **Not met.**
