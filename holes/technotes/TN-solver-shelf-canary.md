# Solver shelf canary — dormant qualification packet

This park implements the authority boundary requested by the F66–F70 Fable
audit. It does **not** activate an experiment or alter an already minted frame.

## Assignment authority

Before the eligible frame is minted, an operator may atomically install
`<campaign-root>/solver-shelf-canary.edn` with this schema:

```clojure
{:schema/version 1
 :canary/id "solver-shelf-c1"
 :eligible/frame-id "fNN"
 :assignment :shelf                 ; or :control
 :matched/size 4
 :shelf/entries [{:memory-id "e-..." :hook "..." :body "..."}]
 :shelf/digest "<campaign-machine/ledger-digest of [entries]>"}
```

For `:shelf`, `:matched/size` must equal the number of entries. For `:control`,
entries must be empty and `:matched/size` records the size of the shelf to
which the control was matched. The assignment applies only to the exact frame;
later frames do not inherit it. Malformed authority selected for a frame makes
Solver request construction fail closed.

The complete entries and digest are covered by frame mint, manifest, request,
dispatch, and receipt identities. The request is durably persisted before job
announcement. The Solver reports surfaced and used IDs, but those observation
fields cannot add exposure: surfaced IDs must equal the controller-owned shelf,
and used IDs must be a subset.

## Activation remains gated

The current sequential one-problem queue cannot construct a scientifically
valid size-matched paired control by itself: it has no pre-result matcher or
randomized assignment mechanism. Activation therefore requires an operator to
preregister both eligible frame IDs, the matching rule, the exact source shelf,
and assignment order before either frame is minted. Installing only a shelf
arm and choosing a control after seeing the result is forbidden.

Qualification/reload order is `solver-shelf-canary`, `live-proof-phases`,
`queued-frame-adapter`, then `countdown-control`. After reload, use a stopped
synthetic queue to demonstrate shelf and control request/receipt pairs before
installing an arm in a live campaign.
