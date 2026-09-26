# COMMISSION-D — what a commissioned click is, and whether the theorem's flight makes one

Discovery, read-only. claude-10, 2026-09-26, for claude-8 (PROOF-2a-PLAN ⟨3⟩0,
opened by WM-COVERAGE-D). Read at futon3c 24b27e1a (http.clj and
wm_org_layer.bb last changed at 7901a5a0 and earlier) and futon2 44656e3d. No
code, no map, nothing run, no click.

**Answer: (b), the flight's click is not commissioned. But the ten wires do not
need a commissioned-click record.** Their `(if commissioned?) then` condition
is an artifact of how the organisation layer picks a path, not a property of
the code. The ordinary, uncommissioned click reaches the same boxes through the
same function (§4).

## 1. What commissioning is

**Who introduced it.**
- `git log -S":r10-commissioned"` on futon3c: df0e47fb (2026-09-14, "R10: route
  opted-in clicks through commissioned dispatch", by codex-24), fixed at
  8331de51, and accepted at 32e41dbb by claude-15.
- The acceptance note is
  `holes/labs/wm-contract/runs/r10-wiring-2026-09-14/ACCEPTANCE.md`.
- It belongs to the R10 "commissioned and dispatched" cell of the R-node
  process census (futon2
  `holes/labs/wm-contract/ALIGN-rnode-process-census.md:374`, "R10 —
  commissioned and dispatched: ESTABLISHED 2026-09-08 (PA6z)").
- In the equation registry, R10 is **plumbing**. futon2
  `holes/labs/wm-contract/aif-equations.edn` `:plumbing [:R9 :R10 :R11 :R12
  :R15 :R20 :TRACE :GRAIN-GATE]` places it among the nodes that run no
  equation.

**What the branch does.** In futon3c `src/futon3c/transport/http.clj`,
`handle-wm-click-start` (:8962-8995):
- `commissioned?` is `(true? (:r10-commissioned payload))`.
- A commissioned click that also carries `:run4-pin-ref` is refused with
  `:r10-commissioned-with-run4-pin-ref`.
- A **commissioned** click resolves
  `futon3c.wm.r10-click-adapter/commissioned-click!` and calls it with **only**
  `{:config config :issuer-provenance issuer-provenance}`.
- An **uncommissioned** click resolves `futon3c.wm.runner-service/click!` and
  calls it with `opts`: the legacy opts, which include the posted `:flight`,
  plus the prepared RUN4 opts. When it is neither commissioned nor RUN4, it
  also installs `:ordinary-click/issue!`, which spends the ordinary click
  budget (`ordinary-budget/consume!`), and runs the cast preflight.

**What the commissioned branch adds, and what it drops.** In
`src/futon3c/wm/r10_click_adapter.clj`, `commissioned-click!`:
- It spends the server-owned single-use commission
  (`r10-commission-binding/authorized-commission`, an operator-selected
  authority file validated by sha256 in
  `r10_commission.clj/load-authorized-commission`).
- It refuses on a missing evidence store or a busy runner.
- It reserves durably, then calls `click-fn` with
  `(cond-> {} issuer-provenance (assoc :issuer-provenance issuer-provenance))`.
- It records a coordination-ledger dispatch
  `{:node :R10 :commission/id … :dispatch/id click-id …}` through
  `ledger/run-scheduled-dispatch!`, then `mark-recorded!`.

So a commissioned click adds operator authority and an R10 ledger receipt. It
**drops every click option**, including `:flight`: `click-fn` receives no flight
at all.

**The ten wires.** These are the entries in `wm-wire-coverage.edn` whose
`:conditions` mention it. The coverage file was generated at ledger rev
06d451d6 and org layer 06d451d6, and is marked `:stale? true`.

| wire (writer → reader, field) | coverage | carries |
|---|---|---|
| `:flight-entry → :r9-close-cause`, `target@flight` | conditional | the flight's target, read by the runner's close cause (`(get-in opts [:flight :target])`) |
| `:gate-refuse → :gate-refusal-read`, `:error` | failure-path | the decision gate's refusal |
| `:r9-close-cause → :failure-cause-record-test`, `:failure-cause` | conditional | the close cause, to its test |
| `:r9-close-cause → :r9-finding-store`, `:failure-cause` | conditional | the close cause, into the repair finding |
| `:r9-decision → :r9-judge-refusal-read`, `:kind` | conditional | the joint decision's typed refusal kind |
| `:r9-judge-refusal → :r9-abstention-carrier`, `:judge-refusal` | failure-path | the judge refusal, into the run record's abstention |
| `:r9-judge-refusal → :r9-judge-refusal-test`, `:judge-refusal` | failure-path | the same, to its test |
| `:r9-judge-refusal-abstention → :r9-failure-classifier`, `:outcome` | failure-path | the abstention's outcome |
| `:r9-phase-kind → :phase-kind-test`, `:failure-kind` | conditional | the phase's failure kind, to its test |
| `:r9-phase-kind → :r9-failure-classifier`, `:failure-kind` | conditional | the same, to the classifier |

That is six conditional and four failure-path. Every one reaches its reader, and
several their writer, through `:click-start → :r9-close-cause`. The org layer
records that call as
`{:caller :click-start :callee :r9-close-cause :conditional "(if commissioned?) then" :via ["futon3c.wm.r10-click-adapter/commissioned-click!" "futon3c.wm.runner-service/click!" "futon3c.wm.runner-service/run-click!" "futon2.aif.full-loop-runner/run-opportunity!"]}`.

## 2. Is the theorem's flight a commissioned click by definition?

No. The theorem says the opposite.
- futon2 `holes/labs/wm-contract/PROOF-2a-THEOREM-draft-2026-09-24.md`, under
  "Per-click conditions" (:46-54): "All eight clauses hold on the single
  linked sequence `L` of PROOF-2 (**ordinary live clicks under A21**)".
- PROOF-2's `L` is "the finite, globally ordered sequence of ordinary live
  clicks declared under A21" (`PROOF-2-THEOREM-draft-2026-09-24.md:34`).
- A21 is `PROOF-2-ASSUME-draft-2026-09-24.md:174`.
- **"Ordinary" names http.clj's third branch**: neither commissioned nor RUN4,
  the one that installs `:ordinary-click/issue!`, whose budget namespace is
  `futon3c.wm.ordinary-click-budget` (http.clj :107, :8987-8989).
- No clause (T, 0, C, 1–6) and nothing in the flight definition (:56-) mentions
  commissioning. Its only use of "commissioned" (:599) is about a mission file
  commissioned by agents, which is unrelated.
- Commissioning is an operator-authority property of the HTTP call, and R10 is
  plumbing. It lies outside the theorem.

## 3. What the flight driver sends today

- futon2 `src/futon2/aif/flight_runner.clj` `http-click-fn` (:526-529) posts
  `(merge {:flight-edn (pr-str flight) :run-id run-id :issuing-caller caller
  :trigger "duree-click-on-demand"} <the cast's string seats>)`.
- There is no `:r10-commissioned`: a grep of futon2 `src`, `scripts` and `test`
  finds none.
- So the flight's click takes the ordinary branch.

**Setting the key would be wrong, not merely unwarranted.**
- A commissioned click calls `click-fn` with no options, so the flight's
  `:flight` (target and wants) would never reach the runner. The click would
  assemble the whole portfolio instead of the flight's target, which breaks
  flight rule 4 and turns every wire that reads the flight record into a
  typed absence.
- It would also need an operator commission file per click, which is
  single-use.
- It is no red-tape-removal case, and it isn't an AIF-validity case either,
  since R10 runs no equation.
- For the ten wires, it would change nothing they carry. The same `run-click!`
  runs either way. It would only add the R10 ledger receipt, and it would take
  the flight target away from the first wire.

## 4. Why the ten wires are not off the flight's path

In `holes/labs/M-wm-wiring/spike/wm_org_layer.bb`, `box-calls` (:190-203) walks
from a box's var through the non-box futon vars it calls. It records each box
var it reaches with:

```clojure
(vswap! out update r #(or % {:callee r :loc fl :via via :conds conds'}))
```

That keeps the **first** path found to each callee, and drops every later one.

In `handle-wm-click-start`, both branches are literal quoted symbols in one
`(if commissioned? 'futon3c.wm.r10-click-adapter/commissioned-click!
'futon3c.wm.runner-service/click!)`. The then-symbol comes first in the text,
so the walk reaches `runner-service/click! → run-click! → run-opportunity!` first
through `commissioned-click!`, and records that path's condition,
`(if commissioned?) then`.

The else-branch symbol is `futon3c.wm.runner-service/click!` itself, **the same
var that path passes through**. So it reaches `run-click!` and
`run-opportunity!`, and the same boxes, with no commissioning condition. The
`(or % …)` discards that path.

So the flight's ordinary click does reach `:r9-close-cause` and everything
after it. What decides each wire's coverage is its other condition, if it has
one:
- **`:conditional`, with no other condition:** the four `:r9-close-cause` and
  `:r9-phase-kind` wires, `:r9-decision → :r9-judge-refusal-read`, and
  `target@flight → :r9-close-cause`. With the path corrected, these are
  witness-path wires on an ordinary click.
- **`:failure-path`:** they keep their own conditions, `(if-let [r (or jr gr)])`
  and `(cond (abstention? decision))`. They need a click that refuses or
  abstains, and would be witnessed on such an ordinary click, not a
  commissioned one.

## 5. Outcome and the next packet

**(b), not commissioned.** Commissioning is outside PROOF-2a, whose clicks are
ordinary by A21. The flight must not set the key: the commissioned branch drops
`:flight`.

The ten wires need neither a commissioned-click record nor a dedicated run. They
need **the organisation layer to keep the unconditional path**. That's a
one-behaviour packet on `wm_org_layer.bb` `box-calls`, whose owner is the
org-layer lane (7901a5a0). The fix would be to keep all paths per callee and
report a call as conditional only when every path is, or to prefer the path
with fewer conditions.

With that fix, the coverage partition should move the six to `:witness` and
leave the four `:failure-path`. The four are owed to an ordinary click that
refuses or abstains: the failure records the flight already writes (the close
cause, the repair finding), or a hermetic test of that branch.

I haven't regenerated the coverage file, and haven't changed the generator.
