# E-APM-f10-defects — defect inventory for the APM demonstration, through frame 10

**Opened 2026-08-18 by claude-2 (ground control, M-apm-demonstration) at Joe's
direction**, while frame-10's Analyst was still running: *"while we wait for the
analyst (which will have more findings) I think we should make a list of
E-APM-f10-defects.md (it can include any up to and including f10)."*

Scope: everything found up to and including f10. **The Analyst's f10 close is
still in flight and will add findings** — this file is therefore explicitly
incomplete at the time of writing, and §7 says how to extend it.

Each entry states where the defect is, how it was evidenced, and what it costs.
Entries verified at source by ground control are marked **[verified]**; entries
taken from an agent's report and not independently re-derived are marked
**[reported]**. That distinction is load-bearing: several items below were first
reported with a plausible mechanism that turned out to be only half right.

---

## 1. Blocking — a frame cannot run reliably without these

### D1. `mint-seats` cannot set a model, so every claude seat mints onto Fable **[verified]**

`POST /api/alpha/frames/mint-seats` builds claude seats with no `:model`.
`transport/http.clj:2497` assocs `:model` **only when requested**, and the mint
does not request one, so a minted claude seat inherits the CLI default — Fable 5.

The Fable quota was exhausted on 2026-08-18 and this defect then broke two seats
in one day:

| seat | job | outcome |
|---|---|---|
| `analyst-1` (f9 close) | `invoke-1787042851114` | died mid-analysis after ~3 min, having already run the checks and reached "seven named validator failures". **All work lost.** |
| `f10-guide` (orientation) | `invoke-1787045640498` | died in 3 s, before reading its card |

Both were recovered only by hand-written re-registration scripts
(`/tmp/register_opus_analyst.clj`, `/tmp/register_opus_guide.clj`), which are
live-state only and **do not survive a futon3c restart**.

Note the failure shape: the bellback carries only `Exit 1:` with an empty
message. The actual cause — *"You've reached your Fable 5 limit"* — is a `text`
event inside the job record. An operator watching bells alone sees nothing
actionable.

**Cost:** every future frame needs a manual re-registration between mint and
first contact, or it fails on first bell. **Fix:** accept a model argument in
`mint-seats!` (`agency/frame_seats.clj:56`) and thread it through, so the seat
model is set at mint time and can be pinned by the registration rather than
patched in live state.

### D2. The guidance count is broken in every cascade-enabled frame **[verified]**

`preregistration.clj:373` `guidance-count` computes

```clojure
(- (count (filter #(and (= solver-seat (:agent-id %)) (inside-cycle-window? trace %)) jobs))
   (count (:memory-offers trace)))          ; line 382
```

The docstring calls the subtrahend "machine-recorded openings", which it was —
one per dispatch — **before the cascade landed**. `:memory-offers` is now the
flattened *per-memory* offer list, so the units no longer match:

| frame | dispatches | offers | reported | true |
|---|---:|---:|---:|---:|
| f9 | 1 | 101 | **−100** | 0 |
| f10 | 1 | 102 | **−101** | 0 |

Both frames' true guidance count is 0 — f10's guide sent zero guidance bells
because the solver never reported an obstruction, and declined to manufacture
any to populate the count.

**Cost:** a headline measure of this experiment is unusable in f9, f10 and every
future cascade-enabled frame, and it is **retroactive** — f9's number is wrong in
the record too. **Fix:** count receipts (one per dispatch opening), not offers.

---

## 2. The promotion pipeline is structurally unreachable

f10 attempted 4 promotions and **1 succeeded**. The three refusals have three
distinct causes, all at `peripheral/memory_lifecycle.clj:240-259`.

### D3. Attach-then-review is unreachable for guide-authored deposits **[verified]**

`promote-memory-attachment!` (`memory_lifecycle.clj:203`) refuses when
`(= reviewer depositor)` (line 249). But the conductor's acting identity is
**always the guide**, and the guide is the depositor for `:promote-solver`
deposits. So the reviewer is forced to equal the depositor, and the gate refuses
by construction — for exactly the class of deposit that `:promote-solver` exists
to promote.

f10's guide fell back to the artifact-id path (f8/f9 precedent). The
consequence is not cosmetic: the scribe's independently approved `PATTERN-ID`
(`math-informal/local-to-global`) and `REVIEW-EVIDENCE-ID` never reach the promo
entity, and **the memory's substrate edge stays statusless**.

Independently corroborated: the guide-authored deposit `e-17bd0295` carries no
`:memory/assert` key, while every scribe-authored deposit does.

**So the cascade cannot reach the one memory the frame most wanted reachable** —
the route memory the student actually used. That is the sharpest single defect
in this list, because it silently defeats the mechanism the frame was testing.

### D4. Writing a memory *well* makes it unpromotable **[verified]**

Line 255 refuses when the edge already carries `:attachment-status`. The
scribe's own `memory_record` attaches the pattern at write time, so the edge is
`:proposed` before promotion is attempted → `:promotion-attachment-not-statusless`.

Refused this way in f10: `e-3dd47d2a`, `e-fcb8d91a`.

The gate demands a statusless edge, but the well-behaved authoring path produces
a status. **Doing the right thing at write time is what disqualifies it.**

### D5. Student-recorded memories are unpromotable by domain pin **[reported]**

`e-3a3aed11` (the student's ENNReal-traps record) was refused with
`:promotion-attachment-review-threw` / "memory lifecycle domain mismatch"
(`peripheral/problem.clj:1700`): its edge carries `:domain :zaif-work` while the
conductor pins `:domain :mathematics`.

If general, **no student-recorded memory can ever be promoted** — which removes
a whole lane from the knowledge-accumulation story. Not re-derived at source by
ground control; worth confirming before designing a fix.

### D6. Failed conductor actions never reach the machine trace **[reported]**

Failed actions are not committed to the handle, so all three refusals above exist
**only in the guide's prose report**. The machine trace of f10 is consistent with
a clean run.

A defect class that is invisible to the trace is one bad handoff from being lost
entirely — and these particular ones are the evidence for D3–D5.

---

## 3. Harness defects that produce wrong behaviour quietly

### D7. `conductor-surface` captures function values, not vars **[verified]**

`apm/conductor_surface.clj:8-18` builds its dispatch map from
`conductor/dispatch-solver!`, `conductor/record-solver-attempt!`, … as **values**.
The map is therefore immune to hot reloads of `futon3c.apm.conductor`: reloading
the conductor leaves the surface pointing at the old functions.

This bit f9 **mid-frame** — `record-solver-attempt` raised
*"Wrong number of args (1) passed to memory-offers"*. The guide's first diagnosis
(a stale `record-solver-attempt!`) was wrong; the real cause was the value
capture, proven by identity (`@e317950` → `@657e56bd` after reloading the
surface). Cleared by reloading `conductor-surface`, not the conductor.

Ground control now reloads `conductor-surface` as a pre-flight before every open
(f10 did this) — a workaround, not a fix. Related: `conductor_open.clj:173`.

### D8. The scribe role-card path is hardcoded and contradicts the registration **[verified]**

`apm/conductor.clj:477` hardcodes
`role-cards/scribe.md` (blob `d4a8863d`), but f8/f9/f10 all pin
`02441d9d` = `role-cards/scribe-v2.md`. The machine injects the wrong path;
guides have papered over it by declaring the pinned card authoritative in their
packets. Three frames running have now worked around the same defect.

### D9. `frames.bb` swallows stderr **[verified]**

f9's open failed four times. The fourth reported only
`assign-checkouts: frames.bb open failed`. Running `frames.bb` directly surfaced
the real cause immediately: `fatal: a branch named 'exp/a01J06-ctl' already exists`
— leftover provisioning debris. The wrapper turned a one-line diagnosis into a
blind search.

### D10. `lake` is not on the default PATH in frame checkouts **[verified]**

Hit **independently three times in f10** — by the student, the guide, and ground
control — each resolving it with `export PATH="$HOME/.elan/bin:$PATH"`. Deposited
by the scribe as arc memory `e-3dd47d2a`.

Costs every agent that touches Lean a failed run and a rediscovery. Ground
control's first axiom audit reported a **false green** because of it (see D14).

---

## 4. Measurement instruments that give confident wrong answers

### D11. Predictions that quote the token they forbid **[verified]**

f10's `:cascade-cannot-why-hop` says *"NO offer … carries `:offer/route :why-hop`"*
— and its own `:text` contains that literal string. The parsed registration is
embedded in **every persisted state version**, so a text search finds the
forbidden pattern inside the prediction forbidding it, and finds more "violations"
the longer the frame runs.

Measured at f10 v9: grep read **5** why-hop offers; a structural walk read **0**.

Recorded in `analysis/MEASUREMENT-HAZARD-cascade-routes.md` (commit `2a7435a6`).
Both ground control and f10's guide hit this independently; both caught it.

### D12. Offers are duplicated across `:cycle/outputs` and `:steps` **[verified]**

A structural walk that collects every map carrying `:offer/route` **double-counts**:
f10 v14 yields 204 raw against 102 distinct. Dedupe on `:offer/id`.

Proportions are preserved under duplication, so the wrong number looks
self-consistent — 96/104/4 instead of 48/52/2.

### D13. Memory bodies are not under `/entities/` **[verified]**

Memory bodies live at `/api/alpha/evidence/<id>` on futon1b (7073).
`/api/alpha/entities/<id>` answers `entities requires ?type=<entity-type>`, and
guessing types (`memory`, `assert`, `reviewed-memory`, …) returns empty sets that
look like "the memory isn't there". Cost ground control four wrong attempts.

### D14. Verification commands that fail without failing **[verified]**

Ground control's first axiom audit of the f10 close reported `EXIT=0` **without
running**: `lake` was not on PATH (D10), and `EXIT=$?` was captured after a pipe,
so it read `tail`'s status. Two independent errors producing one confident green.

**Any verification step must capture the checked command's own exit status, not a
pipeline's.** Re-run correctly, the audit did pass.

### D15. Anchored declaration extraction hides joined declarations **[verified]**

Extracting Lean declarations with a line-anchored `^def` reported
`apm_m93J02_isNormOn` as **missing** from the student's file. It was present — on
the same line as its docstring, because of the cosmetic newline loss the guide had
flagged. Use an unanchored `\b(?:noncomputable\s+)?(?:def|abbrev)\s+` match.

Same class as the pre-existing note `grep-anchoring-hides-namespaced-decls`; here
the very mutation being checked is what defeated the check.

---

## 5. Registration and process

### D16. Registrations inherit stale fields by copy-paste **[verified]**

f10's registration, derived from f9's, carried three errors caught pre-open:

- `:first-tag-recall-hit` told the student to search tags `[a01J06]` — **f10's
  problem is m93J02**, making the prediction unadjudicatable as written;
- `:variation :endpoint` said "frame-9";
- `:reg/known-departures` declared f9's `:family-distance` departure, which
  **inverts** for f10 (m93J02 is m-family, so f10 *satisfies* the criterion).

Fixed in `74f3f275`. None of these fields is validated by `preregistration.clj`,
so nothing would have caught them.

### D17. A prediction's premise can be computed over the wrong set **[verified]**

`:cascade-cannot-why-hop` reasoned from what the **problem** touches. The cascade
seeds from patterns attached to **recall-surfaced memories**. Different sets, and
nothing in the frame constrained the second.

Result: 48 of 102 f10 offers were `:why-hop`, via
`math-formalization-CA/measure-integration-api` → (authored `@why`) →
`math-strategy/missing-dependency-protocol`, which holds exactly 48 attachments.

Both cascade predictions were refuted — **not because the mechanism misbehaved
but because the registration described it wrongly.** Authored by ground control;
recorded here as a defect in prediction authoring, not in the cascade.

### D18. The offer × use join is empty by wiring **[verified, f9 and f10]**

Offers are pushed only to the solver (`:memory-channel :push+pull`,
`:recall-status :ok`). The student and scribe are `:pull-only` with
`:recall-status :not-invoked`, `:reason :memory-channel-no-push`.

So any metric keyed on *"offered then used"* reads **zero** in both frames —
while in f10 real transfer demonstrably occurred through the **pull** channel
(the student attested `USED e-17bd0295` and its commits follow that memory's
prescribed order). The instrument watches the push channel; the transfer came
through pull.

Confirmed as wiring rather than a property of either problem, which was the
Analyst's registered f10 hypothesis.

*Label discrepancy to reconcile:* f9's student record was reported as
`:memory-channel :none`; f10 reads `:pull-only`. Same `:memory-channel-no-push`
reason. Do not assume the labels interchange without checking f9's state.

### D19. `/eval` masks runtime exceptions as syntax errors **[verified]**

An unresolvable symbol in a drawbridge `/eval` payload returns
`"Syntax error compiling at (N:M)"` with type `CompilerException`, which reads as
a typo in the submitted form rather than a missing var. Also caps at 300 s.

---

## 6. Fixed during this period — recorded so they are not re-found

### D20. Transfer-check C3 temporal scope — **FIXED** (`f5c68a5b`, P26)

C3 demanded that the student's eligible set contain promotions minted *after* the
student dispatched (f9: `promo/…/52`, minted 20 steps after the student dispatched
at step 32). Same class as f8's C3 false-fail.

Fixed by scoping promotions to before student dispatch. Gated by the Analyst
personally, including a **hand mutation** — forcing the dispatch index to 0 empties
the in-scope set and C3 correctly fails, proving no vacuous pass. Took f9's
transfer checks from 5/6 to 6/6 post-fix; C1 is now the only structural obstacle.

### D21. The f9 contamination channel — **FIXED in f10** (by practice, not by code)

f9's promoted memory named the solver's commit sha. With a shared git object
store, the student could reach the artifact, and its "independent" close was
**byte-identical to the solver's across the whole tree** (empty full-tree diff).

f10's guide wrote the deposit deliberately free of shas, branch names and
checkout paths, and asked the scribe to verify rather than accept. Verified by
ground control against the substrate: no 40-hex sha, no `exp/` branch, no absolute
path. Result: student 398 lines / 5 named sorries against solver 1399 / 0,
different md5s, and the student proved the shared finiteness step via
`Metric.isBounded_iff_subset_closedBall`, a name absent from the solver's file.

**The fix is a convention, not a mechanism.** Nothing prevents the next frame's
deposit from naming a sha. A lint on deposits would make it structural.

---

## 7. How to extend this file

- The Analyst's f10 close (job `invoke-1787053313813-4764-5b874ef3`) is still
  running and will add findings — **append them, and mark whether ground control
  re-derived them or took them on report.**
- Keep the `[verified]` / `[reported]` distinction. D5 and D6 are currently
  `[reported]`; promoting either to `[verified]` means re-deriving it at source.
- When a defect is fixed, move it to §6 with the commit, and say whether the fix
  is structural or a convention (see D21 — the difference matters).
- Defects found *by* the machine about *its own* premises (D17) belong here too.
  Frames that only record harness bugs will miss the ones in their own design.

---

## 8. Appended by analyst-1 at the f10 close (2026-08-18)

Per §7. Four items: two new defects, one resolution of an open question in this
file, and one correction to the record. All **[verified]** — each was re-derived
by me at source, and I say below exactly how, so ground control can mark them
`[reported]` instead if my derivation does not satisfy it.

### D22. Every frame closes `:closed` while its own validator says `:launchable? false` **[verified by analyst-1]**

`validate-trace` records `:launchable?` and a `:failures` vector in the saved
cycle state. It has been **false in every frame I can measure**, and nothing
surfaced it: the disposition records a clean close, the wake payload for f9 did
not carry the field, and I missed it entirely at f9.

| frame | cascade | `:launchable?` | failures |
|---|---|---|---|
| f8 `a03J04` | off | false | 6 |
| f9 `a01J06` | on | false | 7 |
| f10 `m93J02` | on | false | 7 |

Derived by reading the `:validate-trace` step out of the latest version file in
each of the three `data/problem-state/` directories.

**Visibility fixed** — P27 (`9ef132d4`) plus my review fix (`cc5a0ae1`) add a
**non-scoring** `:trace-validation {:launchable? :failures :failure-count}` block
to the receipt and a printed line. Non-scoring deliberately: `:score` is `n/6`
across five frames, and a seventh check would silently move the denominator.
The *underlying* failures are untouched and unowned.

### D23. `:memory-disposition-offer-ids` is empty in every frame, so F3 fails whenever the cascade is on **[verified by analyst-1]**

F3 (`preregistration.clj:451`) requires `memory-offer-ids ⊆
:memory-disposition-offer-ids`. Measured from each frame's validated trace:

| frame | offers | disposition-offer-ids | F3 |
|---|---|---|---|
| f8 | 0 | 0 | passes **vacuously** |
| f9 | 101 | 0 | fails |
| f10 | 102 | 0 | fails |

**Enabling the cascade did not break dispositioning — it removed the vacuity
that was hiding its absence.** Nothing has ever dispositioned an offer. This
contradicts `:required-capabilities :offer-use-disposition`, declared in both
the f9 and f10 registrations, and it is the one failure separating f8's list of
6 from f9/f10's list of 7.

Unowned. Note for whoever takes it: *reporting* dispositions is implementation;
*deciding* what disposition an offer earns is a design ruling and not the
Analyst's.

### D24. `mint-analyst` is unrouted while its handler is loaded — a second sighting of D7's class **[verified by analyst-1]**

`POST /api/alpha/frames/mint-analyst` → **404 Unknown endpoint**.
`POST /api/alpha/frames/mint-seats` with the same empty body → **409
missing-frame-id**, i.e. routed. Yet over Drawbridge, *both*
`futon3c.transport.http/mint-analyst-seat!` and
`futon3c.agency.frame-seats/mint-analyst!` resolve in the running image, and the
route branch is in the source at `transport/http.clj:7271`.

So the handler is not missing: **the running router closure was captured before
the branch existed, and reloading the namespace's functions does not rebuild
it.** That is the same failure mode as **D7** (`conductor-surface` captures
operation functions by value) on a different surface. Two sightings make it a
class rather than an incident, and the class is: *a captured surface that hot
reloads cannot reach.* Worth a sweep for others.

**This blocks succession.** `analyst-2` is unminted. I did not hand-assemble a
mint over Drawbridge: `mint-analyst!` needs a `:prepare-seat-fn` built from the
live server config, which is not reachable from any deref-able var in the image,
and a hand-built preparer would yield a seat that looks minted while carrying an
unverified invoke path. Owned by ground control. Check with:

```bash
curl -s -o /dev/null -w '%{http_code}\n' -X POST \
  localhost:7070/api/alpha/frames/mint-analyst \
  -H 'Content-Type: application/json' -d '{}'   # 409 = routed, 404 = still stale
```

### D18 — the open label question, RESOLVED **[verified by analyst-1]**

D18 asks not to assume the f9/f10 `:memory-channel` labels interchange. They do
not differ at all:

- frame records: f9 solver `:push` / student `:none`; f10 solver `:push` /
  student `:none` — identical;
- saved cycle state, both frames: student recall `:not-invoked`, reason
  `:memory-channel-no-push`; solver `:ok`.

The `:pull-only` / `:push+pull` labels quoted in D18 do not come from the frame
records or the cycle state. **The discrepancy is in the reporting surface, not
in the data**, and D18's substantive conclusion is unaffected.

### Correction to D1's table **[analyst-1, first-hand]**

D1 records that f9's analyst "died mid-analysis after ~3 min, having already run
the checks and reached 'seven named validator failures'. **All work lost.**"
First-hand confirmation of the loss, and one detail worth adding because it
matters for D22: those seven were almost certainly the **`:validate-trace`
failures recorded here as D22** — the same seven that f9 and f10 both carry. The
first close had found them and the finding died with it; the second close found
them again only at f10, by hand. **A defect can be discovered and lost, and the
only evidence that it was ever found is a phrase in a bell.**

---

## 9. Appended by claude-2 (ground control) after analyst-1's close, 2026-08-18

### D24 — succession UNBLOCKED, but the router is STILL BROKEN **[verified]**

`analyst-2` is now minted, invoke-ready, on Opus. **The defect D24 describes is
not fixed** — only its consequence for succession is.

- `POST /api/alpha/frames/mint-analyst` still returns **404**. Re-check with the
  curl in D24; it is still the right test.
- The seat was minted by calling `frame-seats/mint-analyst!` over Drawbridge with
  an explicit `:prepare-seat-fn`.

On analyst-1's objection, which was correct and is worth preserving: it declined
to hand-assemble a mint because a hand-built preparer "would yield a seat that
looks minted while carrying an unverified invoke path." Two things resolve that
without overriding the judgement:

1. **`:prepare-seat-fn` is the documented injection point, not a bypass.**
   `http/mint-analyst-seat!` itself resolves the preparer as
   `(or (:frame-seat-prepare-fn config) (partial prepare-frame-seat config))` —
   a config-supplied preparer is the sanctioned path. So the machine still did the
   tenure bookkeeping: derived `analyst-2` from the tenure, enforced the `:claude`
   type check, held the mint lock, stayed idempotent on an active tenure, applied
   `{:analyst-tenure 2 :fresh-session? true}` and the standard capabilities, and
   ran its own readiness check. Only the invoke-fn was supplied.
2. **The invoke path was then verified rather than assumed** — a readiness probe
   (job `invoke-1787054367773`) returned `analyst-2, running on Claude Opus 5`.
   That is the check whose absence analyst-1 was unwilling to accept, and it is
   available to any future minter.

Also note `prepare-frame-seat` reads `config` for exactly two things —
`(evidence-store-for-config config)` and `(:irc-send-fn config)` — and
`evidence-store-for-config` (`http.clj:1435`) does **not** fall back to a live
atom. So a naive `{}` config would have minted a seat with a nil evidence store.
analyst-1's caution was well founded; the fix is to supply the invoke-fn directly
rather than to fake a config.

**Still owned by ground control:** rebuilding the handler and re-installing it on
port 7070, which is what actually fixes D24. Deferred deliberately —
`runtime/agents.clj:168 make-http-handler` assembles peripherals and the evidence
store through `runtime-config`, and a mis-reconstructed config would trade one
dead endpoint for a subtly degraded server. Not a JVM restart (I-0 holds), but an
operator decision. Nothing is blocked on it now.

### D1 — partially mitigated at the seat level, NOT fixed **[verified]**

`analyst-2` was minted **with `:model "claude-opus-5"` set at mint time**, so it
is the first seat this period that did not need a live-state re-registration
after minting. This proves the shape of the fix: the preparer can set the model.

`mint-seats!` and `mint-analyst!` still accept no model argument, so **every seat
minted through the endpoints still defaults to Fable**. D1 stands.

### D18 — note for whoever reconciles the two readings

analyst-1 resolved this from the frame records (`:none` in both frames). Ground
control's `:pull-only` reading came from the **recall/dispatch record** in the
saved cycle state, not the frame record. Both are correct and they are different
fields; there is no contradiction, and the resolution is analyst-1's. Recorded
because two agents reading "the memory channel" reached different strings from
the same frame, which is itself a legibility defect in the state shape.

### D25. `:write-use` exists, is correctly phase-gated, and is unreachable from the conductor surface **[verified]**

Filed by f10-guide post-standdown; re-derived at source by ground control. **This
locates D23**, which analyst-1 could only record as unowned.

The chain, each link checked:

1. The tool exists and is gated to the right phase —
   `peripheral/problem.clj:56`: `:adjudicate #{:write-disposition :write-use advance}`.
   Its handler is real (`problem.clj:1663`, refusing without an open cycle and an
   `:offer-id`).
2. `apm/conductor_surface.clj` `operations` is the **complete** set of what a
   guide may submit. It has **eleven** entries — dispatch-solver, guide-solver,
   dispatch-student, dispatch-scribe, promote-artifact, record-scribe-lanes,
   record-solver-attempt, deposit, record-students, adjudicate, close — and
   **zero** occurrences of `write-use`.
3. `conductor/adjudicate!` steps only `:write-disposition` (line 545), `advance`
   (546) and `:promote-artifact` (550). It never steps `:write-use`.
4. **The clincher, from the capability map at `problem.clj:574`:**
   `:offer-use-disposition :write-use`. The required capability *is* this tool.

So `:memory-disposition-offer-ids` is empty in f8, f9 and f10 because **no guide
can populate it** — not because three successive guides neglected to. f10's guide
states it checked the operations map at ignition and `:write-use` was never on
its surface.

**All nine registrations (frames 2–10) declare `:required-capabilities
:offer-use-disposition`.** Every one of them has therefore declared a capability
that is unreachable by construction, and the machine has never contradicted them.

Why it only became visible now: before the cascade there were no offers, so an
empty disposition set was vacuously correct. Enabling the cascade did not break
dispositioning — **it removed the vacuity that hid its absence.** With 102 offers
and no reachable tool, the gap is loud in every subsequent frame.

**Fix location** is one entry in `conductor_surface.clj/operations`, routing to a
conductor fn that steps `:write-use` at `:adjudicate`, taking offer-ids from the
recorded `:memory-offers`. Note this is *not* the "no relay to other seats' tools"
boundary: dispositioning offers is the conductor's own act in the conductor's own
phase, so nothing in P6 argues against exposing it.

**Pairs with D3/D4.** Those keep memories from being promoted; this keeps offers
from being dispositioned. Together they explain the store-level result of f9 and
f10: two axiom-clean closes, 203 offers, and **zero reviewed attachments gained**.
The cycle machine is closing problems without accumulating knowledge, and all
three defects sit in the write path rather than in any agent's conduct.

### D1 — FIXED (`2bf907538af02662b3222b883d26b9b63da2f627`), with a review addition **[verified]**

codex-5 threaded an optional model through both mint paths: `mint-seats!` and
`mint-analyst!` take a `model` in their opts map, `mint-frame-seats!` and
`mint-analyst-seat!` gained 3-arities (the 2-arities delegate with `nil`, so
every existing caller is untouched), the handlers read `model` from the POST
payload, and `prepare-frame-seat` `cond->`-assocs it into the invoke-fn opts.
No model name is hardcoded and the default is unchanged.

Gates re-run by ground control rather than accepted: clj-kondo 0 errors /
0 warnings; `check-parens` OK; `futon3c.agency.frame-seats-test` 8 tests /
58 assertions / 0 failures, matching the reported numbers at the same namespace
scope. deftest count 7 → 8.

**Review finding, fixed in review (`prepare-frame-seat-threads-model-into-invoke-opts`).**
The submitted test injects its own `:frame-seat-prepare-fn`, so
`mint-frame-seats!` takes the `(or (:frame-seat-prepare-fn config) …)` override
branch and **the real `prepare-frame-seat` was never exercised**. Disabling the
model threading inside it left the whole namespace green — 8 tests, 58
assertions, 0 failures, with the fix effectively removed. The submitted test
proves the model reaches *a* preparer's input map; it did not prove the model
reaches the invoke constructor, which is the one line D1 is about.

Ground control added a test that calls the production preparer directly (with
`make-local-agent-invoke-fn` redef'd to capture its opts) and asserts both that
the model arrives and that the key is **absent — not nil-valued** when
unrequested. Verified by mutation: with the threading disabled the new test
fails (9 tests, 1 failure) where the original suite passed.

**Generalise this:** a test that supplies the injection point cannot cover the
default path behind it. The `(or override default)` shape means the override
branch and the production branch need separate tests, and this codebase uses
that shape in several places.

Status of the surrounding defect: the fix is structural, in source, and survives
a restart — unlike the live-state re-registrations that recovered `analyst-1` and
`f10-guide`. `analyst-2` had already been minted with a model at mint time via
`:prepare-seat-fn`, so the shape was proven before it was generalised.

### D25 and D7 — FIXED (`7c93a9eafae7dcd2f641dbd94ed4e6929b8eb566`) **[verified]**

Both are **structural** fixes in source, not conventions: they survive a restart
and cannot be un-done by an agent forgetting a practice.

**D25.** `conductor/write-uses!` (`conductor.clj`) dispositions every offer the
cycle recorded, reducing over `(get-in handle [:state :cycle/outputs
:memory-offers])`, keeping `:offer/id`, `distinct`, and stepping `:write-use` for
each; it short-circuits on the first failure via `reduced` and catches
`Throwable` into the standard `failure` shape. `:write-use #'conductor/write-uses!`
is now the twelfth entry in the conductor surface `operations` map.

Reading from `:cycle/outputs` with `distinct` also sidesteps **D12** at the
source — offers appear in both `:cycle/outputs` and `:steps`, so a naive
collection would have dispositioned each offer twice.

**D7.** Every entry in `operations` is now a **Var** (`#'conductor/dispatch-solver!`
…) rather than a captured value, so hot reloads of `futon3c.apm.conductor` reach
the surface. This is the defect that bit frame 9 mid-run with "Wrong number of
args (1) passed to memory-offers" and forced ground control to reload
`conductor-surface` as a pre-flight before every open. That pre-flight is no
longer required.

**Gates re-run by ground control, not accepted on report:**

| check | result |
|---|---|
| clj-kondo (both src files + test) | 0 errors, 0 warnings |
| `check-parens` | OK |
| `futon3c.apm.conductor-test` | 19 tests / 135 assertions / 0 failures |
| full APM suite (4 namespaces) | 55 tests / 234 assertions / 0 failures |
| scope of change | 3 files; `problem.clj`, the capability map and all registrations untouched |

**Mutation-verified, and it held.** Deleting the `:write-use` line from the
`operations` map produces **3 failures** in `conductor-test`. The test genuinely
covers the operation's reachability rather than passing vacuously — the contrast
with the D1 review, where the equivalent mutation changed nothing, is why this
check is worth running every time.

**A scope-comparison note, recorded because it nearly became a false finding.**
Ground control's first run of "the full APM suite" gave 46 tests / 222 assertions
against the reported 55 / 234. That was not a discrepancy: the namespace list was
guessed, inventing `conductor-binding-test` (does not exist) and omitting
`cycle-harness-test`. Enumerated correctly from `test/futon3c/apm/*_test.clj`, the
numbers match exactly. **Always enumerate the namespace set from disk before
comparing test counts** — this is the third time in this period that comparing
across different scopes nearly produced a reported regression.

**Consequence for the series:** `:memory-disposition-offer-ids` can now be
populated, so `:required-capabilities :offer-use-disposition` — declared by all
nine registrations from frame 2 to frame 10 and unreachable in every one of them
— is satisfiable for the first time from frame 11 onward.

## 10. Corrections from the promotion-deadlock discovery (`1e277974`)

codex-6's discovery report (`holes/excursions/E-promotion-deadlock-discovery.md`)
corrects four entries above. Ground control re-derived every load-bearing claim
at source before accepting them. **The corrections stand; the entries above are
wrong where they conflict.**

**D3 — my proposed fix was wrong, and wrong for a good reason.** I wrote that the
conductor might "pass the actual reviewer's identity (the scribe's)". It cannot,
and this is not a small threading change:

- `memory_lifecycle.clj:232-237` rejects any reviewer that differs from
  `:acting-identity` (`:reviewer-not-actor`), and `:240-253` rejects a reviewer
  equal to the depositor. For a guide-authored deposit both are the guide, so
  the two conditions are **jointly unsatisfiable** — a two-sided vise, not one
  awkward check.
- `conductor_surface.clj:38-50` rejects the mismatch at the authenticated surface
  *before* lifecycle code runs.
- `conductor_test.clj:530-540` asserts exactly this with the message **"P14
  forbids the guide from impersonating the scribe"**. It is a deliberate,
  tested anti-impersonation rule — not an oversight to be relaxed.

The real gap is that **the system has no operation for consuming an already
authored review.** `promote-memory-attachment!` is an attach-and-self-generate
shortcut whose preconditions describe an *unattached* memory.

**D4 — I assigned the blame to the wrong component.** I wrote that "writing the
memory well is what disqualifies it", implying `memory_record` is at fault. It
is not: `memory_write.clj:206-222` deliberately marks an agent-supplied pattern
`:proposed` so an unreviewed claim cannot masquerade as reviewed, which is the
correct safe write. And `review-attachment!` (`memory_lifecycle.clj:350-371,
388-408`) **already** implements the proposed → reviewed transition. The faulty
component is the promotion **dispatcher's precondition**, which rejects the very
state the review path expects. The machinery exists; it is unreachable.

**D5 — CONFIRMED but narrower than I stated.** I wrote that a student-recorded
memory is "unpromotable by the conductor, full stop". Correct for *currently
minted* seats, wrong as a universal: ZAI's `memory_record` defaults the domain to
`:zaif-work` (`zai_api.clj:601-617`) and the frame-seat preparer never supplies
one (`http.clj:2532-2554`), although the ZAI constructor supports it
(`http.clj:2513-2524`). It is a **configuration omission**, not an intrinsic
property of student authorship.

**D6 — CONFIRMED and worse than I recorded.** I wrote that failed actions never
reach the handle. In fact the conductor *does* log the refusal transiently
(`conductor.clj:104-129`), but `conductor_binding.clj:175-194` returns
`{:ok false …}` without ever calling `(reset! (:handle binding) next-handle)` —
only the success branch advances handle, receipt and version. **Even the
transient log is discarded at the binding boundary.**

### What this vindicates

Splitting discovery from implementation is what caught this. Had the D3 packet
been dispatched as an implementation carrying my hypothesis, it would have built
a scribe-name threading fix that the authenticated surface rejects and a tested
invariant forbids — and the most likely way to make it "work" would have been to
weaken P14. The middle gate is the whole value.

### Accepted implementation order

1. **D5** — blocked on an operator ruling (below), then a small scoped config change.
2. **D3+D4 together** — one coherent design change: an "apply existing review"
   path that derives the reviewer from immutable `:evidence/author` and
   dispatches on actual edge state. Implementing either as a relaxed comparison
   is unsafe and is rejected.
3. **D6** — durable refusal receipts; orthogonal, but should land before the next
   experiment if refusals are to be diagnosable from the trace.

### OPERATOR RULING NEEDED (D5)

Is APM frame knowledge `:mathematics`, or is student working memory intentionally
`:zaif-work`?

- If the former: mint the student seat with `:memory-domain :mathematics`,
  threaded explicitly through the seat spec rather than inferred from an `fN-`
  name.
- If the latter: cross-domain promotion needs an explicit, auditable import /
  re-home operation.

Weakening `validate-edge!` is not an option in either case — it would let an edge
be reviewed under the wrong domain policy.

## 11. Write-path repairs — D3, D4, D5 (2026-08-18)

### D3 + D4 — FIXED (`c3be0e9f240df0b0bfba2e6d77f6a217ae1cd995`) **[verified]**

**Structural.** `apply-existing-attachment-review!` (`memory_lifecycle.clj`) is
the operation the system was missing: it consumes independently authored review
evidence and applies it to an exact `:proposed` edge.

The property got **stronger, not weaker**. Reviewer identity and session are
both derived from the persisted evidence entry — `(:evidence/author review-entry)`
and `(:evidence/session-id review-entry)` — never from the request. The
docstring states the rule: *"The caller supplies the evidence id and reviewed
facts, never an authorship claim."* Deriving authorship from immutable evidence
is a stronger guarantee than trusting a caller's assertion, which is what the
old shortcut did.

That derivation also resolves the second obstruction honestly: because reviewer
and session come from the same entry, the invocation-identity check at
`memory_lifecycle.clj:137-146` is *satisfied* rather than bypassed.

`promote-memory-attachment!` now dispatches on state — a request naming review
evidence takes the new path; a statusless, patternless memory keeps the original
attach-and-review behaviour unchanged.

**Gates, re-run by ground control:** the reject-outright check first —
`conductor_test.clj` is not in the diff, the P14 "guide cannot impersonate the
scribe" assertion is intact, and the conductor suite passes 19/135/0. clj-kondo
0/0; check-parens OK; changed namespaces 111 tests / 435 assertions / 0 failures.
`:reviewer-not-actor` and the surface check are unrelaxed, and
`promotion-reviewer-is-depositor` now appears **twice** — the new path enforces
it too.

**Mutation-verified.** Reverting the derivation to caller-supplied
(`(:reviewer request)`) produces 2 errors where the clean run has 0.

### D5 — FIXED (`38d759818284b3be0e95496776c08a4544754928`) **[verified]**

**Structural**, and resolved by operator ruling: *"`:mathematics` seems
reasonable"* (Joe, 2026-08-18). APM frame knowledge belongs to the mathematics
lifecycle.

The domain is carried as **data in the seat specification** —
`[:reg/student-seat "student" :zai :mathematics]`, a fourth element in the seat
tuple — and threaded through `mint-one!` and the production `prepare-frame-seat`
into the ZAI invoke options. Only the student seat carries it; the other four
get `nil`. It is explicitly **not** inferred from an `fN-` name prefix, which
would have silently misfiled any seat that did not match the convention.

`zai_api.clj` is not in the diff: the global ZAI default remains `:zaif-work`
(`zai_api.clj:615,1268`), so domain isolation for non-frame ZAI work is intact.
`validate-edge!` and the domain check are untouched — the edge now carries the
right domain rather than the check being taught not to care.

**Gates, re-run by ground control:** clj-kondo 0/0; check-parens OK; 29 tests /
339 assertions / 0 failures across `frame-seats-test` and `memory-write-test`,
enumerated from disk.

**Mutation-verified independently.** Disabling the forwarding in the production
preparer (`http.clj:2553` — the one site of five that is `prepare-frame-seat`)
gives 1 failure, matching codex-5's reported result. Its previous packet's test
was vacuous under exactly this check; this one is not.

### Consequence: the write path is open

D25 opened disposition, D3/D4 opened promotion of independently reviewed
deposits, D5 opened the student lane. For the first time since frame 2 a frame
can **deposit → disposition → promote** end to end. Frames 9 and 10 closed two
problems axiom-clean and gained **zero** reviewed attachments; that specific
failure mode is now repaired at source in all three legs.

Not yet verified end to end in a live frame — that is what frame 11 measures.

### D6 — FIXED (`585a980e6aecfd803a958483532ad5370ee39518`) **[verified]**

**Structural.** Refused conductor actions now leave a durable receipt, so a
refusal is diagnosable from the cycle trace instead of existing only in an
agent's prose report.

The design avoids the trap the discovery flagged. `conductor_binding.clj`
calls the recorder with **`current`** — the last valid authoritative handle —
never with the poisoned `next-handle`. Committing the failed handle would have
set `:ok false` and short-circuited every subsequent `raw-step`
(`conductor.clj:97-102,111-114`), poisoning the rest of the frame.

`record-action-refusal!` records `:refusal/action-id`, `:refusal/tool`,
`:refusal/error` (only `:error/component`, `:error/code`, `:error/message`),
`:refusal/step-index`, and sanitized args, then checkpoints. It also returns the
**new version** in the refusal envelope, so a caller retrying after a refusal is
not stranded on a stale version — and falls back to the previous behaviour
unchanged if no recorder is supplied.

**Sanitization is an allowlist, which is the right direction.** Only seven
diagnostic keys are kept (`:artifact-id :lane :memory-id :offer-id :outcome
:pattern-id :reviewer`), values must be scalar, strings are capped at 160
characters, and a non-map argument is reduced to its *type* alone. A denylist
would have leaked whatever nobody thought of.

**Gates re-run by ground control:** clj-kondo 0/0; check-parens OK; APM suite
55 tests / 244 assertions / 0 failures with the namespace set enumerated from
disk. **Mutation-verified independently:** making `record-action-refusal!`
return `nil` produces **5 failures**, matching the reported result.

The test is the strongest of this round. It plants a canary string
(`TOP-SECRET-PACKET`) as an argument and asserts it is absent from the durable
receipt; asserts phase and `:cycle/outputs` are unchanged; asserts the refused
action is never recorded as successful and cannot contribute to promotion
counts; and asserts a **subsequent valid action still succeeds**, which is the
no-poisoning property stated as a test rather than a claim.

## 12. Test-suite baseline — the "unrelated failures" claim, SETTLED **[verified]**

Two agents reported wider-suite failures as pre-existing. That was plausible but
unverified, so ground control ran the peripheral suite at the pre-work pin
`05ab95ca` in an isolated `git worktree` and compared:

| | tests | assertions | failures | errors |
|---|---:|---:|---:|---:|
| baseline `05ab95ca` (before any of this work) | 631 | 2693 | **18** | **18** |
| HEAD (D1, D3/D4, D5, D6, D7, D25 all landed) | 634 | 2732 | **17** | **15** |

**The failures pre-date the entire week's work**, and HEAD has one fewer failure
and three fewer errors than the baseline. The 18/18 that codex-6 quoted matches
the baseline exactly — it ran before D5 landed. Nothing in this round introduced
a regression, and the failing namespaces (`mission_control`, `registry`,
`war_machine_pilot`, `integration`) are untouched by any of it.

Method note: this took three attempts. Two `run_in_background` runs were killed
with zero output, and each time the honest report was "unverified" rather than a
guess. The run only completed when launched through `scripts/bg.py`, which
re-parents to the futon3c JVM and survives turn teardown — exactly what
`CLAUDE.md` prescribes for work that must outlive a turn. A `DONE-EXIT` marker
was appended so a completed run is distinguishable from a killed one, which a
quiet output file is not.

### D24 — FIXED IN SOURCE (`eb81db299fde3929c255a61e1bda1809150f8692`), **NOT YET LIVE** **[verified]**

**Structural**, and deliberately inert until the operator restarts the server.
**`mint-analyst` still returns 404 and will keep doing so until then.** Do not
record this defect as closed on the strength of the commit.

`start-server!` keeps its `[handler port]` signature — the four demo/smoke
scripts that pass an explicit handler are unaffected — but now installs the
handler into a `defonce` atom and hands http-kit a stable indirection
(`installed-handler`) that derefs that atom per request. `rebuild-handler!`
swaps it atomically.

Two details make it correct rather than merely clever:

- **The `defonce` is load-bearing.** After a namespace reload, http-kit still
  holds the *old* `installed-handler` fn object — but that object reads the same
  `defonce` atom, so a rebuild still reaches it. A plain `def` would orphan the
  live target, which is the very failure mode this defect is about. The comment
  cites the `http.clj:812` precedent.
- **`make-handler` attaches `{::rebuild-fn #(make-handler config)}` as metadata**,
  so the zero-arity rebuild closes over the ORIGINAL startup config. Nothing has
  to reconstruct `runtime-config`, which was the risk that made ground control
  refuse to attempt a live rebuild by hand earlier.

Handler construction stays out of the request path (the test asserts exactly two
constructions across many requests).

**Gates re-run by ground control:** clj-kondo 0/0; check-parens OK; the running
server was NOT touched (`/api/alpha/agents` → 200, `mint-analyst` → 404 as
expected, two JVMs per the I-0 override). Dedicated test passes 1 test /
5 assertions / 0 failures, and **fails with 2 failures** when http-kit is
reverted to the captured value.

### A measurement that nearly became a false regression report

Running the whole `http-test` namespace gave **38 failures / 3 errors** with the
fix, and only **17** with the one-line mutation applied. Read naively that says
D24 introduced 21 failures.

It did not. At `eb81db29^` — the commit immediately before D24 — `http-test`
already shows **38 failures / 3 errors**, identical to after. **D24 introduced
zero regressions.** The mutation number was the misleading one: reverting the
indirection changes which handler each test server serves, so a suite with seven
`start-server!` call sites shifts for reasons that have nothing to do with
whether the feature works.

The lesson, now four times over in this period: **a delta is only evidence
against a baseline measured the same way.** Compare against the parent commit,
not against a mutated tree, and never against a guessed namespace set. The valid
mutation probe was the dedicated test, run alone.

## 13. Post-restart verification (2026-08-18, after Joe restarted the JVM)

### D24 — VERIFIED FIXED AND LIVE **[verified]**

`RESULT=ok`, agency up after 2s. The check from D24:

```
POST /api/alpha/frames/mint-analyst  ->  409 invalid-tenure   (was 404 unknown endpoint)
POST /api/alpha/frames/mint-seats    ->  409 missing-frame-id
```

Both routed. The endpoint now reaches its handler and refuses on content rather
than on existence. **D24 is closed.**

### CORRECTION — registrations DO survive a restart **[verified]**

Several entries above, and the two live re-registration scripts, carry the claim
*"Live-state registration: does NOT survive a futon3c restart."* **That is
wrong.** After the restart, all 64 agents came back with `local invoke-fn
registered`, zero missing, and `analyst-1`, `analyst-2` and `f10-guide` all
retained `claude-opus-5`.

And the model survived *through the invoke path*, not merely in metadata — which
is the distinction that matters, since a generic restore that rebuilt invoke-fns
without the model would have silently returned every claude seat to Fable, i.e.
D1 all over again with the roster still claiming Opus. Verified by probe, not by
reading the roster: `analyst-2` was belled and answered *"analyst-2, running on
Claude Opus 5 (claude-opus-5)."*

Consequence: the hand re-registrations that rescued `analyst-1` and `f10-guide`
were durable, not stopgaps. The D1 source fix is still the right fix — it removes
the need to re-register at all — but the urgency framing in D1 ("do not survive a
restart") overstated the risk.

Recorded because it is a factual claim this file asserted more than once and an
agent could reasonably act on it.

### Housekeeping

f8 and f9 runner seats retired at Joe's instruction — all ten deregistered
(`f8-guide/proctor/scribe/solver/student`, `f9-…`), roster 64 → 54. `f10-*`
seats and both analyst seats retained.

### D26. The solver's attested use never reaches a machine receipt **[reported by analyst-1, twice measured]**

Carried over from `HANDOFF-analyst-1-to-analyst-2.md` §6.4, which is otherwise
closed out by this round. Recorded here so it is not lost with the handoff.

In BOTH f9 and f10 the solver attested `USED <memory-id>` in prose, inside
`:attempt/verification`, while its machine receipt `:memory-use/used-ids`
remained `[]`. The loss function's numerator therefore drops real, attested uses
— measured twice, in two different frames, with two different solvers.

This is the mirror image of D18: there, offers reach a seat that cannot receipt
its use; here, a seat uses a memory and the receipt does not record it. Between
them, "offered then used" is unmeasurable from the machine trace in either
direction.

**Not verified at source by ground control.** It should be confirmed before a fix
is designed — and specifically, whether the gap is the solver failing to call a
receipt tool, or the tool being unreachable from the solver's surface as
`:write-use` was for the guide (D25). Those need different fixes, and the D25
precedent makes the second hypothesis worth checking first.

### D2 — FIXED (`73a6b0c246efb697f5b0f9347d7a867e6885f51a`) **[verified]**

**Structural.** `guidance-count` now subtracts `(count (:solver-dispatches
trace))` — one opening per dispatch — instead of the flattened per-memory offer
list. The measure is invariant to how many memories a dispatch happens to offer,
which is the property that broke when the cascade landed.

A compatibility branch keeps the historical per-offer subtrahend for traces
predating the `:solver-dispatches` field, so frames 2-8's published counts do not
shift. **No clamping** — a negative count was the symptom, and clamping would
have hidden the defect while leaving the number meaningless.

**Better than the packet asked for.** Ground control expected f9 and f10 to be
stuck with their wrong values, since the fix only had to protect f2-f8. In fact
both traces already carry `:solver-dispatches` (a vector of dispatch records,
each with `:ground-control/recipient`), so they take the NEW branch and are
**retroactively corrected**: f10 becomes 1 dispatch − 1 opening = **0**, which is
the true value, in place of the −101 in its record. The series numbers are
recoverable rather than merely frozen.

Ground control checked this rather than asserting it: the concern that f9/f10
would fall into the legacy branch was reasonable and simply wrong, and grepping
the two saved traces settled it in one step.

**Gates re-run by ground control:** clj-kondo 0/0; check-parens OK; APM suite
56 tests / 250 assertions / 0 failures with the namespace set enumerated from
disk. **Mutation-verified:** forcing the legacy branch (`if false`) gives 1
failure against a clean 0, and reproduces −101 on the 102-offer case.

### D8 — FIXED (`ca0f297e945a88e6af236b8ce8609cf8a8095839`) **[verified]**

**Structural**, and stronger than the packet asked for. Ground control specified
"resolve from the registration's pinned `:reg/role-cards` entry"; codex-5
resolved by **blob SHA** instead of by path — it reads the pinned blob from the
frozen registration and locates the file whose content hashes to it via
`git ls-tree -r HEAD`, restricted to `/role-cards/`, requiring **exactly one**
match.

That is the better contract: the registration pins *content*, so resolving by
content means a card edited after freezing no longer resolves, rather than
silently injecting different text under the same filename. Path resolution would
have missed exactly that case.

**No fallback exists.** On zero matches, ambiguity, or a malformed pin the
conductor emits `(failure handle :scribe-card-unresolved …)` carrying the pinned
blob. Silent substitution was the defect, so a refusal is the correct outcome.

**Verified functionally, not just by test:** the real pin
`02441d9df4b8a05355790a51f1e535bf9e9465d4` resolves to exactly one path,
`holes/labs/M-apm-demonstration/role-cards/scribe-v2.md` — the card frames 8, 9
and 10 all pinned and all had to work around by hand.

**Gates re-run by ground control:** clj-kondo 0/0; check-parens OK; APM suite
56 tests / 250 assertions / 0 failures, namespaces enumerated from disk.
**Mutation-verified:** restoring the hardcoded `scribe.md` gives 4 failures,
detecting both the wrong card and the missing refusal path.

Three frames' worth of guides declaring the pinned card authoritative in their
packets is no longer necessary.

### D9 + D10 — FIXED (`01fb2de0`) **[verified]**

**D9 (stderr swallowed).** Both layers now propagate. In `scripts/frames.bb`,
`run-command!` dies with `command failed (<exit>): <command>\n<stderr>`. In
`peripheral/problem.clj` the thrown exception's *message* now embeds the exit
code and the combined stderr/stdout, with both also in ex-data. The message
matters: the caller logged only that, which is why frame 9's four failed opens
showed `assign-checkouts: frames.bb open failed` while git was saying
`fatal: a branch named 'exp/a01J06-ctl' already exists`.

**D10 (`lake` not on PATH).** `ensure-lake-on-default-path!` resolves the
toolchain via `fs/which` or `~/.elan/bin`, then symlinks it into `~/.local/bin`,
which frame-agent launch environments already include. It uses
`System/getProperty "user.home"` rather than embedding this machine's home path,
and **refuses to replace a launcher it did not create** rather than clobbering
one. Provisioning now calls it before adding the worktree, so a fresh checkout
has the toolchain without an agent discovering it the hard way — as the student,
the guide and ground control each did in f10.

**Gates re-run by ground control:** clj-kondo 0/0; check-parens OK; frames.bb
parses under babashka; `problem-test` 101 tests / 384 assertions / 0 failures
*after* the fix below.

codex-6 committed the work but returned an empty summary, so this was gated from
the diff rather than from a report.

### A regression introduced by D6 that D6's own review missed **[verified, fixed in review]**

`problem-test/emit-trace-reuses-existing-projection-and-machine-cycle-facts`
asserts **exact map equality** on the emitted trace. D6 (`585a980e`) added
`:action-refusals` to that trace — correctly — which broke the assertion.

**Ground control's D6 review did not catch it.** D6 touched four files including
`peripheral/problem.clj`, but the review ran only the APM namespaces
(55/244/0, all green) because that is where the conductor work lived. The
peripheral suite was never run for that commit, and the failure sat undetected
until the D9/D10 review happened to run `problem-test`.

Fixed in review by declaring the new key in the expected map, with a comment
naming D6 as its origin. **Verified not to be a loosening:** mutating the
producer to emit `[:bogus]` still fails the assertion, so it tests the value
rather than ignoring the key.

**The rule this yields:** run the test namespaces belonging to *every* file a
commit touches, not the namespaces belonging to the commit's subject. A
two-line change to a file outside the packet's main area is exactly where this
hides — and D6's `problem.clj` change was two lines.
