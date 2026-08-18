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
