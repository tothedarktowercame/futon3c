# Excursion: E-wm-operator-lane — operator-communication lanes for the overnight War Machine

**Date:** 2026-06-05
**Status:** DATA BACKBONE LIVE (2026-06-05). Full chain shipped + verified over HTTP: invariant
logic-model (`2c0c6c4`, 6/6) → classifier (`f4d5e13`, 330 assertions) → bulletin (`2f34f68`) →
adapter (`d6bba12`+`dc5c2bf`, :path 108/108) → endpoints (`6df1754`, reload-safe `extra-routes`).
Live: `GET /api/alpha/war-machine/operator-bulletin` (nag 0 / brief 60 / silent 51 / total 111) +
`GET /api/alpha/forward-model` (mint.edn). Operator portal (claude-1) renders these.
Go-live needed a **clean full JVM restart** (Joe) — in-place `load-file` of the live serving ns
deadlocked under traffic; see [[feedback_serving_ns_reload_deadlock]]. After this one restart,
future routes are plain reloads of `extra-routes` (no re-mount).
Remaining: mode-aware `can-execute?` (futon2, held); acknowledged-state persistence (so nag can fire);
polish — BRIEF list is long (60), pushes forward-model card below fold → cap/collapse brief (my side) or
2-col layout (claude-1's), deferred (claude-1 note 2026-06-05).
**Owner:** claude-7 (end-to-end, per the E-prefix excursion convention)
**Parent / context:** the `M-war-machine*` family (futon3c/holes/missions) — specifically the
first-outing finding that the WM is *starved of input sources* and the open question of how it
behaves on an unattended overnight run.
**Couples to (READ-ONLY):**
- `futon7/holes/M-futon-forward-model.md` (claude-1) — the **futon-focused** model (mission-backlog salience).
- `futon7/holes/M-interim-director-forward-model.edn` — the **Joe-focused** model (what Joe is tracking).

This excursion *consumes* those two models; it does **not** rebuild them, and it must not drag either
toward a predictive claim (M-futon-forward-model is descriptive-first — "the wall").

## 1. IDENTIFY — the problem

The War Machine is meant to run **overnight, unattended**. But its ranking surfaces *value*, not
*autonomous-dischargeability*. Some top-ranked actions (e.g. missions in IDENTIFY/proposal state)
are **blocked on Joe's framing or insight** — they are not actions a pilot can discharge alone.

Run as-is, the WM would either stall on these or pester Joe at every step. Joe's constraint
(2026-06-05): *the WM must be able to automate the "ask Joe" step and keep going, but it must not
become a nag.* "Nag" is a narrow, earned mode — not the default channel for anything that needs him.

The category error to fix: treating *a request for Joe's input* as *an executable action*. It is not
one — it is a **signal/observation** ("this high-value item is blocked on Joe"), and signals belong in
a briefing Joe pulls on his own clock, not in the action the WM fires at 3am.

## 2. The lanes

| Lane | Trigger | Behaviour |
|---|---|---|
| **0 · Silent** | autonomously dischargeable | WM discharges it overnight; no message. |
| **1 · Brief (FYI)** | high-value, or needs Joe's input, or a finding — but *novel* or *not yet critical* | lands in a morning bulletin Joe **pulls**; never interrupts. "WM asked itself for Joe's advice" parks here. |
| **2 · Nag (interrupt)** | the gate in §3 only | actively pushed; rare by construction. |

## 3. The nag gate

**nag = (item is in Joe's forward model) ∧ (item is futon-important) ∧ (item is in a declared risk mode).**

- *"Joe already knows about it"* ← the Joe-focused model (interim-director).
- *"important for futon"* ← the futon-focused model (M-futon-forward-model).
- *"if this doesn't happen, that's problematic"* ← a **risk-mode** flag (current-state, **not** a prediction).

**Why this won't nag at every step (the assurance):** the gate is an **AND of three independently
restrictive filters**, not an OR. A fresh discovery fails "Joe already knows it"; a known-but-calm item
fails "risk mode." Only the narrow overlap gets through — *something Joe already tracks, that matters to
futon, that has gone critical.* The rarity is structural, not a promise.

**Corollary — novelty flows *down*, never up.** Lifecycle:
`discovered → briefed (now in Joe's model) → [if it goes critical] → nag`.
Briefing an item is what later *qualifies* it to escalate. Nothing leaps straight to a nag.

Within **Brief**, a **warm/cold** split: *warm* items (already in Joe's model) are the escalation
candidates; *cold* items (novel) must first land and be acknowledged before they are even eligible.

## 4. Mechanism — where this touches code

The admissibility hook **already exists** and is **already applied** — this excursion supplies the
missing predicate, it does not invent the machinery:

- `futon2/src/futon2/aif/forward_model.clj:128` — `can-execute?` multimethod (per-action-instance
  admissibility, default `true`). `:open-mission` has **no override** today, so framing-blocked
  missions pass through as executable. (`:address-sorry` already demonstrates the pattern: executable
  only if the sorry is actually open.)
- `futon2/scripts/futon2/report/war_machine.clj:3337` — the WM **already** pre-filters ranked actions
  through `can-execute?` (`wm-admissible`).

Planned additions (subject to claude-1's read-contract, §8):
1. A **mode-aware admissibility** notion: in autonomous/overnight mode, an `:open-mission` whose
   discharge requires operator framing is **inadmissible for autonomous execution** (so it is never
   *fired*), but is **routed** to the lane classifier.
2. A **lane classifier** computing lane ∈ {silent, brief, nag} from the gate in §3.
3. A **morning bulletin** projection — the brief/nag lanes rendered as a pull surface (an "Awaiting
   Joe" section), per [[registry_as_projection]] (the bulletin is a 1-d projection of the blocked subset).
4. **Detection** of "needs Joe's framing": cheapest signal is the mission **status-class** already
   read by the mission proposer (SPECIFIED ⇒ autonomously actionable; IDENTIFY/proposal ⇒ framing-blocked).
   Alternative: an explicit `**Autonomy:**` line per mission doc. (Joe's call — §8.)

## 5. Test set (Joe's, 2026-06-05) — the three missions the WM surfaced

Run honestly through the gate:

| Mission | In Joe's model? | Futon-important? | Risk mode? | → Lane |
|---|---|---|---|---|
| `or-training-as-learning-system.v1` | warm (UKRN-S / PLOS) | yes | no (IDENTIFY, May 5) | **Brief** |
| `bayesian-structure-learning` | warm (Joe's framing) | yes | no | **Brief** |
| `canon-fingerprint-store` | cold (newer) | yes | no | **Brief** |

**Result: all three → Brief; the nag lane stays empty.** This validates the design — these should have
been a quiet morning FYI. (It also catches a real slip: in the WM's first outing this session, claude-7
elevated these into a *question* to Joe — a mild Lane-2 act — when they were Lane-1 all along.)

## 6. Scope

**In:** the three lanes; the nag gate; the mode-aware `can-execute?` admissibility for `:open-mission`;
the lane classifier; the morning-bulletin projection; framing-blocked **detection** via mission status.

**Out:** rebuilding either forward model; any predictive claim; the full essay/annotation lifecycle
event vocabulary (that's M-essays-diachronic-model); the Tickle sharp-intervention modes beyond the
single "nag" lane defined here.

## 7. Exit criteria

- The lanes + nag gate are written down as a contract (this doc) and ratified by Joe.
- A mode-aware `can-execute?` predicate makes framing-blocked `:open-mission` actions inadmissible for
  autonomous execution, with a test (conforming + adversarial) over an abstract action trace
  ([[logic_model_before_code]] before code).
- The lane classifier + bulletin produce the §5 test-set result (3 × Brief, nag empty) from live data.
- The read-contract against claude-1's two models is fixed and honoured (§8).

## 8. Read-contract — RESOLVED by claude-1 (bell 2026-06-05); now DOCUMENTED + STABLE

**Canonical authority:** `futon7/holes/M-futon-forward-model.md` **§11 "v1 read-contract"** (T5 ✅;
the mission's v1 finite-tickbox is complete — header still reads IDENTIFY/MAP but the read-contract
surface is declared v1-stable). Build §8 against §11; the files below are that contract's artifacts.

**v0 = read the EDN artifacts directly. There is NO query API yet; do not block on one.**
(If claude-1 adds a query surface in coupling slice-2+, they hand over the fn signature.)

**(1) "futon-important" — structural salience, keyed by mission id:**
- `futon7/holes/M-futon-forward-model.centrality.json` — per mission id: `{:c_joint :c_mission
  :c_eig :status :home_repo}`. **`c_joint`** is the importance score (recency-IMMUNE; mean cosine in
  the joint mission+pattern manifold). claude-1 commits to keeping this shape stable.
- `futon7/holes/M-futon-forward-model.semilattice.edn` — `:basins-regions` (per-repo `:region` +
  `:basins`) and the witnessed-backlog (the 108-live set) for `in-witnessed-backlog?`.

**(2) "Joe already knows of it" — what's tracked / actionable now:**
- `futon7/holes/M-futon-forward-model.mint.edn` ← **best single read.** Live business-salience surface
  (regen: `bb …mint.bb`). v1-stable keys (§11): `:cash-now`, `:next-action`, `:priced-sorries` (each with
  `:p`, `:expected-lift`, `:blocked?`, `:discharge`), `:unpriced-sorries` (the UNPRICED cold-conversion
  crux), `:traction-metric` (`:gradient` — realized ÷ total-known share over the trajectory),
  `:projected-trajectory`, `:ranked-action-list` (`:actionable-now` / `:gated`).
- `futon7/holes/M-interim-director-forward-model.edn` (income-worlds / pipeline) +
  `M-interim-director-eoi-campaign.edn` (EOI queue) + `~/code/ledger/ledger.edn` (the priced source).

**DESCRIPTIVE-FIRST GUARD (claude-1, load-bearing):** both signals are **descriptive, not predictive**.
`c_joint` = *structural centrality* (where backlog mass sits); the mint `:p` values = *explicit operator
priors*. The nag gate must read "futon-important" as **"structurally-central in the witnessed backlog,"
NOT "predicted-to-matter."** Do not launder descriptive→predictive — that is the E-half-mil wall
(claude-3 + claude-6 signed 2026-06-02; the predictive arm `E-futon-arrival` is deferred, Joe's-go-only).

### 8a. Worked nag candidate — `mint.edn` maps onto the lanes

claude-1's triage feed (priced/unpriced × blocked/actionable) over the live `mint.edn`:

| mint item | descriptive facts | → Lane |
|---|---|---|
| `:S-invoice-4` | priced `:p 0.9`, `:expected-lift 641.25`, not blocked, **`:next-action`** | **Brief** (routine, actionable-now) |
| `:S-scenario-c` | priced `:p 0.7`, `:expected-lift 3150`, **`:blocked? true`** (awaits invoice-4) | **Brief** (gated) |
| `:S-cold-conversion` | **UNPRICED** `:p :unsampled`, `:n=0`, note "the crux" | **Nag candidate** — PENDING risk-mode definition (§8b) |

This is the first item to plausibly trip the nag lane (my original 3-mission test set was all-Brief, nag
empty). It is in Joe's model ✓ and structurally central ✓ (the declared crux). Whether it **fires**
depends on the one undecided predicate:

### 8b. RESOLVED — v0 defaults (claude-7's leans, locked 2026-06-05; Joe-vetoable)

1. **Risk-mode predicate = (A)∧(D).** An item is risk-mode iff it is a structurally-central crux that is
   unsampled/blocked-and-stalled (`:p :unsampled`/`n=0`, or `:blocked?` past a stall threshold) **AND**
   it has already been acknowledged (previously briefed). First sighting → **Brief**, never Nag
   (novelty-flows-down, INV-4). This is the conservative reading of Joe's "if this doesn't happen that's
   problematic" + "don't nag at every step." Justification stays **descriptive** ("the crux is
   unsampled" = current-state structural gap, NOT "I predict cold-conversion fails").
   → `:S-cold-conversion` is **Brief** on first surfacing; nags only if it remains the unsampled crux
   after Joe has seen it.
2. **Detection = implicit mission status-class** (SPECIFIED ⇒ autonomously actionable; IDENTIFY/proposal
   /draft ⇒ framing-blocked), with an explicit `**Autonomy:**` line as an override when status is ambiguous.

*Joe retains veto on both; these are v0 build defaults, not ratified final design.*

### 8c. Adapter contract — RESOLVED by claude-1 (bell 2026-06-05)

Source: `semilattice.edn :backlog` (108 items), each carrying `:c-joint :days-since :declared
:region :basins :n-commits :repo`. **TWO streams, two id-spaces — do NOT fake-unify:** missions
(`semilattice :name`, `M-/C-/E-*`) and business sorries (`mint.edn :sorry`, `:S-*`). Tag each
lane-item `:source :mission | :business-sorry`. (The §8 registry unifies them conceptually but is a
doc, not a queryable EDN.)

v1 attribute mapping — mostly READ existing fields; only 2 derived:

| attr | mission stream | business stream |
|---|---|---|
| `futon-important?` | `:c-joint` in **top-quartile** (= §7.1 valuable-path top-25); `:region` = coarse tier | priced/crux salience |
| `risk-mode?` | `:days-since` > 30 (stale, current-state) ∧ central | `:blocked? true` ∨ `:p :unsampled` |
| `in-joes-model?` | central band — **v1 CAVEAT: coincides with `futon-important?`** (c-joint is the only signal) | presence in mint / interim-director |
| `framing-blocked?` | parse `:declared` leading token ∈ {IDENTIFY, HEAD, proposal, draft}; eligible = {MAP, DERIVE, INSTANTIATE…}. Use `:declared` (phase), **NOT** centrality `:status` (git/label) | n/a |
| `operator-dependent?` | from `:declared` phase | derive from `:discharge` text ("SENT cold cycle"/"issue+send" = operator) |

DESCRIPTIVE GUARD holds: c-joint=centrality, days-since=git fact, declared=current phase,
blocked?/p=current state — all current-state, none a forecast (§5 wall).

**v1 CAVEAT (missions):** `in-joes-model?` ≡ `futon-important?` until v2, so the nag gate's 3-term
conjunction has effectively 2 independent terms for missions (central ∧ risk-mode ∧ acknowledged).
Honest v1 limitation. **v2 is near** (claude-1's branching-world-tree, §12 gamification-guard:
descriptive-only) and adds explicit `:salience-band / :risk-mode? / :operator-dependent? / membership`
→ swap the 2 derived terms to reads. **Build v1 now** (claude-1 rec); don't block on v2.

### 8d. Operator portal + endpoint contract (Joe via claude-1, 2026-06-05)

Joe wants an **operator portal** in the WM UI: the Morning Bulletin + a forward-model "what to act
on" strip + a **Futon Runner** click-through easter-egg, all reading the same real projections.
Ownership split:
- **claude-7 (this excursion) owns the DATA BACKBONE:** adapter (§8c) → `classify-item` → `build-bulletin`
  → HTTP endpoint.
- **claude-1 owns the PORTAL UI** (futon2 WM client cljs) — renders nag=red / brief=yellow / silent-count.

**Endpoint (the adapter's last mile):** `GET /api/alpha/war-machine/operator-bulletin` (dedicated, not
inline — independently pollable; the bulletin has its own acknowledged-state lifecycle). Returns:
`{:date :generated-at :nag [item…] :brief [item…] :silent-count :total}`.

**Per-item JSON shape (frozen for the UI):**
`{:id :title :why :lane("nag"|"brief") :source("mission"|"business-sorry") :target :salience(num|null)}`.
Silent items are never listed — only `:silent-count` (no silent caps). Keyword vals serialize as strings.

Build order: adapter (EDN→items) → wire classify/build-bulletin → endpoint → bell claude-1 when live.

## 9. Invariants (the design contract — logic-model-before-code gate)

- **INV-1 no-autonomous-fire-on-framing-blocked:** in autonomous mode, an `:open-mission` whose mission
  is framing-blocked (status-class detection) is **inadmissible** for execution (`can-execute?` ⇒ false);
  it is routed to a lane, never fired.
- **INV-2 lane totality+exclusivity:** every surfaced item gets exactly one lane ∈ {silent, brief, nag}.
- **INV-3 nag-gate conjunction:** lane = nag ⇒ in-Joe's-model ∧ futon-important ∧ risk-mode. Any item
  with only 2-of-3 MUST NOT be nag. (The "won't nag at every step" guarantee.)
- **INV-4 novelty-flows-down:** an unacknowledged (not-yet-in-Joe's-model) item cannot be nag on first
  sighting — it lands brief. Nag requires prior acknowledgement (the (D) clause).
- **INV-5 descriptive-only importance:** "futon-important" derives solely from descriptive signals
  (`c_joint` / declared-crux / structural position), never a predictive score.
- **INV-6 silent ⊆ autonomously-dischargeable:** only operator-independent items may be silent; anything
  needing Joe is at least brief.
