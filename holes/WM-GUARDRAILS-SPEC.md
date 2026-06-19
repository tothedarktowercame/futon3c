# WM Guardrails Mode — overnight-usable spec (handoff: claude-1, 2026-06-06)

**Owner/reviewer:** claude-1 (author of this spec; will review both diffs).
**Extends:** `holes/missions/M-war-machine-first-outing.md` §8 (the harness that ran the
2026-05-30 base case). This adds the **cadence** capability (the inductive step for
pudding-prover **T4.2**, now `:cadence :base-case` in `futon7/holes/pudding-prover-registry.edn`).
**Operator decision (Joe, 2026-06-06):** posture **A** + reframe **allowlist → guardrails**.
The default flips: the WM does what obviously needs doing, bounded by the hard NOs below;
anything it can't do without Joe becomes a **NAG** (with a concrete unblock) and the loop
**steps forward** — it never wedges.

## The problem this fixes

The first outing wedged at `:learn-action-class = STARVED OF INPUT SOURCES`. The live WM
judgement is **199 ranked-actions, all `:open-mission` / `:learn-action-class`, zero
`:address-sorry`** (the base case discharged the sorry queue). §8.13's allowlist
(autonomous = `:address-sorry` only; everything else halt-and-surface) therefore halts on
*everything* → no autonomous progress. Two changes:

1. **Step-forward, don't halt** (this spec, codex-1): operator-dependent actions emit a
   needs-you NAG item and the selector demotes them, engaging the top *admissible-autonomous*
   action instead. Halting is reserved for the one trust-breach (G3 fake-finish).
2. **The WM must see the real work** (codex-4): its ranking is suspect — it puts
   `futon4-d/essays-diachronic-model` at rank 1, and the `-d` targets smell like
   draft/derived namespaces. Investigate + fix the input-sources hygiene.

---

## Guardrails (the hard NOs — encode these, don't paraphrase)

A WM action is **autonomous-admissible** iff ALL hold; otherwise it is **needs-operator**
(→ NAG, never executed autonomously):

- **Class is in the autonomous set:**
  - `:address-sorry` (audit / doc / targeted fix — as witnessed in the base case).
  - `:fire-pattern` (low addressability today, but admissible).
  - `:open-mission` **only when the target is an already-open mission with ≥1 open hole**
    (bounded *advancement* — discharge one hole). A net-new-mission greenlight is NOT this.
- **Class is NOT:** `:learn-action-class` (the niche/bootstrap signal — needs operator),
  net-new `:open-mission`, or any action carrying a ∇-deform / protocol-defining /
  niche-construction marker.
- **Target touches no forbidden region:** never `<repo>/.state` (sandbox), never anything on
  the futon7-PRIVATE leak boundary (futon7b must be built clean, never forked from futon7
  history). When in doubt, needs-operator.
- **No outward/irreversible op:** sends (EOI, invoice, email, post) are never WM action
  classes, but guard defensively — any such → needs-operator.

Process guardrails the harness already enforces (do not weaken): never master directly
(staging branch `wm-outing/<date>`, Joe merges = the consent gate); never restart the serving
JVM (I-0; Drawbridge `load-file` for own edits; never `:reload` a third-party or
protocol-defining ns); earned discharge only (V2 no-teleport; top-shift TRUE or it's a
fake-finish → **hard-halt**); don't-flood (one hole/cycle; E-cheesemonger).

---

## Unit 1 — codex-1: the guardrails core (the selector + emitter)

**Goal:** the WM steps forward onto autonomous work and parks operator-dependent actions as
NAG items, lighting up claude-7's seam (`futon3c/data/wm/needs-you.edn`, committed `f643b95`).

**Files (`:out` = create/edit):**
- `futon3c/src/futon3c/wm/guardrails.clj` (NEW)
- `futon3c/src/futon3c/wm/needs_you.clj` (NEW)
- `futon3c/src/futon3c/peripheral/war_machine_pilot.clj` (EDIT — add the guardrails mode)
- tests under `futon3c/test/` mirroring existing test layout

**`guardrails.clj` — pure functions over an action + a small ctx:**
```clojure
(defn open-mission-with-holes?
  "True iff TARGET names an existing open mission with >= 1 open hole.
   Consults the mission registry (futon2.aif.mission-registry via requiring-resolve,
   matching the war_machine_pilot.clj pattern). Net-new / closed / hole-free => false."
  [target ctx] ...)

(defn autonomous-admissible?
  "The guardrail predicate. ACTION = {:type kw :target str} (the dT entry's :action).
   Returns true iff the action is safe to execute autonomously per the Guardrails section.
   Pure given CTX {:mission-status-fn :forbidden-path? ...} (inject lookups for testability)."
  [action ctx] ...)

(defn classify-action
  "=> :autonomous | :needs-operator."
  [action ctx] ...)
```

**`needs_you.clj` — the seam emitter (claude-7's confirmed shape + path):**
```clojure
(def needs-you-path "data/wm/needs-you.edn")   ; futon3c-relative; vector of items

(defn action->needs-you-item
  "Build one needs-you item from a stepped-past dT entry. SHAPE (claude-7, salvo 2):
   {:id \"wm-needs-<class>-<target>\"  ; stable across re-emits => dedupe
    :title <short> :why <why-blocked/matters>
    :unblock-action <imperative; class-specific; distinct from :why>
    :lane \"nag\" :source \"wm-needs-you\" :target <id> :path <file Joe opens>
    :salience <g-total> :repo <repo> :wm-action-class <kw>
    :g-total <num> :emitted-at <iso> :run-id <run-id>}
   :unblock-action by class:
     :open-mission (net-new)  -> \"Open <path>, confirm scope, greenlight (or decline).\"
     :learn-action-class      -> \"Feed the WM an input source for <target/area> (see M-war-machine-input-sources).\"
     <other needs-operator>   -> a concrete one-line clearing step."
  [dT-entry run-id] ...)

(defn emit-needs-you!
  "Write ITEMS as the needs-you vector (last-write-wins, deduped by :id). The loop rewrites
   the whole vector each tick: an item that stays parked shows once; dropping it clears it
   from the bulletin (claude-7 reads the live vector each request). Cap to top-K by salience;
   if more were stepped past, append a single advisory item noting the dropped count
   (no silent caps)."
  [items] ...)
```

**`war_machine_pilot.clj` — add `:guardrails?` to `begin-live-cycle!`:**
- When `:guardrails? true`, instead of `top (first dT)`:
  - `autonomous` = first dT entry whose `classify-action` is `:autonomous`.
  - `stepped-past` = the `:needs-operator` entries ranked above `autonomous` → build needs-you
    items, `emit-needs-you!` them, include `:needs-you-emitted N` in the return.
  - `top` = `autonomous`; proceed as today (cg, proposal, request-tick).
  - If NO autonomous entry exists: emit top-K needs-operator entries as needs-you and return
    `{:ok false :reason :no-autonomous-action :needs-you-emitted N}` (harness soft-stops —
    it surfaced the queue, did not wedge).
- Default (`:guardrails?` absent/false) keeps today's behaviour exactly (regression-safe).

**Acceptance:**
- `autonomous-admissible?` true for `:address-sorry`; false for `:learn-action-class`,
  net-new `:open-mission`, `.state`/futon7-private targets; true for `:open-mission` on an
  open mission with holes (via injected `mission-status-fn`).
- `begin-live-cycle! {:guardrails? true}` against the live judgement (199 actions, no
  `:address-sorry`) → emits needs-you items + returns `:no-autonomous-action` WITHOUT throwing
  or wedging; `needs-you.edn` is written, items carry `:unblock-action` + dedupe by `:id`.
- Default mode unchanged (existing pilot tests still pass).

## Unit 2 — codex-4: input-sources hygiene (why the WM ranks drafts)

**Goal:** make the WM's ranked-actions reflect the *real* frontier, not draft/sandbox missions.
This is M-war-machine-input-sources groundwork; independent of Unit 1.

**Investigate (read-heavy, report findings as evidence — file:line, counts):**
- Where does the mission list feeding `:open-mission` ranked-actions come from? Trace from the
  WM scan (`futon2.report.war-machine`) back to the mission source(s). Search `futon4-d`,
  `futon6-d`, `-d/mission/`, mission-registry population.
- What are the `-d` namespaces? Confirm/refute: are draft/derived/sandbox missions being
  ingested and ranked as if live? Cross-check against `<repo>/.state` sandbox exclusion
  (the WM should not rank sandbox missions).
- Are closed/discharged missions still appearing? Is hole-count consulted in the ranking?

**Fix (small, gated):** exclude draft/sandbox/closed missions from the WM's mission input so
the ranking surfaces the live frontier. Smallest correct change at the source; do NOT widen
scope into a registry rebuild — if the fix is bigger than a filter, STOP and report back with
the finding + a proposed plan instead of building.

**Acceptance:** the WM ranked-actions no longer surface `-d`/sandbox/closed missions (or a
clear report that they aren't, refuting my hypothesis); evidence with counts before/after.

---

## Gates (both units — see AGENTS.md)
- `clj-kondo` clean on changed `.clj` (0 new warnings).
- `futon3c`/`futon4 dev/check-parens.el` clean on changed files.
- Relevant tests pass (`clojure -X:test` or the repo's runner); add tests for new fns.
- **Reload discipline:** verify own edits via Drawbridge `load-file`; NEVER restart the JVM,
  NEVER `:reload` a third-party/protocol ns. No synchronous heavy WM calls (use the cached
  query / `request-tick!`, never blocking `tick!`).

## Report back
**Bell `claude-1`** (`futon3c/scripts/agency_send.py --to claude-1 --kind bell`) with: a short
summary, commit SHA(s), gate results (kondo / check-parens / tests), and anything you decided
or any place you stopped-and-surfaced. claude-1 reviews each diff before anything is trusted.
