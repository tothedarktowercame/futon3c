# TN-zai2-session-notes — zai-2 reads the landscape, dispatches a94J06, and finds the memory gap live

(Part 2, the `*zai-repl:zai-1*` survey after completion, appended 2026-07-22
— see below.)

Captured 2026-07-22 by claude (M-typed-memories owner) from the Emacs
"server" buffer `*zai-repl:zai-2*` (session `zai-188835e2ae3d433195f20b3ec9c8d882`,
clocked to M-apm-capability-ratchet). Buffer contents are ephemeral; this note
is the durable record. Raw capture (with elisp-escape artifacts):
session scratchpad only — this distillation is the reference copy.

Purpose: motivating examples **staged for M-typed-memories MAP**. Joe's
methodological point (2026-07-22): "MAP usually looks at what exists in the
landscape, but what exists in a single session *is* now part of the
landscape" — the session below is itself evidence-store-resident (zai-2's own
turn-rounds), so these notes are pointers into the landscape, not a side
channel.

## Session timeline (what happened, in order)

1. **MAP work on M-apm-capability-ratchet** (futon0/holes/missions/). zai-2
   established (recorded there as §2.5): the Evidence Landscape already
   captures agent self-talk for Agency-routed sessions (`run-tool-rounds!` →
   `persist-round!` → `:turn-round` entries; `marks.clj` decorates
   `✘`/`✓`/`💡` as `:self-correction`/`:self-approval`/`:self-idea` tags);
   the APM **cron** pipelines bypass all of it (`run-problem.sh` pipes
   `claude -p -` through `stream-extract.py`, no evidence store;
   `run-apm-problem.sh` advances proof-cycle phases with stub payloads
   `{:approach "auto"}`). The a94J05 bundle proves the loss concretely: the
   informal solution closes part (b) by periodicity-of-exp, the formal Lean
   proof closes it by differentiating and `exp_ne_zero` — and the revision
   event exists only as an inference from comparing two finished documents.
   Pipeline evidence JSON records `lean_status: "closed"`, `sorry_count: 0`.
   That's it.

2. **The brown-out ate the memory-model question.** Joe asked zai-2 how
   zai/zaif agents store memories ("My plan was that they would store
   memories in XTDB2 and query them via XTQL... if an agent tries one
   strategy and then realises it doesn't work, and then finds one that does,
   it should record a memory, which would become a capability later"). The
   first attempt returned `[z.ai invoke failed: ZAIF decision persistence
   was rejected]` — this is the aborted operator turn in
   `futon1b/TN-futon1b-memory-incident.md` §Incident summary, timestamped
   inside the 13:20–13:37 brown-out. The question about durable memories
   was itself lost to the store's instability. Third attempt succeeded.

3. **zai-2's memory-model analysis** (the text Joe relayed into the
   M-typed-memories chartering conversation): 27 tools; six memory-family
   read tools; no `memory_write`/`memory_record`; the four write paths are
   automatic transcript/bug persistence plus the PSR/PUR/PAR trio. Its
   three-layer framing, adopted here as the cleanest statement of where
   M-typed-memories sits:
   - **Capture** (exists): self-talk → `:turn-round` transcript.
   - **Deliberate memory formation** (missing): "I tried X, it failed
     because Y, Z works because W" as a typed, queryable entry.
   - **Distillation into capability** (the ratchet): extract, test, promote
     memories into capability packets.
   Layer 2 is the bridge between M-zaif-harness (learning from evidence)
   and M-apm-capability-ratchet (capability packets). Joe's framing closes
   the chain: a recorded memory "would become a capability later."

4. **The a94J06 dispatch (the live specimen).** At Joe's direction ("get a
   new Zai agent to solve a real APM problem... once we capture the
   self-talk it will be easier to see what needs to be stored as memories"),
   zai-2: flipped zai-1 from `:restored` to `:idle` via drawbridge; checked
   z.ai quota (unit-6 window 39% available < the 50% cron gate — gate
   closed; manual one-sample bypass per Joe's standing instruction); chose
   **a94J06** (complex analysis on the unit circle, contour integral with
   ζ̄ — "looks like a Cauchy integral but isn't; the conjugate changes
   everything"); dispatched via `POST /api/alpha/bell`, job
   `invoke-1784725075981-12-a2d75b10`.

## What the a94J06 run captured (the motivating examples)

Evidence landed (counts as of zai-2's inspection, session
`zai-b14ff8a657ec476dbe375a0339d5da3a`): **100 entries** — 55 `:turn-round`
(rounds 1–93), 1 `:turn-start`, 38 `:zaif-arm-choice`, 6 `:bug`,
1 `:self-approval` (a `✓` parsed from narration by marks.clj).

**Example 1 — strategy narration IS captured (round 17):**

> "The informal proof's core trick: on |ζ|=1, ζ̄ = 1/ζ, converting the
> integrand to 1/(ζ(ζ−z)). Then partial fractions give
> (1/z)(1/(ζ−z) − 1/ζ), and each piece is a standard Cauchy integral...
> The main subtlety: the `conj_eq_inv_on_circle` step requires a pointwise
> rewrite under the integral (via `integral_congr`), since ζ̄ = 1/ζ holds
> only on the circle."

Strategy selection, Mathlib API mapping, and the proof's key subtlety — the
capability-relevant reasoning — all present in free text.

**Example 2 — retrospective self-correction IS (sometimes) captured:** a
`:turn-round` from the same session (sampled independently via
`GET :7073/api/alpha/evidence?author=zai-1`): "✘ Several issues to fix...
`mul_comm` on `(ζ−z)⁻¹ * ζ⁻¹` gives `ζ⁻¹ * (ζ−z)⁻¹` ≠ `(ζ*(ζ−z))⁻¹`. Need
`mul_inv_rev` instead." A completed learning act — strategy refuted,
replacement identified, reason given — as untyped prose.

**Example 3 — but narration runs forward, not backward (zai-2's DERIVE
observation, the sharpest finding for M-typed-memories):** natural
self-talk captures *what* the agent decided and *which* API it chose, but
it does not reliably capture *why a prior attempt failed* or *what was
tried and abandoned* — the agent narrates forward ("now I'll try X") rather
than retrospectively ("X failed because Y, so I switched to Z"). Example 2
shows retrospection occurs; Example 3 says it is uneven and unprompted.
**Consequence for the mission: transcript mining alone inherits this
unevenness; a `memory_record` tool invoked at the moment of realization —
or prompt design that asks for retrospective narration — is what makes the
learning event reliable rather than lucky.**

**Example 4 — provenance/context contamination:** the bell dispatched into
zai-1's *persisted* session, so prior zaif-harness context bled in; rounds
60+ drift away from a94J06. The proof landed partial
(`a94J06/lean/Main.lean`, 6754 bytes, 4 sorries; `status.json` not
updated). For memories this is a provenance warning: a memory formed in a
contaminated session needs its subject/session endpoints to say *which*
work it belongs to — exactly what typed hyperedge endpoints provide and
free-text transcript lacks.

## Runner-stability findings (M-zaif-harness track, not this mission)

- **Session contamination**: neither the bell path nor conductor v3
  `start!` resets the session; each APM problem needs a fresh session.
- **Manual continuation**: conductor v3 `continue!`/`backup!`
  (apm_conductor_v3.clj lines 636/584) are human REPL steps. Auto-continue
  exists inside one invoke (24 rounds × (1+8) via
  `FUTON3C_ZAI_AUTO_CONTINUE_MAX` = up to 216 tool rounds), but
  turn-complete-with-proof-unfinished still requires manual re-dispatch.

Both flagged by zai-2 for the M-zaif-harness track (Joe + claude-2); the
evidence *capture* worked despite them.

## Part 2 — the `*zai-repl:zai-1*` survey (after completion, zero sorries)

Surveyed 2026-07-22 after zai-1 finished: **a94J06 (Putnam 1994 A6,
∮ ζ̄/(ζ−z) dζ on the unit circle) COMPLETE — zero sorries, zero errors,
`lake env lean` exit 0, committed `6bbe1d7`** (apm-lean). Three theorems:
`apm_a94J06_inside` (=0), `apm_a94J06_outside` (=−2πi/z),
`apm_a94J06_at_zero` (=0); the divergent z∈γ case documented as comment
(Mathlib has no contour principal value).

### Session provenance confirmed

The buffer opens with the *prior* turn: zai-1 implementing the
**M-zaif-harness dual-constant shadow recording** for claude-2 (job
`invoke-1784716173241-9-0d828741`) — which explains both the contamination
zai-2 observed AND a pleasing loop: the paired `:zaif-arm-choice` entries
(`:constant-label :sweep`/`:shipped`, shared `:pairing-key`) sampled as the
mission's first evidence probe were recorded by the very mechanism zai-1
had built earlier in the same session. The agent built the instrument that
then measured its own decisions.

### Finding A — the existing deliberate write path FAILED in practice

During the zaif-harness turn: `[psr_select ✗ psr-select failed: Invalid
evidence shape]` (retry unclear), then `[pur_update ✗ Invalid evidence
shape]` **twice**, after which the agent abandoned the PUR and moved on —
the pattern-outcome record was silently lost. So the write side is not
merely missing for memories; **the one deliberate write family the agent
has misfired on shape validation under real use, and the agent had no
recourse**. Any `memory_record` design must treat shape-fit feedback and
retry ergonomics as first-class (a write tool that rejects the agent's
natural payload shape is a write tool that doesn't exist).

### Finding B — the specimen corpus has holes (durability, again)

Mid-proof, four consecutive turns died to
`[z.ai invoke failed: ZAI transcript persistence was rejected]`, plus one
EOF crash and one stalled compile — each requiring Joe to type "continue"
(manual shepherding, 6+ interventions). The zero-sorry proof exists; the
evidence trail of its middle section has gaps where persistence was
rejected. Reinforces the charter's durability stake with the specimen
itself: the corpus we intend to mine is already incomplete.

### Finding C — the route story is A → B → A′, and only memory can tell it

The strategy narrative is richer than one pivot: partial fractions →
(circle-integrability of the pieces gets "complicated"; step back) →
direct single-rational-function evaluation → dead ends (field_simp closing
goals *via `sorry`*; no `zpow_neg_two`) → **back to partial fractions**,
now carrying `‖z‖ ≠ 1` into the helper so `integral_congr`'s
everywhere-equality demand is satisfiable. The first partial-fractions
failure was never about the algebra — it was about `integral_congr`
needing equality at every point including ζ = z. A transcript miner sees
strategy A abandoned then re-adopted; only a typed memory ("A failed
because integral-congr needs everywhere-equality; carry the off-circle
hypothesis" — an IATC `retract` then re-`assert` with a `because`) makes
A′ ≠ A legible.

### Finding D — within-session recurrence: memory pays off before the session ends

The `rw` direction confusion with `h : ζ = w` (does `rw [← h]` rewrite ζ→w
or w→ζ?) recurred **three times** in one session (buffer lines ~813, ~1016,
~1042 of the capture). A memory written at first resolution would have paid
for itself twice before the proof closed. Memory value is not only
cross-session.

### Memory-candidate catalog (for MAP's shape work)

Each is a completed learning act in the transcript, none queryable:
1. `mul_comm` gives wrong inverse shape → `mul_inv_rev` (the charter's
   Example 2).
2. `rw`/`integral_congr` cannot pattern-match inside circle-integral
   notation → `simp only [one_div]` / `convert` / restate integrands in
   `⁻¹` form throughout (an architectural convention, adopted mid-proof).
3. `integral_congr` demands everywhere-equality → carry `‖z‖ ≠ 1` into
   helper lemmas (the A′ fix).
4. `field_simp` can close goals *via `sorry`* silently — verify what it
   produced (a trust-calibration memory about a tactic).
5. `ring` can't handle `⁻¹` (it doesn't know `inv`) — pair with
   `field_simp` or use `ring_nf`.
6. `ζ^(-2)` is not defeq to `(ζ*(ζ-0))⁻¹` → `ring_nf` where `rfl` fails.
7. `rw` direction semantics for `h : a = b` (Finding D).
8. `not_le_of_gt` vs `not_lt_of_gt` when contradicting `≤` from a
   closed-ball membership.
Plus the strategy-level memory (Finding C). Note the span: tactic
behavior, API absence, notation-matching, proof architecture — the
`:memory/*` type family should not assume one granularity.

### Minor corroborations

- `[auto-continue 3/8]` visible near the end — the auto-continue budget
  mechanism working as zai-2 described.
- zai-1's completion bell went `--to http-caller` — an unregistered
  pipeline persona (the 07-16 agent-not-found corollary); its turn text was
  the real reply channel. Typed acts misfire silently on untyped
  infrastructure in more places than one.

## Part 3 — the comparison case: programming-domain memories (zai-2), and the Claude-memory transfer

Added 2026-07-22 at Joe's direction: survey zai-2's own session as a
comparison case (programming/ops domain vs zai-1's mathematics), plus a
transfer analysis from the Claude harness's file-based memory system — a
*working* deliberate-memory implementation — to XTDB/XTQL storage.

### Programming-domain memory candidates (zai-2's session)

1. `reflect_ns ✗ Namespace not found: futon3c.agents.zai_api` — Clojure
   munging: file `zai_api.clj` ↔ namespace `zai-api` (underscore/hyphen).
2. `search ✗ Illegal repetition near index 16` — the search tool takes
   Java regex; literal `{`/`}` in a pattern (`{:name "`) parses as a
   repetition quantifier and must be escaped. A tool-contract memory.
3. `run_readonly ✗ Command rejected: appears destructive` — the readonly
   peripheral's heuristic rejects some innocuous compound commands;
   fallback is `run_shell`. **Hit BOTH zai-2 and zai-1 in the same day**
   — the first concrete cross-agent dedup/sharing case in the corpus.
4. "The earlier query failed because I was querying via the wrong path" —
   evidence query via HTTP :7074 returned empty; the in-process store had
   100 entries. Topology memory (the same :7073/:7074 confusion the
   charter flags). Notably this one WAS narrated retrospectively, with a
   because-clause — natural self-talk sometimes produces exactly the
   memory shape, unprompted (cf. Part 1 Example 3: unevenness, not
   absence).
5. z.ai quota has two TOKENS_LIMIT windows; unit-6 is the binding one for
   the 50% cron gate. Operational reference.
6. Agency agents in `:restored` state are not invoke-ready; flip to
   `:idle` via drawbridge before dispatch. Ops procedure memory.

### The domain contrast that moves the design

- **Validity horizons differ by domain.** Math memories ("`mul_inv_rev`
  for inverse products", "`integral_congr` needs everywhere-equality")
  are essentially timeless. Ops memories ("store is on :7073", "quota
  window unit-6 binds") are valid-until-changed — several in the list
  above will be invalidated by the very hardening/wiring work this
  mission depends on. **Bitemporality is load-bearing for ops memories
  specifically**: valid-time end is how ":7073" stops being retrieved as
  current without deleting the fact that it was once believed.
- **Subjects differ.** Math memories attach to library/domain entities
  (Mathlib, `circleIntegral`, a problem family); ops memories attach to
  infrastructure entities (a service, a tool contract, an agent-state
  machine). The endpoint vocabulary must span both without strain.
- **Sharing topology differs.** Ops memories pay off *across agents in
  the same environment* (candidate 3 hit two agents in one day); math
  memories pay off *across problems* for the same capability. Retrieval
  scoping (who should see this memory, keyed on what) is a MAP question,
  not a nicety.

### Transfer table: Claude file-memory patterns → XTDB/XTQL

The Claude harness memory (one markdown file per fact under
`~/.claude/projects/<proj>/memory/`, `MEMORY.md` index) is a deliberate
write-at-realization memory system in production use — the working
counterpart of the tool zai lacks. Its patterns transfer cleanly:

| Claude pattern | XTDB/XTQL transfer |
|---|---|
| One file = one fact; frontmatter `name`/`description`/`type` | One evidence entry (body = the fact) + typed hyperedge; `description` = compact retrieval hook, i.e. a projection column (matches the hardening pass's compact-projection-then-hydrate discipline) |
| `type: user\|feedback\|project\|reference` | A **content-kind dimension orthogonal to the IATC act dimension** — a memory is (act × kind), e.g. assert × feedback. Two small vocabularies, not one large one |
| feedback bodies carry `**Why:**` + `**How to apply:**` | Structured body slots (the PAR precedent: maps, not strings). "How to apply" is an *application condition* — i.e. a retrieval predicate: when should this memory surface |
| `MEMORY.md` index, loaded every session; one line per memory; "never put content there" | Boot-time bounded XTQL projection (the memory analog of `mission_context`); the projection/hydration split is the same discipline |
| `[[name]]` links; dangling links allowed ("marks something worth writing later, not an error") | Hyperedge endpoints; XTDB doesn't enforce referential integrity, so a dangling endpoint is natural — a *named-but-unwritten node* is a standing query for future memory formation |
| Update-don't-duplicate; delete memories that turn out wrong | Supersession as a new version at the same id — **an improvement in transfer**: the file overwrite destroys history, XTDB2 keeps belief-as-of-T for free; delete-wrong = retract with valid-time end |
| "Don't save what the repo already records" | Dedup gate against derivable knowledge; store the *link* to the transcript evidence distilled (endpoint/in-reply-to to the `:turn-round`), not a copy of it |
| Recall marked "reflects what was true when written — verify before relying" | Already the memory envelope's frame string ("recorded, not necessarily current"); valid-time queries make the discipline mechanical instead of hortatory |
| Relative dates converted to absolute at write time | Timestamps as data; trivial in-store |

Two meta-observations from the transfer: (1) the Claude system demonstrates
that **write-at-realization works in production** — the writing agent
decides significance at the moment of realization, with a curation
discipline (check-for-existing, update, link) that `memory_record`'s
ergonomics should encode rather than re-discover; (2) the (act × kind)
factorization resolves what would otherwise be a vocabulary fight — IATC
performatives say what the memory *does* to prior belief, the kind says
what it's *about*.

## Pointers

- M-typed-memories: `futon3c/holes/missions/M-typed-memories.md` (consumer
  of these examples at MAP).
- M-apm-capability-ratchet: `futon0/holes/missions/M-apm-capability-ratchet.md`
  §2.5 (zai-2's MAP findings recorded in-mission).
- Store incident + hardening: `futon1b/TN-futon1b-memory-incident.md`.
- The session's own turn-rounds are in the evidence store
  (author zai-2 / zai-1, 2026-07-22) — queryable, per Joe's point, as
  landscape.
