# M-typed-bells — type the coordination channel; make ArSE populate by construction

Date: 2026-06-11
Status: **COMPLETE through DOCUMENT; flag-gated OFF by default** (2026-06-11). Full lifecycle
IDENTIFY→MAP→DERIVE→ARGUE→VERIFY→INSTANTIATE→review→DOCUMENT. Codex authored (`b4ed15f`),
Claude owner reviewed independently (author≠reviewer) — diff read against TB-1…7/D1–D6, tests +
clj-kondo re-run; PASS. Watch-item carried into DOCUMENT as
`holes/tickets/T-typed-bell-arse-write-async.md`. Open threads are activations, not unfinished
work: (1) live C-adopt demo once `FUTON3C_TYPED_BELLS` is ON (Joe's call); (2) deferred S4/S5.
Logic model
TB-1…7 PASSES (`typed_bells_invariants_test`: 3 tests/25 assertions/0 fail — witness clean,
7/7 adversarial caught). Implementation slices S1/S2/S3/S-expose landed behind
`FUTON3C_TYPED_BELLS`; default OFF preserves the legacy bell path. Focused INSTANTIATE
gate: 31 tests / 101 assertions / 0 failures across typed-bell invariants, bell-router graph,
and auto-bellback/transport tests; `check-parens.el` clean; `clj-kondo --fail-level error`
clean (0 errors; existing `http.clj` warnings remain). VERIFY added the **adoption** criterion
(C-adopt) + a required exposure slice (S-expose): a protocol no agent uses is dead, so
docs/ergonomics ship with the code. Phases prior: ARGUE (pattern cross-ref, 2 PSRs,
plain-language argument); DERIVE (entities/relations, TB-1…7, 6 IF/HOWEVER/THEN/BECAUSE
decisions, Capability Preservation Matrix); MAP (wire schema, exact insertion points);
IDENTIFY (gap + prior art). Promoted from `holes/excursions/E-typed-bells.md` at Joe's
request 2026-06-11.

Cross-reference:
- `holes/excursions/E-crossed-bells.md` + `src/futon3c/agency/bell_router.clj` — the
  **parent**. The bell router gave us a *structural* conversation graph (threading via
  `:bellback-of`). This mission adds the *semantic* layer (illocutionary type).
- `M-agency-hardening.md` — grand-parent; the transport half (durable queue, drainer v2,
  caller capture, the flag-gated-rollout discipline) is done and is the substrate here.
- `README-arse.md` + `src/futon3c/peripheral/arse.clj` + `src/futon3c/agents/arse_work_queue.clj`
  — the Q&A substrate the typed bells back onto.
- Prior art: **Corneli, Martin, Murray-Rust & Pease 2017**, *Towards mathematical AI via a
  model of the content and process of mathematical Q&A dialogues*
  (`metameso.org/~joe/papers/corneli2017towards.pdf`) — the IATC model is the spec.

## HEAD (one line)

**The gap (Joe's framing, 2026-06-11):**
- **ArSE exists, but isn't used.** The Q&A substrate is built and starved of input.
- **Agent exchanges exist, but aren't stored.** Every bell is a real Q&A act; we keep the
  transport receipt and discard the content.
- **They also aren't typed.** A bell is one *untyped* imperative — "invoke B with this
  text" — so even what *is* logged carries no illocutionary force.
- **However, I wrote a paper in 2017** (Corneli et al., IATC) that explains how to **type
  constructive dialogues** and thereby **populate ArSE with something meaningful**.
- **Added benefit:** typing also **untangles crossed agent communications** (E-crossed-bells)
  — a bell that *declares* itself a `:query` (or an `:answer` carrying a `:ref`) is no longer
  ambiguous between a NEW request and a REPLY to one's own outstanding bell. The type
  disambiguates what threading alone could only guess.

**Type the bells.** Typing recovers the **illocutionary force** (ask / answer / assert /
challenge / …), so the conversation graph stops being merely structural (who-replied-to-whom)
and becomes *semantic* (what-speech-act). The `:query`/`:answer` types then back **losslessly,
at birth** onto ArSE — turning the live agent mesh into a running instance of the **IATC**
dialogue model the 2017 paper built to *analyse frozen human traces*, now *generating* the
dialogues. Coordination traffic we currently throw away becomes the Q&A substrate.

- *Jazz-head reading:* the generative kernel is **illocutionary force as a first-class
  field on the coordination edge**. Threading, ArSE-population, the work-queue, the
  conversation graph, and crossed-bell disambiguation are all improvisations over that move.
- *AIF-head reading:* satisfaction condition = a `:query` bell *is* an ArSE question routed
  to a helper and its `:answer` reply *is* that question's ArSE answer — **with no mining
  step**, because the agent declared the type at send time.

## Motivation

Four things converge here, which is why this is a mission and not a one-off:

1. **The data is free and we discard it.** Every bell is a speech act between agents. We
   log the *transport* (job ledger, mesh edges) but not the *meaning*. The bell router
   recovered the thread structure for free; typing recovers the meaning for nearly free
   (one payload field, agent-declared).
2. **ArSE was built for exactly this and is starved of input.** ArSE (`README-arse.md`) is
   Q&A-as-first-class-evidence with a work-queue of legible gaps — but questions only enter
   when an agent *deliberately* runs `!ask`. Most real questions are asked *in the act of
   coordinating* ("hey B, is X true?"). Typed `:query` bells route that normal traffic into
   ArSE without a separate deliberate step.
3. **It instantiates a decade-old research program live.** The 2017 paper modelled
   Mathoverflow traces with IATC and argued Q&A is a route to machine-reasonable knowledge.
   The agent mesh now *generates* such dialogues — Turing's "machines that converse to
   sharpen their wits" (paper ref 41) made operational. This is the substantive bet, not
   just an ergonomic.
4. **It untangles crossed communications (the E-crossed-bells payoff).** Threading alone
   leaves the A↔B simultaneous-bell case ambiguous: an agent can't tell a NEW request from a
   REPLY to its own outstanding bell. An *illocutionary type* resolves it — a `:query` is a
   fresh ask; an `:answer` carrying a `:ref` is unambiguously a reply to that referent. So
   typing doesn't just feed ArSE, it directly buys down the coordination-confusion cost the
   parent excursion was chasing.

## Prior art — IATC (Corneli et al. 2017) is the spec

The paper hands us four things almost verbatim (detail in the excursion):

1. **Mechanism = IAT (Inference Anchoring Theory):** anchor each locution to its
   illocutionary performative. *That is "typing the bell."*
2. **Type vocabulary, for free:** IATC's performatives — **assert, query, challenge, agree,
   define, retract, suggest**. Start here; do not invent a taxonomy.
3. **Sub-typologies** for `:query`/`:answer` when traffic earns them: the Martin–Pease
   question typology (Conjecture/What-is-this/Example/…) and the Tausczik–Kittur
   collaborative acts (provide-info/clarify/critique/revise/extend).
4. **The conversation graph IS IATC:** the paper's Figs 1–5 are typed-performative dialogue
   graphs; `bell_router/graph` is the same object one type-layer down.

The paper also gives an **evaluation** (SEMATCH: recover Q↔A pairings blind) and a standing
**caution** (argument-relation structure is hard, often hand-coded → grow the taxonomy from
live traffic, don't pre-build it).

## Existing Ground (IDENTIFY / MAP — what is already built)

- **Bell payload + threading.** `transport/http.clj` `handle-bell` already extracts
  `in-reply-to` (multiple key spellings) and `create-invoke-job!` carries `:bellback-of`
  (flag `FUTON3C_BELL_ROUTER`). Adding `:type` (+ `:ref`) follows the *exact* pattern
  `in-reply-to` already established — same destructure site, same flag gate.
- **Conversation graph.** `agency/bell_router.clj` `graph` → `{:open :answered :crossings
  :by-agent}` over jobs. Typing adds a `:type` field to each edge; the graph gains a
  type-aware view.
- **Mesh edge schema.** `agency/mesh_qa.clj` `job->edge` already projects `:bellback-of`;
  add `:type` alongside.
- **Surface header.** `wrap-surface-header` already prints a `Thread:` line when the router
  is on; it grows a `Type:` line ("Type: query — help resolve ArSE <ref>").
- **ArSE — the back-end is done and aligned.** `peripheral/arse.clj` `make-arse` +
  endpoints `/api/alpha/arse/ask`, `/answer`, `/unanswered`. Crucially, **ArSE already
  threads with `in-reply-to`** (questions → claim-type `:question`; answers → `:conclusion`
  with `in-reply-to` → the question's evidence id). That is the *same primitive* as the bell
  router's `:bellback-of` — the two layers were built to fit. A `:query` bell's `:ref` is an
  ArSE thread-id; the `:answer` bell calls `/answer` with that thread-id.
- **ArSE work-queue.** `agents/arse_work_queue.clj` (`next-unprocessed`, `queue-status`,
  `entity->issue`, `emit-arse-evidence!`) is the open-question queue. A **directed** `:query`
  bell (→ B) is a directed assignment off that queue; a **broadcast** `:query` is the open
  queue. Bell-mesh and ArSE-queue are one machine seen from two ends.

## MAP — the wiring (territory mapped to exact sites, 2026-06-11)

### Wire schema — two new payload fields, mirroring `in-reply-to`

A typed bell adds **`:type`** (an IAT performative keyword) and optional **`:ref`** (a
pointer — an ArSE thread-id, or the bell/evidence id the act is about):

```jsonc
POST /api/alpha/bell
{ "agent-id": "claude-3", "caller": "fable-1", "surface": "bell",
  "prompt": "Is the drainer reentrant under A↔B?",
  "type": "query",            // one of the IAT seven; absent ⇒ :request (today)
  "ref":  "ask-1772986500-3"  // optional: the ArSE thread / referent this act concerns
}
```

`:type` accepts `query answer assert challenge agree define retract suggest` (+ `request`
as the untyped default). `:ref` is opaque to transport; only the ArSE bridge (S3)
interprets it.

### Type → behaviour table (the three-layer boundary, made operational)

| `:type`     | Conversation graph | ArSE | Crossing semantics |
|-------------|--------------------|------|--------------------|
| `query`     | opens a Q edge | **ask** (no `:ref`) or **route** (`:ref`→existing) | a fresh ask — never a reply |
| `answer`    | closes the `:ref` edge | **answer** `/arse/answer` against `:ref` | a reply — collapses a false crossing |
| `request`   | opens a task edge | — (ambient) | today's behaviour, unchanged |
| `assert`/`challenge`/`agree`/`define`/`retract`/`suggest` | typed edge, ambient | — (S5: dialectical edges, deferred) | informational |

Only `query`/`answer` touch ArSE. Everything else stays in the ambient log — this *is* the
Evidence ⊇ ArSE ⊋ ambient boundary as a dispatch rule, not a doc paragraph.

### Insertion points (verified line numbers — DERIVE becomes mechanical)

1. **Payload extract** — `transport/http.clj` `handle-bell` **§2756–2758** (where `in-reply-to`
   is read): add `bell-type`/`ref` reads with the same multi-spelling `(or (:type p) (get p
   "type") …)`.
2. **Job create call** — `handle-bell` **§2772–2777**: pass `:bell-type`/`:ref` alongside the
   existing `:bellback-of (when (bell-router-enabled?) …)`, gated on `(typed-bells-enabled?)`.
3. **Job map** — `create-invoke-job!` **§547 destructure + §568–572 job map**: add `:bell-type
   (some-> bell-type …)` / `:ref …` next to `:bellback-of` (§572). One ledger field each.
4. **Recipient thread context** — `run-invoke-job!` **§2584–2585** (builds `thread` from the
   job's `:bellback-of`): add `:type`/`:ref` so the surface header can show them.
5. **Surface header** — `wrap-surface-header` **§2270–2276** (the `Thread:` line): add a `Type:`
   line ("Type: query — help resolve ArSE `<ref>`").
6. **Conversation graph** — `agency/bell_router.clj` `graph` **rows §38**: carry `:type (norm
   (:bell-type b))`; the crossing test **§44–49** gains a guard (below).
7. **Mesh edge** — `agency/mesh_qa.clj` `job->edge` **§72** (where `:bellback-of` is projected):
   add `:type`/`:ref`.
8. **ArSE bridge (new, S3)** — a small `agency/arse_bridge.clj` (or a branch in the drain
   finalizer) that, on a `query`/`answer` bell, calls `peripheral/arse` ask/answer. The
   *only* genuinely new module; everything 1–7 is a field added to an existing flow.

### Crossing-resolution mapping (the E-crossed-bells payoff, concretely)

`bell_router/graph` flags a crossing when `open-dir` contains both `[A B]` and `[B A]`
(bell_router.clj §44–49). Typing collapses the **false** crossings:

- If the `[B A]` bell is `:type answer` with `:ref` = the `[A B]` bell, it is **not** a
  second open bell — it is the reply. Resolution: in `graph`, an `:answer` carrying a `:ref`
  resolves its referent's edge (same as `:bellback-of` does today), so it never enters
  `open-dir`. The crossing disappears *because the type said so*, not by heuristic.
- A genuine crossing (both `:query`, neither referencing the other) still surfaces — that's
  the real "both asking, get on a whistle" case. Typing **sharpens** the signal: crossings
  that remain are exactly the ones worth reconciling.

### ArSE correspondence (why S3 is a bridge, not a build)

The two layers already share the threading primitive:

| Bell field        | ArSE field            | note |
|-------------------|-----------------------|------|
| `:type query`     | claim-type `:question`| ask creates the thread |
| `:type answer`    | claim-type `:conclusion` | answer posts to the thread |
| `:ref`            | `in-reply-to` (→ Q evidence id) | **identical primitive** to bell-router `:bellback-of` |
| `:caller`/`agent-id` | `author`           | who asked / answered |
| `prompt`          | question / answer body | the content |

So S3 maps bell→ArSE by field renaming + one HTTP call to `/arse/ask` | `/arse/answer`.

## DERIVE — slice plan (each flag-gated, OFF path byte-for-byte unchanged)

> Discipline (inherited): default OFF, OFF path byte-identical; logic-model-before-code for
> any path touching the invoke/queue concurrency; no Codex handoff (Joe: "no bells").

### Entity types

- **TypedBell** — *not a new entity*; the existing invoke-job extended with two fields.
  `:bell-type` ∈ `{:query :answer :assert :challenge :agree :define :retract :suggest
  :request}` (IAT-seven + `:request` default); `:ref` (opaque string — an ArSE thread-id or
  a referent bell/evidence id). Identity = the existing `:job-id`. Source = `:authored` (the
  sending agent declares the type). Untyped legacy bell ≡ `{:bell-type :request :ref nil}`.
- **ArseThread** — *existing* (`peripheral/arse.clj`): a `:question` evidence node + its
  `:conclusion` answer(s) linked by `in-reply-to`. Typed bells *produce* these; the mission
  introduces no new ArSE entity.

### Relation types

- **`:bellback-of`** (binary, existing) — *this job replies to that job*. The thread spine.
- **`:ref`** (binary, new) — *this act is about that referent* (an ArSE thread, a prior bell,
  an evidence id). Distinct from `:bellback-of`: a `:query` can `:ref` a *topic* without being
  a *reply*; an `:answer` typically has both (`:bellback-of` the asking bell, `:ref` the ArSE
  thread). Most n-ary structure is deferred to S5 (dialectical edges); v1 is binary only.
- **type-edge** (the conversation-graph edge carrying `:type`) — the labelled-edge view
  `bell_router/graph` produces.

### Invariant rules (checkable propositions — the VERIFY logic model, stated now)

Per `logic-model-before-code`, these are written so VERIFY can run them as a core.logic/pldb
model over an adversarial trace *before* code hardens. A conforming trace ⇒ 0 violations;
each invariant gets an adversarial trace that must be caught.

- **TB-1 (type validity):** every bell's `:bell-type` ∈ the allowed set. Absent ⇒ coerced to
  `:request`. Present-but-unknown ⇒ **rejected at the door** (400), never coerced — *anti-drift*.
- **TB-2 (OFF inertness — the fidelity tripwire):** flag OFF ⇒ the job map carries no
  `:bell-type`/`:ref`, the surface header has no `Type:` line, `graph` is byte-identical to
  today, and **no ArSE write occurs**. OFF is the legacy system exactly.
- **TB-3 (answer ⇒ ref):** a `:answer` bell MUST carry a non-blank `:ref`. An answer with no
  referent is malformed (you cannot answer nothing) ⇒ 400.
- **TB-4 (ArSE iff Q&A):** an ArSE write happens **iff** `:bell-type ∈ {:query :answer}`. No
  other type ever touches ArSE. *This is the three-layer boundary as a dispatch invariant.*
- **TB-5 (ref ↔ in-reply-to fidelity):** when a `:query` creates a thread, the bell's `:ref`
  is stamped with the new thread-id; the matching `:answer`'s ArSE post carries
  `in-reply-to == :ref`. Bell-`:ref` and ArSE-`in-reply-to` agree by construction.
- **TB-6 (crossing soundness — the E-crossed-bells property):** any crossing remaining in
  `graph` has *no* `:answer`-with-`:ref` linking the two directions. Typed replies are never
  reported as crossings; the crossings that survive are exactly the genuine both-asking cases.
- **TB-7 (idempotent ArSE write):** re-running/replaying a `:query` bell does not create a
  second ArSE thread (mirror the auto-bellback once-only guard via the job's `:ref` stamp).

### Data flow

```
agent A sends  POST /bell {type:query, prompt:"is X true?"}        (no :ref → a fresh ask)
  └─ handle-bell: extract :bell-type/:ref            (http.clj §2756 region, gated)
       └─ [ArSE bridge, send-time] POST /arse/ask {author:A, question:prompt}
            └─ thread-id "ask-…"  ──stamp──▶ job :ref := "ask-…"        (TB-5, TB-7)
       └─ create-invoke-job! {:bell-type :query :ref "ask-…" …}    (§547/§572)
  └─ drain → recipient B's surface header shows  "Type: query — help resolve ArSE `ask-…`"
agent B replies  POST /bell {type:answer, ref:"ask-…", bellback-of:<A's bell>, prompt:"yes…"}
  └─ handle-bell: TB-3 ok (ref present)
       └─ [ArSE bridge] POST /arse/answer {thread-id:"ask-…", answer:prompt, author:B}
       └─ create-invoke-job! {:bell-type :answer :ref "ask-…" :bellback-of <A> …}
  └─ bell_router/graph: A→B edge resolved by B's :ref answer → NOT a crossing   (TB-6)
```

The **ArSE write fires at send-time inside `handle-bell`**, not at the recipient's drain.
Rationale below (D5). It is therefore server-side, observable, and consented-by-construction.

### Design decisions (IF / HOWEVER / THEN / BECAUSE)

- **D1 — type is a first-class payload field, never inferred from prompt text.**
  IF a bell needs to carry its illocutionary force, HOWEVER inferring it from the prompt
  string is cheaper to ship, THEN we add `:bell-type` as a first-class field on the canonical
  bell event, BECAUSE inferring-from-text is precisely the *side-channel / schema-drift*
  failure (PSR-1, `canonical-typed-event-vs-side-channel`) and it reintroduces the lossy
  mining step the mission exists to delete. Agent *declares* the type.
- **D2 — only `:query`/`:answer` reach ArSE; the other five stay ambient.**
  IF typing recovers seven performatives, HOWEVER ArSE is a Q&A store not a debate log, THEN
  only Q&A types write to ArSE (TB-4), BECAUSE over-promotion floods ArSE with coordination
  chatter and collapses the Evidence ⊇ ArSE ⊋ ambient boundary. Dialectical edges
  (assert/challenge/…) are S5, landing on the *conversation graph*, not ArSE.
- **D3 — unknown type is rejected, not coerced.**
  IF forward-compat suggests coercing unknown types to `:request`, HOWEVER silent coercion is
  how taxonomies rot, THEN unknown ⇒ 400 (TB-1), BECAUSE a rejected bell is a loud, fixable
  signal; a coerced one is invisible drift.
- **D4 — reuse ArSE's `in-reply-to`; do not build a thread store.**
  IF replies need threading, HOWEVER ArSE already threads via `in-reply-to` and the bell
  router via `:bellback-of`, THEN `:ref` *is* that same primitive (TB-5), BECAUSE three names
  for one relation is the cheapest correct design and keeps both layers in lockstep.
- **D5 — the ArSE write fires at send-time in `handle-bell`, not at drain.**
  IF the write could fire when the recipient finishes, HOWEVER the *speech act is the send*
  (asking a question is asking it, regardless of whether B ever answers — an unanswered
  `:query` is a legible gap, which is the point), THEN write on send, BECAUSE it makes the
  question durable immediately, keeps the bridge off the concurrency-sensitive drain path,
  and means an `:answer` bell's own send is what records the answer (symmetric).
- **D6 — `FUTON3C_TYPED_BELLS` is independent of `FUTON3C_BELL_ROUTER`.**
  IF typing builds on the router's threading, HOWEVER coupling the flags forces an
  all-or-nothing rollout, THEN keep them independent but let typing *read* router state, BECAUSE
  staged activation (router first, typing later) is the established hardening discipline.

### Fidelity contract (Capability Preservation Matrix)

Donor = the current bell path. This mission *extends*, so per protocol it carries a CPM.

| Capability (donor) | Disposition | Tripwire / compat assertion |
|---|---|---|
| Untyped bell dispatch | **preserve** | OFF & ON-untyped→`:request` both byte-identical; `auto_bellback_test` stays green + new OFF-path job-map test (TB-2) |
| Bell-router threading (`:bellback-of`) | **preserve** | existing `bell_router_test` + `auto_bellback_test` bell-router cases unchanged |
| Surface header | **adapt** | OFF header byte-identical; ON adds one `Type:` line (header test on/off, TB-2) |
| `bell_router/graph` shape | **adapt** | untyped fixture → identical graph; typed fixture → `:type` present + crossing guard (TB-6) |
| `mesh_qa` edge schema | **adapt** | `:type`/`:ref` added as nil on untyped edges (existing mesh tests green) |
| ArSE ask/answer endpoints | **preserve** | bridge *calls* them; no change to `peripheral/arse.clj` (its tests untouched) |
| `:query`/`:answer` → ArSE write | **add** | new capability, no donor; covered by S3 keystone gate + TB-4/5/7 |

### Sharpened slice gates

- **S1 — type on the wire.** Add `:bell-type`/`:ref` to the bell payload; thread it
  through `handle-bell` (§2756) → `create-invoke-job!` (§547/§572) → job map, mirroring
  `:bellback-of`. Untyped → `:request`; unknown → 400. Flag `FUTON3C_TYPED_BELLS` (default
  OFF). *Gates:* **TB-1** (valid coerces, unknown 400), **TB-2** (OFF job-map byte-identical),
  **TB-3** (answer-without-ref 400). Tests modelled on `auto_bellback_test`'s router cases.
- **S2 — typed graph + surface header.** `bell_router/graph` rows (§38) carry `:type`; the
  crossing test (§44–49) gains the `:answer`-with-`:ref`-resolves guard; `wrap-surface-header`
  (§2270) prints the `Type:` line; `run-invoke-job!` (§2584) threads `:type`/`:ref` into
  `thread`. *Gates:* **TB-6** (typed reply never a crossing; genuine cross still surfaces),
  header on/off (TB-2), graph untyped-fixture byte-identical.
- **S3 — `:query`/`:answer` ↔ ArSE bridge (keystone).** New `agency/arse_bridge.clj`, called
  from `handle-bell` at send-time (D5), gated. `:query` no-`:ref` → `/arse/ask`, stamp `:ref`
  with the thread-id (TB-5/7); `:query` with `:ref` → route only (no new thread); `:answer` →
  `/arse/answer` against `:ref`. *Gates:* **TB-4** (ArSE write iff Q&A), **TB-5** (ref ↔
  in-reply-to agree), **TB-7** (replay ⇒ no duplicate thread); end-to-end `:query`→`:answer`
  yields exactly one thread + one answer; OFF yields none. The at-birth, lossless population.
- **S4 — work-queue unification.** Broadcast `:query` ↔ `next-unprocessed`; directed `:query`
  ↔ assignment. *Gate:* a queued ArSE question dispatched as a directed `:query` bell, its
  answer closing the queue entry. (May spin its own excursion if it grows.)
- **S5 (deferred) — sub-typologies + SEMATCH.** Add question/answer sub-types only when
  traffic shows the need; wire the SEMATCH benchmark over the typed-bell corpus (ground-truth
  Q↔A pairs come free from `:ref`/`:bellback-of`).
- **S-expose — adoption (added by VERIFY, co-equal with S3, not optional).** A protocol no
  agent uses is dead (Joe, VERIFY 2026-06-11). Make typing the path of least resistance:
  (a) `scripts/agency_send.py` grows `--type`/`--ref` flags so the canonical send tool makes
  typing one keystroke (PSR-1's "make taxonomy extension cheap" applied to *humans/agents*);
  (b) the recipient surface header already shows `Type:` (in-band nudge, the strongest pull —
  built in S2); (c) document the contract where agents actually read it — `~/code/CLAUDE.md`
  (Claude owner), `AGENTS.md` (Codex side), `README-bells-and-whistles.md`, `README-arse.md`;
  (d) connect the existing `/ask`/`/answer` ArSE skills to typed bells so the two front-doors
  agree. *Gate:* a fresh agent, given only the docs, sends a correct `:query` bell and sees its
  answer land in ArSE — adoption demonstrated end-to-end, not assumed.

### PSR — Pattern Selection Records (DERIVE)

#### PSR-1: `peripherals/canonical-typed-event-vs-side-channel` for D1 (type lives on the wire)

- Pattern chosen: peripherals/canonical-typed-event-vs-side-channel
- Candidates: canonical-typed-event-vs-side-channel; realtime/structured-events-only;
  "infer type from prompt text" (the rejected non-pattern).
- Rationale: The decision of *where* illocutionary type lives is exactly this pattern's
  subject — it even names "bell payload metadata" as the motivating example and cites the
  2026-05-23 bell-payload-drift thread as evidence. Inferring type from prompt text is the
  pattern's named failure mode (side channel → schema-drift → consent/observability bypass).
  Adding `:bell-type`/`:ref` as first-class fields on the canonical bell event, written into
  the ledger, surfaced in the header, and read by `graph`/`mesh_qa`, *is* the pattern's THEN.
- Confidence: high — the pattern is direct, well-evidenced, and the alternative is its
  explicit antipattern.

#### PSR-2: `realtime/structured-events-only` for the invariant shape (TB-1…7)

- Pattern chosen: realtime/structured-events-only
- Candidates: structured-events-only; free-text-with-later-mining (rejected).
- Rationale: TB-1's reject-unknown-types and the fixed `:bell-type` enum are this pattern's
  "stable schema enables parsing, dedupe, audit." The typed-bell corpus is only a usable Q&A
  substrate (and SEMATCH ground-truth) if its schema is disciplined; free-text would force the
  empirical-rediscovery death-spiral the pattern warns against.
- Confidence: high.

## ARGUE

### Pattern cross-reference (futon3/library/)

- **`peripherals/canonical-typed-event-vs-side-channel`** (📜/典) — *the* governing pattern;
  applies to D1. Typing-as-first-class-field is its THEN; infer-from-text is its FAILURE-MODE.
  It revised nothing in DERIVE (DERIVE already chose its THEN) but it *hardens* the rejection
  of the cheaper inference shortcut — the design is the pattern's textbook instance. (PSR-1.)
- **`realtime/structured-events-only`** (🌀/天) — applies to the enum + TB-1/TB-3 door checks.
  Bells become structured typed events with a documented schema, not free text. (PSR-2.)
- **`orchestration/state-in-substrate-deltas-in-messages`** — applies to D5. The ArSE thread
  is durable *state in the substrate*; the bell is the *delta/message*. Writing the question to
  ArSE at send-time (not parking it in the bell) is this pattern: messages carry deltas, the
  substrate holds state. Confirms send-time write over drain-time.
- **`social/ARGUMENT`** + the futon `aif` family — the theoretical bridge to IAT; the
  conversation graph as a dialectical structure (assert/challenge/agree) is where S5's
  dialectical edges land. Flagged for S5, not v1.
- **`feedback_mechanical_vs_semantic_consent`** (agent memory) — applies to the consent
  trade-off (below): a typed-`:query` write is *consented-by-construction*, the mechanical end
  of the axis, so it needs a flag (rollback) but not a per-write semantic gate.

### Theoretical coherence

The design serves the IDENTIFY anchoring (IATC, Corneli et al. 2017) without drift: `:bell-type`
**is** IAT's illocutionary anchoring of a locution; the conversation graph **is** IATC's
dialogue graph; `:query`/`:answer` → ArSE **is** the paper's thesis that Q&A is a
machine-reasonable knowledge form, now generated live rather than mined from frozen traces.
The one place the theory *sharpened* under DERIVE: the paper treats all performatives uniformly,
but D2 splits them — Q&A types are substrate-bearing (ArSE), dialectical types are
graph-bearing (S5). That is not a departure from IATC; it is IATC's content/inference layering
(content nodes vs reasoning tactics) realised as a dispatch rule.

### Trade-off summary (what we give up, and why)

- **Taxonomy now, not full IATC.** We ship the IAT-seven and defer the question/answer
  sub-typologies (Martin–Pease, Tausczik–Kittur) to S5. Give up: expressive granularity.
  Gain: we honour the paper's own scaling caution (relation structure is hard, often
  hand-coded) and let live traffic earn each sub-type. Reversible — sub-types are additive.
- **Reject unknown types (D3).** Give up: forgiving forward-compat. Gain: no silent drift; a
  bad type is a loud 400, not invisible rot. Aligned with PSR-1's institution-not-commons framing.
- **Q&A-only ArSE writes (D2).** Give up: a complete dialogue archive in ArSE. Gain: ArSE
  stays a Q&A store, not a chatter dump; the three-layer boundary (Evidence ⊇ ArSE ⊋ ambient)
  stays crisp. The non-Q&A slice remains in the ambient log, mineable later (sorry-mining shape).
- **Send-time ArSE write (D5).** Give up: coupling the write to a *successful* answer. Gain:
  an unanswered `:query` is a durable legible gap (the whole point), and the bridge stays off
  the concurrency-sensitive drain path.
- **Write-amplification / consent.** A `:query` now causes an ArSE write. We accept this
  *because* the agent declared `:query` (consent-by-construction), and we gate the whole
  capability behind `FUTON3C_TYPED_BELLS` so it is rollback-able until validated live.

### Generalization notes

The move generalises beyond bells: **any untyped coordination channel gains a semantic graph
+ a substrate feed by anchoring an illocutionary type at send-time and routing the
substrate-bearing types to their store.** IRC lines, forum posts, and WS frames are the same
shape — an untyped carrier that could declare its speech-act. The binding contract is exactly
two fields (`:type`, `:ref`) + one dispatch rule (type → store). To apply elsewhere, swap ArSE
for that channel's substrate (forum → proof-tree, IRC → ?). The pattern this would crystallise
into: *typed-channel-feeds-its-substrate*.

### Plain-language argument (no jargon)

Right now when one agent messages another, the system records *that* a message was sent but
not *what kind* — a question, an answer, a heads-up. So a perfectly good store of questions and
answers (ArSE) sits empty while real questions fly past in the message traffic and vanish. The
fix is small: let an agent tag a message with what it is. Tag it "question" and the system files
it as a question others can answer; tag the reply "answer" and it lands in the right place
automatically — no extra step. The same tag also tells two agents who happened to message each
other at once whether the second message was a fresh question or just a reply, which untangles a
confusion they hit today. One little label turns throwaway chatter into a growing, searchable
record — and it's an idea from a 2017 paper we can finally run live instead of just study.

## Review checkpoint — Claude owner, independent gate (2026-06-11)

Codex implemented + committed (`b4ed15f`, "Instantiate typed bells"); reviewed here under the
author≠reviewer protocol. **Verdict: PASS, merge-as-is (flag OFF). No defects; one
watch-item for flag-ON.**

What I checked (read the diff + re-ran the verify, not a rubber stamp):
- **Re-ran focused tests** → reproduced exactly **31 tests / 101 assertions / 0 failures**.
- **clj-kondo** on the 3 changed `.clj` files → **errors: 0** (15 warnings, all pre-existing
  `http.clj` — matches the report); test namespaces compile + load clean.
- **Read the implementation against TB-1…7 / D1–D6** (not just the green bar):
  - TB-1/D3 — unknown type → `400 invalid-bell-type`; `normalize-bell-type` maps nil/blank →
    `:request`, so **untyped-while-ON is not rejected** (backward-compat holds).
  - TB-2 — `typed?` gates type/ref extraction, both 400 guards, the bridge, the `Type:` header,
    and the job fields; OFF path unchanged (+ explicit `typed-bells-off-ignores-type-and-ref`).
  - TB-3 — answer-without-ref → `400 answer-ref-required` (+ test).
  - TB-4 — `maybe-typed-bell-arse-bridge!` `case` writes only on `:query`/`:answer`; **default
    branch `{:ok true :ref ref}` is inert** (no write, no throw) for the other seven; a
    `:query` *with* `:ref` routes without writing. Safety + liveness both hold.
  - TB-5 — `:query` stamps the job `:ref` with the new thread-id; `:answer` posts via
    `arse-answer!` against `:ref` (+ tests).
  - TB-6 — `bell_router/graph` `reply-of` treats an `:answer` whose `:ref` ∈ bell-ids as a
    reply, so the referent leaves `:open` and cannot appear in `:crossings` (real code + test).
  - TB-7 — replay with the same job-id returns `202 reused` **before** the bridge runs → no
    second ArSE thread (+ explicit replay test).
  - D5 — the ArSE write fires send-time in `handle-bell`, before `create-invoke-job!`.
- **Accepted deviation from DERIVE:** no separate `agency/arse_bridge.clj`; the bridge is
  inline (`maybe-typed-bell-arse-bridge!`) and Codex refactored the ArSE HTTP handlers into
  reusable `arse-ask!`/`arse-answer!` so the bridge calls them **in-process** (cleaner than an
  HTTP self-call; MAP §8 allowed a drain-finalizer branch as the alternative).

**Watch-item for when the flag flips ON (not a merge blocker):** the synchronous
`entities.json` read+write on the `POST /bell` request thread. Carried into DOCUMENT as a
deferred-item ticket → **`holes/tickets/T-typed-bell-arse-write-async.md`** (full context +
fix direction there).

## VERIFY (2026-06-11)

### Logic model — TB-1…7 checked before any protocol/bridge code

Per `logic-model-before-code`, the invariants were encoded as a core.logic/pldb model over
an abstract bell-trace and run *before* writing the transport/ArSE code:

- **Model:** `src/futon3c/logic/typed_bells_invariants.clj` (house idiom, sibling of
  `logic/outing_invariants.clj` + `agency/mesh_qa`). A trace is a vector of bell-records;
  `build-db` asserts them; one query per invariant returns violating rows.
- **Test/gate:** `test/futon3c/logic/typed_bells_invariants_test.clj`.
- **Result (auditable):** `clojure -M:test -n futon3c.logic.typed-bells-invariants-test` →
  **3 tests, 25 assertions, 0 failures.** The conforming witness trace (`q1` query→`a1`
  answer→`r1` request) yields **zero** violations across all 7 categories; each of the 7
  adversarial traces is **caught by its own category** (`:verified? true`, `n-invariants 7`).
  - TB-1 unknown `:type :frobnicate` accepted → caught. TB-2 `:flag-on? false` with leaked
    effects → caught. TB-3 `:answer` without `:ref` → caught. TB-4 `:request` bell with an
    ArSE write → caught. TB-5 answer write with `in-reply-to ≠ ref` → caught. TB-6 resolved
    asked-bell left `:open?` → caught. TB-7 a bell creating 2 threads → caught.
- **What this proves / doesn't:** it validates the *design* (the invariants are mutually
  consistent, a conforming implementation has a witness, and each failure mode is detectable).
  It does **not** validate the implementation — that is INSTANTIATE, where these same checks
  become live mesh-QA probes (cf. `mesh_qa` MQ-*).

### Decision log (VERIFY-time revisions to DERIVE)

- **TB-4 refined into safety + liveness directions.** Encoding the "ArSE write iff Q&A"
  invariant surfaced the `:query`-with-`:ref` exception (D-S3: a query that *routes* an
  existing thread does not create one). So TB-4 is really **(4a) safety:** an ArSE write ⇒
  Q&A type (no over-promotion — the load-bearing boundary); **(4b) liveness:** a
  `:query`-no-`:ref` *or* an `:answer` ⇒ an ArSE write (no silent under-population). Both are
  in the model. No design change, just a sharper invariant statement.
- **S-expose promoted to a required slice.** See completion criteria C-adopt below; this is
  the one VERIFY finding that *adds scope* to DERIVE.

### Completion-criteria pre-check

Each criterion traced to the design element that satisfies it (✓ = design covers it; ⚠ =
covered only once a slice lands; the flag keeps all of it dark until then):

| # | Criterion | Satisfied by | Status |
|---|---|---|---|
| C1 | A bell can declare a type; untyped behaves as today | S1 + TB-1/TB-2 | ✓ design + model |
| C2 | OFF path byte-for-byte unchanged | TB-2 + CPM tripwires | ✓ model (impl proof at INSTANTIATE) |
| C3 | `:query`/`:answer` populate ArSE losslessly, at birth | S3 + TB-4/5/7 | ⚠ S3 |
| C4 | Typed replies stop being reported as crossings | S2 + TB-6 | ✓ model (impl at S2) |
| C5 | Conversation graph is type-aware | S2 | ⚠ S2 |
| **C-adopt** | **A fresh agent, from the docs alone, sends a correct typed bell and sees it in ArSE** | **S-expose** | **⚠ S-expose — the riskiest criterion** |

**C-adopt is the criterion most likely to fail silently**, because nothing about a correct
protocol forces its use. Joe's VERIFY directive makes it co-equal with the keystone: the
exposure surfaces (`~/code/CLAUDE.md`, `AGENTS.md`, `README-bells-and-whistles.md`,
`README-arse.md`, the `scripts/agency_send.py --type/--ref` ergonomic, the in-band `Type:`
header, and the `/ask`//`/answer` skills) are INSTANTIATE deliverables, not documentation
afterthoughts. The adoption gate is behavioural (a fresh agent succeeds from docs alone), not
"the docs were edited."

### VERIFY exit

Design checked against the available structural constraints (the 7 invariants hold for a
conforming witness and catch their adversarial traces); the one risk static analysis can't
settle — adoption — is named, assigned a behavioural gate (C-adopt), and given a required
slice (S-expose). DERIVE revisions recorded above. **Ready for INSTANTIATE** (S1 → S2 → S3,
S-expose interleaved so each capability ships with its exposure).

## INSTANTIATE (2026-06-11)

### S1 — type on the wire

`transport/http.clj` now gates typed bells behind `typed-bells-enabled?`
(`FUTON3C_TYPED_BELLS`, default OFF). With the flag ON, `handle-bell` accepts
`type`/`bell-type` and `ref`/`bell-ref`, normalizes absent type to `:request`,
rejects unknown types, and rejects `:answer` without `ref`. `create-invoke-job!`
stores `:bell-type` and `:ref` only when the typed path passes values, preserving
the OFF job-map tripwire.

### S2 — typed graph + recipient header

`wrap-surface-header` emits a `Type:` line when typed bells are enabled and the
job carries a type. `run-invoke-job!` threads `:bell-type`/`:ref` from the
canonical job ledger into that header. `agency/bell_router.clj` carries
`:type`/`:ref` on graph rows and treats an `:answer` whose `ref` names a prior
bell job as resolving that edge, so false A↔B crossings collapse while mutual
typed queries still surface. `agency/mesh_qa.clj` projects `:type`/`:ref` on
mesh edges.

### S3 — query/answer to ArSE at birth

The existing ArSE ask/answer endpoint write paths were factored into reusable
`arse-ask!` and `arse-answer!` helpers. `handle-bell` calls them at send-time:
fresh `:query` creates one ArSE thread and stamps the returned thread id into
the job's `:ref`; routed `:query` with an existing `ref` creates no new thread;
`:answer` writes to the referenced ArSE thread. Replaying a typed bell with the
same explicit job id reuses the existing job and does not create a second ArSE
thread.

### S-expose — adoption surface

`scripts/agency_send.py` now accepts `--type` and `--ref`. The protocol is
documented in `README-bells-and-whistles.md`, `README-arse.md`, `CLAUDE.md`,
and `AGENTS.md`, with shell examples using the canonical send tool.

### Gates

- `emacs --batch -l /home/joe/code/futon4/dev/check-parens.el ...` — clean.
- `clj-kondo --fail-level error --lint ...` — 0 errors (existing `http.clj`
  warnings remain visible).
- `clojure -M:test -n futon3c.logic.typed-bells-invariants-test -n
  futon3c.agency.bell-router-test -n futon3c.transport.auto-bellback-test` —
  31 tests, 101 assertions, 0 failures, 0 errors.

## DOCUMENT (2026-06-11)

The "how to use it" surfaces shipped *with* the code (S-expose, by design — adoption is the
C-adopt criterion, not an afterthought). DOCUMENT's job here is to (a) confirm those surfaces
are in place and (b) capture the deferred-item a future agent must be able to pick up.

### Exposure surfaces in place (the C-adopt documentation half)

| Surface | What it carries | State |
|---|---|---|
| `~/code/futon3c/CLAUDE.md` | "Typed Bell Contract" — types, query/answer semantics, shell example | committed (`b4ed15f`) |
| `AGENTS.md` | "M-typed-bells protocol available behind flag" — Codex-side how-to | working-tree |
| `README-bells-and-whistles.md` | typed-bell section in the bells/whistles doc | committed |
| `README-arse.md` | typed-bell → ArSE population path | committed |
| `scripts/agency_send.py` | `--type`/`--ref` flags (the one-keystroke ergonomic) | committed |
| in-band `Type:` surface header | the strongest pull — recipients *see* the type live | committed |

These satisfy the *documentation* half of **C-adopt**. The *behavioural* half — a fresh agent
sending a correct `:query` from the docs alone and seeing it land in ArSE — is a **live demo
still owed**, and only runnable once `FUTON3C_TYPED_BELLS` is ON on the JVM (Joe's call).

### Deferred-item ticket (carried from the review watch-item)

- **`holes/tickets/T-typed-bell-arse-write-async.md`** (WATCH) — the synchronous
  `entities.json` read+write on the `POST /bell` thread. Not a defect (flag-OFF + Q&A-only +
  no new I/O class), but a thing to *watch for* once the flag is live and ArSE fills: move the
  ArSE write off the request thread (async enqueue) while preserving TB-5/TB-7. Full context +
  fix direction in the ticket, with enough for a new agent to pick it up cold.

### Still-deferred design scope (not tickets yet — future missions/slices)

- **S4 — work-queue unification** (broadcast `:query` ↔ ArSE `next-unprocessed`; directed
  `:query` ↔ assignment). Designed in DERIVE, not built. May spin its own excursion.
- **S5 — sub-typologies + SEMATCH** (Martin–Pease / Tausczik–Kittur sub-types; the Q↔A blind
  matching benchmark). Deferred until live traffic earns the sub-types (paper's own caution).

### DOCUMENT exit

The capability is discoverable from the agent-facing docs without reading this mission file;
the one deferred risk has a pickup-ready ticket; remaining design scope (S4/S5) is named with
enough context to reopen. **Mission lifecycle complete through DOCUMENT** — the only open
threads are (1) the live C-adopt demo on flag-ON and (2) the named deferred scope, both of
which are Joe's-call activations rather than unfinished work.

## Live activation checkpoint (2026-06-11)

Flag activated on the laptop JVM (`scripts/dev-laptop-env` → `FUTON3C_TYPED_BELLS=true`).

- **C-adopt validated in the wild.** ArSE store carries **2 typed-bell QAPairs**
  (`tags:["typed-bell"]`, which only the bridge sets), both from **claude-3**, both
  **complete question→answer round-trips** (its FutonZero ground-control "cycle closed →
  source-checked confirmation" exchange). A real `:query` minted an ArSE thread and a real
  `:answer` closed it, lossless, with no `!ask` — the behavioural half of C-adopt, met live.
  Volume is low (matches "agents aren't using it all the time yet").
- **Bug found + fixed (small blast radius window).** `arse-ask!` hard-coded `:synthetic true`
  (inherited from the synthetic-QA-seed handler it was refactored out of), so genuine
  agent/human questions were mislabeled as generated seed data — latent, but bites S4 (the
  work-queue is built around *synthetic* QA). Fixed both ends while only 2 entries were
  affected: (a) code — `arse-ask!` now writes `:synthetic false` (the real-ask path; provenance
  is the `typed-bell` tag), reloaded **live via Drawbridge** (`load-file`, no JVM restart);
  (b) data — the 2 mislabeled entries corrected to `:synthetic false` (24 genuine seed entries
  left untouched), verified through the JVM's own `arse-load-entities`. clj-kondo errors: 0.
  Adjacent check: `:unanswered` is *not* mislabeled (`arse-answer!` `dissoc`s it correctly).

## Relations

- `src/futon3c/logic/typed_bells_invariants.clj` + its test — the VERIFY logic model (TB-1…7).
- `holes/tickets/T-typed-bell-arse-write-async.md` — DOCUMENT deferred-item (the flag-ON
  synchronous-ArSE-write watch-item).
- `holes/excursions/E-typed-bells.md` — the design sketch + full prior-art reading this
  mission promotes.
- `holes/excursions/E-crossed-bells.md` / `src/futon3c/agency/bell_router.clj` — parent;
  structural graph this types.
- `README-arse.md` / `peripheral/arse.clj` / `agents/arse_work_queue.clj` — the substrate.
- `holes/technotes/TN-arse-tickling-handoff.md` — existing ArSE-handoff note (read before S4).
- Surface contracts (futon3c CLAUDE.md) — a bell-type is a *richer surface contract*
  (accurate environment info: illocutionary force), not a capability restriction.
- Sorry-mining chain (memory) — the ambient→promotion pattern; typing makes the Q&A slice
  first-class instead of mined, the same way typed sorrys beat un-typed wishlists.
