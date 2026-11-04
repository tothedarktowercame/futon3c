# M-formal-agency: receipts, common notions, and the conditions of agency

**Status:** OPEN — IDENTIFY (2026-11-04, Joe + codex-7)

**One-line:** State and eventually check the conditions under which a network of
participants can be said to have acted together: sends are distinguished from
receipts, feedback reaches every affected participant, and shared operational
understanding increases the participants' power to act.

This mission begins at IDENTIFY. The gap is already concrete, so a HEAD
bootstrap would add ceremony rather than preserve missing operator context.

## Acceptance checklist

- [ ] A trace vocabulary distinguishes dispatch, transport acceptance,
  participant receipt, acknowledgement, work observation, reply, and feedback
  closure without inferring a later event from an earlier one.
- [ ] The participants and required feedback threshold of an episode can be
  stated, and a check identifies exactly which participant or witness prevents
  closure.
- [ ] At least one live Agency path emits enough correlated evidence to decide
  the above predicates without reconstructing intent from prose or logs.
- [ ] A formal model rejects the known counterexamples: an unconsumed inbox
  delivery, a reply addressed to a detached seat, and a transport-accepted send
  that never reaches its affected consumer.
- [ ] One positive worked episode demonstrates participant-complete feedback
  and binds the verdict to the evidence used to derive it.
- [ ] The relationship between common notions and agency is stated precisely
  enough to test, while claims stronger than the evidence remain explicit
  holes rather than definitions disguised as results.

## 1. IDENTIFY

### 1.1 The gap

Agency has an execution system, but not yet a formal account of **agency**.
The stack records bells, jobs, sessions, tool activity, replies, acknowledgements,
parks and wakes. These records answer many local questions, but no common model
states when their composition warrants a higher-order claim such as:

- the recipient received what the sender sent;
- the result reached everyone it affects;
- the participants now share the operational understanding needed to continue;
- the collective, rather than only one process within it, completed an action;
- an encounter increased the participants' capacity to act together.

This is not a speculative absence. On 2026-11-04, `wm-full-loop` had 528 durable
inbox files and no consumer, while Agency health reported 293 unconsumed jobs.
Registration had prevented message loss, but nothing owned receipt and
acknowledgement; 235 filesystem records had also outlived the job-ledger epoch
that health treated as authoritative. The system could prove a send and durable
delivery while the feedback loop remained open.

The operator-facing version appeared the same day. A resumed Claude displayed
"Claude is thinking" while Agency was actually reconstructing context through
slow store reads, and later running tools. The surface asserted a state broader
than its evidence. The proposed turnline begins as an observability repair, but
its event sequence is also the smallest useful substrate for this mission: a
claim should be no stronger than the witnessed transition supporting it.

### 1.2 Why this is not "formalise it because we can"

Alinery supplies a strong neighboring implementation vocabulary: tasks,
playbooks, steps, sessions, artifacts, occurrences, dependencies, human gates,
and distinctions among permission, acceptance, shutdown and successor launch.
Its runtime records producer execution and ancestry. That is valuable prior art
for execution provenance.

The remaining gap is not another workflow engine. Alinery treats a step body as
an opaque prompt, does not claim to learn, and does not provide a criterion that
feedback has reached every affected participant. An accepted artifact can
satisfy a workflow dependency without establishing receipt by an owner,
acknowledgement by a reviewer, or changed capacity in the collective. FUTON
already needs those stronger claims for bells, reviews, operator rulings,
pattern uptake and War Machine handoffs. Formal agency fills an operational gap
that Alinery leaves open.

The practical payoff is refusal and diagnosis. Given a purportedly completed
episode, the system should be able to say not merely `false`, but: *the send was
accepted, the author's reply exists, the affected consumer has no receipt, and
therefore participant-complete feedback is not established*. This is useful
before any deeper philosophical result.

### 1.3 Candidate event and claim layers

IDENTIFY records this separation without choosing the final formalism.

**Observed events** are append-only facts with a source and correlation
identity. The initial vocabulary is:

```text
dispatch-issued
transport-accepted
participant-received
participant-acknowledged
work-observed
reply-produced
reply-received
episode-closed
```

The event names are not synonyms and do not imply their successors. In
particular, a send is not a receipt; a receipt is not acknowledgement; one
participant's acknowledgement is not feedback closure for an episode with
several affected participants.

**Derived claims** state what the evidence warrants. Candidate shapes include:

- `receipt-of(send, participant, receipt)`;
- `acknowledges(ack, receipt)`;
- `requires-feedback-from(episode, participant, threshold)`;
- `feedback-complete(episode)` iff every required participant has a witness at
  or above the episode's declared threshold;
- `shared-notion(episode, notion, participants)` when the same composable
  operational relation is adequately available to those participants;
- `agency-witness(episode, participants, result)` when their differentiated
  contributions compose to the result and the required feedback is closed.

These are candidates for MAP and DERIVE, not settled definitions. In
particular, `episode-closed` must not become a self-certifying event that makes
`feedback-complete` true merely because a producer emitted it.

### 1.4 Spinoza: common notions as a route to agency

The Spinozan direction is load-bearing but not yet a theorem. Existing FUTON
work treats a pattern as a candidate common notion: an affect or recurrent
encounter converted into an adequate, composable idea that increases capacity
to act. Common notions are not private labels and do not depend on who authored
them; they identify relations genuinely shared by bodies and therefore support
adequate composition.

Formal agency asks what follows when that account is applied to participants in
an episode. A collective is not an agent merely because messages crossed or a
workflow advanced. A stronger candidate is that participants possess enough
adequate common structure to compose their actions, and that feedback makes the
result available to the participants whose subsequent action depends on it.
On this reading, participant-complete feedback is not clerical notification: it
is part of how a transient coordination becomes an increase in shared power to
act.

This section deliberately stops short of identifying common notions with
consensus, identical internal representations, or message receipt. Those would
be glib collapses. MAP must distinguish at least:

1. common access to the same record;
2. compatible operational understanding;
3. demonstrated ability to compose actions using it; and
4. an observed increase in individual or collective capacity.

The mission may find that only the first three are mechanically decidable. If
so, the fourth remains a philosophical and empirical frontier rather than being
smuggled into a Boolean predicate.

### 1.5 Formal directions to survey, not commitments

- **Natural deduction / proof objects:** events introduce justified facts;
  receipts discharge obligations created by sends; a closure claim is a
  derivation whose premises name its evidence.
- **BV and deep inference:** dispatch, concurrency, interaction and feedback
  may be better represented by inference inside a structure than by a single
  sequential proof tree. This is a candidate for episodes with several sends
  and crossed replies, not a chosen implementation language.
- **Event structures, session types and protocol logics:** neighboring accounts
  may already distinguish causality, participant-local observation and global
  progress. MAP must look before inventing a bespoke calculus.
- **Institutional semantics:** "feedback reaches every participant" is a rule
  over named roles, information, control and violations, not just a transport
  property. The episode declares which participants and which receipt threshold
  constitute closure.
- **Alinery:** a concrete comparison for steps, occurrences, artifacts,
  provenance, human gates and lifecycle distinctions; also a boundary case
  showing why workflow completion alone is weaker than formal agency.

### 1.6 Scope

**In scope for the first mission arc**

- the event/claim distinction and correlation identities;
- named participants, roles and per-episode feedback obligations;
- send, receipt, acknowledgement and closure semantics;
- evidence-bound verdicts and counterexample-producing diagnostics;
- one Agency/turnline instrumentation path and one worked multi-participant
  episode;
- comparison with Alinery and survey of candidate formalisms;
- a bounded account of common notions as composable operational understanding.

**Explicitly deferred**

- proving a general philosophical definition of agency;
- reducing subjective understanding to transport telemetry;
- requiring identical beliefs or consensus among participants;
- formalising all of Spinoza, the War Machine, or the pattern language here;
- choosing BV, Lean, a proof assistant or a protocol language before MAP;
- redesigning the whole Agency transport while specifying its semantics;
- treating the turnline UI itself as the formal model.

### 1.7 Relationship to the formal mission series

- **`M-formal-war-machine`** specifies the control loop's stages, transitions,
  interfaces and qualification chain. Formal Agency asks when the participants
  enacting those transitions have actually completed their handoffs and closed
  feedback.
- **`M-formal-patterns`** treats patterns and pattern languages as composable
  structures whose coherence can be checked. Formal Agency asks how such
  candidate common notions become available in coordinated action and what
  evidence supports uptake rather than mere exposure.
- **Turnline / Cooked line work** supplies a small, human-readable projection
  of observed turn events. It is an instrumentation client of the event model,
  not the authority for it.
- **Agency delivery lifecycle and Inbox Zero** supply negative cases where
  durable production, consumption, acknowledgement and health disagree.
- **The apex thesis requirement "feedback reaches every participant"** and its
  Way of Working "a send is not a receipt" provide the institutional claim this
  mission must make checkable.

Together the three formal missions concern structure at different loci:
patterns formalise reusable common structure; the War Machine formalises the
control loop; Formal Agency formalises the witnessed relations by which
participants compose and can justifiably be said to have acted.

### 1.8 MAP questions

- [ ] **Q1 — Evidence inventory:** Which current Agency, bell, park/wake,
  invoke-stream, turnline and evidence records already distinguish each event
  in §1.3, and where are distinctions currently collapsed?
- [ ] **Q2 — Counterexample corpus:** Assemble at least five real episodes,
  including the `wm-full-loop` orphan inbox and the silent Claude resume, and
  state which claim each episode refutes.
- [ ] **Q3 — Participant declaration:** Where are author, reviewer, owner,
  affected consumer, dispatcher and monitor named today, and can an episode's
  required participant set be recovered without inference from prose?
- [ ] **Q4 — Neighboring formalisms:** Compare natural deduction, BV/deep
  inference, event structures, session types and protocol/institutional logics
  against the counterexample corpus; report fit rather than selecting by taste.
- [ ] **Q5 — Alinery boundary:** Run the same episode vocabulary over one real
  Alinery playbook execution and state exactly which claims its provenance can
  and cannot decide.
- [ ] **Q6 — Common-notion ladder:** Give observable criteria separating common
  record, compatible understanding, composable action and increased capacity;
  identify which levels are presently testable.
- [ ] **Q7 — Authority:** Identify the authoritative record for each event and
  how reconciliation works when the job ledger, durable inbox and UI disagree.

### 1.9 IDENTIFY exit criterion

IDENTIFY exits when Joe agrees that the gap is the missing evidence-bound
semantics of participant composition—not merely missing transport states or an
opportunity to apply logic—and that the first arc is correctly bounded to
receipt, acknowledgement, feedback closure and one cautious common-notions
bridge. MAP may begin immediately on the unchecked questions; agreement is the
phase exit, not a prerequisite for the mission to exist.

## Sources at charter

- `holes/excursions/apex-thesis-narration-2026-11-02.md`, especially
  "Therefore 3: feedback reaches every participant"
- `futon4/holes/delivery-lifecycle.md` and `.v1.md`
- `futon2/holes/missions/M-formal-war-machine.md`
- `futon5/holes/M-formal-patterns.md`
- `futon0/holes/missions/landscape/deep-alinery.md`
- `futon4/holes/missions/M-writing-ethics.md`, for patterns as candidate common
  notions and the discipline of not confusing authorship with structure
- Agency implementation under `src/futon3c/agency/` and
  `src/futon3c/transport/http.clj`
- Emacs turn-stream projections in `emacs/agent-chat.el`, `emacs/claude-repl.el`
  and `emacs/codex-repl.el`

**Owner:** Joe for the philosophical and institutional claims; implementation
owners to be assigned after MAP locates the smallest formal and instrumentation
slices.

**Repos:** `futon3c` is the mission home and Agency evidence source; `futon2`,
`futon4`, `futon5` and `futon0` contain sibling missions and source material.
