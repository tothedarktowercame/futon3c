# E-proof-dialogues: Lakatos-style traces for held-out Lean proofs

**Status:** ACTIVE — four Matrix roles are live and the first dialogue has
started in `FUTON 象 Demo`.

## Pilot

The held-out proof is APM `t94A01`, the weighted metric on the countable cube.
It has six named supporting lemmas, a two-branch final theorem, and no `sorry`.
The pilot freezes apm-lean commit `9e8b8ec4580ff1fa85e763d3834ca8adeaeee84e`:

- problem: `problems/t94A01/problem.md`, SHA-256 `fe25b6c8…b41c8`;
- informal solution: `problems/t94A01/informal-solution.md`, SHA-256 `8beca552…6dee0d9`;
- Lean proof: `problems/t94A01/lean/Main.lean`, SHA-256 `c380fee0…dcc6b`.

Teacher, student, critic and scribe will conduct a linear dialogue. The teacher
may consult all three frozen objects. The other roles receive the problem and
progressively disclosed mathematical claims, not the reference Lean proof.
Scope mining happens only after the transcript is frozen.

## Trace contract

`scripts/proof_dialogue_trace.py` owns an append-only hash-chained JSONL log.
Turns carry stable move/parent IDs, roles, move kinds, exact body bytes and
source pointers. Matrix delivery receipts are later records, so attaching an
event ID never edits the originating turn. The verifier rejects mutation,
duplicate IDs, missing parents, delivery before a turn, and broken chains.

Matrix is a view, not the record of truth. The configured account
`@fumarimo:matrix.paragogy.net` was verified joined to room
`!WXULllaFhTOchbIrmo:matrix.paragogy.net`, whose live name is `FUTON 象 Demo`.
It is a notebook agent, not one of the four proof roles, and will not be used to
impersonate them.

## Live receipt (2026-10-13)

Public registration is disabled on `matrix.paragogy.net`. The four non-admin
accounts were therefore provisioned with Synapse's registration tool on the
homeserver, invited by the existing room admin `@fucodex`, and joined to the
unencrypted invite-only room. Tokens remain in mode-600 files and are not part
of this repository.

The live bridge maps Teacher→claude-15, Student→codex-76,
Critic→claude-16, and Scribe→zai-5. Transport tests pass 23/23; trace tests pass
2/2. Teacher move `m0001` and Student reply `m0002` are visible in Element.
The canonical `trace.jsonl` stores the turn bytes, their parent relation and
Matrix event IDs. The first reply exposed a protocol defect—“Next role:
Teacher” is not a routable mention—so briefs now require a literal final
`@proof-…` mention before continuing the chain.

## Tool readiness and build order

The required capabilities mostly exist, but they are not yet joined into an
analysis pipeline.  In particular, a Matrix transcript and a Lean file are
not by themselves evidence that conversational scopes correspond to proof
structure.

| Stage | Reusable substrate | Missing pilot artifact |
|---|---|---|
| Capture | Hash-chained moves and immutable Matrix delivery receipts | Automatic Matrix-event-to-trace ingestion; the first two receipts were transcribed into the canonical trace |
| Mathematical NLP | futon6 LaTeX S-expression/AST/term extraction, theorem extraction, performative analysis and scope-span tooling | An adapter from `trace.jsonl` to a blinded M-象 scope-labelling packet that preserves move IDs and exact source bytes |
| Formal analysis | apm-lean `Tools/DeclarationReachability.lean` and declaration indexes | A proof-region manifest: theorem/branch/subgoal boundaries, local hypotheses, named declaration uses and source positions |
| Labelling | The M-象 labelling protocol and accumulated conversational labels | A frozen assignment to labellers who did not author the dialogue, plus an agreement measurement |
| Alignment and evaluation | futon6 proof-frame alignment templates and provenance tools | An implemented scope-to-proof-region comparator, with shuffled-turn, stripped-standing and informal-proof-only controls |
| Presentation | Element room transport and the room-authorized `xiang-chat` web surface | A room-scoped KaTeX view; ordinary Matrix formatted messages do not currently render the TeX in the dialogue |

The next critical path is therefore: (1) automatic transcript ingest, (2) a
Lean proof-region manifest for `t94A01`, (3) a blinded labelling packet, and
(4) the alignment comparator and negative controls.  These are the minimum
pieces needed to test the claim rather than illustrate it.

The LaTeX view can proceed independently.  It should be a read-only,
room-scoped widget backed by the existing `xiang-chat` membership check,
rendering message bodies with KaTeX while retaining the literal source.  It
must remain a projection of Matrix events, not a second transcript store.
