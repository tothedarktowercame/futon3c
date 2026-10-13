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

The Prover drives a stepwise dialogue and submits every inference to the
Critic, who either discharges it or returns one localized objection for repair.
The Teacher may consult all three frozen objects, but intervenes only with a
bounded mathematical clarification when a repeated obstacle concerns Lean
machinery. The Scribe is outside ordinary turn-taking and writes only at an
explicit phase checkpoint or after the transcript is frozen. The other roles
receive the problem and progressively disclosed mathematical claims, not the
reference Lean proof. Scope mining happens only after the transcript is
frozen.

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

## First formal and presentation increments (2026-10-13)

`scripts/lean_proof_regions.py` now generates the pilot's versioned
`lean-proof-regions.json` directly from the frozen Lean bytes.  It records
declaration ranges and in-file uses, tactic branches, named local steps,
parents and source positions.  The artifact declares its boundary: this is a
syntactic manifest, not reconstructed tactic states or semantic reachability.

The existing Matrix-backed FUTON room client now renders explicit `$…$`,
`$$…$$`, `\(…\)` and `\[…\]` delimiters using locally packaged KaTeX.  It
continues to read the original Matrix event bodies; KaTeX's MathML annotation
retains the source TeX.  No alternate transcript or server-side formatted
body is introduced.

The built client was deployed to `https://zone.hyperreal.enterprises/xiang-widget/`.
The live JavaScript and KaTeX CSS were fetched through Caddy and matched the
tested build byte-for-byte (SHA-256 `3a8e5a17…54be` and
`19095127…2e75`, respectively).

`@fucodex` registered that deployment in `FUTON 象 Demo` as the Matrix widget
`futon-proof-dialogue-tex`, named “Proof dialogue · TeX”; the state write event
is `$HYfNbs_Q0M27L5mubj2wbtwov0hnvjKvku5Fz0zcu1Y`.  A power-0 proof-role
account independently read the resulting `im.vector.modular.widgets` state,
and an unauthenticated HTTP probe found both the expected page title and local
KaTeX stylesheet.  This verifies registration and serving, not interactive
browser rendering inside Element.

`scripts/lean_formal_capture.py` adds the semantic layer without replacing the
source manifest.  It opens the frozen proof in Lean's language server and asks
for the elaborated goal at each of the 24 recorded region positions.  It also
runs apm-lean's `#declaration_reachability` against the compiled theorem
module.  The frozen `lean-formal-capture.json` has no diagnostics, 24 goal
captures, a 15,444-constant transitive closure, all 9 local declarations
reachable, and no local orphan declarations.

`scripts/lean_goal_features.py` normalizes those raw goals into the formal half
of the future alignment relation: case names, typed local hypotheses, targets,
references to reachable local declarations, target identifiers and hashes of
the exact Lean goal text.  The generated `lean-goal-features.json` contains 24
regions and 25 goals (the first theorem branch exposes both directions).  Its
generator reads no dialogue text or scope labels, so this half can be frozen
before conversational labelling begins.

`scripts/proof_dialogue_label_packet.py` now supplies the converse isolation
boundary.  It verifies the canonical hash chain and emits exact message bytes,
ancestry and stable anonymous speakers while excluding the Lean source, formal
captures, goal features, authored move kinds, role names, source references
and Matrix event IDs.  The pilot packet is bound to trace head
`3e29e5cf…3a97b` and has packet hash `78f68d92…0653`.  Its two messages validate
the ingest contract, but are not enough dialogue to justify commissioning or
interpreting a scope comparison yet.
