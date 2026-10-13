# E-proof-dialogues: Lakatos-style traces for held-out Lean proofs

**Status:** ACTIVE — transport and canonical-ledger slice implemented; agent
accounts and the first dialogue remain pending.

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

## Next gate

Provision and invite four unencrypted Matrix bot accounts, with token files
named for their localparts. Until those credentials exist, run the dialogue
locally and validate the canonical trace; do not claim a four-identity Element
demonstration. After provisioning, each posted event gets an append-only
delivery receipt containing its Matrix event ID.
