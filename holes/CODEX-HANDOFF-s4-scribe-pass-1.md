# CODEX-HANDOFF — S4 scribe pass 1: mine pilot row 1 + mint the Young frontier record

Mission: `holes/missions/M-codex-sorry-loop.md` S4. Prepared 2026-07-28
by claude-6. **Delivery: Agency bell from claude-6. Bell claude-6 back
with summary + file paths (drafts are files, not commits — see below).**

## Seat and scope

You are the **scribe** for the codex pilot lane (seat separation: you
mine; codex-6 runs; claude-6 verifies). This pass: DRAFTS ONLY — memory
drafts as EDN files for owner review; no store writes this pass (the
store OOM'd earlier today — all store writing waits for owner
promotion, which happens after review). READ-only store access is fine.

## Inputs (read all)

- Harvested runner transcript: evidence rows
  `e-codexroll-019f9b12-t001` … `t007` on :7073 (GET
  `/api/alpha/evidence/<id>`; t007 is the Young proof turn — 72 tool
  digests + messages; reasoning is encrypted-only, so mine messages,
  tool digests, and the runner's own error→fix log).
- The row record: `holes/labs/M-codex-sorry-loop/pilot-1-ledger.edn`
  (incl. the runner's cited memory ids and the frontier statement).
- The commit: `git -C /home/joe/code/apm-lean show 8208ca7`.
- Census row context:
  `holes/labs/M-codex-sorry-loop/sorry-census-20260728.edn` (the
  young-convolution-L1-L2 rows).
- Draft-shape precedent: any draft EDN under
  `holes/labs/M-zai-learning-loop/` (the zai scribe lanes) — match the
  spirit: name, body, subjects, evidence ids, confidence, lane.

## Outputs (create under `holes/labs/M-codex-sorry-loop/`)

`scribe-pass-1-drafts.edn` — a vector of memory drafts, four lanes:

1. **solve-lane**: lemma-location + proof-shape memories from the
   session (e.g. the `integral_undef` contradiction trick for deriving
   Integrable from ∫g=1; the `(μ := volume)` metavariable fix; the
   `congr 1` before `lintegral_add_right_eq_self` move) — each with
   evidence ids (the e-codexroll rows), commit, problem.
2. **arc-lane**: scoped error→fix rewrite rules from the runner's log +
   tool digests (scope / before / after / confidence / evidence-ids).
3. **trajectory-lane**: process memories — what was expensive, what was
   abandoned, the bounded-reconnaissance behavior.
4. **frontier record (the mint)**: `integral-minkowski-eLpNorm-bochner`
   as a first-class frontier draft: exact statement (from the ledger),
   `:anchor :literature` (Schep,
   https://people.math.sc.edu/schep/minkowski-corrected.pdf, duality +
   Fubini–Tonelli + Hölder), `:unblocks` (young main theorem; the
   heat-semigroup contraction per `e-dfea2de9`), `:demand 2`,
   `:status`, and **typed-edge proposals** (frontier→blocked-items,
   memory→pattern attachments, supersession/refinement links to
   `e-dfea2de9`) — edges as DATA in the draft (`:proposed-edges`), for
   owner wiring at promotion. NOTE: an S6 frontier session is running
   concurrently (codex-6, may close the frontier) — draft the record
   anyway with `:status :session-in-flight`; the record is the
   frontier's history, not just its openness.

Also `s4-note.md` (≤40 lines): yield per lane, anything the
transcript could NOT support (thin-capture honesty), and cross-model
observations (these are zai-pattern-shaped memories mined from a CODEX
session — note where the register differed).

## Discipline

- Every draft cites evidence ids; no reconstructed why-clauses beyond
  what the runner narrated (plausible-but-wrong rationale is worse than
  absent — mark inference as inference).
- Refuse false merges (absent-API ≠ prerequisites-unmet; keep the
  taxonomy sharp).
- **RAM care**: store reads bounded (the 7 rows by id — no scans); no
  writes at all this pass.
- `git diff --stat` in futon3c: only this packet's outputs.

## Acceptance

- [ ] Drafts EDN parses; every draft has evidence ids + lane + subjects.
- [ ] Frontier record present with anchor, demand, proposed edges.
- [ ] Note states per-lane yield and capture limits honestly.
- [ ] Bell claude-6 with summary + paths.
