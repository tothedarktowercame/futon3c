# E-captain-pass-to-claude-2 — the loop and the captain seat

**From claude-1 (Fable, session 72fd77ea) to claude-2 (Opus), on Joe's
order (2026-08-12: "fully pass control of the loop and captain seat to
claude-2, because I am nearly out of Fable usage"). Effective when the
handover bell citing this doc reaches you. Everything below is yours to
run; Joe is the only authority above you.**

## Read first, in this order

1. `holes/missions/M-case-studies.md` — the mission (mining now,
   one-problem-at-a-time solving after).
2. `holes/excursions/E-compression-conductor-runbook.md` — ENTIRE file;
   the lessons sections at the bottom are the expensive parts.
3. `holes/labs/M-zai-learning-loop/batch-2-report.md` + prereg
   amendments 1–9 — how the batch era ended and why.
4. `docs/retrieval-whitepaper-v3.md` §2.1 — the 13-instance silence
   catalogue. It is the campaign's immune system; you will add to it.

## Your first action: resolve the reviewer conflict

You have been the REVIEWER. As captain you cannot review your own
pipeline (operator ≠ reviewer ≠ scribe is a hard rule). Register
claude-4 (Opus) as the new reviewer (pattern:
`scripts/register_opus_reviewer.clj`, adapted; registration is
LIVE-STATE — it dies with a JVM restart, keep the re-registration list
below). Your own pending review verdicts (the eight staged candidates)
were formed in the reviewer role before this handover: execute them as
captain without re-review, but claude-4 reviews everything after.

## State at handover

- **Corpus**: apm-lean master ~204/448 solved (b01A04 closed by case 1;
  a02J04 + m01J03 closed in batch-2 merges). Joe pulls via
  `git pull zone-joe:/home/joe/code/apm-lean master`; Zone cannot push.
- **Mining**: slices 1–3 done on the 12-problem algebra set. Slice 3 =
  operative result 312 marks / 6 clusters / 1 authored candidate, but
  FAILED the fabrication gate at 5.77% vs 5.0% (threshold deliberately
  unmoved). Upgrades mandatory for slice 4: rubric v3 (PARAPHRASE lane)
  + tolerant matcher as standard verifier.
- **Scaling rule (Joe's, binding)**: slice 4 passes clean → scale
  immediately (zai-3/zai-4 worker seats, second conductor on
  ams-codex-2, disjoint problem sets, ONE dedicated student seat per
  conductor). No unconsumed quality signals before scaling.
- **Tide test (stage 4)**: due after slice-3 review — now yours. It is
  the anti-sandcastle gate; run it before deep scaling.
- **Job cap**: soft, now 24h (was 35min; the label caused a
  false-death recovery race). Overrun ≠ death, ever. Re-verify
  terminal state before ANY recovery dispatch.

## Open items you inherit

1. Execute your own eight review verdicts; then assays (session-
   isolated arms!) for approved candidates; store deposit only after
   assay pass.
2. Slice 4 (same 12 problems' remainder or next panel — conductor
   ams-codex-1 knows the drill; two jobs per slice, stage-1 + stage-2).
3. Scaling per the rule, then the remaining ~35 slices.
4. XTDB2 evidence package: 951 manifest rows point at an absent export
   dir — needs an oxf-codex-6 re-transfer (Joe liaises with Dionysus).
5. Zone's futon3 is NOT under version control — pattern-library writes
   are mirrored in `futon3c/data/pattern-staging/` until Joe resolves
   the sync; registration of approved patterns into patterns-index.tsv
   is yours after review+assay.
6. claude-3's held item: the store-wide retraction audit ("no other
   rows affected" still asserted, not established).
7. Packet v-next (explicit axiom gate text is in amendment 6) + frame
   contract read-scope: land as ONE versioned change before
   case-solving resumes.
8. Case 2 selection: Joe's pick, when he greenlights solving.

## Hazards bought with blood (all of them this week)

- **Parallel incarnation**: NEVER send a bell addressed to your own
  agent id from inside any workflow; a bell arriving while your REPL
  turn runs spawns a concurrent twin on your session. Verify passively
  via the jobs ledger instead.
- **D4 is everywhere**: one seat = one session across its queue.
  Assay/experiment arms need verified session resets or distinct
  seats. zai seats ALSO accumulate invoke-fn context — rotate via
  POST /agents/restore with a fresh session-id (verified to truncate).
- **Recovery discipline**: re-verify death before recovering; scope
  recovery evidence by CONTENT named in surviving reports, never by
  memory of directory layout; stale generations get tombstones.
- **Gates indict the innocent**: every detector's first accusation
  gets the same skepticism as the accused (naive quote-matcher: 84%
  false-negative rate; runner_gate regex; fabrication ≠ unverifiable).
- **Drawbridge masks asserts** as "Syntax error macroexpanding" — wrap
  in try/catch and read .getMessage.
- **Traffic discipline with Joe**: one report per major event; his
  quota anxieties are real; silence between events is the system
  working.

## Post-JVM-restart re-registration list (live state that dies)

claude-2 (you — via cr restore), claude-3, claude-4 (once registered),
zai-1 (needs invoke-fn wired with the evidence store:
`(futon3c.dev/make-zai-invoke-fn {:agent-id "zai-1" :session-file
"/tmp/futon-zai-session-id-zai-1" :evidence-store
@futon3c.dev/!evidence-store})` + update-agent!), the operator
bellback watcher (`scripts/bg.py launch "python3
/home/joe/code/futon3c/scripts/operator_bellback.py" ...` — flock
guard means safe to relaunch), and NOTE: plain codex-N/zai-N lanes
minted by cx/cz vanish too. ams-claude-1 must NOT be re-registered —
it resolves to claude-1 natively and its row was deliberately
obliterated (visual-twin disease).

## The one-line philosophy, if you want it

Authored norms decay into sentinels unless some instrument or fresh
reader is charged with disbelieving them — and that instrument's own
claims get the same treatment. You are now the fresh reader in chief.
