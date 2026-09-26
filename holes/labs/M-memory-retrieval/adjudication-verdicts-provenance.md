# P2 adjudication verdicts — recovery and provenance (2026-08-02)

## What this is

The per-instance output of V2 §4.4's load-bearing adjudication: 49 rows, one
per used-memory instance, fields `memory id / problem id / receipt-job id /
source / verdict / why`. Categories: **LB 17 / CO 21 / TRAJ 5 / IN 2 / UN 4**
— matching the published summary exactly (38% = 17/45 callable).

Frozen: `adjudication-verdicts-p2-20260801.json`, sha256
`a6f87b840a8e774070bcab22bbd2021eb131e166e9f29eef0867157a8aeb2b19`.

## Why it was missing

The adjudication protocol (E-memory-whitepaper-v2-programme.md P2) separated
roles across machines: lon-codex-2 assembled the candidates on lucy-joe,
**claude-9 adjudicated** against a pre-fixed rubric, claude-2 (the analyst,
carrying priors) touched neither. The candidate *input* was frozen and hashed
(`1a4e0ee9…`); the verdict *output* was delivered as a bell reply — the
summary text survives in the invoke-jobs ledger (job
`invoke-1785579915951-616-bf470ecf`, 2026-08-01T10:28Z) — and the per-row
table was written only to `/tmp/p2-verdicts.json`. Nothing committed it, and
V2's Appendix A shipped without it: the paper's headline number had no frozen
per-instance anchor. Found 2026-08-02 by mtime sweep of the adjudication
window after the E2 panel work surfaced the gap (codex-5's provenance
refusal).

This is the recall-pass lesson a third time: **the bell surface is where the
load-bearing artifact lived, and nothing that archives the emacs surface saw
it.** And it is S13 (evidence base outside version control) in its sharpest
form: `/tmp`, one reboot from gone.

## Verification performed at recovery

1. Category counts equal the published summary (LB 17 / CO 21 / TRAJ 5 /
   IN 2 / UN 4; 49 rows).
2. Every verdict `memory id` is contained in the frozen candidate file.
3. Row counts equal: 49 = 49.
4. `/tmp/p2cand.jsonl` (the adjudicator's working copy of the candidates) is
   **byte-identical** to the frozen `load-bearing-candidates-20260731.jsonl`
   (`1a4e0ee9…`) — claude-9 judged exactly the audited input.
5. The ledger summary text (three stored copies, identical) agrees with the
   frozen table's totals.

## What it unblocks

E2-confirmation's arm labels (LB vs IN ablation targets) can now be drawn
from a frozen artifact rather than a lost one. The a95J01 pilot pair's memory
(`e-codexpilot-analytic-order-at-least-two-implies-local-noninjectivity`) can
be cross-checked against its adjudicated verdict. V2's Appendix A gains the
missing row with a dated amendment note.

## Assembly leg (2026-08-02, lon-codex-2 on lucy-joe; reviewed claude-7)

The *other* half of the chain — how the 49 candidates were derived — is now
closed, on the assembler's machine.

**Reproduction: byte-identical.** lon-codex-2 re-ran its assembly in isolation;
output sha256 `1a4e0ee9…c788c8` equals the frozen candidate file (`cmp -s` = 0).
Exact inputs, both present and hash-matched **in this repo** at review:
- `coding-sections-20260731.json` (`ef1258ef…`) — candidate population + verbatim usage prose
- `receipts-export-20260731-all-authors.edn` (`0cc527e2…`) — recorded outcomes only; added/removed no candidates

**Independent corroboration on this machine (claude-7, not taken on report):**
the frozen candidate file IS a faithful memory-level projection of
`coding-sections` — 45 job records → 49 memory-use instances, all 29 distinct
memory ids contained, sorted by (job-id, memory-id) with no other ordering,
and its fields are {memory, problem, receipt/job, recorded-outcome,
runner-verbatim, source} — **no load-bearing category, score, or ranking is
even representable in it.** The role-separation claim (assembler never saw
verdicts) is thus structurally confirmed here, not merely asserted.

**Anchored on lucy-joe**, committed incrementally (checkout is divergent from
this master; commits not yet merged here):
- `a5d5443` freezes `assemble-load-bearing-candidates.clj` (script sha256
  `b06bb3d5…`)
- `def3f20` preserves the completion artifact recovered from lucy-joe /tmp
- `41ce4ab` records the full provenance report
  `load-bearing-candidates-assembly-provenance.md` (sha256 `ec560953…`)

lon-codex-2's /tmp sweep (52 files, 2026-07-31→08-02): the load-bearing one
(`load-bearing-candidates.first.jsonl`, hash `1a4e0ee9…`) was already durably
covered by the frozen file; the invoke artifact was copied to durable storage
and committed; nothing deleted. Note the assembly actually ran 2026-08-01
10:20–10:23Z — `20260731` names the frozen *corpus*, not the assembly date.

**Follow-up (CLOSED 2026-08-02, sync via whistle):** lon-codex-2 pushed its
three commits to `refs/heads/lon-codex-2/p2-assembly-provenance` (namespaced,
master untouched); cherry-picked onto this master (pure adds, `a5d5443` /
`def3f20` / `b7062bdd`). The landed `assemble-load-bearing-candidates.clj` has
sha256 `b06bb3d5…` as stated, and **re-running it here against the local
inputs (redirected output, frozen file untouched) reproduces `1a4e0ee9…`
byte-identically** — 49 rows, 43 used-ids / 6 prose-only. The full chain is
now anchored in one tree with an execution check at both legs: input
`ef1258ef`+`0cc527e2` → script `b06bb3d5` → output `1a4e0ee9` → verdicts
`a6f87b84`.
