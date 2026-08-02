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
