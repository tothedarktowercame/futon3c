# SHARD-NOTE — moves-scale3 shard-1 (batch 3, mission scopes), 70 records (zai-1, 2026-10-12)

## Counts

70 sidecars, 168 acts (145 operator fragments, 23 reply acts across the 6
records that have any), 16 answers (8 closes / 8 keeps).  Scoped acts:
opens 10, within 62, closes 0.  open_ports references: 17, to 5 distinct
opener ids.  0 unlabelled.  `validate.py *.moves.json`: **exit 0, total
valid=168, void=0** across all 70.

## Move vocabulary

47 distinct labels, 17 one-offs, nearly all from moves/vocabulary.json:
approve/plain x14, report-problem/observed x13, ask-action/commission x13,
challenge/other x11, ask-action/status-check x8, constrain/standing-rule
x8, propose/directive x7, clarify/ask-decision x7.  Invented (no
vocabulary fit, each names the dialogue move): propose/sequencing (order
milestones), propose/reframe (red-teaming reading), propose/analogy
(WEFT/WARP mapping), clarify/condition, clarify/assertion,
clarify/distinction reused from vocab, report/observed-output,
report-problem/retrospective (looking back at wasted work),
collect/idea, explain/reflection, explain/interpretation,
explain/decline.  No topic labels; batch-2 discipline kept.

## Five most-cited openers (open_ports targets)

1. claude-12-turn-4-f-s1-0 "please run it and report back." x6 — the
   standing run-and-report delegation of the M-G-over-cascades session;
   most later run/commission turns sit within it.
2. claude-11-turn-2-f-s1-0 "I think we should work on a health scan..."
   x5 — the health-scan opener of M-landscape-positioning; sequencing and
   redirect turns read inside it.
3. claude-18-turn-7-f-s2-2 (training-set proposal) x3 — the LSP/training
   thread of M-futonzero-prelim-practice.
4. claude-12-turn-13-f-s1-0 (debugging lane) x2.
5. claude-12-turn-88-f-s7-0 (intransparent run) x1.

## Scope findings

10 opens: standing rules (direct push no PRs; codex-only for build work;
debugger-watching required), plans (explain the slow policy first;
parser-only mode first; learn from the best; lean-derived curriculum),
framing regions (one system, sessions as efficiency; mfuton false-start
retrospective).  62 within — most against an open_ports opener, the rest
against a same-record fragment that an earlier fragment governs.  No
closes: no turn in this shard explicitly retired an opener.

## Input anomalies

- Several records carry the SAME text twice under two intents
  (turn-5iFDkQ f-s1-0 clarify + f-s1-1 collect; turn-DtnrMF f-s5-0/f-s5-1;
  f-s2-0/f-s2-1) — upstream fragmenter duplicates; labelled both, noted.
- One intended `within` target (claude-12-turn-88-f-s8-0, the
  debugger-watching rule) is NOT in turn-DryWNa's open_ports even though
  the challenge cites it — the kernel no longer holds it open; the
  reference is kept in the note instead.  Worth the owner's attention:
  rules that still govern dialogue drop out of open_ports.
- Operator typos preserved verbatim in quotes (tge/drbugger/goid/wiyh).

## Time

~4.5 h (70 records, mostly single-fragment; scope decisions dominated).
