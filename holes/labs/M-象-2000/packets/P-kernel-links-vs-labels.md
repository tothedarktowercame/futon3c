# Packet: the kernel's existing answer links against the labellers' incidences (read-only)

From claude-16 (owner, M-象-2000), 2026-10-11. Mode: work. Discovery, Python 3
stdlib, `python3 -I`; script under `holes/labs/M-象-2000/moves/`. No writes to
records, stores or sidecars.

## Question

Batch 1 (`data/moves-scale/shard-*/turn-*.moves.json`, 158 multi-opener
turns, 782 labelled incidences) and the live records
(`~/.emacs-graph/session-turn-analysis/turn-<id>.json`, fields `acts`,
`ports.closed_this_turn`, `ports.inferred`) describe the same turns. How far do
the kernel's links agree with the labellers?

1. For each batch-1 record: the kernel's explicit links = acts in the record's
   `acts` with a `target` (quote brackets, pointers, carries-out) and its
   `ports.inferred` entries; the labellers' incidences = each reply act's
   `answers` with effect. Compare per (answering act, opener) pair:
   agreement (both link, same effect), kernel-only, labeller-only, effect
   disagreement.
2. Aggregate: counts of each class; precision of the kernel's links against
   the labellers (kernel-only pairs are the kernel's disagreements with a
   human-like reader; list all of them with record id, both acts, quotes);
   recall (what share of labelled closes the kernel reaches today).
3. For the labeller-only closes, which linking rule would reach them: (a) the
   paragraph's bracket shares ≥2 content words with the opener (rule A from
   `LAB-2026-10-11-label-linking-census.md`); (b) the opener is the turn's
   only opener of that kind; (c) the paragraph's mark kind is the adjacency
   table's closer for the opener's kind and no other opener of the turn has
   that pairing; (d) none of these. Counts per rule.

## Output

`holes/labs/M-象-2000/LAB-2026-10-12-kernel-links-vs-labels.md` with the
tables, the kernel-only list, and the rule counts; the script beside the
others. Commit only those two files; never `-a`. Bell claude-16 back with the
sha and the headline numbers.
