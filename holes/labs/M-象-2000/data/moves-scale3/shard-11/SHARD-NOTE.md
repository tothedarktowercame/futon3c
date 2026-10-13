# SHARD-NOTE — batch 3 wave 4, shard 11 (zai-2, 2026-10-13)

`python3 -I ../../../moves/validate.py *.moves.json`: exit 0, **53 files
valid, void=0**. Re-dispatch after shard 9 died at 01:16Z on a Zai-side
HTTP 500 ('Internal network failure'); the 7 shard-9 sidecars survive as
8f4fd1816. No 429 hit this run. Committed in two waves (44 then 53) as
instructed.

## Counts
- 156 acts labelled, 0 unlabelled; 0 answers (fragments-only shard, as
  announced — no reply acts anywhere).
- Scoped: 18 opens, 93 within, 0 closes; **0 open_ports targets, 2
  in_force targets** (claude-1-turn-148 self-chains dominate via own
  fragments instead — pre-stamping sessions mostly resolve scope locally).
- 44 distinct labels, all from vocabulary.json; **no inventions** (the
  batch-3 inventions like challenge/other and clarify/assertion are already
  in the consolidated vocabulary).

## Oddities in the inputs
- All open_ports lists sit at the 24 cap with many stale/no-text ports
  (kind None, empty text, e.g. claude-17-turn-45-f-s1-2); several since
  fields are '?'. Not labelling against them was deliberate.
- No mission_id on any record, as announced for wave 4.
- turn-QKVqQp has 17 fragments — one long reflective monologue; labelled
  with two nested chains (the selection challenge, then the how-Joe-chooses
  account under it).
- turn-dqrhT8, turn-bvt80x, turn-ZSG0LK fragments embed reply-like bullet
  spillover after the operator text (quoting gave only the head sentence).
- `:acts` truncation at 120 chars cost one quote each in turn-W4rIHp,
  turn-YTU5uF, turn-Zj89sM, turn-YM7slx (noted per act).

## Time
About 2.5 h of seat time across the 500'd and re-dispatched runs.
