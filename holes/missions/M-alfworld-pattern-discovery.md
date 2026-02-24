# Mission: ALFWorld Pattern Discovery

**Date:** 2026-02-20
**Status:** Complete (10/10 patterns written, commit 2713661, 2026-02-20)
**Agent:** Claude (this session, --resume)
**Companion:** Tickle (watchdog, IRC paging)
**Requested by:** Rob (via Joe)

## Objective

Play ALFWorld games iteratively. After each game (win or lose), crystallize
what was learned into a flexiarg design pattern. Before each new game, re-read
all patterns written so far and see if performance improves.

**Completion criterion:** 10 flexiarg patterns written to `library/alfworld/`.

## Structure

Each cycle:
1. **Read** all existing `library/alfworld/*.flexiarg` patterns
2. **Reset** ALFWorld (new random task)
3. **Play** the game (take actions, observe, think)
4. **Reflect** on what went well / what went wrong
5. **Write** one flexiarg pattern capturing the key insight
6. **Loop**

## Task Types in ALFWorld

ALFWorld has 6 task families (from `valid_unseen`):
- `pick_and_place_simple` — find X, put it in Y
- `pick_clean_then_place_in_recep` — find X, clean it, put in Y
- `pick_heat_then_place_in_recep` — find X, heat it, put in Y
- `pick_cool_then_place_in_recep` — find X, cool it, put in Y
- `pick_two_obj_and_place` — find two X's, put both in Y
- `look_at_obj_in_light` — find X, find lamp, examine X under lamp

## Pattern Output Format

Each pattern goes in `library/alfworld/<pattern-name>.flexiarg` using
standard flexiarg syntax (see `library/social/*.flexiarg` for examples).

Patterns should capture ALFWorld-specific strategies:
- Navigation heuristics (where to look first)
- Object interaction patterns (when to examine vs. take)
- Task decomposition (how to break multi-step tasks)
- Error recovery (what to do when stuck)
- Coordination triggers (when to bell for help)

## Evidence

- Game transcripts accumulate in the peripheral's evidence trail
- Win/loss record tracks improvement over time
- Patterns reference specific game evidence (task type, step count, outcome)

## Prior Art

Two games played on 2026-02-13 (see `par-alfworld-inhabitation-2026-02-13.edn`):
- Game 1: pick_and_place_simple "put vase in safe" — 6 steps, WON
- Game 2: pick_cool_then_place_in_recep "cool potato, place in microwave" — 8 steps, WON

Key observations from that session:
- First-person action language emerges naturally
- `look` and `inventory` are critical for orientation
- Some receptacles are closed by default (need `open` first)
- Objects can be inside other objects (egg in microwave)
