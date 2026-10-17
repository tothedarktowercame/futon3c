# PACKET: minus40/scripts/inject.py (E-prove-memory-useful, G4 for Zai seats, selection half)

From claude-3. Read futon3c/holes/excursions/E-prove-memory-useful.md, section
"G6, first run", and futon3c/holes/excursions/G4-zai/CONTRACT.md (the CLI
contract; follow it exactly, another seat is wiring the caller to it in
parallel).

## Goal (one behaviour)

Given a packet's text, pick the memories to put in front of a Zai seat
before round 1, and render them as one block.

Selection: run the same ranking `m40_near` uses (`minus40/scripts/m40_mcp.py`
`t_near`, which runs the futon3a venv; import or call it, do not copy the
embedding code) with the whole packet text, k = 12; collapse facts to their
memory id keeping each memory's best score; keep the top 3. No threshold in
this version (say in your bell what scores you saw).

Block: a heading line exactly
`minus40 memories injected before round 1 for this turn (picked by meaning from the packet text; verify before relying):`
then per memory `--- memory <name> (<project>) ---`, the memory's
statements in force as `m40_state` would show them for that memory (or the
memory file's body after frontmatter, if that is simpler and you say so),
capped at 1500 chars per memory and 4000 in all, then
`--- end of injected memories ---`.

## Files

- in: `minus40/scripts/m40_mcp.py`, `minus40/scripts/m40.py`, `minus40/scripts/brief.py` (opt-in file and never-fail conventions).
- out: `minus40/scripts/inject.py`, `minus40/scripts/test_inject.py`.

## Acceptance

1. Tests (stubbed ranking): distinct ids, best score kept; caps applied;
   disabled agent prints `{"enabled": false, ...}` and calls nothing; a
   ranking failure or timeout gives the empty answer and exit 0.
2. Live, against the real store, for each of
   `futon3c/holes/excursions/G6-task-bank/T1.md`, `T4.md`, `T5.md`
   (with `inject-enabled.txt` containing a test id, restored after): paste
   the `memories` list. Expected from claude-3's probe: T1 has
   agency-send-mode-brief-trap in the top 3, T5 has lean-top-is-analytic,
   T4 likely does NOT have slow-tagged-tests-hide (it ranked 7th). Report
   what you see; do not tune to make T4 pass.
3. Do not create `inject-enabled.txt` for real seats; claude-3 does that.
4. Commit on minus40 master.

Bell claude-3 back with a summary + commit shas.
