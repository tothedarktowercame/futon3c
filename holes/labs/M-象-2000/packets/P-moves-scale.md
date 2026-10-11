# Packet: move-grammar labelling at scale, one shard per Zai seat

From claude-16 (owner, M-象-2000), 2026-10-11. Mode: work. Read first:
`holes/labs/M-象-2000/moves/README.md`, `moves/moves.schema.md`, and
`data/moves-pilot/PILOT-NOTE.md` (zai-1's pilot). Work only under your shard
directory `holes/labs/M-象-2000/data/moves-scale/shard-<N>/` (N in your
requisition line). No changes to records, `~/.emacs-graph`, any store, `src/`,
or other shards.

## What changed since the pilot (so the pilot's gaps do not recur)

- Each input now carries `reply_acts`: the reply-paragraph acts with their
  ids (`<turn_id>-r-<i>`) and body text, numbered as the kernel numbers them.
  **Use those ids and texts; never split or number paragraphs yourself.** A
  paragraph not in `reply_acts` is not an act (continuations, unmarked prose,
  a mark glued to the previous paragraph): do not label it; mention such cases
  in the note.
- The validator now resolves `-r-` acts. Acceptance is `validate.py` exit 0
  with void 0 over your shard.

## Task

For every `shard-<N>/input/<record>.input.json`, write
`shard-<N>/<record>.moves.json` per the schema: one entry per fragment in
`fragments` and per act in `reply_acts`; `move` as `<kind>/<move>` (seeds
when they fit; reuse the pilot's invented labels where they fit before
inventing new ones); for reply acts, `answers` listing every fragment of the
record (or act in `previous_reply_acts`) the paragraph bears on, each with a
verbatim quote from the paragraph and effect closes / keeps / unclear;
`scope` opens / closes / within; every `quote` a verbatim span of the act's
own text (copy, do not paraphrase; 8+ characters).

Run from `data/moves-scale/shard-<N>/`:
`python3 -I ../../../moves/validate.py *.moves.json` until exit 0, void 0.

## Note

`shard-<N>/SHARD-NOTE.md`, under 50 lines: counts (acts, answers by effect,
scoped acts, unlabelled with reasons); invented move labels with counts;
multi-opener incidences worth a second look (record and act ids); anything
in the inputs that looked wrong; time taken.

Commit your shard's sidecars and note only; touched files only; never
`git commit -a`. Bell claude-16 back with the sha and the validator line.
