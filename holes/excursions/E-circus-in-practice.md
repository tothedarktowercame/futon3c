# E-circus-in-practice: watch the REPL Circus in use

**Status:** OPEN (2026-11-17). Carried from [M-repl-circus](../missions/M-repl-circus.md),
closed by Joe the same day: "I'd much rather close the mission and see how
the system works in practice than prove things like this about it."

Population (Joe, 2026-11-17): operator turns in the Claude and Codex Emacs
REPLs. Service levels: saved 2 min, animals 15 min, graph 30 min, Giraffe
priorities 6 h. The specification is that the system is event driven;
Giraffe should stay up to date with small updates that rarely reorder much.

Carried items, to take up when practice shows a need:

1. Per-turn stage table and overdue report (M-repl-circus obligation 3),
   for the population above.
2. A correction or rejection of an earlier reading, seen reaching Joe in
   Mojira (obligation 2.3).
3. Giraffe band stability: CL-47 (keep the last complete focus input when
   the needs read is incomplete) in progress at closure; afterwards watch
   that bands move only on real focus changes.
4. Giraffe run storage: 45 MB per run, 2.9 GB over 265 runs, because every
   priority row repeats the run's evidence pin.
5. Recovery: a query for rejected and superseded readings; bounded counts
   for the :7093/:7094 stores (obligation 4, kept as practical checks; the
   "same graph" proof was dropped).
6. Independent acceptance by a seat that built none of it (obligation 5),
   if a reader beyond Joe needs it.
7. Extension path: Zai/Kimi once they make 象 marks; interactive agents;
   intents across bells and whistles with "xiao" variants of the animals.
8. **馬 runs along turns as one strand** (Joe, 2026-11-17, relayed by
   claude-5 from the M-arxana-incubator work): "this becomes a requirement
   for 馬 to "run" along turns sequentially, and keep them knitted together
   as a unified strand. That was implicit before but the N-1 neighbourhood
   reading makes it more concrete. Effectively this gives the circus a
   "short term memory"." Background: claude-1,
   `~/notes/claude-1/tetrahedron-and-metaca-2026-11-17.md` §5 (MetaCA
   locality; exotype D21: a carried record stands in for longer reach).
   Today (claude-13, checked on session 4c3508ce…, turns turn-SMYalA,
   turn-tfegJB, turn-APtqbQ): `xiang/ma_occurrence.clj` `chain-window`
   re-reads the record plus up to 7 earlier operator turns of the session
   from the store on every turn, and `:pattern_verdicts :window` lists
   those 8 ids, so consecutive windows overlap by 7 but no verdict refers to
   the previous turn's 馬 record or carries anything from it.
   `xiang/ma_reading.clj` (`:alignment`) reads one turn's reply against its
   own 象 acts only. Missing: (a) a `:prev` link to the N-1 operator turn's
   馬 record (id + digest), typed `:session-start` at a first turn; (b) a
   carried state (patterns whose IF/HOWEVER/BECAUSE/THEN parts are mapped
   so far, with turn ids) computed from N-1's carried state plus turn N
   only, replacing the 8-turn raw re-read; (c) ordering: the `:aligned`
   listener fires per turn, so turn N's verdict can be written before
   N-1's model whistle returns, or after N-1 was interrupted; N must wait
   for, or record a typed `:prev-pending`/`:prev-absent`, never silently
   skip. Test: on one real session, every 馬 record after the first names
   the preceding operator turn as `:prev`; the strand has no gaps or forks;
   recomputing turn N's carried state from N-1's record plus turn N alone
   equals what is stored. Bad case: a carried state that needs a turn
   earlier than N-1 (which is what every verdict does today, via the
   window).
   **Revised by Joe, 2026-11-17:** "Well, 8-turn reread should be OK, though
   perhaps realistically we only need 4. This is still "locality" it's just a
   bigger locality. I think an N-1 requirement is OK in MetaCA but we don't
   *need* to be that strict for Circus. That said, being able to make an N &
   N-1 connection where one exists (or "no connection" if one doesn't) seems a
   requirement in any case." So: the window re-read stays (8 turns, 4 may do;
   it is a bounded locality, not a defect), and (b) the carried state and the
   N-1-only bad case are withdrawn. What remains required is (a) and (c): each
   馬 record names the N-1 operator turn's 馬 record when one exists, and
   records a typed "no connection" (`:session-start`, `:prev-absent`,
   `:prev-pending`) when it does not; never silently nothing. Test: on one real
   session, every 馬 record has exactly one of a `:prev` link or a typed
   no-connection, and the `:prev` links form one chain without gaps or forks.

