# Mission: M-autoclock-in

**Status:** IDENTIFY (stub, opened 2026-06-02) — scoped deliberately as its own mission rather than winging it.
**Owner:** TBD (Joe to assign).
**Repo:** futon3c (clock-in lives in the agent-chat/REPL surface).

## HEAD (Joe, 2026-06-02, verbatim sense)

The going-forward clock-in (`C-substrate-completion §8.1`) is manual: `cr/cx new <target>` or `agent-chat-clock-in`/`-excurse`. Many turns *identify* a mission (e.g. "let's work on M-foo") that already exists on the filesystem / in XTDB — so we could **auto-promote** it: detect the mission in the turn, confirm it exists, and clock in. **But we should not wing it** — a fuzzy heuristic that silently mis-clocks would poison the very `turn→mission` evidence the §8 experiments depend on. Hence a proper mission with a witnessed promotion rule, not an ad-hoc hack.

## The tension

- *Pull:* manual clock-in is friction; the turn often already names the mission; auto-promote would make the `C › M › E` typing effortless and the §8 dataset richer by default.
- *Risk:* auto-clocking on a fuzzy mention (the exact anti-pattern `M-vsatarcs-invariants-integration` principle #8 warns about — *explicit, not fuzzy*) would mislabel turns and corrupt `turn→mission` ground truth. A wrong auto-clock is worse than `[no mission]`.

## The idea (to refine in MAP/DERIVE)

Auto-promote a mission/campaign/excursion target identified in a turn **iff**:
1. it **resolves** to a real `M-*`/`C-*`/`E-*` that exists (filesystem + ideally XTDB), not a fuzzy guess;
2. it respects **single-active** (auto-promote *switches*, never stacks — same constraint as manual clock-in);
3. it is **operator-overridable / confirmable**, never silent — the promotion is a *witnessed* proposal (status-as-witness discipline), surfaced (e.g. a prompt or a one-key confirm in the repl hydra), not an assertion;
4. it leaves an audit trail (why it auto-clocked) so a mis-promote is diagnosable.

## Relations

- `C-substrate-completion` §8.1 (manual clock-in instrumentation) — this is the *automation* layer over it.
- The repl **clock-in hydra** (🍒) — the manual pivot UI; auto-clock is its inferred counterpart (the hydra is the override surface).
- `:mission/mentions-file` / the mention map — a turn mentioning a mission is the candidate signal; resolution must be explicit (principle #8: no fuzzy text matching as truth).
- `E-the-dark-tower-2` — turns-as-typed-processes; auto-clock is *inferring* a turn's type rather than asserting it, so the inference must be evidence-bound.

## IDENTIFY exit (when picked up)

Name the resolution rule (turn → candidate target → existence witness → confirm/switch) and the override surface, such that auto-clock can never *silently* mislabel a turn. Until then: manual clock-in + the hydra are the floor; this stub holds the idea so it isn't winged.
