# Mission: M-autoclock-in

**Status:** INSTANTIATE-1 (first implementation, 2026-06-03) — explicit resolved target auto-promotion is implemented in `agent-chat.el`; broader confirmation/XTDB witnesses remain future work.
**Owner:** codex-2 for first implementation; Joe/agents for review and follow-up.
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

## MAP (2026-06-03)

### Existing Infrastructure

- Manual single-active clock target already exists in `futon3c/emacs/agent-chat.el` as `agent-chat-set-clock!`, with `C › M › E` display via `agent-chat-mission-label`.
- Manual override surface exists as the repl clock-in hydra (`agent-chat-clock-menu`, bound to `C-c C-o` and `🍒`) plus `agent-chat-clock-in`, `agent-chat-excurse`, and `agent-chat-clear-excursion`.
- Target completion already resolves filesystem IDs through `agent-chat--clock-target-candidates`. As of commit `d708e83`, it scans both `holes/` and `holes/<kind>/`, so top-of-holes missions are visible.
- User-turn evidence is emitted through `agent-chat-emit-turn-evidence!`; clock fields come from `agent-chat--mission-body-fields`.

### Ready vs Missing

| Ready | Missing / Deferred |
| --- | --- |
| Exact `C-*`/`M-*`/`E-*` candidate lists from filesystem paths. | XTDB target existence witness; filesystem is the first witness. |
| Single-active local clock path. | Confirmation prompt; first cut auto-promotes only under a narrow no-fuzzy rule and leaves hydra override. |
| User-turn evidence body can carry clock metadata. | Dedicated evidence event type for auto-clock promotions. |
| Manual override hydra. | Operator policy for disabling auto-clock per buffer/session beyond `agent-chat-auto-clock-enabled`. |

### MAP Findings

The safe first implementation point is `agent-chat-send-input`: the user turn is available as a trimmed string, and the REPL-specific `before-send` hook has not emitted user-turn evidence yet. Therefore auto-promotion can happen before evidence capture, so the promoted `clocked-target` and the witness can be recorded on the same turn that caused the promotion.

The risky part is not detection but over-detection. Therefore the first implementation refuses fuzzy matching entirely: no aliases, no lowercase repair, no inferred mission from prose, no substring search. A turn must name an exact target token that resolves against existing candidate IDs.

## DERIVE (2026-06-03)

### Witnessed-Promotion Rule

A user turn becomes an auto-clock-in only when all of the following are true:

1. The turn contains one or more explicit target tokens matching `C-*`, `M-*`, or `E-*`.
2. Every target token resolves by exact ID against the filesystem-backed completion candidates for its level.
3. The turn names at most one campaign, at most one mission, and at most one excursion.
4. The resolved target differs from the current buffer clock target.
5. The promotion switches the single active path to exactly the resolved `C › M › E` components; absent components become nil rather than being guessed.
6. The promotion records an audit witness with rule name, source, explicit tokens, old target, and new target.

If any explicit target is unresolved, or if multiple targets at the same level are named, no auto-clock promotion happens.

### Design Decisions

IF the mission's central risk is false attribution, HOWEVER many turns name missions in ordinary prose, THEN the resolver only accepts exact `C-*`/`M-*`/`E-*` tokens, BECAUSE a missed auto-clock is less damaging than a false one.

IF a turn names both `C-*` and `M-*`, HOWEVER the existing model is single-active, THEN the auto-clock path becomes exactly that campaign/mission pair, BECAUSE this matches the manual `C › M` clock-in shape without stacking multiple missions.

IF the agent names only `E-*`, HOWEVER inheriting the current mission would be an implicit guess, THEN first implementation clocks a bare excursion without inferred parents, BECAUSE "explicit-not-fuzzy" applies to omitted parents as well as named targets.

IF the operator disagrees with an auto-promotion, HOWEVER the automation has already changed the target, THEN the hydra remains the override surface, BECAUSE it can immediately switch, clear excursion, or return to no mission.

### Audit Shape

`agent-chat--last-auto-clock-witness` is attached to the next user-turn evidence body as:

```elisp
((rule . "explicit-resolved-target")
 (source . "user-turn-explicit-token")
 (tokens . ["M-autoclock-in"])
 (old-target . "no mission")
 (new-target . "M-autoclock-in"))
```

The transcript also receives a short system line:

```text
system: [auto-clock: no mission -> M-autoclock-in via M-autoclock-in]
```

## INSTANTIATE-1 (2026-06-03)

Implemented in `futon3c/emacs/agent-chat.el`:

- `agent-chat--explicit-clock-target-tokens`
- `agent-chat--resolve-auto-clock-token`
- `agent-chat--auto-clock-target-from-text`
- `agent-chat--maybe-auto-clock-from-turn`
- `agent-chat-auto-clock-enabled`
- `agent-chat--last-auto-clock-witness`

`agent-chat-send-input` now calls `agent-chat--maybe-auto-clock-from-turn` after inserting the user turn and before the REPL-specific `before-send` hook emits evidence. The witness is cleared immediately after `before-send` so it does not leak into later turns.

### INSTANTIATE-1 Smoke Checks

- `please advance M-autoclock-in` resolves to `(:mission-id "M-autoclock-in")`.
- `work on C-substrate-completion and M-autoclock-in` resolves to `(:campaign-id "C-substrate-completion" :mission-id "M-autoclock-in")`.
- `maybe M-does-not-exist` does not promote.
- `M-autoclock-in and M-vsatarcs-invariants-integration` does not promote because two missions are named.

### Remaining Work

- Add XTDB-backed target existence witness when mission/campaign/excursion entities are stable enough for this surface.
- Decide whether an explicit bare `E-*` should remain bare forever or inherit a currently active campaign/mission under a separate, explicitly documented rule.
- Add a dedicated promotion evidence event if the turn-body witness is not enough for downstream analysis.
