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

0. **The buffer is at the no-target floor** — no campaign, mission, *or* excursion is currently clocked. Auto-clock **fills the `[no mission]` floor; it never switches or overrides an active clocking.** (Joe, 2026-06-03: a turn that mentions another mission while you are already clocked must not move you — that mention is turn-level mention-graph data, NNexus-style, not a clock change. Gating on the *full* floor — not just "no mission" — also avoids the campaign-wipe edge, where mentioning `M-bar` while on a bare `C-foo` would re-parse with no inheritance and clear `C-foo`.)
1. The turn contains one or more explicit target tokens matching `C-*`, `M-*`, or `E-*`.
2. Every target token resolves by exact ID against the filesystem-backed completion candidates for its level.
3. The turn names at most one campaign, at most one mission, and at most one excursion.
4. The resolved target differs from the current buffer clock target.
5. The promotion switches the single active path to exactly the resolved `C › M › E` components; absent components become nil rather than being guessed.
6. The promotion records an audit witness with rule name, source, explicit tokens, old target, and new target.

If a target is already clocked, or any explicit target is unresolved, or multiple targets at the same level are named, no auto-clock promotion happens.

### Design Decisions

IF the mission's central risk is false attribution, HOWEVER many turns name missions in ordinary prose, THEN the resolver only accepts exact `C-*`/`M-*`/`E-*` tokens, BECAUSE a missed auto-clock is less damaging than a false one.

IF an exact token can still appear in a turn that merely *discusses* a mission ("unrelated to `M-foo`"), HOWEVER the operator is already clocked on real work, THEN auto-clock fires **only at the no-target floor** and never switches an active clocking, BECAUSE the eager-mention false-positive is harmful precisely when it would move you *off* what you are on — and a mention made while clocked is still captured as turn-level mention-graph data, just not as a clock change.

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

## INSTANTIATE-1.1 (2026-06-03) — floor-only guard (review fix, claude-3)

Review (claude-3, of `a1add6d`) found the detector promoted on *any* exact resolved token, including a mission named in passing while already clocked — so it could switch you off active work. Per Joe's directive, `agent-chat--maybe-auto-clock-from-turn` now fires **only when the buffer is at the no-target floor** (`agent-chat--campaign-id`, `--mission-id`, and `--excursion-id` all nil). Rule 0 above. Verified (batch, mutators stubbed) + redefined live on the `server` socket:

- clocked on a mission, mention another → **no promotion** (the mention is left for turn-level capture).
- clocked on a bare campaign, mention a mission → **no promotion** (no campaign-wipe).
- at the no-target floor → promotion fires as before.

`check-parens` clean. The pure detector (`agent-chat--auto-clock-target-from-text`) is unchanged — it still resolves mentions for the mention-graph; the floor guard sits in the promotion wrapper.

### Remaining Work

- **Creation-clock rule (Joe, 2026-06-04 — via the `eoi-new head` excursion).** `eoi-new head <slug>` (and any
  mission-*creating* flow) should auto-clock onto the **just-created** mission so the operator needn't clock it in.
  The existing `explicit-resolved-target` rule **cannot** cover this: Rule 2 requires the token to resolve against
  *existing* filesystem candidates, but a mission being created does not exist when the turn is parsed. So this is a
  **distinct post-creation clock-in** — fire *after* `M-<slug>.md` is written (when it now resolves). **Design twist:
  unlike the floor-only mention rule (Rule 0), creation-clock should *switch* the active clock even if one is set** —
  invoking `eoi-new head X` is unambiguous explicit intent to start X, not a passing mention, so the floor-only guard
  does not apply. Surfaces: a post-assembly hook in the `eoi-new` launcher (`futon0/scripts/eoi-new`) or an
  `agent-chat.el` creation-clock path. Audit witness `(rule . "creation-clock") (source . "eoi-new-head") …`.
- Add XTDB-backed target existence witness when mission/campaign/excursion entities are stable enough for this surface.
- Decide whether an explicit bare `E-*` should remain bare forever or inherit a currently active campaign/mission under a separate, explicitly documented rule.
- Add a dedicated promotion evidence event if the turn-body witness is not enough for downstream analysis.

## INSTANTIATE-2 (2026-06-04) — creation-clock

Implemented a distinct creation-clock rule in `agent-chat.el`:

- `agent-chat-creation-clock-mission!` clocks to a just-created mission only after it resolves through the filesystem-backed mission candidates.
- The rule records an audit witness with `(rule . "creation-clock")`, source, token, old target, and new target.
- Unlike the mention-based `explicit-resolved-target` rule, creation-clock does **not** require the no-target floor; it may switch an active clock because mission creation is explicit operator intent.
- `agent-chat-watch-creation-clock-mission!` arms a buffer-local post-creation watcher, so `eoi-new head <slug>` can wait for `M-<slug>` to appear on disk before switching.

The `eoi-new` launcher now arms that watcher for `eoi-mission-head` / `head` invocations when a mission slug is provided.

## INSTANTIATE-3 candidate — edit-activity reclock (Joe, 2026-06-08)

**Motivation (Joe):** *"I really can't be bothered to clock in properly most of the time, which makes
the mission tagging a bit rubbish."* The lossy `turn→mission` tagging directly degrades the
`pattern→turn→mission` dataset that [[M-pudding-peradams]] §12 (turns-as-exotype) depends on — the
exo→geno coupling. A third, **fully autonomous** rule closes it: **reclock on repeated edits to a
`C-*`/`M-*`/`E-*` file.**

**Why this is *more* discipline-compliant, not less:** editing `M-foo.md` is **explicit, non-fuzzy
operator action** — the file *is* the target (resolves by path against the existing
`agent-chat--clock-target-candidates`), strictly stronger evidence than a prose mention. It does not
violate "explicit-not-fuzzy" (principle #8); it strengthens it.

**The rule.** Reclock to target `X` iff:
1. a save lands on a file whose path resolves by **exact ID** to an existing `C-*`/`M-*`/`E-*` candidate
   (no fuzzy text; the file path is the witness);
2. the file has accrued **≥ N saves within a window W** (default e.g. N=3, W=10 min) — *repeated*, so a
   single stray edit never flips the clock;
3. `X` differs from the current clock, and `X` is the **dominant** recently-edited target (hysteresis:
   if two C/M/E files are edited alternately, require clear dominance before switching — no thrash);
4. it records an audit witness and remains hydra-overridable.

**Switch, don't floor-gate (the key decision vs Rule 0).**

> IF a turn merely *mentions* a mission, THEN auto-clock fires only at the no-target floor (Rule 0,
> never switches). HOWEVER **repeated edits to a mission file are unambiguous "I am working on X now,"
> not a passing mention,** THEN edit-activity reclock **switches the active clock** (like creation-clock,
> §INSTANTIATE-2), BECAUSE sustained editing is operator intent of the same grade as creating the file —
> the floor-only guard would defeat the whole point (you are usually already clocked on *something* stale).

**Surface.** An Emacs `after-save-hook` (operator edits land in Emacs) that matches `holes/**/{C,M,E}-*.md`,
maintains a per-target save count + recency, and calls a `agent-chat-edit-activity-reclock!` when the
threshold + dominance test passes. (Agent/tool edits that bypass Emacs save are out of scope for v1 —
the target signal is the *operator's* focus.) Debounce to avoid re-firing for the same target.

**Audit shape.**

```elisp
((rule . "edit-activity")
 (source . "repeated-file-edits")
 (file . ".../holes/M-foo.md")
 (edit-count . 3) (window-seconds . 600)
 (old-target . "M-stale") (new-target . "M-foo"))
```

**Remaining design qs:** the N/W defaults (calibrate against real edit cadence); whether to also count
edits to a mission's *associated code files* (the `file→mission` edges) or only the doc (v1: doc only);
hysteresis threshold for the dominance test. **Owner:** codex-2 (impl, per this mission's pattern);
gates — `futon4/dev/check-parens.el` on the elisp + the agent-chat smoke checks.

## INSTANTIATE-3 checkpoint — edit-activity reclock (codex-2, 2026-06-08)

Implemented in `futon3c/emacs/agent-chat.el` as an additive third auto-clock rule:

- `after-save-hook` watches Emacs saves to `holes/**/{C,M,E}-*.md` mission docs only.
- Saved file paths resolve by exact basename ID through `agent-chat--clock-target-candidates`; no prose/fuzzy matching is used.
- Reclock requires repeated saves within the configured recency window (`agent-chat-edit-activity-clock-threshold`, default `3`; `agent-chat-edit-activity-clock-window-seconds`, default `600`).
- The dominance test is conservative: a target must meet the threshold and beat the next recent target by a margin, so alternating edits do not thrash.
- Unlike the explicit mention rule, edit-activity switches an active stale clock. The switch target is single-active: `C-*` sets campaign only, `M-*` sets mission only, `E-*` sets excursion only; absent components remain nil.
- The next user-turn evidence body carries the `auto-clock-witness` audit alist with rule/source/file/edit-count/window/old-target/new-target.

Batch smoke coverage was added in `futon3c/test/agent-chat-edit-activity-smoke.el` for the accepted v1 cases:
3 saves switch, 1 save does not, alternating saves do not thrash, active `M-bar` switches to repeated `M-foo`, and single-active is preserved.
