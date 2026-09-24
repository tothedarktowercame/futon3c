# Mission: M-autoclock-in

**Status:** INSTANTIATE-1 (first implementation, 2026-06-03) — explicit resolved target auto-promotion is implemented in `agent-chat.el`; broader confirmation/XTDB witnesses remain future work.
**Owner:** **claude-4** (going-forward, 2026-06-27 — Joe passed it here; it is D1 of campaign C-cascade-real: the durable agent↔session↔mission lineage, building on the bg-process process-tree node-type). codex-2 did the INSTANTIATE-1 first implementation; Joe/agents review.
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
- [[M-operational-vocabulary]] (futon2) — a downstream **consumer** of this `turn→mission` link: it mines memes/sorries from turns and needs them attributed to the right mission. Until auto-clock is hardened it routes around via **turn→pattern→mission**. (NB: that mission was spun up after a long move-mining session was mis-clocked *here* — a live instance of the gap this mission exists to close.)

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

## INSTANTIATE-4 candidate — AGENT-side reclock (Joe, 2026-06-14)

**The gap that broke it (2026-06-14, claude-2 / M-typed-holes).** Joe lost track of which
agent/mission was doing the typed-holes formal-modelling work; the mission-session history view
(reconstructed from clock-tagged turn evidence) showed claude-2 under **E-the-dark-tower** with
**M-typed-holes invisible** — even though claude-2 demonstrably worked it (signed the doc, commits
`c694d55`/`a75205d`). Root cause: **all three rules above key off OPERATOR signals only** —
INSTANTIATE-1 watches the operator's REPL user-turns, INSTANTIATE-2 the `eoi-new` launcher,
INSTANTIATE-3 the **Emacs `after-save-hook`** (operator saves). An *agent* edits mission docs
through its **tools** (never an Emacs save) and its work is its own invoke turns (not the operator's
REPL turns), so **no rule ever fires for agent work** — exactly as INSTANTIATE-3 admits
("agent/tool edits that bypass Emacs save are out of scope for v1"). Since agents do most of the
mission work, the clock tracks the operator's focus but is blind to the agents → the history is
incomplete.

**The rule (agent-edit-activity reclock — the agent-side mirror of INSTANTIATE-3).** The clock-bearing
side here is the **JVM**, not Emacs. Maintain a **per-agent-session clock** (mission/campaign/excursion)
in futon3c, and reclock it from agent-native signals, same explicit-not-fuzzy discipline:

1. **Primary — agent tool-edits to `{C,M,E}-*.md`.** The agent's `Edit`/`Write`/`MultiEdit` tool-uses
   are already surfaced to the JVM during invoke (the `:on-event` tool_use stream in
   `make-claude-invoke-fn`). Extract the `file_path`; if it resolves by exact basename to an existing
   `{C,M,E}-*` doc, accrue a per-session edit count (window W). On threshold N + dominance (hysteresis,
   no thrash), **switch** that agent-session's clock to the target — switch-not-floor, because sustained
   editing is unambiguous "this agent is working on X now" (same grade as INSTANTIATE-2/3, not a passing
   mention). The file path is the witness; no fuzzy/prose matching. *This is the signal that captures the
   claude-2 case.*
2. **Secondary — dispatch/invoke `mission-id`.** When an agent is invoked/belled carrying a `:mission-id`
   (handoffs name a mission; `emit-invoke-evidence!` already records it), set the session clock on
   receipt. Cheap, but only fires when the dispatcher names the mission — so it complements, doesn't
   replace, the edit signal (claude-2's autonomous work carried no mission-id, which is why it was lost).

**Recording.** The agent's invoke-evidence (`emit-invoke-evidence!`) must carry the session's current
clock (mission/campaign/excursion) — that is what makes agent turns show up in the history. Each reclock
records an audit witness `(rule . "agent-edit-activity") (source . "agent-tool-edit") (file . "…") …`,
mirroring the operator witnesses.

**Companion — the history VIEW.** The data exists once agent turns are clock-tagged; surface it: query
clock-tagged evidence → a **session↔mission timeline** ("git history for mission-session interactions",
Joe's framing). Prototyped 2026-06-14 by scanning `/api/alpha/evidence` for clock fields — promote that
to a real surface (an `/api/alpha/...` endpoint and/or an Emacs view).

**Decisions / open qs.** N/W defaults (reuse INSTANTIATE-3's 3 / 600s as a start; calibrate to agent
edit cadence); whether to also count edits to a mission's *code* files (the `file→mission` edges) or docs
only (v1: docs only); where the per-session clock store lives (registry metadata vs a dedicated
clock-store atom, durable so it survives the per-turn cold-resume); switch-not-floor confirmed (vs Rule 0).
Detection point: the invoke `:on-event` stream is where session→file is already linked — alternative is a
dedicated tool-use evidence consumer if tool-uses are persisted with file paths. **Owner:** codex (handoff,
this mission's pattern); reviewer: claude-owner (author≠reviewer). Gates: clj-kondo + check-parens +
tests; do NOT restart the JVM (dev.clj invoke path is boot-code — note next-restart; the clock-store +
evidence-tagging can reload/verify via proof-eval.sh).

## DIAGNOSIS 2026-09-21

Discovery by codex-17, requested by claude-5 for Joe. No implementation or
runtime reload in this handoff. Source inspected on master at `eca529f7`;
the shared worktree also contains unrelated edits, including mission-mode.
Service observations used GETs only, principally at 17:48–17:53 UTC.
**Auto-clock exists, but it is conditional, split across independent stores,
and incomplete across surfaces. It is not an automatic accounting contract.**

### 1. Implementation, runtime evidence, and firing receipts

Line references below are relative to futon3c unless a sibling is named.
“Present” means source exists; “observed” means a persisted receipt exists.
Neither source presence nor reflection of a Var establishes that every
running closure or buffer has that version enabled.

| Step | Code and wiring | Evidence / operational verdict |
| --- | --- | --- |
| INSTANTIATE-1: resolved mention | `emacs/agent-chat.el:1083` token extraction, `:1107` resolution, `:1231` application; operator-only call at `:2381`; witness fields at `:3025`. | **Observed working historically and yesterday**, not universal. 85 retained explicit-resolved-target witnesses since June 1; latest 2026-09-20T19:58:45.144316414Z, evidence `emacs-b76a80d7a6bf9dc7e462e13d9d731555`, M-a-wmc-scaling. A bell does not pass through this operator-turn trigger. |
| INSTANTIATE-1.1: floor guard | `agent-chat.el:1264` requires campaign, mission, and excursion all empty for bare mentions. Arrow override at `:1251` deliberately permits switching. | **Present; guard execution today not observable.** Guard rejections emit no receipt, so there is no firing count for 1.1. 16 separate explicit-switch-arrow witnesses exist; latest 2026-08-27T14:00:28.695574630Z, `emacs-7778949f1799f242048d031df5f8f547`. |
| INSTANTIATE-2: creation | `agent-chat.el:1158` creation-clock, `:1187` watcher; sibling `futon0/scripts/eoi-new:230` target, `:389`–`:391` watcher arm with source eoi-new-head. | **Present, no successful firing receipt found.** Zero creation-clock witnesses in the June-to-now Joe evidence population. Launching through another path or creating a file through an agent does not arm this launcher watcher. Zero receipts does not prove the function never ran. |
| INSTANTIATE-3: Emacs saves | `agent-chat.el:1288` document resolution, `:1332` dominance, `:1352` reclock, `:1391` save recording, `:1422` after-save hook. Threshold 3 / 600 seconds, margin 2. | **Observed June 8–13, currently unverified.** Four edit-activity receipts, latest 2026-06-13T15:30:39.366339184Z, `e-d76158b3-23f3-410d-9d6f-5f08b50b36c7`, M-smart-emacs-cursor. Only matching mission/campaign/excursion documents count. Save recording visits chat buffers (`:1408`); this is not evidence identifying which agent wrote a file. |
| INSTANTIATE-4: agent tools + explicit dispatch | `src/futon3c/agency/clock_store.clj:17`, `:59`, `:126`, `:180`, `:193`; `clock_lineage.clj:159`, `:177`; `dev/futon3c/dev.clj:1085`, `:1094`, `:1120`, Claude feeds `:3729` and `:3966`, Codex explicit dispatch `:4533`; HTTP preclock `src/futon3c/transport/http.clj:4728`, job path `:4876`, durable post-result dispatch `:4933`. | **Implemented beyond the stale “candidate” text; JVM functions loaded; partial surface coverage.** GET reflection exposes clock-store, clock-lineage, record-agent-tool-use!, record-agent-tool-details!. Durable agent-edit receipt latest 2026-09-14T23:05:37.797Z (claude-19, M-turns-first); durable dispatch receipt latest September 12, 17:34:18.250Z (codex-18, M-f11-find-production-successor). Claude Edit/Write/MultiEdit bodies feed edits; Codex exec/apply_patch has no equivalent feed. No current clock state was found in any of the 90 roster sessions. |

Joe's Emacs process started September 8 at 13:59:19. This establishes neither
buffer-local flags (`agent-chat.el:282`, `:285`) nor loaded definitions/hooks.
There is no identified GET-only Emacs inspection endpoint. Under this
handoff's no-eval/no-reload restriction, **today's exact Emacs loaded/active
state remains unverified**, including the creation timer and after-save hook.
The September 20 receipt is evidence of successful recent mention inference,
not proof about every current buffer. JVM GET reflection establishes loaded
Vars, not invocation of their callbacks today. Calling these steps all “dead”
would go beyond the evidence.

**Weekly retained receipt counts**, Monday UTC weeks, June 1 through
September 21 17:48 UTC:

- M = operator explicit-resolved-target; A = explicit-switch-arrow;
  C = creation-clock; E = Emacs edit-activity.
- D / T = currently retained durable clock/clocked-on rows whose latest
  witness is dispatch-mission-id / agent-edit-activity, grouped by
  clocked-at-ms. These are **not weekly firing totals**.

| Week starting | M | A | C | E | D (retained) | T (retained) |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| 2026-06-01 | 5 | 0 | 0 | 0 | 0 | 0 |
| 2026-06-08 | 9 | 0 | 0 | 4 | 0 | 0 |
| 2026-06-15 | 7 | 0 | 0 | 0 | 0 | 0 |
| 2026-06-22 | 11 | 0 | 0 | 0 | 0 | 0 |
| 2026-06-29 | 10 | 2 | 0 | 0 | 0 | 0 |
| 2026-07-06 | 5 | 1 | 0 | 0 | 1 | 0 |
| 2026-07-13 | 5 | 1 | 0 | 0 | 8 | 1 |
| 2026-07-20 | 5 | 0 | 0 | 0 | 1 | 1 |
| 2026-07-27 | 7 | 0 | 0 | 0 | 10 | 4 |
| 2026-08-03 | 3 | 0 | 0 | 0 | 0 | 0 |
| 2026-08-10 | 6 | 3 | 0 | 0 | 5 | 1 |
| 2026-08-17 | 4 | 2 | 0 | 0 | 20 | 0 |
| 2026-08-24 | 3 | 7 | 0 | 0 | 3 | 1 |
| 2026-08-31 | 2 | 0 | 0 | 0 | 6 | 0 |
| 2026-09-07 | 1 | 0 | 0 | 0 | 4 | 0 |
| 2026-09-14 | 2 | 0 | 0 | 0 | 0 | 1 |
| 2026-09-21 | 0 | 0 | 0 | 0 | 0 | 0 |

The operator population contains 105 witnesses: 85 M, 16 A, 0 C, 4 E.
GET `:7073/api/alpha/hyperedges?type=clock%2Fclocked-on&limit=1000&include-total=true`
returned 68 current rows with an exact count: 58 D, 9 T, and one
selection-decision (September 2; outside the four rules). Hyperedge IDs are
agent/target based and puts replace their current projection; retractions
end validity. This endpoint's current rows are not an append-only firing log.
Multiple targets still occur for some agents; do not assume one unambiguous
restoration candidate per agent.

A **complete weekly count of actual firings is not recoverable from this
audit trail**. Emacs keeps only the last witness until the next user-turn,
then clears it (`agent-chat.el:2385`); intervening firings can be overwritten
or never sent. JVM `clock_store.clj:228` repeats its last witness on subsequent
evidence without a firing ID/time or consume operation. The last-week invoke
population has 304 dispatch-witness rows and 8 agent-edit-witness rows, which
must not be reported as 304 and 8 transitions. The table's zeroes mean no
retained receipts in that population, not proven inactivity. There is no
receipt for a rejected floor check or for “no inferable mission.”

### 2. Last-seven-day coverage

Window: **[2026-09-14T17:48:00Z, 2026-09-21T17:48:00Z)**.
Cursor-paginated GETs against `:7073/api/alpha/evidence`, `limit=1000`,
`author=joe&since=2026-06-01` fetched 14,006 rows for history;
`tags=invoke&since=2026-09-14T17:48:00Z` fetched 9,164 invoke-family rows.
Applied the exact upper/lower timestamp bounds locally (the response included
a Joe row just after the requested upper bound). Decoded EDN-string bodies
as well as JSON-map bodies. “Mission” means nonempty mission-id or
clocked-mission, not merely an excursion/campaign clock.

| Recorded population / surface | Turns with mission / turns | Coverage |
| --- | ---: | ---: |
| Operator chat-turn, role=user, emacs-claude-repl | 236 / 1,369 | 17.24% |
| Operator chat-turn, role=user, emacs-codex-repl | 18 / 161 | 11.18% |
| Operator inbound, marimo | 0 / 5 | 0% |
| JVM invoke-start, Surface: bell | 87 / 906 | 9.60% |
| JVM invoke-start, Surface: emacs-repl | 57 / 1,526 | 3.74% |
| JVM invoke-start, Surface: emacs-claude-repl | 0 / 20 | 0% |
| JVM invoke-start, Surface: marimo | 0 / 18 | 0% |
| JVM invoke-start, Surface: auto-bellback | 7 / 543 | 1.29% |
| JVM invoke-start, Surface: whistle | 1 / 13 | 7.69% |

These populations measure different recording boundaries and **must not be
summed**: an operator turn can also produce an invocation, and resumed park
text can be recorded as an operator turn. emacs-repl is a prompt-envelope
surface, not a reliable model/type classifier. Other starts: matrix 41
(0 mission, 3 other clock targets), unspecified 9 (0 mission). All starts
total 3,076; completions 3,037, errors 33, retrieval events 3,018. Completions
were not counted again as turns. All requested rows with a nonempty clock
target also had a mission in this window; the three matrix rows are the
exception outside the requested surfaces.

Bell surface classification uses Surface in invoke-start prompt-preview.
This is coverage among **retained recorded starts**, not proof every actual
delivery was recorded. Joe's Marimo inbound population and JVM Marimo starts
are both shown because their denominators differ. These numbers contradict
“nothing ever clocks,” while substantiating very poor automatic coverage.

### 3. Why today's roster is nil

GET `/api/alpha/agents` at 17:48 UTC returned 90 registrations, all with
mission-id nil (67 restored, 17 idle, 6 invoking). Separate session-specific
GET `/api/alpha/agent-clock?agent-id=...&session-id=...` checks for all 90
returned empty campaign/mission/excursion and no witness. The seven named
agents—claude-3, claude-5, codex-13 through codex-17—are included. Thus this
is not merely a roster display discrepancy today.

There are three independent state locations:

1. Emacs buffer clocks. Auto paths call set-clock! with suppress-callback
   true (`agent-chat.el:1169`, `:1251`, `:1270`, `:1362`), so they do
   not directly run the metadata/session update callbacks used by manual
   clocking (`claude-repl.el:1627`, `codex-repl.el:4532`).
   Codex's dispatch payload can still forward the buffer clock
   (`codex-repl.el:3885`, `:3905`). Server synchronization
   (`agent-chat.el:877`, `:912`) applies only nonempty responses, so an
   empty server clock does not prove an empty Emacs clock.
2. JVM clock-store's private `!sessions` atom (`clock_store.clj:20`),
   keyed by agent/session, with an agent/nil fallback (`:216`). Invoke
   evidence reads this store (`dev.clj:1068`); /agent-clock reads it
   (`http.clj:6670`). It is not registry metadata.
3. Roster projection reads external-invoke fields and agent metadata
   (`registry.clj:1723`, `:1762`), **not clock-store**. Registration/
   restoration imports only supplied metadata (`http.clj:4092`–`:4142`);
   it does not infer or rehydrate a session clock.

The serving JVM PID 4018538 started at **16:11:02 UTC**, cwd canonical
futon3c, service futon3c-zone.service. Its journal reports
`[dev] agent roster restore: restored=49 attempted=49`.
Six of the seven selected agents registered around 16:11:16; codex-17's
current registration is 17:10:32. A JVM restart necessarily discards the
clock atom. `clock_lineage.clj:271` reconstitute reads durable lineage for
a view; no startup path was found restoring that result into clock-store.

**Established:** all current queried session clocks and roster mission fields
are empty; restart loses RAM clocks; subsequent unlabelled dispatches do
nothing; roster and invoke clocks use different readers.
**Not established:** that these exact sessions had clocks before 16:11 and
were cleared, rather than never acquiring one. Retained historical edges for
claude-3/5 and codex-14/17 have different session IDs and cannot justify
blindly restoring an old mission into today's session. The source plus
observations support restart loss as a mechanism, not a fabricated
before/after measurement.

Durability is also partial: `clock_lineage.clj:76` requires a resolvable
canonical substrate endpoint; `:133` skips persistence if absent and writes
asynchronously otherwise. A durable edge is not a checked, replayable session
decision stream. Roster repair alone would not fix automatic inference.

### 4. Missing inference and the first fix

**Dispatch lineage:** explicit --mission exists
(`scripts/agency_send.py:31`, `:143`), with the HTTP/dev wire points above.
No caller-clock inheritance exists in those paths. Caller and mesh provenance
are available, but they are not consulted to choose a mission. Unlabelled
dispatch from a clocked caller therefore loses an inference source that
requires no operator naming. Inheritance must use the caller's exact session
and clock at dispatch, not whichever session later occupies the agent ID.
An explicit override must be resolved/validated; currently
`clock_store.clj:193` normalizes dispatch IDs without filesystem resolution.

**Activity:** Emacs doc saves and Claude native doc edits exist. Codex
exec/apply_patch edits and associated source files do not feed clock inference.
The association substrate is already partly built:
`src/futon3c/watcher/file_ingest.clj:655` and `:696` project
mission/code-paths into code/v05/file→mission edges, invoked at `:983`;
`src/futon3c/aif/mission_delta_t.clj:39` consumes that relation elsewhere.
These edges explicitly mean **mission mentions file**, not exclusive ownership.
Neither clock-store nor Emacs's edit resolver consults them. Reuse that
relation with session-attributed file activity, conservative dominance, and
explicit ambiguity handling; do not treat a shared utility file as a unique
mission claim. A global watcher seeing a write is insufficient attribution.

**No inferable mission must be a recorded decision**, e.g.
`{:status :unclocked :reason :no-inferable-mission ...}`, with distinct
reasons for ambiguous candidates, unresolved explicit target, or unavailable
inference evidence. Include agent, exact session, turn/job ID, time,
considered sources and relevant provenance. Distinguish a known absence from
an unavailable dependency. Do not silently preserve a stale unrelated target
or invent a mission.

**Proposed first fix: one behaviour—every accepted turn has one durable,
explicit clock decision, including the negative decision.** Make this the
turn-admission accounting contract across Emacs, bell, and Marimo; use
existing resolved clock inputs for the positive case and explicit
:unclocked/reason for the otherwise case. Use a stable turn/job identity for
idempotent recording. This first handoff establishes observable completeness;
it does not claim to improve inference coverage. Caller inheritance,
agent code-file inference, and state restoration/projection are separate
subsequent behaviours. Do not combine those repairs into this first fix.

Acceptance must construct the current bad case: run a complete isolated
session through each surface, with no manual clock, no named target, no
inherited clock and no qualifying edits; finish it. Every accepted turn must
have a persisted decision, and session history must end with explicit
:unclocked/:no-inferable-mission rather than missing fields. Repeat with an
existing valid resolved clock input and expect the positive decision. Read
both cases back through a newly constructed real durable backend client;
retry the same turn and assert no duplicate decision. If recording fails,
assert it is surfaced as an accounting failure, not reported as a successful
recorded decision. Keep absence and storage failure distinct.

This is discovery only. Documentation validation: explicit-path diff review
and git diff --check; no Clojure source was changed, and no live mutation,
Emacs evaluation, JVM evaluation, reload, or deep-health request was used.

## INSTANTIATE-5 — durable clock decisions (FIX 1, 2026-09-21)

Implemented by codex-17 for claude-5's review. Registry invocation admission
now computes and verifies a durable `:clock-decision` evidence entry before
running the agent. Incoming operator `chat-turn` evidence uses the same
resolver. The HTTP stream paths and social mesh wrapper forward surface,
turn/job identity, explicit clock, and configured backend as applicable.
Marimo invocations use this same admission boundary.

Precedence is explicit resolved target, existing session clock, uniquely
attributed Claude edit activity, then `:unclocked` with `:no-source`,
`:ambiguous`, or `:unresolvable-target`. For REPL input, newly named targets
supersede the stale buffer clock carried in the payload; explicit dispatch
`--mission` remains authoritative. Explicit assignments replace the entire
clock, rather than stacking campaign/mission/excursion fields.

Activity consumes the existing Edit/Write/MultiEdit detail feed and the
canonical mission parser's `:mission/code-paths`. An unclocked session clocks
on a unique qualifying path at the tool event, without needing another user
turn; shared-path ambiguity is recorded instead of guessed. The existing
session clock takes precedence over activity, as specified in this handoff.
The decision retains source number, path/targets, agent, session, surface,
turn/job identity, phase, time, and stable decision ID. Admission and activity
are separate append-only decisions for the same turn. When the runtime first
supplies a session ID, a session-resolved decision joins it to that turn.

Only a real Futon1bBackend is accepted in production. Unit tests explicitly
bind volatile storage. RAM clock publication follows verified persistence;
older retried decisions cannot roll back a newer clock. Roster projection
reads the decision's session clock, and invoke evidence carries its decision
ID. Existing mission-graph projection remains canonical-node guarded and
uses the selected backend's URL. **FIX 2 must consider decision evidence,
including negative decisions, rather than restoring solely from old positive
hyperedges.** No startup restoration was added here.

Warm callbacks preserve the exact turn binding; existing callbacks lacking
that binding can resolve only a unique active agent/session invocation.
Ambiguous callback ownership is refused. Callback accounting failures are
retained on the invocation and surfaced when it finishes, including when the
pouch's consumer catches the original exception. No Emacs source change or
Emacs reload is required. Caller inheritance and the Codex edit feed remain
FIX 3; neither was implemented.

Validation (each namespace invoked separately):

| Namespace | Tests / assertions | Result |
| --- | ---: | --- |
| `futon3c.agency.clock-decision-test` | 8 / 39 | Pass, including the isolated real-backend slow test |
| `futon3c.agency.clock-store-test` | 7 / 16 | Pass |
| `futon3c.agency.clock-lineage-test` | 7 / 18 | Pass |
| `futon3c.agency.registry-test` | 54 / 207 | Pass |
| `futon3c.agency.invariant-test` | 12 / 28 | Pass |
| `futon3c.dev-test` | 24 / 98 | Pass |
| `futon3c.social.coordination-ledger-test` | 5 / 23 | Pass |
| `futon3c.social.whistles-test` | 12 / 42 | Pass |
| `futon3c.transport.http-test` | 128 / 654 | 43 failures, 4 errors; identical failing-test counts on baseline |

The slow test uses a real Futon1b server/XTDB node on an ephemeral loopback
port, genuine fixture mission/source files, registry invocation, and a newly
constructed client for read-back. It covers source-3 assignment after an
actual file write, negative decisions on four invocation surfaces, roster
visibility, and an operator's named target overriding a stale carried clock.
The callback case deliberately removes the dynamic turn binding to exercise
existing warm callbacks. Fast tests cover precedence, ambiguity, idempotent
replay, production volatile-store refusal before execution, and a swallowed
callback accounting error failing the session.

Commands: `clojure -M:test -n <namespace>`; the focused namespace including
its slow test used `clojure -M:test:test-all -n futon3c.agency.clock-decision-test`.
HTTP baseline used the saved pre-change source/test files first on an isolated
process's classpath; no checkout replacement or shared-runtime loading.
Both HTTP runs have the same nine failing test names and per-test counts.
clj-kondo: zero errors, the same three pre-existing dev.clj warnings on both
baseline and changed files. check-parens and explicit-path diff check pass.
No shared JVM or Emacs was mutated.

Owner reload order from the canonical checkout:
`futon3c.agency.clock-store`, `futon3c.agency.clock-lineage`,
`futon3c.agency.clock-decision`, `futon3c.agency.registry`,
`futon3c.social.coordination-ledger`, `futon3c.transport.http`, `futon3c.dev`.


## INSTANTIATE-6 — restore durable decisions (FIX 2, 2026-09-21)

Clock recovery now selects the maximum `(Instant(decided-at), decision-id)`
for each exact `(agent, session)` in durable `:clock-decision` evidence. A
latest `:unclocked` restores an empty clock and retains its decision/reason;
it cannot resurrect an older positive clock or stale roster metadata.
Live publication uses the same total order, including equal-time ID ties.
Recovery gathers the result before publishing RAM state and refuses partial
cursor results with `:clock/recovery-incomplete`.

`clock-decision/restore!` accepts the configured durable backend and an agent,
optionally a session. `restore-registered!` rebuilds every saved session for
registered agents. Bootstrap calls it after roster restoration, before opening
the HTTP listener or resuming queue drainers. Constructing an HTTP handler
with a configured backend also restores registered agents, covering client
reconstruction. A newly admitted turn whose exact session has no RAM state
restores that session before deciding; later reconnects therefore cannot erase
a saved clock by first writing a new `:no-source` decision. Unregistered
historical agents are not scanned globally; their state is recovered when
admitted, or through explicit `restore!`. Existing manual RAM clocks remain
an input to admission. Recovery does not append new evidence.

Ordering safety before and after futon1b `5d9938c`: reads are narrowed by
agent author, optional session, and the clock-decision tag. Since tags and
ephemeral exclusion are post-filters in futon1b, recovery first counts the
agent/session/time predicates **without tags and including ephemeral rows**.
A window exceeding 10,000 such rows is recursively divided by time before
any evidence page is requested. Each resulting cursor walk stays below both
the 102,400-row external-sort spill and the backend's 20-page budget. Latest
selection happens in the client, by decision time and ID, never by taking the
first returned row. A window still over the bound at millisecond resolution
fails explicitly with `:clock/recovery-dense-window`; it is not partially
restored. Recovery disables the short query cache so a new client observes
external writers immediately. The same algorithm remains valid after the
server ordering fix is deployed.

Validation, one namespace per command (baseline is pre-FIX-2 source):

| Namespace | Before | After |
| --- | --- | --- |
| `futon3c.agency.clock-decision-test` | 8 tests / 39 assertions, pass | 11 / 67, pass, including both real-backend slow tests |
| `futon3c.agency.clock-store-test` | 7 / 16, pass | 7 / 16, pass |
| `futon3c.agency.registry-test` | 54 / 207, pass | 54 / 207, pass |
| `futon3c.evidence.futon1b-backend-test` | 22 / 94, pass | 22 / 94, pass |
| `futon3c.dev.bootstrap-test` | 7 / 28, pass | 7 / 28, pass |
| `futon3c.transport.http-test` | 128 / 654; 43 failures, 4 errors | 128 / 654; 31 failures, 4 errors |

HTTP has no new failing test names/assertions: the 12 existing
`portfolio-step-returns-recommendation` failures did not reproduce in the
post-change run; the remaining baseline failures/errors reproduced. This
handoff does not claim to repair that namespace's existing failures.
Clj-kondo: zero errors/warnings on changed Clojure; check-parens: OK.

The new isolated real-Futon1b test writes a positive decision, resets RAM,
reconstructs the backend and HTTP handler, and checks both `/agent-clock` and
the roster. It then writes a later negative decision directly through the
real substrate (outside the backend's cache invalidation), repeats recovery,
and checks that both projections clear despite stale roster metadata. A
further admission after RAM teardown inherits its own recovered session
clock as source 2. Unit coverage checks reversed insertion order, equal-time
ID ties, separate sessions, bounded query subdivision, and partial-page refusal.
The FIX-1 empty-session fixture now actually uses a different session ID:
resetting RAM alone correctly no longer makes a durable session empty.

Deployment: no shared JVM or Emacs was mutated. Namespace dependency order is
`futon3c.evidence.futon1b-backend`, `futon3c.agency.clock-store`,
`futon3c.agency.clock-decision`, `futon3c.social.coordination-ledger`,
`futon3c.transport.http`, `futon3c.dev.bootstrap`. The ledger is unchanged but
imports the durable backend class. **A bare hot reload with old backend
instances is insufficient**: an isolated reload test confirmed that redefining
`Futon1bBackend` makes the old instance fail `instance?`. A cold startup creates
all clients correctly. Hot deployment must reconstruct the configured backend
and replace its captured transport configurations as well as `dev/!evidence-store`
(the existing HTTP `reconfigure-handler!` can rebuild its handler); old client
references must not remain in pending invocations or WS configurations. This
is a deployment constraint for the reviewing owner, not authorization to
restart the shared JVM. No Emacs change is needed. FIX 3 inheritance and Codex
edit activity remain untouched.

### Surface check, 2026-09-21 18:47:40Z (report only)

GET evidence with `tags=clock-decision&since=2026-09-21T18:33:00Z`
returned three entries, no continuation: **bell 2, auto-bellback 1; Emacs,
whistle and marimo 0**. The only Joe operator evidence in the same queried
window was `emacs-c2e40242acc76ca0c25cb82ae0bf4253`, turn
`claude-5-turn-42`, surface `emacs-claude-repl`, at **18:33:04.326568319Z**.
The `futon3c-zone.service` journal records that turn entering
`turn-drainer-claude-5` at **18:33:04.581487961Z** with surface `emacs-repl`.
Claude's local session transcript
`~/.claude/projects/-home-joe-code/de4c2047-bf32-4b18-bd55-8f97e94c6252.jsonl`
records that same turn issuing the seven-namespace reload at
**18:33:27.884Z**, then the probe command at 18:33:34.701Z. The first durable
probe decision is at 18:33:46.694715951Z.

Thus this sample is a turn admitted **before** its own reload, not evidence
of an Emacs surface outside accepted-turn handling. Source paths are
`emacs/claude-repl.el:1020` (POST invoke-stream),
`src/futon3c/transport/http.clj:5872` and `:5900` (queued/direct registry
invocation), and `src/futon3c/agency/registry.clj:1113` (decision admission).
Operator evidence separately uses `emacs/claude-repl.el:795` and
`src/futon3c/transport/http.clj:2998` (`operator-clock-decision!`). A configured
Emacs evidence URL pointing directly to futon1b would bypass that latter
Agency endpoint, but it would not bypass invoke admission; this sample does
not establish such a configuration. A new operator turn after reload is
needed to measure live Emacs decision coverage. No surface code was changed.

## INSTANTIATE-7a — dispatch lineage (FIX 3a, 2026-09-21)

Job creation snapshots a registered caller's positive current decision into
`:inherited-clock` in the durable job ledger. Execution uses that snapshot,
not a later caller clock. Explicit targets take precedence; otherwise an
inherited clock precedes the recipient's existing session/activity sources.
The durable decision has `:source :inherited` and evidence containing
`:caller-id` and `:caller-decision-id`. Unclocked callers supply no inheritance.
The social mesh invoke wrapper carries the same snapshot. Auto-bellbacks are
excluded at capture and decision time. Activity events subsequently use the
normal session/activity precedence, rather than repeatedly applying inheritance.

Tests: `futon3c.agency.clock-decision-test` 12 tests / 80 assertions pass
(baseline 11 / 67), including a real-backend slow test through actual job
creation and execution: caller switch while queued, explicit override, own
negative reason, and no return-clock echo. `futon3c.social.coordination-ledger-test`
5 / 23 pass (unchanged). `futon3c.transport.http-test` 128 / 654, 31 failures
and 4 errors, matching the preceding FIX-2 run. Kondo and check-parens pass.

Deployment: FIX 3 adds no records/types/protocols, but cannot independently
hot-load over today's live FIX 1: the current `clock-decision` source also
contains pending FIX 2's reference to `futon1b-backend/*query-cache-enabled*`,
which does not exist in that JVM. Loading the backend would recreate its live
record class. Therefore deploy with the already planned FIX-2 restart; do not
attempt a bare hot load of master. After that restart, FIX-3a-only namespace
reload order is `clock-decision`, `social.coordination-ledger`, `transport.http`
(all under `futon3c`). No shared JVM mutation was performed.

## INSTANTIATE-7b — Codex edit activity (FIX 3b, 2026-09-21)

The Codex CLI stream reader now consumes completed file-change receipts and
passes each witnessed path into the same `clock-decision/record-tool-use!`
activity boundary used by Claude. It understands exec NDJSON
`item.completed/file_change` and rollout `event_msg/item_completed/FileChange`,
including patches nested inside `exec`. Relative paths resolve against the
invocation cwd; absolute paths and move destinations retain their full names.
Receipt thread identity or `thread.started` supplies the session. Each CLI
invocation captures its admitted turn context, and item/path IDs make duplicate
receipts idempotent. Proposed patches, failed receipts and command text do not
assert writes. Shell commands without file-change receipts supply no witnessed
path; repository-wide mtime changes are not attributed to an individual agent.

Storage failures remain on the admitted turn and in the CLI error result while
the reader continues draining stdout/stderr; losing a clock write cannot become
a successful session merely because an event callback caught an exception.
No new record, type or protocol is introduced, and existing Codex invoke
factories reach the stream reader without an Emacs change.

The provenance-documented fixture `test/fixtures/codex/exec-apply-patch.jsonl`
is a real September 21 rollout excerpt, with only the edited path and receipt
session ID substituted for an isolated test tree. The slow test runs an actual
subprocess that writes that path and emits the captured events, through the
production Codex stream reader inside registry admission. Starting unclocked,
it ends clocked to the declared mission with source 3 and the exact path;
a reconstructed real Futon1bBackend reads that decision. A duplicated receipt
produces one activity decision. Fixture JavaScript is data, never executed.

Validation, one namespace per invocation:

- `futon3c.agency.clock-decision-test`: 13 tests / 90 assertions pass, including
  all four real-backend slow tests (3a baseline: 12 / 80).
- `futon3c.agents.codex-cli-test`: 21 / 110 pass, unchanged from baseline.
- `futon3c.agents.codex-activity-test`: new namespace, 2 / 8 pass; completion
  status, proposed calls, both receipt shapes, absolute map keys and moves.
- Clj-kondo: zero errors/warnings; check-parens: OK.

Deploy together with pending FIX 2 and 3a at the planned restart, for the
backend-class reason documented above. Once FIX 2 is running, the class-safe
FIX-3 reload order is `futon3c.agency.clock-decision`,
`futon3c.social.coordination-ledger`, `futon3c.transport.http`,
`futon3c.agents.codex-activity`, `futon3c.agents.codex-cli`. No `dev` or Emacs
reload is required for the Codex consumer. The shared JVM was not mutated.

## Top-level catalog correction (2026-09-21)

The clock catalog now includes direct `holes/[CME]-*.md` children, while
preserving its recursive missions/campaigns/excursions scans. The added intake
uses the public `mission-doc-path?`, `excursion-doc-path?`, and
`campaign-doc-path?` predicates from `futon3c.watcher.file-ingest`. Mission
control obtains top-level documents from this watcher-fed substrate inventory
(`mission_control_backend.clj:989`); its filesystem fallback at line 828 is
still missions-directory-only. No new independent top-level naming rule was
introduced. Duplicate IDs remain ambiguous; unrelated directories directly
under holes are not recursively scanned by this addition.

The regression covers a top-level M-foo, nested M-bar, ignored archive/M-hidden,
and a duplicate M-foo added after the catalog is cached. The real
`/home/joe/code/futon2/holes/E-operator-as-attached-agent.md` resolves as source 1
when the canonical futon2 root is explicitly configured. The changed clock
namespace contains no record/type/protocol definitions; hot-load only
`futon3c.agency.clock-decision`. No backend or watcher reload is required.

**Separate deployment blocker found:** `/proc/392550/environ` shows the running
JVM's FUTON3C_REPOS contains futon3c, futon3b, futon3a, futon5, futon3, futon4,
and futon6, but not futon2 (nor futon0/futon1b). Reloading this catalog alone
therefore cannot resolve the requested futon2 excursion. The owner must add
futon2 to the configured roots as well. Unconfigured sibling auto-discovery
instead includes worktree copies: the first real-default-roots test correctly
refused this excursion as ambiguous. The real-file test explicitly supplies
its canonical futon2 root; production discovery/ambiguity rules were not
weakened to make it pass. Runtime/configuration was not modified in this handoff.

Validation: `clojure -M:test:test-all -n futon3c.agency.clock-decision-test`:
15 tests / 100 assertions pass (baseline 13 / 90 pass), including the existing
real-backend slow tests and the real-file check. Clj-kondo: zero errors/warnings;
check-parens: OK on both changed Clojure files.

## INSTANTIATE-8 — kimi seats work per named target (claude-11, 2026-09-24)

Every kimi seat kept one conversation across all its dispatches, so jobs
opened at 166k-335k tokens of other work and exhausted Kimi's 5-hour quota
twice on 2026-09-24 (`holes/labs/kimi-5h-limit-2026-09-24.md`).

**Joe's rule:** to do work on a Kimi seat, pass the mission, excursion or
ticket name; if it changes, compact.

A first cut (`65b1f708`) inferred the target from the job's clock decision
and only cleared above a token floor. Joe rejected that: the target must be
passed, not inferred. It was replaced by:

- `agency_send.py --target M-*|E-*|T-*` → payload `work-target`. The server
  also accepts `ticket-id`, `excursion-id` and `mission-id`, in that order
  after `work-target` (`http.clj` `payload-work-target`). An `M-*` target
  also sets `mission-id`, so the recipient is clocked as before.
- The target travels explicitly through every job path (bell, whistle,
  whistle-stream, invoke, invoke-stream, announce→activate) into the seat's
  invoke context with the caller. It is also stored on the job record.
- `zai-api/context-carry-decision` with kimi's `{:cap-tokens 128000}`:
  - no target → refused before any model call (`:work-target-required`);
  - a name that is not `holes/**/<name>.md` in a canonical futon repo →
    refused (`:work-target-unresolved`);
  - a different target from the one that built the conversation → cleared
    (`:target-change`);
  - the same target → kept, unless it has grown past the cap (`:over-cap`).
  - Replies to the seat's own bells (`auto-bellback`) and park resumes carry
    no target and continue the seat's current one.
- "Compact" is implemented as clear: a conversation about another target is
  not summarised into the next one. The new prompt says what was cleared.
  Each clear writes a `:context-compaction` evidence record.

zai seats have no policy and are unchanged.

**Clock as the default target (Joe, 2026-09-24).** "The calling agent should
clock in on something and then just send that as its work target." A job
with no named target now takes the caller's clock at dispatch time
(`:inherited-clock`, INSTANTIATE-7a): its excursion if set, else its mission.
The seat's own clock is never used, because it is left over from its
previous job and would never change. With neither a named target nor a
clock, the job is refused and the caller's current session gets a typed
followup (`:kimi-work-target`, through the inbox-zero followup queue, one
outstanding per session): "You can't use a Kimi seat without a work
target…".

**Open: clock drift.** This makes the gate only as good as the caller's
clock. Seen live at 16:20Z: claude-10 was clocked to M-futon-seams by a
mention in its operator turn, and claude-1 was clocked to M-futon-seams
*by inheritance* from a claude-10 dispatch. Every kimi call claude-1 makes
(per-turn analysis included) would therefore carry M-futon-seams until
something reclocks it. A clock that doesn't follow the work means the same
target is sent while the topic moves on, so the conversation is never
cleared. The 128k cap is the only backstop for that until reclocking tracks
the work.
