# Excursion: Night Shift

**Type:** Excursion (E-prefix; bounded scope-out from a mission; owned end-to-end by a single hand-off agent; see [[project_e_prefix_excursions]] for the convention).
**Status:** PARTIALLY EXECUTED by Codex on 2026-05-25. The envelope, registry/spec wiring, invariant tests, and spike-check are landed; pilot-side hop integration and a live operator-reviewed PR demonstration remain open.
**Date:** 2026-05-25
**Author:** claude-1 (inhabiting `:war-machine-pilot` peripheral; emacs-repl surface paired with Joe).
**End-to-end owner:** Codex.
**Sibling excursions (also out-of-band from M-war-machine-pilot):**
- `E-street-sweeper.md` (claude-2-owned; working-tree commit hygiene)
- `E-pilot-vsatarcs-feed.md` (codex-2-owned, EXECUTED 2026-05-25; substrate auto-feed to VSATARCS)

**Parent mission:** `futon3c/holes/missions/M-war-machine-pilot.md` (v1 cycle, claude-1 inhabitant).
**Surface that authored this:** emacs-repl 2026-05-25 between claude-1 and Joe.

## Why this exists (Joe, emacs-repl 2026-05-25)

Investigating the WM's `→ Close 🐜6` recommendation surfaced a **two-layer action gap**:

- **(a) Step the PI loop itself** — `POST /api/alpha/portfolio/step` with an agenda payload.  Within the pilot's envelope (or one-line extension of `:wm-api-query` from GET to POST).  The pilot can drive this directly.
- **(b) Apply whatever PI then recommends** — code/file changes across the stack to actually close the agendas PI ranks.  **Not in any current peripheral's envelope.**  And critically, code modification is the capability class where structural risk is highest — wrong move overwrites operator work or destabilises the running system.

Joe's reframe (emacs-repl 2026-05-25): *"are we going to hand the actual fix off to some other agent, or what?  My guess is that similar to E-street-sweeper we need some other 'role' for code fixes.  And, like with E-street-sweeper we need some invariants.  An obvious one is to put new work into a branch, in such a way that it's guaranteed non-destructive to the existing system.  That way the Operator (that's me) can review PRs — presumably with the assistance of an agent — in a standard interactive round.  But we can just put the pedal to the metal and fix things safely in branches with gay abandon overnight or whatever."*

The pattern: **WM identifies → pilot recognises → E-excursion grants a constrained capability class with structural safety invariants that make broader trust well-grounded.**  Joe's "I broadly trust claude or codex to deal appropriately" + "but we need some invariants" is exactly the pattern E-street-sweeper realised for working-tree hygiene; this excursion realises it for code modification.

## What "Night Shift" is

A new peripheral, provisionally named **`:night-shift`**, that grants safe code-modification capabilities to an inhabiting agent.  The pilot (or any agent reaching a code-fix-class task surfaced by PI / WM / a mission) hops into `:night-shift` to perform the work; output is a PR (draft or ready-for-review), not a merge.  Operator merges (possibly with assistance from a sibling review-agent).

The "night-shift" name captures three load-bearing properties:

- **Asynchronous-from-operator** — the agent can work while the operator is asleep, in a meeting, or otherwise unavailable; nothing blocks on real-time operator presence
- **Bounded-by-shift** — each "shift" is a single agenda → single branch → single PR; the agent's work is finite and reviewable rather than open-ended
- **Hand-off-cleanly** — the next shift inherits via the operator's morning review, not via uncommitted in-memory state

Joe's exact phrasing (canonical): *"put the pedal to the metal and fix things safely in branches with gay abandon overnight or whatever."*  Branch isolation is what makes the pedal-to-the-metal safe.

## Execution outcome (Codex, 2026-05-25)

- Landed the new peripheral files at `futon3c/src/futon3c/peripheral/night_shift.clj`, `night_shift_backend.clj`, and `night_shift_shapes.clj`.
- Added `:night-shift` to the runtime registry, the social `PeripheralId` shape, and `resources/peripherals.edn`.
- Implemented a strict git/PR backend rather than a generic `bash-git` wrapper.  Unsafe operations remain structurally unreachable because there is no merge tool, no force path, no rebase/amend path, and no generic shell tunnel inside the envelope.
- Added a **frame-provision** path modeled on the older futon6 proof-frame discipline: night-shift can now create an isolated frame container with metadata receipts plus a clean git worktree checkout under that frame, instead of asking callers to operate directly from a dirty shared checkout.
- Wrote a dedicated invariant suite at `test/futon3c/peripheral/night_shift_test.clj`.  The suite exercises branch-name rejection, protected-branch rejection, dirty-worktree refusal, one-branch-per-shift binding, no-cross-repo, secret-pattern blocking, commit-trailer provenance, ready-for-review test gating, force-push refusal, and discard-list bell emission.
- `spike-check` now returns `{:valid-config? true :make-night-shift-ok? true ...}`.
- Provisioned one real frame on `futon3c` to prove the path against the live dirty checkout: branch `e-night-shift/wm-night-shift-demo/bootstrap`, frame root `/home/joe/code/futon3c/.state/night-shift-frames/wm-night-shift-demo/e-night-shift-wm-night-shift-demo-bootstrap`, checkout root `/home/joe/code/futon3c/.state/night-shift-frames/wm-night-shift-demo/e-night-shift-wm-night-shift-demo-bootstrap/checkout`.  The source checkout stayed dirty; the frame checkout came up clean on the feature branch.
- The excursion itself also closed the open live-event criterion from `E-pilot-vsatarcs-feed`: `E-night-shift.md` was ingested into canonical VSATARCS as `hx:vsatarcs-align:auto:pilot-e-night-shift-excursion`.

## Open edges after landing

- **No special pilot-hop features are required.**  The generic hop discipline is sufficient; `:night-shift` does not need a pilot-only transition mechanism.
- **A real remote PR demonstration is still open, but the dirty-base blocker is gone.**  The live repo is dirty on `master`, and the envelope correctly refuses direct branching there; the new frame-provision path solves that by giving night-shift a clean isolated checkout.  The remaining choice is simply which concrete low-risk agenda should become the first pushed draft PR from that frame.
- **Discard-list bell emission is covered in tests, but not yet wired into a long-running night-shift service loop.**  v0 has the tool; scheduling it is a follow-on operational decision.

## Trigger conditions

`:night-shift` is hopped into when an inhabiting agent (today: `:war-machine-pilot`) reaches a code-fix-class agenda.  Initial triggers:

1. **PI step output** with a code-fix recommendation (e.g., "implement HGO spec", "wire portfolio-poll cadence").  PI's `:ranked-actions` carries the candidate; night-shift inhabits it.
2. **Open `M-*.md` mission** with phase `:INSTANTIATE` and a known file-list to modify.
3. **Pilot-discovered code gaps** (e.g., my `:st/envelope-needs-substrate-file-create-tool` finding earlier today; the kind of thing that gets surfaced and then sits because no agent has the envelope to address it).
4. **Operator-directed code task** explicitly belled to night-shift via Agency.

The trigger is not "metabolic pressure" (that's E-street-sweeper's territory) and is not "evidence drift into VSATARCS" (codex-2's E-pilot-vsatarcs-feed's territory).  It is specifically "code change needed; safety required."

## Night-shift envelope (spec for codex-2 to refine)

### Allowed actions (substantive — Pilot-I1-analogue cg-id required per action)

| Tool | Purpose | Scope | Safety invariants |
|---|---|---|---|
| `:frame-provision` | Create an isolated night-shift frame container plus clean git worktree checkout for one agenda | one repo, one frame, one branch per consent-gate | branch-name still MUST match `^e-night-shift/[^/]+/[^/]+$`; source dirty checkout allowed because the work happens in the fresh frame checkout; frame root collision rejected |
| `:repo-scan` | Read `git status`, `git branch -l`, `git log -<N>` for one repo | one repo at a time | read-only |
| `:branch-create` | `git checkout -b <branch-name>` from a base ref (default: current main/master/trunk equivalent) | one repo, one branch per consent-gate | branch-name MUST match `^e-night-shift/[^/]+/[^/]+$` pattern; reject if branch already exists |
| `:read-file` | `cat <path>` (read repo files for context) | one repo at a time | read-only |
| `:edit-file` | Narrow `:replace-first` / `:replace-all-occurrences` edits to an existing tracked file on the active feature branch | active feature branch only; rejects if HEAD is on main/master/trunk | structured op (mirror E-street-sweeper's narrow-and-auditable design); never `:overwrite-file`-class |
| `:create-file` | Create a new file under tracked-or-trackable path on the active feature branch | active feature branch only | path must not match `.gitignore`; rejects secret patterns |
| `:repo-stage` | `git add <file1> <file2>` for listed files (inherits E-street-sweeper's secret-pattern blacklist) | active feature branch only | same secret-pattern reject set as street-sweeper |
| `:repo-commit` | `git commit -m <message>` with trailers | active feature branch only | message must include cited cg-id, source-agenda-id, recommendation-source trailers (see §Commit-message provenance) |
| `:run-tests` | Invoke the repo's standard test command (read from a `:test-cmd` field in repo metadata; e.g., `clojure -X:test`) | active feature branch only | timeout-bounded; result captured into the commit context |
| `:push-feature-branch` | `git push -u origin <branch-name>` for the current feature branch ONLY | active feature branch; never main/master/trunk | rejected for any branch matching `^(main|master|trunk|develop|prod|production)$` |
| `:pr-create` | Open a PR via `gh pr create` (or repo's PR API) | feature branch → main/master/trunk | draft-by-default; ready-for-review requires explicit consent-gate field |

### Read-only ambient tools (no cg-id required)

| Tool | Purpose |
|---|---|
| `:list-agendas` | Read PI ranked-actions / mission INSTANTIATE-phase lists / open substrate-threads; produce candidate work-orders |
| `:bell-emit` | Same as war-machine-pilot's; signal operator-side claude / claude-1 etc. |
| `:hop-in` / `:hop-out` | Agent-identity-preserving transition to/from `:war-machine-pilot` (or wherever the agent hopped from) |

### Hard structural invariants (PROTOCOL — enforced by envelope code, NOT by agent discipline)

These are the guardrails that justify the broader trust.  Each is structural (envelope refuses requests that violate them) rather than disciplinary (relying on the agent to behave).

1. **Branch isolation.**  All file-modifying actions (`:edit-file`, `:create-file`, `:repo-stage`, `:repo-commit`, `:push-feature-branch`) are only valid when `HEAD` is on a feature branch matching `^e-night-shift/[^/]+/[^/]+$`.  Operations on main/master/trunk/develop/prod/production are structurally unreachable.
2. **PR-as-deliverable.**  No merge tool exists.  The merge button stays operator-only.  `:pr-create` is the terminal action of a night-shift cycle.
3. **No force operations.**  `--force` / `--force-with-lease` / `push --delete` / `branch -D` / `reset --hard` / `clean -fd` structurally unreachable.
4. **One agenda → one branch → one PR.**  A single `:consent-gate-event-id` authorises actions on exactly one branch.  Bundling multiple agendas into one PR requires explicit `:bundle-rationale` field in the consent-gate-emit payload with operator-set justification.
5. **Test/hook integrity.**  Pre-commit hooks run (never `--no-verify`).  Failing tests block the `:pr-create` transition to ready-for-review; draft PRs allowed with explicit `:tests-failing-context` annotation.
6. **No cross-repo atomic ops.**  One repo at a time per consent-gate.  Multi-repo refactors require multiple PRs.
7. **Commit-message provenance.**  Every `:repo-commit` produces a commit message ending with these trailers:
   ```
   Source-Agenda: <agenda-id>
   Source-Recommendation: <WM/PI/mission citation>
   Consent-Gate: <cg-id>
   Co-Authored-By: <agent-id> via :night-shift <noreply@anthropic.com>
   ```
8. **Inherited from E-street-sweeper.**  Secret-pattern blacklist (`.env`, `.envrc`, `*.pem`, `*credentials*`, `.admintoken`, `*.key`); 50-file commit cap (tunable per cg with `:commit-cap-override` + rationale); no `git config` writes.
9. **Operator-can-always-discard.**  Branches matching `^e-night-shift/.*` older than N days (configurable; default 14) with no PR-acknowledgement OR with PR closed-without-merge get marked for reaping (envelope DOES NOT auto-delete; emits a bell to the operator with a "discard-candidates" list).  Prevents the branch space from accumulating stale agent work indefinitely while keeping discard authority operator-side.
10. **No-rebase / no-amend in v0.**  Operator-judgement-dense; v0 commits only with no history rewriting.  Amend/rebase land in v1 if the operator wants them.
11. **Operator-branch protection.**  If the operator already has a branch matching the proposed `e-night-shift/<agenda-id>/...` name, the envelope refuses and bells the operator.  Never overwrite operator-touched branches.

### Branch-naming convention

```
e-night-shift/<agenda-id>/<short-slug>

Examples:
  e-night-shift/wm.close-s6.v1/step-pi-with-stack-observation
  e-night-shift/M-symbol-grounding/instantiate-paper-A
  e-night-shift/st-envelope-substrate-file-create-tool/add-tool
```

The `<agenda-id>` is the upstream source (PI agenda id, mission id, substrate-thread id, or operator-directed slug).  The `<short-slug>` is a human-readable summary, kebab-case, ≤40 chars.

Branch naming is structural: the envelope rejects any branch creation that doesn't match the pattern.  This makes the origin of work grep-able (`git branch -a | grep e-night-shift/`) and reapable.

### Consent-gate density

Per the existing pattern in E-street-sweeper: in v0 of `:night-shift`, **every substantive action requires its own consent-gate-emit**.  Code modification is operator-judgement-dense in the same way commit hygiene is; per-action cg-ids leave a per-edit provenance trail in the bell log.

Once v0 settles, an operator-approved-plan mode (one consent-gate-emit authorises a multi-step shift) becomes a v1 question.  Cadence loosening should require evidence (multiple safe shifts; per-step gate-density not surfacing safety failures) — same discipline as elsewhere.

## Hop protocol (war-machine-pilot ⇄ night-shift)

Same agent identity per futon3c invariant I-3.  The hop protocol mirrors E-street-sweeper's design (referenced there with three candidate paths; codex-2 picks one or refines).

Hop-into payload schema:
```clojure
{:hop-from     :war-machine-pilot
 :hop-into     :night-shift
 :hop-cause    :code-fix-class-agenda
 :hop-target   {:repo            "<repo-name-or-path>"
                :agenda-id       "wm.close-s6.v1"  ;; or M-<mission>, or st/<thread>, etc.
                :source-recommendation  "<PI/WM/mission citation>"
                :proposed-branch "e-night-shift/<agenda-id>/<short-slug>"
                :scope-hint      {:files [...]
                                  :spec  "..."}}
 :hop-cg-id    "cg-<uuid>"                         ;; consent-gate authorising the hop
 :return-to    :war-machine-pilot}
```

Hop-back payload schema:
```clojure
{:hop-from   :night-shift
 :hop-into   :war-machine-pilot
 :hop-result {:branch         "e-night-shift/wm.close-s6.v1/step-pi-with-stack-observation"
              :commits        [{:sha "..." :message "..." :cg-id "..."}]
              :tests          {:ran? true :result :pass}
              :pr-url         "https://github.com/.../pull/N"  ;; or local-pr-id if not yet pushed
              :pr-status      :draft  ;; or :ready-for-review
              :files-changed  [...]
              :unaddressed    [{:reason "..."}]}}
```

## Integration with M-war-machine-pilot (claude-1 side, will land when night-shift is callable)

- `core.cljs` — when a PI ranked-action is code-fix-class, render a "Hop to night-shift" call-to-action; clicking fires the hop (operator-confirmed via consent-gate prompt).
- `pilot-inhabitations.edn` — add `:event :hop-in` / `:hop-out` entries when the agent transitions; record the agenda-id + branch + PR URL for the inhabitations log.
- `pilot-inhabitations.edn :open-substrate-threads` — add `:st/hop-protocol-into-night-shift` (mirror the existing entry for E-street-sweeper).

claude-1 (or whoever's currently inhabiting `:war-machine-pilot`) will add these once codex-2's envelope is callable.  Not codex-2's territory.

## Operator-review affordance (separate excursion if it grows)

Joe (emacs-repl 2026-05-25) named: *"reviews PRs — presumably with the assistance of an agent — in a standard interactive round."*

This is **out of scope for E-night-shift** but worth naming as a follow-on possibility:

- **Inline-comment-review-agent** — a sibling agent that reads the PR diff and posts inline comments (concerns, questions, approval signals) to assist the operator's review
- **Pre-flight-summarise-agent** — an agent that writes a structured PR description: what changed, why, what tests cover it, how to verify, what risks remain

Either could be a sibling excursion (e.g., `E-pr-review-assist`).  Codex-2 should NOT build this as part of night-shift; flag it as a candidate excursion if it surfaces during the build.

## Success criteria

1. **Envelope file** exists at `futon3c/src/futon3c/peripheral/night_shift.clj` (+ `*_shapes.clj` + `*_backend.clj`), mirroring `war_machine_pilot.clj`.
2. **Spike test passes**: `(ns/spike-check)` returns `{:valid-config? true :make-night-shift-ok? true ...}`.
3. **All 11 hard structural invariants enforced** — tests in `test/futon3c/peripheral/night_shift_test.clj` cover each:
   - branch-isolation (operations on main rejected)
   - PR-as-deliverable (no merge tool)
   - no-force (--force unreachable)
   - one-cg-one-branch (cross-branch reject)
   - test/hook integrity (--no-verify unreachable; failing tests block ready-for-review)
   - no-cross-repo (multi-repo reject)
   - commit-message provenance (trailers required)
   - secret-pattern blacklist (inherits E-street-sweeper tests)
   - operator-discard (stale-branch list emitted, never auto-deleted)
   - no-rebase/amend
   - operator-branch protection (collision reject)
4. **One real PR landed** via the envelope on a test agenda (codex-2 picks; could be a small documentation cleanup, a typo fix, or implementing one of the open substrate-threads listed in `pilot-inhabitations.edn`).  The PR's commit trailers cite the right cg-id + agenda-id + source-recommendation.
5. **Branch-naming reject works**: attempting to create a branch outside the `^e-night-shift/[^/]+/[^/]+$` pattern fails with a structured `:envelope-rejected` error.
6. **Hop protocol round-trips**: at least one (war-machine-pilot → hop → night-shift → action → hop-back → war-machine-pilot) cycle recorded in `pilot-inhabitations.edn` with `:hop-in` / `:hop-out` events.
7. **Discard-list bell**: with a stale `e-night-shift/...` branch present, running the reap-check emits a structured bell listing it; never auto-deletes.
8. **Bell-back to claude-1** when ready, so claude-1 can wire the hop-trigger UI in core.cljs and add the `:st/hop-protocol-into-night-shift` thread to pilot-inhabitations.edn.

## What codex-2 owns end-to-end

- Refine the allowed-action shape; pick hop protocol implementation; refine the safety invariants if codex-2 sees gaps
- Write the .clj files; tests; spike-check; agency.registry integration
- Drawbridge `(load-file ...)` to hot-reload
- Pick a test agenda; do one real PR demonstration end-to-end with commit-message provenance
- Bell claude-1 when ready
- If a sibling excursion surfaces (e.g., `E-pr-review-assist`), draft a stub `.md` and surface it — but do NOT build it

## What codex-2 does NOT own

- **The WM-I4 / WM-I5 vocabulary additions** — claude-2 / claude-4 territory (vocabulary canonicalisation)
- **Multi-repo refactors** — out of scope for v0
- **Rebase / amend / branch surgery** — out of scope for v0
- **Merges** — operator-only forever; the envelope never grows a merge tool
- **Operator-review-assist agents** — `E-pr-review-assist` (or analogous) is a separate excursion
- **The hop-trigger UI on the war-machine-pilot side** — claude-1's territory (mirrors the E-street-sweeper integration discipline)
- **Direct touches to operator-owned branches** — the operator-branch protection invariant makes this structurally unreachable

## Cross-references

- `M-war-machine-pilot.md` — parent mission; the inhabitation log this excursion's hops will appear in
- `E-street-sweeper.md` — sibling excursion (claude-2-owned); shares the secret-pattern blacklist + commit-cap discipline (cite where the night-shift envelope inherits from it)
- `E-pilot-vsatarcs-feed.md` — sibling excursion (codex-2-owned, EXECUTED); the open "next live event" criterion of that excursion is auto-satisfied when this file lands (new E-*.md under `holes/missions/` hits the feeder's mission-doc scanner)
- `futon5a/data/pilot-inhabitations.edn` — log substrate; hop events land here
- `futon3c/src/futon3c/peripheral/war_machine_pilot{,_shapes,_backend}.clj` — clone-from shape donor
- `futon3c/src/futon3c/util/edn_comment_preserving.clj` — for any EDN substrate writes night-shift makes outside the active branch (rare; mostly via `git`-shell, but include)
- `futon3c/library/realtime/` patterns — for hop-as-transport-pivot and consent-gate-as-substantive-discipline patterns
- `futon3c/README-peripherals.md` — peripheral pattern; constrained capability envelope
- `futon3c/README-bells-and-whistles.md` — inter-agent communication transport
- `M-peripheral-gauntlet.md` — boundary-property tests; applies to night-shift

## Pattern emerging (worth memory)

With this excursion landing, the E-prefix pattern now shows a clear shape: **WM identifies need → pilot recognises code-class action → E-excursion grants a constrained capability class with structural safety invariants that make broader trust well-grounded.**  Each excursion adds a row to the capability lattice:

| Excursion | Capability class | Core invariant | Owner |
|---|---|---|---|
| `E-street-sweeper` | working-tree commits | secret-pattern blacklist + commit cap | claude-2 |
| `E-pilot-vsatarcs-feed` | substrate sync to VSATARCS | idempotency + no-overwrite-of-human-entries | codex-2 (EXECUTED) |
| `E-night-shift` | code modification | branch isolation + PR-as-deliverable | codex-2 (proposed) |

claude-1 to canonicalise this pattern in `[[project_e_prefix_excursions]]` memory after this excursion lands.

## Provenance

- Operator framing: Joe via emacs-repl, 2026-05-25
- Excursion authored: claude-1 via emacs-repl, 2026-05-25, while inhabiting `:war-machine-pilot` in cycle investigating PI step / S6 closure
- E-prefix convention: see `[[project_e_prefix_excursions]]` and the sibling excursions
