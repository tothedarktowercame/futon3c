# Q-21 Guidance Note: Hinge vs Block — Relationship and In-Flight Block Identification

**Mission:** M-bounded-in-flight-state
**Resolves:** Q-21 (the open item the 2026-05-04 reopen names)
**Status:** guidance note — conceptual calibration for operators and the block-detection pipeline.

## Thereopen's actual question

The 2026-05-04 reopen (second review) named Q-21 after the G-1/G-2 fix landed:

> A Block (one revolution of the futonic loop, per `block-as-futonic-revolution`) and a *hinge* (Cook-Ting, per `cook-ting`) are at different scales: hinges are cognitive pivot points where the structure of a problem changes; Blocks are work-units that revolve through the futonic loop and may sit inside one hinge, span hinges, or run in parallel across missions.
>
> Open: a guidance-note clarifying the hinge-vs-Block relationship and the "in-flight Block (identified but uncommitted)" status that the current Block-detection pipeline cannot see (Block: trailers only exist on commits).

This note answers both parts.

## 1. Hinge vs Block: different scales

A **hinge** is a *cognitive pivot point* — a structural slot where the mission's direction can shift. It is not scheduled; it is *recognised* (`cook-ting`: "find the joints already in the structure; do not cut through gristle"). Hinges are the slots where perception happens: the operator sees that the problem's structure has changed and a decision is now load-bearing. The `hinge-point` pattern names what makes a hinge: a point where direction is contingent on operator state or external input that wasn't available at design time, where proceeding without reflection would risk collapsing a typed void prematurely.

A **Block** is a *work-unit* — one full revolution of the futonic loop (象→部→咅→鹽→香→🔮→未知→味), from perception through articulation to committed closure. A Block is the smallest unit the operator can attest to and the system can record durably. It has five phases: identification (perceive the hinge), specification (name the included changes), elaboration (articulate the shape), passion (do the work), closure (the commit lands).

**The relationship is many-to-many:**
- A Block closes *at* a hinge (identification = perceiving the hinge; closure = the commit that records the resolution). Per `block-as-futonic-revolution`: "block boundaries are recognised at hinges, not scheduled."
- But multiple Blocks can close *within one hinge* — e.g. the 2026-05-03 futon3c sweep produced 9 commits inside one conceptual "clear all WIP" block, all at the same hinge.
- A Block can *span* hinges if the work is genuinely large (an excursion-block crossing multiple mission phases).
- Blocks can run *in parallel* across missions: Joe's calibration (Q-21) is that a session can have multiple identified Blocks in flight at once (IDENTIFY for one mission + MAP for another) before any commits.

**The pressure function does not create hinges** — it reveals that they are nearby or overdue. `metabolic_balance.clj`'s `compute-channel-pressure` computes `max(count/N, age/D, bytes/B)` and maps it to tiers (silent/advisory/high/stop-the-line). The tier system IS the hinge-proximity signal: advisory means a hinge is approaching, high means a hinge is overdue, stop-the-line means a hinge has clearly been *missed*. Per MAP-1b: "Stop-the-line tier should fire only when an obvious hinge has been missed — not to manufacture one."

## 2. In-flight Blocks: the pipeline blind spot

The current Block-detection pipeline can only see *closed* Blocks — those with `Block:` footers on commits. `commit_ingest.clj`'s `parse-block-trailer` extracts a `{:tag :kind :ymd :slug}` map from the commit body via the regex `#"^\s*Block:\s+([a-z][a-z0-9-]*?)-(\d{4}-\d{2}-\d{2})-([A-Za-z0-9._-]+)\s*$"`. This fires only on committed work — the closure phase (phase 5).

**An in-flight Block** is one that has been *identified* (the operator perceives a hinge and names the work to be done) but has not yet *closed* (no commit with a Block: footer exists). The pipeline cannot see these because they exist only in the operator's working state — the uncommitted edits, the half-written mission section, the session with an active task.

This is the same blind spot the war-machine section rename addressed: "Blocks in Flight" was renamed to "Block Closures" because the war-machine surface can only show closed Blocks (visible via commit footers), not in-flight ones.

### How to identify in-flight Blocks today

The in-flight Block is not directly visible, but its *symptoms* are — through the metabolic-balance pressure function:

1. **Working-tree pressure** (`metabolic_balance.clj` `check-working-tree-pressure`): uncommitted edits past natural hinge-cadence produce advisory/high/stop-the-line readings. Each such reading implies an in-flight Block: the operator has started work (identification + specification + possibly passion) but hasn't closed (no commit).

2. **Disposition entries** (`.futon-disposition.edn`, parsed by `disposition_edn.clj`): an active disposition entry with a `:review-by` date declares "this work is intentionally in-flight with this reasoning." This is the operator's explicit identification of an in-flight Block that they have chosen not to close yet.

3. **Session activity**: an active agent session (visible via the agency registry / `/agency/connected`) with uncommitted edits in its working tree is an in-flight Block at the session grain. Joe's calibration is that multiple sessions can each carry their own in-flight Block.

### What the pipeline still needs (the honest gap)

The commit-footer pipeline (`commit_ingest.clj` `parse-block-trailer` → `code/v05/block-trailer` edge → mana credit via `credit-block-mana!`) is **close-only** by construction. Detecting in-flight Blocks requires a *pre-commit identification step* — a surface where the operator (or agent) declares "I am starting Block X" before the commit lands. This is explicitly named as remaining work in the Q-21 resolution scope: the 2026-05-04 reopen says in-flight Blocks "require a separate identification step per Q-21."

The disposition surface (`.futon-disposition.edn`) is the closest existing mechanism: it already declares in-flight work with reasoning. But it is opt-in and file-based, not wired to the Block lifecycle. A future enhancement would bridge disposition entries to the Block schema, so an in-flight Block can be tracked from identification through closure — but that is beyond this guidance note.

## Citations

**Shipped code:**
- `futon3c/src/futon3c/logic/metabolic_balance.clj` — `compute-channel-pressure` (the `max(count/N, age/D, bytes/B)` pressure function, ARGUE-3); `pressure->tier` (silent/advisory/high/stop-the-line tier mapping); `check-working-tree-pressure` (the probe check-fn factory); `tier->outcome` (:ok for silent/advisory, :violation for high/stop-the-line); `default-working-tree-nominals` ({:N-count 20, :D-age-days 7, :B-bytes 10485760}, calibrated against V-1 anchors).
- `futon3c/src/futon3c/watcher/commit_ingest.clj` — `parse-block-trailer` (Block: footer regex extraction → `{:tag :kind :ymd :slug}`); `block-footer-pattern` (`#"(?m)^\s*Block:\s+([a-z][a-z0-9-]*?)-(\d{4}-\d{2}-\d{2})-([A-Za-z0-9._-]+)\s*$"`); `list-commits` (git log walk with Block: detection); the ingestion loop posts `code/v05/block-trailer` edges and calls `credit-block-mana!` for session-attributed mana (the G-1/G-2 fix).

**Pattern canon:**
- `futon3/library/structure/cook-ting.flexiarg` — "find the joints already in the structure; do not cut through gristle"; the blade-of-no-thickness; the diagnostic ("if the operator's hand is hovering... the blade is in the wrong place").
- `futon3/library/structure/hinge-point.flexiarg` — a hinge is "a structural slot in a mission where direction can shift"; what makes a hinge (direction contingent on operator state/external input); what is NOT a hinge (routine phase transitions, scheduled reviews).
- `futon3/library/structure/block-as-futonic-revolution.flexiarg` — a Block is "one full revolution of the futonic loop"; the five phases (identification/specification/elaboration/passion/closure); "block boundaries are recognised at hinges, not scheduled."

**Mission doc:**
- `futon3c/holes/missions/M-bounded-in-flight-state.md` — Q-21 (line 44ff); MAP-1b "Blocks are hinge-encounters" (line 195ff); the 2026-05-04 reopen and G-1/G-2 fix (line 1ff).
