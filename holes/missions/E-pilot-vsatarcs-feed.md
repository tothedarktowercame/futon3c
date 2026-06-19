# Excursion: Pilot → VSATARCS Auto-Feed

**Type:** Excursion (E-prefix; bounded scope-out from a mission; owned end-to-end by a single hand-off agent; see [[project_e_prefix_excursions]] for the convention).
**Status:** EXECUTED by Codex on 2026-05-25. Mechanism landed; 9-item backfill ingested into canonical VSATARCS; next live substrate event still needed to demonstrate post-landing auto-flow.
**Date:** 2026-05-25
**Author:** claude-1 (inhabiting `:war-machine-pilot` peripheral; emacs-repl surface paired with Joe).
**End-to-end owner:** Codex.
**Sibling excursion (also out-of-band from M-war-machine-pilot):** `E-street-sweeper.md` (claude-2-owned; working-tree closure peripheral).
**Parent mission:** `futon3c/holes/missions/M-war-machine-pilot.md` (v1 cycle, claude-1 inhabitant).
**Surface that authored this:** emacs-repl 2026-05-25 between claude-1 and Joe.

## Why this exists (Joe, emacs-repl 2026-05-25)

Joe asked claude-1 to "double check whether the closures that you've logged already in the inhabitation log are being fed through properly to VSATARCS to put pressure on it — my guess is not."  Investigation confirmed:

- `/home/joe/code/futon4/docs/vsatarcs-alignment-completeness.aif.edn` has 21 `:bilateral-evidence` entries; latest `:landed` is `2026-05-21`.
- Today is `2026-05-25`.  9 substantive pilot substrate events have landed since 2026-05-21 (claude-9's two Pair-stage cycles, the v0-close 0010 envelope-exercise demo, claude-1's 0011 reframe, the `pilot-inhabitations.edn` substrate bootstrap, the Inhabitation Log card UI swap, the `:μ/override-modes` vocabulary entry, the `war_machine.clj` mode-override wiring, the `stop-the-line-banner` cljs, the E-street-sweeper authoring + claude-2 bell handoff).  **None are in VSATARCS.**
- Zero references in `/home/joe/code/futon4/` to: `pilot-inhabitations`, `wm-ui-anchor:0011`, `wm-ui-anchor:0020`, `:placeholder-to-living-view`, `:μ/override-modes`, `E-street-sweeper`, `:stop-the-line` override mode.

Pilot work is producing significant substrate evidence (anchored writes, new sub-kinds, new evidence-kinds, new strategic-mode classes, new artefact-type-conventions) but the apparatus-completeness record (VSATARCS) does not see it.  The audit is no longer the record; it is a partial record frozen 4 days ago.

Joe's directive: **fix the mechanism, not the backlog.**  *"I think we should go via the 'Mechanism' fix — let's hand that to codex-2 instead as a bell (not claude-4)."*  Build an auto-feed; let claude-4 (or whoever) refine entries over time rather than transcribe 9 items manually.

## Scope (v0)

Build a feeder that detects substantive pilot substrate events and stages or auto-writes corresponding `:bilateral-evidence` entries into `vsatarcs-alignment-completeness.aif.edn`.  Backfill the 9 missing events as part of v0.  Hot-reloadable; idempotent (re-runs do not duplicate entries).

## Execution outcome (Codex, 2026-05-25)

- Landed feeder implementation at `futon3c/src/futon3c/vsatarcs/feeder.clj`.
- Extended `futon3c.util.edn-comment-preserving` so append-only writes into top-level vectors preserve comments and use atomic temp-file replacement.
- Chose **(p3) periodic poll** as the v0 mechanism and **Option A direct write** into canonical `:bilateral-evidence`, with `:auto-generated true`, `:auto-source`, `:auto-content-hash`, and `:review-status :pending`.
- Used the canonical VSATARCS file itself as the durable dedup substrate.  No auxiliary cursor file or restart-sensitive watcher state is required for v0.
- Backfilled the 9 missing pilot events into `futon4/docs/vsatarcs-alignment-completeness.aif.edn`, moving `:bilateral-evidence` from 21 entries to 30.
- Verified idempotency immediately after the write: a second `ingest!` pass appended 0 entries and logged skip/no-op actions for all 9 already-landed `:vsatarcs-id`s.
- Wrote targeted tests for the feeder and the comment-preserving append path.
- Left one success criterion intentionally open: a new post-landing substantive pilot event still needs to occur so the feeder can demonstrate pressure-on-VSATARCS without manual backfill.

## v0 implementation notes

- The current reader/consumer surface in `futon4` appears to expect a closed set of familiar `:evidence-kind` values, so v0 maps the new pilot items into existing kinds (`:one-sided-extension` and `:consent-gated-writer-event`) and preserves the excursion's more specific proposed kinds under `:auto-suggested-kind` for later vocabulary review.
- `war-machine-strategic-vocabulary.edn` is not safe to treat as plain strict EDN in all cases, so the vocabulary signal in v0 is text-marker based rather than a full `edn/read-string` parse.
- The current mission/excursion scanner is intentionally narrow and backfill-oriented.  It recognises the live `E-street-sweeper` pilot excursion case that motivated this excursion; broadening that into a general mission-file substrate sweep is future work if Joe wants it.

## What "substantive substrate event" means (the source watchlist)

The feeder must monitor these substrates and recognise the following event-shapes:

| Source file | Watched signal | Event shape produced |
|---|---|---|
| `/home/joe/code/futon5a/data/pilot-inhabitations.edn` | New entry in `:events` vector | `{:source :pilot-inhabitations :event-id <:id> :event-kind <:event> :pilot-agent <:pilot-agent> :landed <:at or :at-approx>}` |
| `/home/joe/code/futon5a/data/wm-ui-anchors.edn` `:coherence-evidence` | New entry in vector | `{:source :coherence-evidence :row-id <:id> :sub-kind <:sub-kind> :evidence-kind <:evidence-kind> :landed <:landed>}` |
| `/home/joe/code/futon5a/data/wm-ui-anchors.edn` `:anchors` | New entry in `:pilot-flip-trail` of any anchor (anchor status flipped) | `{:source :anchor-flip :anchor-id <:id> :new-status <:status> :cited-cg <:cited-consent-gate-event-id> :landed <:flipped-at>}` |
| `/home/joe/code/futon5a/data/war-machine-strategic-vocabulary.edn` | New entry in `:μ/modes`, `:μ/override-modes`, `:C/mode-prior`, etc. | `{:source :vocabulary :path <:keypath> :new-key <:k> :landed <today>}` |
| `/home/joe/code/futon3c/holes/missions/` | New `M-*.md` (mission) or `E-*.md` (excursion) file | `{:source :mission-doc :file <:path> :kind :mission-or-excursion :owner <front-matter or first-author-line> :landed <file-mtime>}` |
| `/home/joe/code/futon3c/src/futon3c/peripheral/` | New `*.clj` file (new peripheral) | `{:source :peripheral-source :ns <ns-name> :file <:path> :landed <file-mtime>}` |

Codex-2 should refine this watchlist if any source is missing or any signal is mis-shaped.

## Mapping a substrate event to a `:bilateral-evidence` entry

Existing bilateral-evidence entries (claude-4-authored over weeks) have this shape:

```clojure
{:vsatarcs-id   "hx:vsatarcs-align:v<ver>:<slug>"
 :wm-id         "<reference to mission / cycle / anchor / event>"
 :evidence-kind :one-sided-extension  ;; or :symmetric-confirmation, :reframe, etc.
 :landed        "2026-05-25"
 :principle     "<one-paragraph framing of what this evidence demonstrates>"
 :note          "<longer prose explaining context, implications, follow-ons>"
 :forward-pointer "<optional: open questions or next-step references>"}
```

The auto-feeder should produce candidate entries with these fields populated.  The `:principle` and `:note` fields are prose; the feeder can produce a sensible default (e.g., extracting `:summary` / `:note` from the source event) but **flagging it as `:auto-generated true` so claude-4 (or whoever) can refine the prose**.  Auto-generated entries should be **distinguishable from human-authored entries** until reviewed.

Suggested distinguishing field:
```clojure
{...
 :auto-generated true
 :auto-source    {:from :pilot-inhabitations :event-id "inhab/claude-1/cycle/cg-686fdc10-..."}
 :review-status  :pending}  ;; → :reviewed / :refined when claude-4 touches it
```

When a human refines an entry, they remove `:auto-generated true` and bump `:review-status :reviewed`.

## Implementation paths (codex-2 picks)

**(p1) File-watcher** — `directory-watcher` library (already used by `multi_watcher` per CLAUDE.md history); watch the source-substrate files; on change, diff against last-known-state; emit `:bilateral-evidence` candidates for newly-detected events.  **Pros:** automatic; no agent action required.  **Cons:** needs persistent last-known-state across futon3c JVM restarts.

**(p2) Bell-driven** — agents emit a structured `:vsatarcs-pressure` bell when they make substantive substrate writes.  The feeder listens, materialises bilateral-evidence entries from bell payloads.  **Pros:** agent-explicit; rich payload available; matches existing bell discipline.  **Cons:** requires every substantive tool (`:anchor-flip`, `:coherence-row-author`, `:pilot-action`) to additionally bell the feeder — small envelope-extension burden across peripherals.

**(p3) Periodic poll** — every N minutes, scan source substrates, detect changes since last scan, propose entries.  **Pros:** zero coupling to peripherals; simple.  **Cons:** latency; same persistence question as p1.

**(p4) API endpoint** — `POST /api/alpha/vsatarcs/propose-entry` that agents call explicitly with a structured payload.  Feeder accumulates into a staging file or directly into bilateral-evidence.  **Pros:** clearest contract; agent owns the proposal.  **Cons:** requires agent code changes; not retroactive for substrates that don't go through agents.

**My read (claude-1's, non-binding to codex-2's pick):** **(p1) for substrate-file changes + (p2) for envelope-mediated tool actions, hybrid.**  File-watcher catches changes regardless of who made them (operator-paired or runner-mediated); bell-driven catches the rich-payload events the tools already structure.  Both write into the same staging area; deduplication via content-hash.

## Direct-write vs stage-for-review

**Option A — direct write:** feeder writes bilateral-evidence entries directly into the canonical file with `:auto-generated true`.  claude-4 refines in place by removing `:auto-generated` and adjusting prose.  Risk: bilateral-evidence file accumulates noise if events are dribbled in faster than claude-4 can review.

**Option B — stage-for-review:** feeder writes to `vsatarcs-staging.edn` (sibling file).  Reviewer (claude-4 or operator) periodically lifts items from staging into the canonical file.  Cleaner separation; slight extra step for review.

**Option C — branch-of-canonical:** feeder writes to a `:auto-staged-bilateral-evidence` vector inside the canonical file, distinct from `:bilateral-evidence`.  Refinement = move from one vector to the other.  Compromise; no extra file; visual separation.

**Codex-2 picks.**  My read: **(C)** keeps everything in one substrate while preserving the auditability of automated vs human-curated entries.

## Hard constraints (structural; enforce in feeder code, not by discipline)

1. **Never overwrite an existing human-authored entry.**  If a `:vsatarcs-id` already exists in `:bilateral-evidence` (without `:auto-generated true`), the feeder must skip / no-op / log.  Idempotency invariant.
2. **Preserve EDN comments.**  Use the same comment-preserving writer pattern that landed in `futon3c.util.edn-comment-preserving` (codex-8's deliverable #1).
3. **Deduplicate by content-hash, not by recency.**  If the source event has already been ingested (content hash matches), no-op.  Re-running the feeder must be safe.
4. **Atomic writes.**  Write to temp file + rename; never leave the canonical file in a half-written state.
5. **No retroactive edits of `:landed`.**  Once an entry's `:landed` is set, it doesn't change.  If a re-scan finds a date conflict, log a warning and skip.
6. **Log every action.**  Every entry the feeder proposes / writes / skips must produce a log line (stdout or a dedicated log file in `/tmp/` or `futon4/data/`).  Auditable trail.
7. **Respect existing schema.**  Match the existing entry shape (keys, conventions) so claude-4's existing tooling and Emacs readers continue to work.

## Backfill — the 9 missing items (v0 deliverable)

The feeder, once built, should be runnable in "backfill mode" once to ingest the 2026-05-22 → 2026-05-25 window.  The items it should produce:

| # | Source event | Suggested `:wm-id` | Suggested `:evidence-kind` | Notes |
|---|---|---|---|---|
| 1 | claude-9 cycle 408e44ed (coherence-row for anchor 0020) | `M-war-machine-pilot Phase-5 cycle 408e44ed` | `:one-sided-extension` | First Pair-stage substantive cycle; new `:sub-kind :symptom-to-root-traceback` introduced |
| 2 | claude-9 cycle 408d02d1 (pilot-action back-fill) | `M-war-machine-pilot Phase-5 cycle 408d02d1` | `:recursive-qa-closure` (new kind?) | Pilot found own substrate-discipline gap; surfaced ACTION-2 |
| 3 | v0-close anchor 0010 envelope-exercise demo (cg-8367e269) | `M-war-machine-pilot v0-close demo 0010` | `:capability-demonstration` (new kind?) | C6 PARTIAL → FULL; live-substrate write via comment-preserving writer |
| 4 | claude-1 0011 reframe (cg-686fdc10) | `M-war-machine-pilot v1 cycle-3 (claude-1)` | `:reframe-of-substrate-disposition` | New `:sub-kind :placeholder-to-living-view`; new evidence-kind for VSATARCS too |
| 5 | `pilot-inhabitations.edn` substrate bootstrap | `M-war-machine-pilot v1 cycle-3 substrate-creation` | `:new-substrate-class` (new kind?) | Living record of pilot inhabitations; first instance of agent-self-document substrate |
| 6 | Inhabitation Log card replaces Pilot Contract card | `M-war-machine-pilot v1 cycle-3 UI swap (anchor 0011)` | `:ui-discharge` | First data-bound UI replacement of a static future-capability stub |
| 7 | `:μ/override-modes` + `:stop-the-line` vocabulary | `M-war-machine-pilot v1 cycle-4 vocabulary-extension` | `:vocabulary-extension` | First override-mode in strategic vocabulary; AIF-first design |
| 8 | `war_machine.clj` judge mode-override + `stop-the-line-banner` cljs | `M-war-machine-pilot v1 cycle-4 implementation` | `:vocabulary-realised-in-code` | Override-mode wired end-to-end; banner data-driven from `:judgement.mode` |
| 9 | E-street-sweeper.md authored + claude-2 bell handoff | `M-war-machine-pilot v1 cycle-5 excursion-handoff` | `:cross-agent-excursion` (new kind?) | First E-prefix excursion; first hop-protocol scope |

Items 2/3/5/7/8/9 introduce evidence-kinds that may not exist in VSATARCS yet.  Codex-2 should treat these as proposed-new evidence-kinds and surface them to claude-4 for vocabulary review.

## Success criteria

1. **Feeder source file** exists at `futon3c/src/futon3c/vsatarcs/feeder.clj` (or analogous; codex-2 picks).
2. **Trigger mechanism** lands per chosen path (p1/p2/p3/p4 above).
3. **Backfill mode** runs once and ingests the 9 items above into `vsatarcs-alignment-completeness.aif.edn` (per option A/B/C for staging).
4. **Idempotency** verified: running the feeder twice does not produce duplicate entries.
5. **Comment preservation** verified: the resulting file's existing comments are byte-equal to the pre-feeder state.
6. **Hot-reloadable**: code change picks up via Drawbridge `(load-file ...)`; no JVM restart needed.
7. **Logging**: feeder action log is inspectable.
8. **At least one new substantive substrate event after the feeder lands** flows through correctly without manual intervention.  This is the demonstration that pressure-on-VSATARCS is operational.
9. **Bell-back to claude-1** when ready, so claude-1 can verify by checking VSATARCS-side acknowledgement of the 9 backfilled items.

## What codex-2 owns end-to-end

- Pick trigger mechanism, write the feeder code, write tests
- Implement the source-substrate readers (parsing pilot-inhabitations, coherence-evidence, anchor flips, vocabulary extensions, mission/excursion file presence, peripheral source files)
- Implement the entry-shape mapper (event → bilateral-evidence candidate)
- Implement the idempotency / content-hash check
- Implement the comment-preserving write
- Run backfill mode on the 9 items
- Hot-reload via Drawbridge
- Verify on at least one new event after the feeder is live
- Bell claude-1 when complete

## What codex-2 does NOT own

- **The prose refinement of auto-generated entries** — that's claude-4's territory (apparatus narrative); codex-2 produces structured defaults, claude-4 polishes
- **Schema evolution of VSATARCS itself** — if a new evidence-kind is needed, codex-2 surfaces it; claude-4 or Joe decides whether to canonicalise it
- **The R-criterion-audit re-runs** — VSATARCS has a separate `:r-criterion-audit` field; codex-2 does not need to update it (claude-4 territory)
- **The Emacs Arxana readers** in `futon4/dev/arxana-vsatarcs-*.el` — those consume VSATARCS; the feeder writes to it.  Readers can continue to work with auto-generated entries as long as the schema-shape is preserved (see constraint #7)

## Cross-references

- `M-war-machine-pilot.md` — parent mission; the inhabitation log this excursion's feeder reads
- `E-street-sweeper.md` — sibling excursion (claude-2 owned); proves the E-prefix convention works for multi-agent parallel work
- `futon4/docs/vsatarcs-alignment-completeness.aif.edn` — destination
- `futon5a/data/pilot-inhabitations.edn` — primary source
- `futon5a/data/wm-ui-anchors.edn` — secondary source (anchors + coherence rows)
- `futon5a/data/war-machine-strategic-vocabulary.edn` — tertiary source (vocabulary)
- `futon3c/src/futon3c/watcher/multi.clj` — clone-from for file-watcher pattern
- `futon3c/util/edn_comment_preserving.clj` — comment-preserving writer (codex-8's deliverable #1)
- `futon3c/src/futon3c/peripheral/war_machine_pilot_backend.clj` — example of bell-emit (for p2 path)

## Provenance

- Investigation that surfaced this excursion: claude-1 + Joe, emacs-repl, 2026-05-25.  Joe's directive: *"I think we should go via the 'Mechanism' fix — let's hand that to codex-2 instead as a bell (not claude-4)."*
- Excursion authored: claude-1 via emacs-repl, 2026-05-25, while inhabiting `:war-machine-pilot`.
- E-prefix convention: see `[[project_e_prefix_excursions]]` and `E-street-sweeper.md` (sibling).
