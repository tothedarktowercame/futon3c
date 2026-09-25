# Mission: M-wm-wiring

**Status:** HEAD (2026-09-25); IDENTIFY pending. HEAD exit needs Joe's recognition of this text.
**Owner:** claude-10 (the wiring owner throughout PROOF-2a: flight loop, `:construction` tick, read step), proposed; claude-8 leads PROOF-2a and reviews; Joe to confirm.
**Repo:** futon3c (the mission lives here). Components live in futon2 (`src/futon2/aif`, `scripts/wm`), mathlib4 (`DarkTower/WarMachine`), futon3c (checkers, cascade-real).
**Governing record:** futon2 `holes/labs/wm-contract/PROOF-2a-THEOREM-draft-2026-09-24.md` (66120f4c at drafting): the theorem is completion; its holes are the components below.

## HEAD (Joe, 2026-09-25, emacs-repl; sense preserved, quotes verbatim)

**Operator-voice anchor.** On H-G-target part 2, ~15:30Z: "'constructed candidate' should be on the fly, as you've said, which implies that we need a different way to do the initial selection otherwise we go around constructing cascades for every mission before we can choose one. A cheap way to select missions is needed. This is what the 'outer loop' is supposed to supply. Now, recent thinking on my part is that the outer loop should itself be a cascade, i.e., one global interpretation (if you will) of Cascade Live; but a live one that gives us a way to get unstuck." Then, ~16:10Z: "since that section comes in very modularly with the rest of the material as a kind of HEAD, I think we should set up a new mission M-wm-wiring and start to get all the other components wired."

**What is already felt to be true.** Every PROOF-2a hole that could be closed on its own has been, in its own process, with its own tests and a test-registry warrant (inventory below). Cascade Live regenerates from real data with zero hand-typed rows (`/api/alpha/cascade-real`, spine 229, O1 176, O1×O4 73, held 124, lineage 88 on 2026-09-25). The operator's own turns are being read in pattern terms (M-the-perfect-crime, claude-12: 2,130 of 3,286 records over 2026-08-22..09-21 published). The old outer loop existed and its defects are on record (futon2 `holes/E-outer-loop.md`, aedcc6ae). What is missing is the joining: nothing calls the components in a tick, and the machine has not flown a lifecycle mission under the clauses.

**Anti-glibness discipline.** (1) A hole is closed at component level before it is wired; wiring never closes a hole (Joe, 2026-09-25 ~04:15Z). (2) No new required field or refusal without a red-tape-removal or AIF-validity case. (3) An absence is typed on the record, never a value standing in for it. (4) The outer loop is the machine's, no hand catalog and no imposed ordering (futon2 c2061e27); owner-stated wins over computed. (5) Red tape is a cost term from a measured rate, never a guard. (6) A flight is the test of the wiring and is recorded whether it fails; an accurate record of failure is the deliverable, not a fake success. (7) Each wiring step has its own test and warrant, author ≠ reviewer.

**Working-economy position.** This mission underwrites PROOF-2a's completion: the theorem is proved when the machine flies a mission under Clauses C, D, E, T with the witness conditions checkable on the record. It is underwritten by the components (below), by Cascade Live as the substrate the outer cascade reads, and by the hand-placed first target M-autoclock-in (c2061e27).

**Carried-forward tensions.**
1. The outer cascade's definition is pending OUTER-CASCADE-D (claude-12, invoke-1790349371724, read-only; claude-10 first reader). Until it lands, Clause T's selection has no G for any target; H-G-target part 2 stays open and the mixture law with a seeded record (claude-10, futon2 d615d06e) is its no-data fallback.
2. Which attempt is the grain attempt is unmarked on candidates; the W_c checker requires some successful attempt naming a G_c pass (futon3c 4bc95005). The flight's writer must mark it.
3. The W_c checker's warrant does not cover `grain_gate.clj` and `roles.clj`, loaded by absolute path in the child bb process.
4. Served-by links beyond M-futon-seams are proposed by a reader and verified (futon2 cedf100b): seats propose quotes, the read step turns them into spans; the join of quote to span, and `:quote-ambiguous`, are unbuilt.
5. `sourced-rates`' R5 call site passes nil labels; the click record carries no `:measurement`.
6. Selection reads E from the store the legacy writer and the close rule fill, not from the enactment-habit fold.
7. H-publish's observation exists only at flight time.
8. Counts that produce their rate but are impossible (3/2) reach the kernel (futon2 64f005d4, reach limit).
9. Operator-turn evidence is 18% pattern-cited and has no tested join to missions.

**Provenance.** Drafted by claude-8 from Joe's emacs-repl turns of 2026-09-25 (~15:30Z, ~16:10Z), the PROOF-2a draft at futon2 66120f4c, and two whistles with claude-12 (~15:55Z, invoke-…a624f627, …f5cd753e).

## Wiring inventory (carried into IDENTIFY)

Closed means: in its own process, tests green, warrant registered. Wiring means: the call from the tick or the flight that does not exist yet. Owner of every wiring row is claude-10 unless noted.

| # | hole / variable | component closed at | wiring to do |
|---|---|---|---|
| 1 | Target (Clause T) | Lean `TargetGrainG` (mathlib4 759b8ca884, warrants test-registry-eddbe897); part 2 pending OUTER-CASCADE-D | selection as the outer cascade over the field; `:g` per feasible entry citing `deltaG_localises`; pair overlap → comparable or `:incommensurable`; draw seed on the record; Clause T paragraph |
| 2 | C, read step (H-C, H-C-reach) | extractor v5 (futon2 bf38b5c5, test-registry-e9d8b317); `verify-proposed-link` (cedf100b + 6387fc77 + 1a98d0dc, test-registry-f4576030) | click computes outcomes at read time (v5 beside the wants); quotes → spans; every proposal recorded with basis or refusal; text sha recorded |
| 3 | interpretation (H-interp D11) | `wi/prompt`, `validate-response` | the flight's ask step calls them; edge reasons sent and requested |
| 4 | order (H-order D4) | constructor emits `:order` (Lean 69c2…) | the kernel reads `:order` instead of the list kernel |
| 5 | grain (H-grain G_c) | `grain-gate`; W_c checker re-runs it (futon3c 4bc95005, test-registry-1757d974) | the flight calls the gate before the grain attempt is committed and marks the grain pattern |
| 6 | A, rates (H-A) | `rates-by-class`, `measured-cell` (futon2 7ba427ab, 64f005d4, test-registry-eb815e57) | R5 passes admitted labels; `:measurement` on the click record |
| 7 | E, habit (H-E) | fold (futon2 47842175, test-registry-b33dc491; 531cfaaa, test-registry-2730f868) | selection reads the folded state; first G_c-bearing enactment |
| 8 | universe (H-C follow-up) | `:universe` honoured (futon2 a98f5879, pinned 506549de, test-registry-0ca6a69d) | the field computes each pair's overlap |
| 9 | selection law (A5) | checker reads `:join-unverifiable` (4bc95005) | the click record writes the selected candidate's id under `:selection-law` |
| 10 | publish (H-publish) | observation exists at flight time | one tick observes it |
| 11 | warrants | worktree warrants check (futon3c 4e66c56c); `--pinned` refs resolved (futon2 8eaa3e23) | done: non-author warrant test-registry-35e113f3 (claude-13, pinned 1a98d0dc, covers 6387fc77); nothing owed |

## IDENTIFY (pending)

**Gap.** The components are tested singly; nothing joins them into a tick; the machine has not flown a lifecycle mission under the clauses.

**Completion criteria (proposed, for IDENTIFY to harden).** (i) One flight of the hand-placed target M-autoclock-in produces a click record on which every witness condition W_0..W_t is checkable, each passing or typed-refused, no absence read as a value. (ii) The outer cascade selects the next target from the field with the draw and its seed on the record. (iii) Every wiring row above has its own test and warrant, and no hole was closed by wiring. (iv) The record of a failed flight is accepted as a deliverable when it is accurate.
