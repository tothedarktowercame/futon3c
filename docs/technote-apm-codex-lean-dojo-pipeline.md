# Technote: APM Codex-to-LeanDojo Pipeline

Date: 2026-04-06

## Why this note exists

The original plan was simpler than the process we actually needed.

We started with the idea that `codex-1` would act as a closer on already-started
Lean files, and anything it could not close would go straight to LeanDojo. In
practice, that was too coarse. The blocked set contained three different kinds
of cases:

- files that were already local-hole driven and genuinely ready for search
- files that were still skeletal and needed Codex to state the real theorems
- files blocked on substantial missing Mathlib theory

So the pipeline now has an explicit scaffold stage between Codex close and
LeanDojo handoff.

## The actual stages

### 1. Codex close

Purpose: finish an already-started Lean file if that is structurally possible.

Entry point:

```clojure
(dev/start-apm-codex-close!)
(dev/start-apm-codex-close! :n 5)
(dev/start-apm-codex-close! :problem-ids ["a00J02"])
```

Behavior:

- builds the target Lean module
- works only on the current file and direct dependencies
- tries to close existing `sorry`s
- may make minimal structural theorem repairs if the current statement is false
- stops quickly if the remaining blocker is a real theory/API gap

Outputs:

- raw replies in `data/apm-codex-close-replies/`
- one EDN line per run in `data/apm-codex-cleanup-log.edn`
- manifest refresh via `dev/build_apm_problem_manifest.sh`

### 2. LeanDojo handoff classification

Purpose: classify the result of a close/scaffold pass into a handoff state.

Artifacts:

- manifest columns:
  - `dojo_handoff_path`
  - `dojo_state`
- handoff files in `data/apm-lean-dojo-handoffs/`

Current `dojo_state` values:

- `dojo_ready`
  The file has explicit local `sorry`s in the real proof path and is ready for
  search.
- `needs_codex_scaffold`
  The file is still too skeletal for LeanDojo. It needs Codex to add theorem
  statements, helper statements, and a local proof shape first.
- `deep_mathlib_blocked`
  The file appears to depend on substantial missing theory or library
  development and should not be handed to LeanDojo first.

Rebuild command:

```clojure
(dev/build-apm-lean-dojo-handoffs!)
(dev/build-apm-lean-dojo-handoffs! :problem-ids ["a00J02" "a03J04"])
```

### 3. Codex scaffold

Purpose: turn `needs_codex_scaffold` files into `dojo_ready` files.

Entry point:

```clojure
(dev/start-apm-codex-scaffold!)
(dev/start-apm-codex-scaffold! :n 5)
(dev/start-apm-codex-scaffold! :problem-ids ["a03J04"])
```

Behavior:

- builds the current Lean target
- does **not** try to finish the proof
- adds missing main theorem statements when the file only contains helpers
- reduces vague placeholders into explicit local `sorry`s
- keeps the file building
- regenerates the LeanDojo handoff automatically on return

Outputs:

- raw replies in `data/apm-codex-scaffold-replies/`
- updated handoffs in `data/apm-lean-dojo-handoffs/`
- refreshed manifest row with new `dojo_state`

### 4. LeanDojo handoff

Purpose: search only on files that are already localized and honest.

LeanDojo should receive only:

- `dojo_state=dojo_ready`

LeanDojo should not receive first-pass inputs from:

- `needs_codex_scaffold`
- `deep_mathlib_blocked`

## Why the scaffold stage is necessary

The key failure mode of the original `close -> LeanDojo` plan was that some
files had no meaningful local holes at all. Typical examples:

- the file only contained helper lemmas
- the main theorem was not stated
- the file ended in a placeholder tautology
- the informal outline existed, but the formal file had not yet been reduced to
  explicit obligations

Those are bad search inputs. LeanDojo works best when the file already contains
the target theorem, the helper structure is visible, and the remaining work is
localized to real `sorry`s.

Codex scaffold exists to create exactly that shape.

## How the manifest should be read

The manifest at `data/apm-problem-manifest.tsv` is now the main control table
for this pipeline.

Relevant columns:

- `processed_level`
- `cleanup_closed`
- `cleanup_remaining`
- `cleanup_state`
- `dojo_handoff_path`
- `dojo_state`

Interpretation:

- `cleanup_state=complete`
  The current Lean file closes fully.
- `cleanup_state=blocked`
  Codex close/scaffold did not finish the file.
- `dojo_state=dojo_ready`
  The blocked file is still a good LeanDojo candidate.
- `dojo_state=needs_codex_scaffold`
  Run scaffold before LeanDojo.
- `dojo_state=deep_mathlib_blocked`
  Do not route to LeanDojo first.

`processed_level=lean_dojo_handoff_ready` is the visible summary that a row has
become `dojo_ready`.

## Typical operator loop

### Close pass

Use this when a file already looks reasonably complete.

```clojure
(dev/start-apm-codex-close! :n 10)
```

### Scaffold pass

Use this on the current `needs_codex_scaffold` set.

```clojure
(dev/start-apm-codex-scaffold! :n 10)
```

### Handoff refresh

Usually automatic after close/scaffold, but can be rebuilt explicitly.

```clojure
(dev/build-apm-lean-dojo-handoffs!)
```

### Then hand only `dojo_ready` onward

The intended routing is:

```text
started Lean file
  -> Codex close
  -> if needed, Codex scaffold
  -> if dojo_ready, LeanDojo
```

Not:

```text
started Lean file
  -> Codex close
  -> all blocked files to LeanDojo
```

## Current state as of April 6, 2026

After the close and scaffold passes run so far:

- `dojo_ready`: 16
- `deep_mathlib_blocked`: 2
- `needs_codex_scaffold`: 0

That is the key sign that the extra stage is doing useful work: it converts
skeletal blocked files into localized LeanDojo-ready files instead of treating
all blockers as equivalent.

## Practical rule

If a blocked file still makes you say "the theorem is not really stated yet" or
"this is not a search problem yet", it belongs in Codex scaffold, not LeanDojo.
