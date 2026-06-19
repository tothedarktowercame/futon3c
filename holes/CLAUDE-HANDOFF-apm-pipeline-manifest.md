# Relay to Claude: APM Pipeline Manifest Snapshot

**From:** Codex
**To:** Claude
**Date:** 2026-04-07
**Source of truth:** `/home/joe/code/futon3c/data/apm-problem-manifest.tsv`

## What changed

The APM cleanup flow is no longer just:

```text
Codex close -> LeanDojo
```

It is now:

```text
Codex close -> handoff classification -> Codex scaffold if needed -> LeanDojo
```

The reason is that the original blocked set contained a mix of:

- files with real local `sorry`s, ready for search
- skeletal files that still needed theorem statements and proof shape
- files blocked on deeper Mathlib theory gaps

So the pipeline now tracks LeanDojo readiness explicitly.

## Current manifest counts

From the current `apm-problem-manifest.tsv`:

- total problems: `490`
- `source_only`: `339`
- `informal_outline_present`: `63`
- `canary_frame_present`: `64`
- `codex_cleanup_blocked`: `4`
- `codex_cleanup_complete`: `4`
- `lean_dojo_handoff_ready`: `16`

Cleanup state counts:

- `complete`: `4`
- `blocked`: `20`

LeanDojo handoff state counts:

- `dojo_ready`: `16`
- `deep_mathlib_blocked`: `2`
- `needs_codex_scaffold`: `0`

So the scaffold pass did its job: the temporary scaffold-needed bucket has been
drained, and the blocked set is now split into LeanDojo-ready vs genuinely deep
Mathlib blockers.

## The 16 LeanDojo-ready problems

Each of these has a handoff file under `data/apm-lean-dojo-handoffs/` and
should be treated as the current search-ready frontier:

- `a00J02`
- `a01A01`
- `a01A09`
- `a01A11`
- `a01J01`
- `a01J02`
- `a01J03`
- `a01J05`
- `a01J06`
- `a02J01`
- `a02J02`
- `a02J03`
- `a02J06`
- `a03J01`
- `a03J03`
- `a03J04`

Concrete handoff files:

- `/home/joe/code/futon3c/data/apm-lean-dojo-handoffs/apm-a00J02.md`
- `/home/joe/code/futon3c/data/apm-lean-dojo-handoffs/apm-a01A01.md`
- `/home/joe/code/futon3c/data/apm-lean-dojo-handoffs/apm-a01A09.md`
- `/home/joe/code/futon3c/data/apm-lean-dojo-handoffs/apm-a01A11.md`
- `/home/joe/code/futon3c/data/apm-lean-dojo-handoffs/apm-a01J01.md`
- `/home/joe/code/futon3c/data/apm-lean-dojo-handoffs/apm-a01J02.md`
- `/home/joe/code/futon3c/data/apm-lean-dojo-handoffs/apm-a01J03.md`
- `/home/joe/code/futon3c/data/apm-lean-dojo-handoffs/apm-a01J05.md`
- `/home/joe/code/futon3c/data/apm-lean-dojo-handoffs/apm-a01J06.md`
- `/home/joe/code/futon3c/data/apm-lean-dojo-handoffs/apm-a02J01.md`
- `/home/joe/code/futon3c/data/apm-lean-dojo-handoffs/apm-a02J02.md`
- `/home/joe/code/futon3c/data/apm-lean-dojo-handoffs/apm-a02J03.md`
- `/home/joe/code/futon3c/data/apm-lean-dojo-handoffs/apm-a02J06.md`
- `/home/joe/code/futon3c/data/apm-lean-dojo-handoffs/apm-a03J01.md`
- `/home/joe/code/futon3c/data/apm-lean-dojo-handoffs/apm-a03J03.md`
- `/home/joe/code/futon3c/data/apm-lean-dojo-handoffs/apm-a03J04.md`

## The 2 deep-Mathlib blockers

These are blocked, but not good first LeanDojo targets:

- `a01A12`
  - `/home/joe/code/futon3c/data/apm-lean-dojo-handoffs/apm-a01A12.md`
- `a02J05`
  - `/home/joe/code/futon3c/data/apm-lean-dojo-handoffs/apm-a02J05.md`

Interpretation: these appear to need larger missing-theory development rather
than local search over existing holes.

## Important process note

The stable description of the pipeline now lives at:

- `/home/joe/code/futon3c/docs/technote-apm-codex-lean-dojo-pipeline.md`

That note explains:

- `start-apm-codex-close!`
- `start-apm-codex-scaffold!`
- `build-apm-lean-dojo-handoffs!`
- manifest columns `dojo_handoff_path` and `dojo_state`
- why `needs_codex_scaffold` had to exist

## Recommendation to Claude

If you are deciding what to send onward or what to review next:

1. Treat the 16 `dojo_ready` files as the live LeanDojo frontier.
2. Do not send `a01A12` or `a02J05` first unless you explicitly want deep
   Mathlib-gap exploration rather than local proof search.
3. If new Codex close/scaffold passes run, re-read the manifest rather than
   relying on this note, because the manifest is the live control table.

## Short version

The pipeline worked.

It started with a mixed blocked set and now ends with:

- `16` clean LeanDojo handoffs
- `2` honest deep blockers
- `0` remaining scaffold-needed files
