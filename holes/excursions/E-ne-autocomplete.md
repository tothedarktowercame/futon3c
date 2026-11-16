# E-ne-autocomplete: suggest likely entities as `[[...]]` links while Joe types

**Status:** OPEN, for later; not on M-repl-circus's path to closure (Joe,
2026-11-16).

## Joe's question (2026-11-16)

"given that 猴 is not a known term until I make it one, viz., [[猴]], should we
build some system that recognises likely named entities and suggests them as
tags, e.g., 「猴」as a text that would TAB-complete as a wiki-link? This isn't on
our critical path for closure by the way, but an interesting excursion for
later, e.g., E-ne-autocomplete"

## Why it came up

Working out 猴's operator focus (M-repl-circus, 2026-11-16): none of Joe's 15
turns to claude-13 that night named an item id. He wrote "猴", "the cascade",
"the batch", and none of those is an entity until introduced. A focus read from
named items alone found nothing. Entities Joe introduces with `[[X]]` would give
猴 (and the mention records, `[:workplan/mention 2]`) something exact to count.

## What already exists (read before building)

- Joe's `[[X]]` and `[[X | r1, r2]]` introductions are recorded as
  `[:workplan/introduction 1]` / `[:workplan/referent 1]` (futon3c
  `src/futon3c/arxana/sessionista_introductions.clj`; futon4
  `holes/excursions/E-gcc-schema.md` B4, §10).
- The lexicon, with 589 candidate referents
  (`~/notes/cascade-live-outline/lexicon/merge/all.json`).
- Word rules: `[:workplan/entity-match-rule 1]`, used by the backlinks builder
  (`src/futon3c/arxana/workplan_backlinks.clj`).
- Exact mentions of items with codepoint offsets, quoted spans (`>>>`) kept
  apart (`src/futon3c/arxana/turn_mentions.clj`).

## Questions to settle

- **Candidates:** what counts as a likely entity: repeated noun phrases, CJK
  terms in 「」 or alone, capitalised names, lexicon words, terms agents use
  for things Joe refers to?
- **Where:** a `completion-at-point` function in the REPL input buffers (TAB
  turns 猴 into `[[猴]]`), or a suggestion shown after the turn is sent?
- **The suggestion is not the introduction:** only Joe's accepted `[[X]]` writes
  an introduction. A suggestion that is declined is worth keeping as evidence of
  what is not an entity.
- **Retroactive:** once `[[猴]]` exists, earlier exact occurrences of 猴 can be
  proposed as mentions (the same kind of retroactive claiming as unfiled work,
  e.g. claude-3's notmuch.el turns under a later M-futon-comms).
