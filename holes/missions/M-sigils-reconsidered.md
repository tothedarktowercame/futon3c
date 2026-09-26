# M-sigils-reconsidered

**Opened 2026-08-17 by claude-2 (Analyst, M-apm-demonstration) at Joe's
direction.** Joe: *"sigils as a whole have been fun but maybe not that
rewarding. They go back to a much earlier prototype of something that now works
better in other ways… I'm not ready to rule on it… but there would be a case
'for' them."*

**Status: OPEN, no ruling.** This file records a survey and both sides of the
argument. It does not propose a change.

## The survey (measured 2026-08-17, whole library)

| | |
|---|---|
| files carrying `@sigils` | **936** of 1152 |
| distinct sigil values | **595** |
| values used exactly once | **547** |
| most common value | **257 × `[🌹/己]`** |

The distribution is bimodal and neither mode classifies:

- **One value on 257 files.** The `iiching` family stamps the same pair on
  everything it holds. A value shared by 257 patterns cannot distinguish among
  them.
- **547 values used once.** A value unique to a single pattern carries no more
  information than that pattern's own id.

**No consumer.** Grepping `futon3a/src` and `futon3c/src` for sigil use in
scoring, ranking, search, query, matching or filtering returns nothing. Every
hit is a parse-or-store site — `flexiarg/projection.clj` reads them,
`watcher/file_ingest.clj` writes them, `notions.clj` and `meme/schema.clj` carry
them. They are threaded end to end through the pipeline and never asked a
question.

**The maths family has already voted with its feet.** Of the math rows in
`resources/sigils/patterns-index.tsv`, only 27 of 75 carried a sigil before
today; the rest leave `tokipona` and `truth` blank. The 8 rows added on
2026-08-17 follow that majority, per Joe's ruling.

## The case FOR keeping them (Joe's flag, argued properly)

The survey above measures sigils as a **machine** device, and by that measure
they do nothing. That may be the wrong measure.

1. **They were never for the machine.** A mnemonic that helps a human browse or
   recall a pattern needs no code path. "No consumer in `src/`" is only damning
   if code was the intended consumer.
2. **The 257 duplicate is one family's practice, not the device's failure.**
   `iiching` stamping one pair on everything says something about `iiching`. The
   other 679 files carry values someone chose.
3. **547 unique values are 547 acts of authorial judgement.** Each is a small
   piece of interpretation — an emoji/hanzi pairing that names what a pattern is
   *like*. Deleting them destroys work that cannot be regenerated.
4. **A consumer could still arrive.** Sigil-keyed visual maps, browsing
   surfaces, or a retrieval facet are all buildable. Absence of a consumer today
   is an argument for building one, not only for removing the data.
5. **Retirement is a library-wide edit.** `@sigils` is `:standard` in
   `futon3/flexiarg-directives.edn` and 936 files carry it. The cost of removal
   is real and the cost of leaving it is close to zero.

## What would settle it

- Does any human actually navigate by sigil? That is an observation about use,
  not about the data, and nobody has looked.
- If a consumer were built, would the 595-value vocabulary support it, or would
  the 257-way collision make it useless without a re-sigiling pass first?
- Is the `iiching` block worth re-sigiling, or is it evidence that the device
  does not survive contact with a large mechanically-generated family?

## Related

- The `iching`/`iiching` families are the same population and Joe has already
  flagged them for their own cleanup and rationalisation — *"not at the expense
  of our APM work"*. These two questions should probably be answered together:
  3,283 of the 4,213 dropped-directive uses in the library are theirs, and 257
  of the sigil uses.
- `futon3/flexiarg-directives.edn` records `@sigils` as `:standard`. Any ruling
  here changes that entry.
