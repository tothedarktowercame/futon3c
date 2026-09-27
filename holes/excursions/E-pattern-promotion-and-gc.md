# E-pattern-promotion-and-gc: how patterns climb, fall and are collected in the cascade

Opened 2026-09-27 by claude-17 at Joe's direction, as a follow-on to M-象-2000's P21
ruling. **Not to be solved before the cascade exists**: M-象-2000 builds the cascade
first, then comes back to this.

## The questions (Joe, 2026-09-27, by voice)

- **Promotion.** P21 settled that attestation happens at the point of use and that
  well-attested patterns rise toward the top of the cascade as *gateway* patterns
  where a search starts ("a problem → a mathematics problem → an analysis problem → a
  complex analysis problem"). *How* a pattern climbs the hierarchy is an operation in
  its own right, "a little bit like chess, moving towards the back row".
- **Demotion.** A pattern whose uses stop bearing it out should be able to move down.
- **Garbage collection.** Nobody deals with patterns that are never used, duplicated,
  or superseded. The library has 1,431 patterns and the mined graph leaves ~375
  singletons.
- **Not by link-counting.** Incoming `@why`/`@how` links are connectivity, never
  attestation (P21: no "SEO"); counting applications is itself naive and to be revisited.

## Forensics on the cascade

Joe connects this to the forensic lines already under way: the code base as a crime
scene (M-the-perfect-crime), and agent chats as a crime scene
(E-classical-wastage-scanner). The pattern cascade needs the same level of analysis:
how a pattern came to sit where it sits, which uses raised or lowered it, and which of
its links are real. The three should feed each other: wastage incidents are uses where
a pattern failed or was missing; code forensics shows where a pattern's THEN was or
was not carried out.

## Starting material, when this opens

The attestation ledger P21 builds (per-use records, author, dependencies); the
cascade files (`holes/labs/M-象-2000/cascade-象2000-v2.edn` and later ones);
`scripts/mined_pattern_graph.py` (components, `why`/`how` kinds); the library census
(`scripts/xlate.py census`).
