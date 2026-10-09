# E-pattern-promotion-and-gc: how patterns climb, fall and are collected in the cascade

**VERDICT (2026-10-09, provisional):** ACTIVE — Explicitly deferred until the M-象-2000 cascade exists, but git shows measured pattern-graph work on 2026-09-30 and the doc ends with dated open measurement questions. _(WM status classification by zai-1, medium confidence; not yet confirmed by the author.)_

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

## Measurements, 2026-09-30: how the graph grows, enriches and collects today

Reopened by Joe (2026-09-30): alongside retraction (individual cascades cut out of
the one connected graph), "we need to understand how the graph grows / enriches /
garbage collects". This section is what happens now, measured by replaying the 4,647
analyses (2026-08-22 → 2026-09-30) in `created_at` order through the edge rules of
`scripts/mined_pattern_graph.py`. Next-in-session edges were left out of the replay.

**Growth.** Nodes and edges have different sources.
- Nodes are library files. Since 2026-08-13, about 330 flexiargs were added to
  `futon3/library` and 17 deleted, all by hand-authored commits. The analyses add no
  nodes: every `pattern_ref` in every analysis resolves to a library id (0 unknown).
- Edges come from authoring (545 `@why`, 90 `@how`) and from the analyses (492
  co-cited, 299 rejected-beside, 1,655 co-rejected, 1,684 next-in-session).
- Giant component by cumulative kind: authored 257; + co-cited 441; +
  rejected-beside 565; + co-rejected 818; + next-in-session 827. So 253 of the
  giant's patterns are attached only by "both turned down for the same fragment",
  which mostly records what the search returned together.
- Over time: giant 257 (08-25) → 389 (08-28) → 635 (09-07) → 693 (09-20) → 818
  (09-30). Patterns ever cited: 68 → 368. Still rising: 79 of the 227 patterns cited
  in the last quarter of analyses had never been cited before.

**Enrichment.** Almost none.
- 8% of mined edges have been observed in more than one fragment or turn, and that
  share has been flat (5–9%) since August. A new analysis adds new pairs far more
  often than it repeats an old one, so edge evidence counts cannot yet separate a
  recurring relation from a single coincidence.
- Citations are concentrated: `orchestration/recorded-handoff` 103,
  `orchestration/consent-gate` 71, then 37 and below; 110 of the 368 cited patterns
  were cited once.
- Mined edges carry no direction and no fragment role. Authored `@how` prose cites
  other patterns only 90 times.

**Collection.** There is none; the graph only accumulates.
- No edge expires, and no analysis is withdrawn: a turn that was rewound or whose
  reading was superseded still contributes its edges.
- The one removal that happens is silent: an edge endpoint whose library file is
  deleted or renamed drops out because the builder filters on current ids.
- Of 1,431 patterns: 368 cited at least once; 512 retrieved and only ever rejected;
  551 never retrieved at all. Of those 551, 214 also have no authored edge (iching 45,
  math-formalization-CA 17, math-formalization 16, liberation 9, vsatlas 8, …).
- "Never retrieved" is a fact about the search and the operator turns analysed as
  much as about the pattern: these are operator turns about software work, and the
  unretrieved families are mostly about other subjects. It is a candidate list for
  review, not a deletion list.

**What these measurements leave open.**
1. Whether co-rejected edges should connect the graph at all, or only be reported.
2. What would make an edge gain weight (repeat observation, an outcome after the
   turn, operator confirmation) — P21 rules out counting links.
3. What withdraws evidence: rewound turns, superseded readings, deleted patterns.
4. Whether growth of the node set should stay hand-authored only.
