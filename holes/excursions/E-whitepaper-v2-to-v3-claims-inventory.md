# E-whitepaper-v2-to-v3-claims-inventory — what V3 would lose if it simply superseded V2

*claude-19 (Fable), 2026-08-26, draft 1. Written after reading both drafts in
full (`docs/retrieval-whitepaper-v2.md`, Draft 2 of 2026-08-01;
`docs/retrieval-whitepaper-v3.md`, draft with sections through 2026-08-25) and
the programme (`E-memory-whitepaper-v3-programme.md`) and bank
(`E-memory-v3-staging.md`). Joe's brief: move from V2 to V3 "in a way that
preserves the best of both working drafts". This is the preparatory step —
an inventory, not an edit. Nothing in either paper is changed by this file.*

## The situation in one paragraph

V3 is written as a successor whose only inheritance from V2 is its method
("treat our own instruments as the object of study"). It cites none of V2's
numbers; its abstract summarises V2 in one sentence ("found the instruments
wanting"). V2 meanwhile contains four things V3 does not: the two lexical
falsifications (§4.2), the store-shape analysis and its E1 reversal (§4.6),
the reachability axis (§5.1), and a sourced related-work section with an
explicit "asserted on our own authority" list (§§8, 10). V3 has stronger
results than V2 on every axis they share — witnessed use, silence, twins — and
a weaker apparatus around them: no related work, no hashes appendix, an "[TO
MAINTAIN]" own-authority list that has not been maintained. So the merge is
asymmetric: V3 keeps its results and takes V2's *scaffolding*; V2's results
either get a one-paragraph home in V3 or are cited as V2 and left there.

## Section-by-section: V2 claims and where they stand against V3

| V2 § | claim | V3 status | recommendation |
|---|---|---|---|
| Abstract / §1.1 | construct validity, self-applied; "instruments measured a property of a thing, the phenomenon was a relation" | V3 §1.1 inherits the *method* in one sentence; the thesis itself is not restated | keep as V2's contribution, cited; V3 §1.1 should say in two sentences what V2 established, not only that it did |
| §1.2 | fifth trust boundary (*load-bearing*, between attribution and witness); *reachability* as an orthogonal axis | V3 §3.1 replaces "load-bearing" with the **fingerprint standard** — a strictly stronger criterion. Reachability appears nowhere in V3 | fingerprint supersedes load-bearing: say so explicitly in V3 §3.1 ("V2's fifth boundary, made mechanical"). Reachability needs a home — see §5.1 row |
| §2 | memory system as a service; `(client, witness-source)` pairs; decision-keying | absent from V3; V3's design (§5a) is a *different* architecture (solver→scribe→proctor→shelf→student) | V3 §5a should state how its frame design instantiates or replaces the service/lane model; otherwise the two papers describe two systems with no bridge |
| §3 | derivation before execution; preregistration with falsifiers; frozen artifacts with hashes; separation of roles; withheld repairs | V3 has preregistration (`prereg-capability-transfer-v1`) and role separation, but no hashes appendix and the withheld-repairs discipline is **inverted** — V3's frames are run on a "continuously repaired instrument" by design (§8 last bullet) | V3 must state the inversion as a deliberate change of design, with V2 §7.2 ("withholding repairs was load-bearing") as the thing it is giving up and why (the frame is the unit, so repairs between frames are allowed). Currently it is stated only as a caveat |
| §4.1 | rejection taxonomy: 65% off-topic, 12% discriminating tail, discoverability scores 0 as a structural silence | not in V3 | keep in V2 only; V3 §4.1 ("The defect") could cite the 65% in one clause |
| §4.2 + §4.2.1 | DF U-curve (p=0.618) and co-occurrence with the wrong sign (p=0.0172): both lexical mechanisms falsified; Weeber et al. extension | V3 §4.3's anchor-DF repair is the *direct descendant* of this and does not cite it | V3 §4 should open with two sentences: what V2 falsified, and that the anchor repair targets a different sub-stage (anchor selection, DF-source mismatch) — the programme doc already says this (delta 4); the paper doesn't |
| §4.3 | structural sensitivity: both arms ranking-critical, n=10, 80/90 empty baselines | not in V3 | leave in V2; superseded in interest by V3's twins |
| §4.4 | 38% load-bearing by adjudication; prose-only asymmetry (n=6); rubric reproduced the substitutive defect | V3 §3.2: the adjudication rubric "would have passed both" marginal uses — V3 *refutes* the 38% as measurement | V3 should say this in the words it means: the 38% is retired, not refined. One sentence in §3.2 |
| §4.5 | λ₂ informative (~15 SD below null) with inverted threshold across 59× | not in V3; V2 §10 calls it "the paper's most likely genuine contribution" | keep in V2 and cite; V3 has no graph result to replace it. **This is the clearest case for V2 continuing to exist as a paper rather than being absorbed** |
| §4.6 | store is a forest of stars; multi-attachment representable but never used; E1 reversal (pattern arm substantial on the tail, untested on the corpus); "graph never populated" | not in V3 — and this is the section the cascade work now depends on (`TN-APM-cascades-exist-unused` addendum, `PLAN-apm-cascade-demo-instance` whitepaper check) | **needs a V3 section** — see "the delivery layer" below. The 62%-slots-unused and star-forest numbers are V2-era; the D0 run (real expander over f42) gives V3 its own numbers for the same claim |
| §5.1 | reachable ≠ retrievable; corpus indexes advice not artifacts; grep beat recall, n=1 | V3 §6 (votes → build → callback) and §7 (terms vs fit) are the *answer* to §5.1 and don't cite the question | V3 §6 should open by naming §5.1's gap as what the demand-side pipeline closes. Reachability then has its home |
| §5.2 | attachment-layer conjecture | V3 §5a's supply/demand asymmetry (all approvals from the guide) is partial evidence | note as still-a-conjecture in V3 §8's own-authority list |
| §6 | threats: one corpus, one model, unrunnable counterfactual, small n, lexical coding, current-graph-not-dispatch-time | V3 §8 has an own-authority list but no threats section | V3 needs a threats section; V2's is the template, and three of its six items still apply (one domain; small n at the points that matter; instrument-version boundaries replacing "current-graph") |
| §7.1–7.4 | four denominators failed on recount; withholding repairs; twelve instrument defects (two shapes); adversarial division of labour | V3 §2.1 is the same genre at larger scale (20 instances) and *does* cite V2's "unenforced-docstring fields" | V3 §2.1 could take V2 §7.3's two-shape taxonomy ("absent quantity as evidence" / "semantics only in a comment") as its classification — every one of the 20 falls into one of the two. That is the best of V2 the V3 silence chapter can absorb |
| §8 | related work, hand-verified | V3 has none | **port wholesale** with a paragraph added for the twin/ablation literature (Srivastava 2026 CMI is already cited in V2 §8 and is the closest prior art to V3's paired blind dispatch) |
| §9 | "instrument the relation, not the artifact"; "do not repair before you measure" | V3's thesis (§1.2) is "enforce by construction; make absence loud; audit the instrument" | the two are compatible and the second follows from the first; V3 §9 (unwritten) should carry both |
| §10 | four claims asserted on own authority, marked | V3 §8 "[TO MAINTAIN]" — nine bullets, none sourced | merge the lists; keep V2's discipline of saying which were *searched for and not found* vs *not searched* |
| App. A | frozen artifacts + sha256 | V3 App. A is an artifact index with paths, no hashes | add hashes for the frame corpus (which V3 §2.1 item 13 says was unversioned until 2026-08-25 — the hashes are the fix's receipt) |
| App. B | preregistrations and outcomes, 4 rows, 3 wrong | V3 has one preregistration and reports its ladder in §5a's table | same table shape for V3: every prereg, expectation, outcome |

## What V3 has that V2 lacks and must not be diluted by the merge

- The 20-instance silence catalogue with a mechanism (§2.1–2.2). Nothing in V2
  is at this altitude.
- The fingerprint standard, applied mechanically to 35 events with base-file
  differencing (§3.1a), and the cross-problem chain counted as zero by three
  instruments (§3.1b).
- Twins: self-reported benefit contradicted by the twin's wall clock (§3.2).
- The demand-side pipeline that proved a theorem in ninety minutes (§6).
- The claim ladder (2a established / 2b not run) with the ablation named as
  the missing arm (§5a) — this is V2's "counterfactual is unrunnable" turned
  into a concrete unrun experiment.
- §7a, the same loop over software and operator corrections as corpus.

## The section V3 is missing, and where the cascade work lands

V2 §4.6 established that the store's *graph* was never populated. V3 says
nothing about the graph, and nothing about how memories are **delivered** —
yet every V3 §5a result is about a student handed a shelf, and the shelf is
(TN) a flat list sorted by hash, with a cascade expander built and never on
the path. Proposed V3 section — working title **"The delivery layer"** —
carrying:

1. what the student is actually handed (TN: 48 leaves, hash order, used
   memory at 47/48), and that the packet text itself is not archived (H1 is
   the fix; until it lands, "surfaced and ignored" and "never shown" cannot be
   told apart — a §2.2-shaped silence in the independent variable);
2. the cascade that exists and its receipt shape (`:leaf`/`:why-hop`/
   `:co-incidence`, cap, `:truncated?`), and the fact that its artifacts are
   absent from every campaign on disk (D1 will say whether "on disk" is the
   whole story for the round-1 path);
3. the real expansion over f42 (D0's numbers replacing claude-13's
   reimplementation), read against V2 §4.6: descent from a star centre does
   not narrow, it dumps — the flooding argument, with the store's shape as
   the cause rather than the expander's design;
4. what "built and used" must mean for a cascade (PLAN §"precisely"), and
   the H4 judgement once made.

This section is the natural bridge: it is where V2's storage-side result
gets its consequence and where V3's delivery-side experiments get their
independent variable described. It should be written after D0 returns and
not before.

## Recommendation on form

Two papers, not one. V2 stays a paper (its λ₂ and lexical results have no
V3 replacement and were sent to Rob under that title). V3 takes from V2 its
scaffolding — related work, threats, own-authority list, hashes, prereg table
— and cites V2's results rather than restating them, except in §1.1 (two
sentences), §4 (two sentences), and the new delivery-layer section (which
restates §4.6's shape claim because V3's argument depends on it). The
"[SKELETAL]" sections of V3 (§5, §7) are unaffected by any of this.

## Not done here

- No edits to either paper.
- The related-work port needs the twin/ablation literature checked, not
  assumed; V2's own rule (hand-verify every reference) applies.
- Whether V3's title continues the arc is programme decision 5, still Joe's.
