# Cover note to Aleks Kissinger — draft for Joe, 2026-10-13

Dear Aleks,

Congratulations on the funding for *Teaching theorem provers to understand
diagrams*. Your combination of a Lean/Rocq formalisation, automated tactics,
and a visual interface overlaps with a line of work we have been pursuing, at
a much earlier stage, around machine-assisted Lean proofs.

We began with 489 qualifying-exam problems. In an April 2026 pilot, agents
produced 19 complete Lean proofs and about 30 partial proofs whose remaining
`sorry`s were classified; we also extracted twelve recurring formalisation
patterns. Since then we have built a supervised pipeline for Lean attempts:
it records runs, checks builds and residual `sorry`s, and routes
failed attempts for review or repair. A separate Emacs tool lets a reviewer
accept or reject inferred dependencies between theorem and proof blocks in a
parsed category-theory corpus.

The part closest to your project is still a design, not an implementation. Our
next experiment is to represent proof steps as typed, composable pieces of a
wiring diagram and check their composition before asking Lean to elaborate
the resulting proof. We do not yet have the diagrammatic tactics or polished
interface described in your project, so I would be especially interested in
whether our corpus, failure records, or review workflow could provide a useful
external test case as your implementation develops.

Separately, we have built a small Boolean prototype that composes causal
models of recurring collaboration patterns. On one hand-authored incident
model it computes causal ancestry, intervention and counterfactual queries,
and conditional-independence predictions. Those predictions have not yet
been tested against the underlying record, and this is not yet a
Markov-category formalisation. It may nevertheless offer a second, small
test case for diagrammatic tactics.

If that sounds relevant, I would be glad to send a short technical note or
show you the current pipeline and data. I would also be interested to hear
what intermediate representation you plan to use between a drawn diagram and
Lean or Rocq.

With best wishes,

Joe

---

## Claim ledger (not part of the letter)

Checked 2026-10-13. These references support only the claims retained above.

- **Recipient's project.** The fund page says the project will translate
  diagrammatic reasoning into Lean and Rocq, develop automated tactics, build
  a visual proof interface, and evaluate AI-supported proof development:
  <https://renaissance-philanthropy-v2.squarespace.com/teaching-theorem-provers-to-understand-diagrams>.
- **489 problems; April pilot: 19 complete, about 30 partial, twelve
  patterns.** `holes/missions/M-diagramprover.md`, section "The Problem".
  These are explicitly historical pilot figures, not a statement of current
  coverage.
- **Supervised proof pipeline and its limits.**
  `holes/labs/M-diagramprover/capability-proof.md`, especially N7–N9. This
  records mechanical gates, supervised trials, and warrant levels; it also
  marks incomplete components as designed or under construction.
- **Typed wiring-diagram experiment.** `holes/missions/M-diagramprover.md`,
  section "WS-D" and the risk table's R5 row. Both say this work has not
  started; the letter therefore describes the proposed manual experiment.
- **Human review of inferred theorem dependencies.**
  `emacs/paper-dependency-review.el` and
  `futon6/src/futon6/paper_dependency.py`. The letter makes no corpus-size or
  accuracy claim.

- **Boolean causal-model prototype.** The implementation is
  `src/futon3c/diagramprover/causal/open_theory.clj`; the worked incident and
  its tests and limitations are recorded in `holes/missions/M-diagramprover.md`,
  section "Skeleton use case 2" (2026-10-02). Its mechanisms were authored
  from mission text, its values are Boolean, and its predictions remain
  untested. The Markov-category extension is explicitly listed there as not
  built.

Omitted deliberately: mutable file counts; the three-day dialogue-lab totals;
claims that the system already covers three of the project's four parts; and
the Caus/Poly/Markov-category conjectures. None is needed to establish the
practical overlap, and the categorical claims remain research questions.
