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

One further piece bears on your fourth aim, co-developing proofs with
machines. We keep every exchange between the human operator and the agents
as a record of speech acts and the obligations they create, and we model
the recurring working arrangements (our design patterns) as small open
causal models. Each pattern's DAG presents a causal theory in the sense of
Fong's 2012 thesis, the free copy-discard category on its variables and
mechanisms, and the pattern's Boolean structural equations are an open
model of it in Set, open as in Lorenz and Tull's open causal models, with
some variables left as inputs without a mechanism. An incident is the
gluing of the patterns that bear on it. The gluing identifies variables by
shared name and refuses on a missing interface declaration, a doubly owned
variable, or a cycle. A reading pass this month corrected our first
description of it: it is not decorated-cospan composition but gluing in the
cospan operad restricted to copy-discard diagrams, with the two refusals
being Fritz and Liang's left-monogamy and acyclicity conditions. On one
recorded incident the glued model returned a causal cone, an interventional
counterfactual, and 103 conditional independencies implied jointly by the
patterns, which have not yet been tested against the record. The fragment
is finite and Boolean; the extensions we want are mechanisms in a Markov
category and promises as processes with a hole. I mention it because it is
the part of our work where diagrammatic causal reasoning is already doing
something, and because your tactics, if they reach Markov-categorical
string diagrams, are the natural next step for it.

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

- **Patterns as open causal models; gluing and its refusals.**
  `notebooks/daxiang_live.clj` §5 and references [2]–[6] (Fong
  arXiv:1301.6201 §4.1–4.2; Lorenz–Tull arXiv:2304.07638 §5, Rem. 59;
  Fong arXiv:1502.00872; Baez–Courser arXiv:1911.04630; Fritz–Liang
  arXiv:2204.02284 Defs 3.5–3.6); code
  `src/futon3c/diagramprover/causal/open_theory.clj`.
- **Attribution correction (not decorated cospans; cospan-operad gluing on
  copy-discard diagrams).** `futon5a/holes/excursions/E-the-dark-tower-3.md`,
  REPORT Q1, verdicts 1b (REFUTED as worded) and 1c.
- **Causal cone, counterfactual, 103 independencies on one incident.**
  `holes/missions/M-diagramprover.md`, "Skeleton use case 2" (2026-10-02),
  including the stated limits: mechanisms authored from mission text, Boolean
  values, predictions untested.

Omitted deliberately: mutable file counts; the three-day dialogue-lab totals;
claims that the system already covers three of the project's four parts; and
the Caus/Poly/Markov-category conjectures. None is needed to establish the
practical overlap, and the categorical claims remain research questions.
