# Cover note to Aleks Kissinger — draft for Joe, 2026-10-13

*Drafted by claude-16 at Joe's request ("a summary cover note to send to
Aleks Kissinger in connection with his newly funded project Teaching theorem
provers to understand diagrams"). In Joe's voice; every figure below is
dated and has a source in the repos. Trim as you like.*

---

Dear Aleks,

Congratulations on the funding for *Teaching theorem provers to understand
diagrams*. I read the summary with some recognition: over the past year a
small group of us (people and a fleet of coding agents) have been building a
stack that meets your project at three of its four parts, and I wanted to put
what we have in front of you in case any of it is useful, or in case you would
like one of your case studies to have an outside test bed.

**Agents proving in Lean, with the proof landscape kept as a diagram.** Our
DiagramProver work started from a curriculum corpus (489 qualifying-exam
problems against the Mathlib API) rather than competition problems. An early
overnight run (April 2026) gave 19 complete Lean proofs, about 30 partial
proofs with diagnosed `sorry` boundaries, and a library of twelve
formalisation patterns extracted from the successes. The design thesis is
that the pattern library, the `sorry` boundaries and the record of which
patterns worked where form a diagram of the proof landscape, and that the
next intervention should be read off that diagram rather than searched for
afresh per goal. Concretely: tactic traces as string diagrams whose typed
ports are goal-state types, composition type-checked before Lean runs, DPO
rewriting over a plain kernel in Clojure, and the open question of
derivation certificates (the sequence of rule applications as a checkable
object). The same engine is now being used to state laws of our own
multi-agent system as string diagrams with open ports, where an unanswered
message is an output port.

**Formal requirements first, code second.** For the agent control loop
itself (an active-inference loop that selects work by expected free energy)
we write the requirements in Lean (`mathlib4/DarkTower/WarMachine`, 238
files as of today, each requirement with a witness and a negative) and let the
implementation conform. This is the discipline your "translate it into the dependent type
theory spoken by Rocq and Lean" step will meet in practice: the formal
statement is the specification, and the test of an implementation is whether
it instantiates it.

**Where diagrams and our mathematics meet.** A sequence of research notes
(we call them the Dark Tower notes) asks whether one categorical setting
holds our control loop, our dialogue model and the human operator. Your work
is already in them: the second note takes Caus[𝒞] and BV (Simmons and
Kissinger, arXiv:2205.11219) as the skeleton for how our processes compose,
reads an operator turn as a one-way-signalling sequential composite, and
treats our wiring diagrams and process-theory string diagrams as the same
notation. In June a grounded reading pass against the papers left three
items underdetermined that we wrote down as questions to put to you, and
have not yet asked: whether the tower of orders of change is 2-truncated or
carries real 3-morphisms in the comb hierarchy; whether our retract/lift
pair (a lossy coarsening of a turn and its free structure-adding inverse)
is the closure `c = c**` and the isomix map in Caus[𝒞]; and whether the
Caus[−] typing embeds inside Poly. A fourth, still only a conjecture: that
the normalisation condition in our free-energy bookkeeping is the causality
condition ("discarding the output = discarding the input") that makes the
stack a precausal category. The current candidate for the common setting
is Markov categories. If any of this is right, the diagrammatic reasoning
your project puts into Lean is the language in which our own system's
correctness should be stated, and we would be motivated users of the
tactics.

**An interface for humans judging machine-inferred structure.** On the
informal side, futon6 is a mathematics dictionary where informal and formal
arguments coexist, indexed by patterns. It carries a hypergraph over roughly
9,800 category-theory arXiv papers (LaTeX sources, theorem/proof blocks,
definitions, term usage) and, as of this week, a theorem-dependency graph
with explicit and inferred edges in the manner of KnowTeX, and an Emacs
review buffer where a reader marks each inferred edge right or wrong. Those
verdicts are saved beside the paper and become the test set for the next
rule. The graphical algebraic geometry case study would need exactly this
kind of corpus-and-review loop if it is to be evaluated on real texts, and
the corpus is already parsed.

**The co-creation question, measured.** Your last aim, assessing how well AI
tools can co-develop rigorous diagrammatic proofs, is the one we have spent
the most effort on, from the other end. Every operator and agent turn in our
system is read as a speech act (ask, propose, approve, constrain, report,
and so on), with the resulting obligations kept as records: what is owed,
by whom, under which standing rule. In the last three days we labelled 943
turns (4,659 acts) for the finer grammar of moves and for scope, the
relation between a standing constraint and the later acts read under it,
with every label carrying a verbatim quote so it can be checked. The
finding that bears on your assessment: the structure of a long human-machine
collaboration lives in a few standing framings ("formal requirements in
Lean", "debugger, not thin path", "run end to end, no side quests") that
govern hundreds of later moves, and an interface that does not show those
binders leaves a later reader with texture and no meaning. Our working rule
for reviewing agent work is author ≠ reviewer with a recorded warrant per
test run; it is cheap, it has caught vacuous green tests repeatedly, and it
would transfer directly to judging co-developed proofs.

Most of this is prototype rather than product, in Clojure, Python and
Emacs, and the numbers above are dated. If any of it is of interest I would
be glad to talk, show the running system, or hand over the CT corpus and the
dependency-graph tooling for your case study.

With best wishes,

Joe

---

## Sources for the figures (for Joe, not for the letter)

- 489 problems, 19 proofs, ~30 partial, 12 patterns: `futon3c/holes/missions/M-diagramprover.md` §The Problem (2026-04-01).
- String diagrams, typed ports, DPO, certificates, agency skeleton: same file, WS-D (l.1494) and §Generalisation (2026-10-02).
- WarMachine Lean modules: `mathlib4/DarkTower/WarMachine/`, 238 `.lean` files on 2026-10-13.
- Caus[𝒞]/BV, the three underdetermined conjectures (C1 2-truncation, C5 retract/lift ≅ closure/isomix, C6 Caus-in-Poly) and the §4 normalisation conjecture: `futon5a/holes/excursions/E-the-dark-tower-2.md` §3–5 and the 2026-06-12 checkpoint ("questions to carry to Kissinger"); Markov categories: `E-the-dark-tower-3.md`.
- CT corpus (~9,800 papers): `futon3c/holes/excursions/E-arse-ct-probe.md` says 9,798; the warp handoff says 9,742 eprints. Pick one and say "roughly".
- Dependency graph and review buffer: `futon6/src/futon6/paper_dependency.py` (23aa212d), `futon3c/emacs/paper-dependency-review.el` (c58ebc1e).
- 943 turns / 4,659 acts: `futon3c/holes/labs/M-象-2000/LAB-2026-10-12-move-grammar.md`, batch-3 totals (2026-10-13 01:30Z).
- Author ≠ reviewer, warrants: `futon2/holes/labs/wm-contract/NOTE-test-registry.md`.
