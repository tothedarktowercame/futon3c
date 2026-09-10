# TA reconstruction and feedback prototype

This is a completed teaching reconstruction of the previously developed a93A01
paper proof, not a new solution, Student trial, or live role change. It supplies
one concrete unit for both proposed roles: a solver can deposit this account
after proof checking, and a retrospective loop can reconstruct it from a
historical solution. The same schema preserves which mode produced the account.

The machine-readable cascade contains 11 goal nodes, exact revisions of the
five existing methods cited, explicit conditions, proof dependencies distinct
from the teaching hierarchy, and two unpublished candidate abstractions. The
Student card presents prompts and progressively more specific construction hints.
The full proof is in
[the earlier development record](../pattern-construction-2026-09-10/prelim-development.md).
It supplies the constants M=1+max|f(c_i)|, N=2M/δ and reverse δ=ε/N; the teaching
card initially withholds these to make the intermediate constructions visible.
The record's source hash is in `a93A01-ta-cascade.json`.

## What the TA should teach here

The central move is recognizing that the forward direction needs a global
numerator bound. “Use uniform continuity” alone omits that construction. A
finite net plus one uniform oscillation bound supplies it even though the domain
is not compact. The reverse direction is different: it needs a high/low ratio
split, not the global bound. This contrast is the teaching content between a
broad strategy and a named fact.

B2 is explicitly ordinary mathematical work: build the midpoint mesh. No
existing-pattern or memory-use credit is assigned to that construction. The two
drafts collect finite-net oscillation and threshold dichotomy, respectively;
they are not promoted based on this single example. Retrospective extraction
should next look for another proof using each construction and a counterexample
to each missing condition. Mere mention of boundedness is not a cross-example.

## One completed feedback-to-repair exercise

**Source-backed issue:** the earlier library consultation found the bounded-image
pattern's general bounded-domain premise insufficient. This is an observed
library-content finding in the development record, not an observed Student error.

**Synthetic Student response, explicitly a diagnostic:** “The interval is
bounded, so uniform continuity makes f bounded.” The conclusion is true on this
interval, but the offered general justification omits total boundedness.
The TA assigns `condition-gap` at B, not `retrieval-miss`: this response names a
method but does not establish its needed premise. It does not prove which text
the Student read; a read receipt is still needed before attributing the error
to a particular library revision.

**Repair applied in this packet:** B now requires a finite net; B1 asks for a
single UC tolerance; B2 constructs domain centers; B3 applies a finite maximum.
Student hint level 2 explicitly asks why the open interval need not be compact.
The canonical pattern has not been edited. This is a teaching repair to the
prototype, with a separate library correction candidate.

**Mathematical contrast used to check the repair:** take ℕ with discrete metric
ρ(m,n)=1 for m≠n. This domain is bounded, and f(n)=n into ordinary ℝ is uniformly
continuous: δ=1/2 forces m=n for every ε. Its image is unbounded. It has no finite
η-net for η<1. The repaired premise rejects this case and admits (0,1), whose
midpoint mesh supplies a finite net. Thus the proposed correction distinguishes
a real missing condition; it is not merely more verbose advice.

**What remains untested:** whether a Zai Student retrieves the card, benefits
from these hints, or repairs its proof after feedback. No such attempt occurred.

## Error signal contract for a later Student attempt

Record problem and cascade revision, node, attempt artifact, actual search/read
receipts, attempted inference, and relevant checker/reviewer finding. Use the
narrowest supported category; uncertain evidence remains `undetermined`.

| Signal | Evidence needed | TA action |
|---|---|---|
| retrieval-miss | search/query results and exposure record; relevant entry absent | improve discovery terms or links; do not rewrite a valid proof method by default |
| applicability-not-recognized | relevant entry read; attempt never connects its conditions to the goal | add recognition cues or a contrasting worked example |
| condition-gap | attempted step uses the method without a needed premise | expose that premise as a goal, with a counterexample when absent |
| construction-gap | conditions established; failed attempt at the supplied construction | expand one intermediate construction or provide a staged hint |
| library-error | source text itself invalid, independently checkable contrast | queue reviewed library correction; preserve old revision and affected accounts |
| verification-failure | a proposed proof step fails a checker/reviewer obligation | repair the proof; citations cannot overrule the failure |
| undetermined | incomplete exposure or proof evidence | collect the missing record without assigning blame |

Do not infer retrieval failure from an unsuccessful proof, or successful use
from a citation. Do not attribute a synthetic response to a live Student.
The TA may offer a new candidate pattern, but authoring it cannot discharge
the open proof obligation that motivated it.

## Bounded live/retrospective role contract

For a live Codex solver, retain the checked proof as the primary mathematical
artifact and add this TA account as a separate deliverable: decomposition,
recognition cues, conditions, staged hints and known wrong applications. For
historical extraction, also compare the encoded statement with the original
problem and label conditional/repaired results. Preserve proof provenance and
never silently rewrite an old solution to make a proposed pattern fit.

This packet implements the deliverable format and one teaching example. It does
not implement either runtime loop or change role prompts. Its small feedback
exercise tests a specific premise correction mathematically; it does not measure
teaching effectiveness. Those are distinct future validation tasks.
