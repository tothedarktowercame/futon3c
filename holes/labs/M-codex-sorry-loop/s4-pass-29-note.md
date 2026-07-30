# S4 scribe pass 29

- Mode: two overnight proving runs; drafts only; no store-write endpoint was
  called.
- Run A: a01A06, solved axiom-clean.
- Run A turn-rounds: `e-codexroll-019fa2c1-t041`,
  `e-codexroll-019fa2c1-t042`.
- Run A receipt: `927e38e5-4296-468a-8631-909543486ceb`.
- Run A commit: `9ea0efdcf844292928590cf5cf20b94a16f96e75`.
- Run B: a01A07, blocked with one new axiom-clean helper and two serial
  remaining constructions.
- Run B turn-rounds: `e-codexroll-019f9b12-t043`,
  `e-codexroll-019f9b12-t044`.
- Run B receipt: `57ca09c6-cfd2-441e-a99b-c96f6c2fffaa`.
- Run B commits: `fc069ee1ad78ebe7ecf311dce122c24675014fd5`,
  `da77ea7421f9f266630a9a193922a09e7b77e2e4`.
- Solve-lane yield: 2 drafts.
- Arc-lane yield: 0.
- Frontier-lane yield: 2 drafts.
- Trajectory-lane yield: 0.
- Total yield: 4 drafts.

All six supplied evidence ids resolved and matched their stated runs. Both
proof files were read at the supplied commits.

Run A yields the singular-power Orlicz counterexample. The central choice is
`g(x)=x⁻¹ᐟ²`: `g` is in `L¹`, `g²=x⁻¹` is not integrable, and
`g log g` is dominated by the integrable `4x⁻³ᐟ⁴`. Young–Fenchel follows from
`Real.add_one_le_exp`; choosing the bound from the finite entropy integral
avoids an irrelevant exact evaluation.

Run B yields one axiom-clean solve record and two serial frontier records.
`norm_le_circleAverage_norm` isolates the analytic circle identity. The first
frontier is the polar-coordinate conversion from circle averages to disk area.
The second uses that disk-area estimate to construct local uniform convergence
on half-radius balls and then invokes the local-uniform/compact-uniform API.

## Verdict on transitive `sorryAx`

This is a confirming and sharpening instance of the promoted memory
`e-codexpilot-separate-lexical-sorry-count-from-real-proof-hole-count`, not of
the section-`include` memory.

The section-`include` record concerns the elaborated *type*: an unused section
variable silently omitted from a declaration. This run concerns the elaborated
*axiom dependency closure*: two declarations contain no direct `sorry`, yet
both inherit `sorryAx` through `tendstoUniformlyOn_of_L1_on_disks`. These are
different mechanisms.

The proof-hole-meter memory already states the correct invariant: inspect
elaborated declarations and require absence of `sorryAx`; never infer closure
from a lexical count. This run supplies a stronger confirming case in which
the lexical count improves by two while genuine discharges are zero. A new
memory would duplicate that rule. The note proposes amending its observed
cases with:

- direct sorries: 4 → 2;
- newly usable declarations: 0 from those two reductions;
- both rewritten declarations still transitively include `sorryAx`;
- exactly one separate helper, `norm_le_circleAverage_norm`, became
  axiom-clean.

The promoted meter memory was fetched successfully before this classification.

## Verdict on the third route correction

Run A is a third confirming instance of
`e-codexpilot-override-a-documented-proof-route-when-component-evidence-favors-another`.
The supplied `x⁻³ᐟ⁴` witness was mathematically viable, but component evidence
favoured `x⁻¹ᐟ²` because its squared failure is exactly the standard borderline
`x⁻¹`; `x⁻³ᐟ⁴` then serves only as the entropy dominator.

Three instances raise the existing inference from a single observed override
to a recurring rule across distinct areas. They do not add a new trigger or
decision procedure, so I did not draft a duplicate trajectory memory. The
solve draft records this as `:third-confirming-instance`; owner review may
amend the existing memory's confidence and evidence list.

The promoted route-override memory was fetched successfully before this
classification.

## Recall scope

Run B produced no recall or terrain inference. Dispatch recall timed out, so
Metric 3 is not measurable.

Run A's completed 5-surfaced/0-used result is a valid benchmark negative, but
it does not yield a reusable mathematical rule. Each candidate was declined
for a specific mismatch; the closest memory concerned essential boundedness
rather than the required `L²` counterexample. This remains benchmark evidence,
not a new memory.

## Subject handles

Reused:

- `M-codex-sorry-loop`
- `a01A06`
- `a01A07`
- `math/measure-integration-api`
- `math/holomorphic-disk-api`

Minted: none.

The two pattern handles were confirmed in the read-only live graph. Searchable
terms—Orlicz, entropy, Young–Fenchel, power singularity, circle average, polar
coordinates, disk-area sub-mean, local uniform convergence, and transitive
`sorryAx`—are present in hooks and bodies rather than only in subjects.

The related promoted memories
`e-codexpilot-package-closed-ball-holomorphicity-as-DiffContOnCl-for-Cauchy-estimates`
and
`e-codexpilot-derive-circle-curve-length-from-the-average-of-the-derivative`
were also fetched. The circle-submean draft explicitly distinguishes its norm
inequality from those existing derivative-estimate and curve-length uses of
the same interface.

Every hook supplies an actionable mathematical or verification cue beyond its
memory name, and every solve draft has a nonempty `:how-to-apply`.
