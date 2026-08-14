# E-formalisation-defect-memories

**Opened 2026-08-06 (claude-3), from Joe's observation:** we have treated
*proving* as a learnable skill, scaffolded by the memory system, but *formalisation*
as a one-off throwaway process with review as a safety net. If proving is
learnable, formalisation should be too.

This excursion records why that asymmetry exists mechanically (not just by
oversight), what the APM statement-bank review produced that can close it, and
— the part that changes the design — **which defects should become memories at
all**, measured rather than assumed.

## 1. The asymmetry is structural, not an oversight

The proving lane has a genuine measurement instrument: `apm-driver/GRADING.md`
defines machine-checked citation markers, a plan-impact field
(SUPPLIED/CONFIRMED/UNCLEAR), a **mechanical veto-grep** that can overrule an
agent's self-report at `sha-pre`, use-witness typing, and an edge ledger with
n-counts.

The formalisation lane has one hedged line in `templates/statement-only.md`:

> Consult `memory_search`/`psr_search` **if available** for encoding patterns
> previously used …

It was never available. `memory_search` and `memory_record` are defined **only**
in `src/futon3c/agents/zai_api.clj` — the zai adapter. The entire statement
campaign ran on Codex seats, which have neither tool.

**Receipt:** 259 problems formalised across waves 1–5, with **zero memory reads
and zero memory writes**. The `if available` hedge made the disconnection
invisible: the packet looked memory-aware and the lane was not.

Review findings had no home either. They land as free-text
`flagged:<reason>` strings in `statements-manifest.jsonl` — not typed, not
retrievable, not citable. The loop is open at both ends.

## 2. Why formalisation is learnable — and has an advantage proving lacks

Proving memories are *route* memories: "this lemma closed that goal." Their
value is hard to attribute (hence the veto-grep, and the DASHED
memory-used → problem-solved edge in GRADING.md).

Formalisation defects come in **matched positive/negative pairs inside the same
corpus**, which is a strictly stronger training signal:

| defect (negative) | worked example (positive) |
|---|---|
| `t97A01`, `t01A02` — homology via opaque types / an `axiom` | `t00A02`, `t02A03` — real `singularHomologyFunctor` |
| `m97A06` — Euler–Lagrange *postulated* as a definition | `m01J06`, `m02J04` — EL *derived* from the first variation |
| `t00J01` — manifold facts via a fabricable record | `t91A02` — a record whose `euler_eq` **pins** the invariant |
| `t00A03` — conclusion baked into the hypothesis | `t02A01` — genuine reduction (commutator in `Perm(ZMod n)`) |

A defect memory can carry "don't do X, here is the corpus problem that did Y
instead" — advice with an executable referent, not exhortation.

There is also a **cleaner warrant than the proving side has**. Proving grades
rest on agent self-report plus a mechanical veto. Formalisation defects are
labelled by the *reviewer*, not the author. There is no self-report to
discount, so no veto-grep is needed.

## 3. The taxonomy is a triage device, not a memory schema

The 34 flags from the 259-statement review fall into six species:

1. **Unprovable by construction** (14) — `opaque` constant in the *conclusion*.
2. **Informationless hypothesis** (4) — `opaque` carrying the structure premise.
3. **Vacuous** (2) — object of study absent; answer dischargeable by fabrication.
4. **Assumes the conclusion** (3) — the hard step is a hypothesis.
5. **Tautological conjunct** (4) — `P ↔ <the definition body of P>`.
6. **False as stated** (1) — unconstrained parameters admit a counterexample.

**The design claim: not all of these should be memories.** A memory is
retrieved *probabilistically*; a lint is not. Any species with a reliable
mechanical signature belongs in the statement gate, where it costs nothing per
problem and cannot fail to fire. Promoting a finding to a gate **retires** it;
promoting it to a memory only makes it *likely* to be recalled.

### Measured, not assumed (2026-08-06, n=247 labelled statements)

Detector = `axiom` present, or an `opaque` constant appearing in the theorem:

| | count |
|---|---|
| fires & flagged (true positive) | 20 |
| fires & approved (**false positive**) | **1** |
| silent & approved (true negative) | 212 |
| silent & flagged (judgment-only) | 14 |

Precision **20/21**. The single false positive is `t94J05`, which I approved
*despite* its opaque hypothesis because the conclusion (every map `T² → S³` is
null-homotopic, `S³` being 2-connected) holds regardless — a reasoned
exception, not a detector error.

Two other detectors were tried and are **not** gate-worthy:

- *helper defined but unused in the theorem* — 1/1, but n is too small to
  promote; caught `m02A02` (the false-as-stated case). Advisory.
- *definition-mirroring `↔`* — **2/13**. Eleven were genuine equivalences
  (`m95A03`'s Dirichlet principle, `m98A06`'s explicit solution family).
  Advisory only; this is precisely the species that needs a human read.

**Split: ~20 of 34 defects (59%) are gate-able; 14 (41%) are irreducibly
judgment.** Only the second population justifies memories.

## 4. Proposed shape

**Gate lane (blocking, deterministic).** Extend `statement_gate` in
`statement_campaign.py` with the opaque/axiom detector as a `defective`
outcome, plus a named allowlist so a reasoned exception (`t94J05`) is recorded
as a decision rather than silently tolerated. The two advisory detectors join
`conclusion_lint`, which currently runs ~85% false positives and should be
re-scored against these labels.

**Memory lane (judgment species only).** Record shape, one per defect:

```
species          # one of the six
mechanism        # WHY it is a defect ("Lean opaque has no defining equations")
detector         # mechanical signature, or explicitly "judgment-only"
repair           # what to do instead
negative-anchor  # problem id + the exact offending construct
positive-anchor  # corpus problem that did it right
labelled-by      # reviewer + date (no self-report to discount)
```

**Read path.** `dispatch_with_recall`'s `:push` channel assembles a text packet
and is adapter-agnostic, so it reaches Codex; `:pull-only` needs the zai tools
and does not. The statement campaign calls `agency.dispatch_fn` directly and
bypasses recall entirely. Wiring formalisation dispatch through `:push` is the
minimal change that gives the lane a read path at all.

**Write path.** Review verdicts are the source. Persisted out of `/tmp` to
`apm-driver/review/`:

- `statement-review-verdicts-20260806.tsv` — **117** per-problem adjudications
  with reasoning (every flag, plus the approvals whose mathematics I re-derived).
  Not all 259: the remainder were approved on a read that produced no finding
  worth recording, and carry only their manifest status.
- `statement-labels-20260806.txt` — the FLAG/OK labels the §3 measurement used.

The 117 are the seed corpus. The gap matters for §5: a positive anchor is only
usable if someone wrote down *why* the good version is good, and that is
exactly what the unrecorded approvals lack.

## 5. Falsifiable prediction

If defect memories work, the next formalisation wave should show:

- **~zero** defects in the gated species (that is the gate's job, not learning's);
- a **lower rate in the judgment species** than this campaign's 14/259, with
  cited positive anchors appearing in the declared-repairs headers.

If the judgment rate does not move, the honest conclusion is that formalisation
defects are not transmissible by retrieval and the gate is doing all the work —
which is itself worth knowing, and is why the gated and judgment populations
must be counted separately rather than reported as one "defect rate".

## 6. Open — needs Joe

- Is the seed corpus the 34 flags only, or all 259 (approvals carry the
  positive anchors, and the pairs are what make this stronger than route
  memories)?
- Does this stay APM-local, or is "formalisation defect" a corpus-independent
  category that should live alongside the pattern library?
- BPM (~1,019 problems) is the natural test: it has never been formalised, so
  it is a clean arm for the prediction in §5.
