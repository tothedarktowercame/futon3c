# STATEMENT REPAIR packet — a reviewer rejected this statement

Repo: `/home/joe/code/apm-lean`. FIRST, in every shell you use:
`cd /home/joe/code/apm-lean && export PATH="$HOME/.elan/bin:$PATH"`.
The `cd` is REQUIRED: elan resolves the toolchain from this repo's
`lean-toolchain`, and on hosts with no global default `lake` fails with
"no default toolchain configured" from anywhere else. Verify with
`lake env lean --version` BEFORE deciding anything; if it fails, STOP
and report a toolchain blocker.

Problem: `{problem_id}`
Source of truth: `problems/{problem_id}/problem.md` (the TeX is the contract).

## What the reviewer found

{flag_reason}

## Your job

Rewrite `problems/{problem_id}/lean/Main.lean` so the finding no longer
applies, keeping everything else about the statement faithful. Exactly ONE
theorem, named `apm_{problem_id_lower}`, proof body exactly `:= by\n  sorry`.

This is a REPAIR, not a fresh formalization: preserve whatever the previous
version got right, and change what the finding names. Read the existing file
first.

## The failure modes this campaign has actually hit

These are the six species reviewers found across 259 statements. Yours is one
of them; do not introduce another while fixing it.

1. **Unprovable by construction** — an `opaque` constant appearing in the
   CONCLUSION. Lean `opaque` has no defining equations, so nothing about it can
   ever be proved. Never put one in a conclusion.
2. **Informationless hypothesis** — an `opaque` predicate carrying the whole
   structural premise (e.g. "is a genus-2 surface"), so nothing can be
   extracted from it. Encode the structure concretely instead.
3. **Vacuous** — the object of study never appears, so the theorem is
   dischargeable by fabricating data. A record whose fields are unconstrained
   is fabricable; a record whose fields are PINNED by an equation is not.
4. **Assumes the conclusion** — the hard step appears as a hypothesis, leaving
   a formal triviality. If the mathematical content of the problem is the thing
   you are assuming, the statement is wrong.
5. **Tautological conjunct** — `P ↔ <the literal definition body of P>`,
   closable by `Iff.rfl`. State something the definition does not already say.
6. **False as stated** — parameters left unconstrained so a counterexample
   exists. Anything the mathematics needs must be a hypothesis.

## Worked examples IN THIS REPO — read the one that matches your species

- Real homology / projective space instead of opaque or `axiom`:
  `problems/t00A02/lean/Main.lean`, `problems/t02A03/lean/Main.lean`,
  `problems/t01A01/lean/Main.lean`.
- Euler–Lagrange DERIVED from the first variation rather than postulated:
  `problems/m01J06/lean/Main.lean`, `problems/m02J04/lean/Main.lean`.
- A structure whose invariant is pinned so it cannot be fabricated
  (`euler_eq`): `problems/t91A02/lean/Main.lean`.
- A genuine algebraic reduction of a topological fact (commutator in
  `Perm (ZMod n)`): `problems/t02A01/lean/Main.lean`.

## Hard rules

- NEVER declare an `axiom`. It pollutes `#print axioms` at the proving gate.
- If an object is hard to encode, encode it anyway or leave a documented
  encoding question — NEVER simplify the statement to scalar shadows of the
  real objects. A statement that drops the problem's central object is
  rejected again.
- If the SOURCE is wrong (typo, false claim, contradictory cases), repair it
  minimally and declare it under `## Statement repairs` with the reason.
  Undeclared repairs are rejected.

## Verify and commit

`lake env lean problems/{problem_id}/lean/Main.lean` must exit 0 (the single
`sorry` warning is expected). Then, path-limited:

    git add problems/{problem_id}/lean/Main.lean
    git commit -m "{problem_id}: statement repair (review flag)"

Do NOT use `git add -u`.

Report: the new theorem verbatim, how it addresses the finding, which worked
example you consulted (if any), repairs declared, and the commit sha.
