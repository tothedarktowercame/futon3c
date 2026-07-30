# S4 scribe pass 2

- Mode: drafts only; no store-write endpoint was called.
- Turn reads: three bounded GETs for the exact requested rollout ids.
- Relevant proof evidence: `e-codexroll-019fa2c1-t003`.
- Solve-lane yield: 3 drafts.
- Arc-lane yield: 1 draft.
- Trajectory-lane yield: 1 draft.
- Frontier yield: 0; the needed general theorem already exists in Mathlib.
- Total yield: 5 drafts.

The packet labels t002 as the proof turn, but t002 is the interrupted MetaCA
experiment. T003 is the Schwarz turn and contains the theorem discovery,
orientation fix, validation report, commit, and memory non-use account.

The provided receipt value `5efcc598` is not a resolvable evidence id:
bounded exact GETs for it and `e-5efcc598` both returned 404. No prefix search
or store scan was attempted. Every draft instead cites the complete t003
evidence id; the receipt prefix is retained as unresolved metadata.

The high-value discovery is
`Complex.affine_of_mapsTo_ball_of_norm_dslope_eq_div`: specializing the
general `dslope` equality theorem at zero replaces a bespoke Schwarz
maximum-modulus construction. `Complex.norm_mul_exp_arg_mul_I` then converts
the unit derivative coefficient to rotation form; its observed compilation
fix was simply `.symm`.

All five surfaced memories were explicitly ignored as unrelated. The absence
of a relevant offered memory is recorded as curriculum signal for
complex-analysis coverage, not as proof that an exhaustive live-store census
is empty. The new solve drafts are candidates to become that terrain's first
direct Schwarz-equality memories.

Capture limit: t003 is truncated at the 16KB body cap, but its duplicated final
report preserves the exact theorem names, non-use list, `.symm` fix, and commit.
No unreported motive or timing breakdown was reconstructed.
