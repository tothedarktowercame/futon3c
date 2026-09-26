# S4 scribe pass 31

- Mode: two problem closures; drafts only; no store-write endpoint was called.
- Run A: a01A12 solved axiom-clean, sorries 3 → 0.
- Run A receipt: `4c90c60d-1040-4531-b154-e1bd159a567d`.
- Run A turn-rounds: `e-codexroll-019f9b12-t048`,
  `e-codexroll-019f9b12-t049`.
- Run A commits: `3ed025ee450a88066e19babdc70ed67d62eae335`,
  `3fa3a921d2aad03e4606df1e91801c487a8b0a76`,
  `fddc86cc3bef10cf5072fc575275591dcdf95050`.
- Run B: a02J05 closed axiom-clean on attempt 3.
- Run B receipts: `f8586558-7882-4ce9-b2cf-29402de797b1`,
  `1a5d2232-4c2a-4e0c-a31b-155e635899da`,
  `00cf1cdd-0bf5-47f3-ab5c-3b93c9ea331c`.
- Run B turn-rounds: `e-codexroll-019fa2c1-t046`,
  `e-codexroll-019fa2c1-t047`.
- Run B final commit: `32c53bafff982b6dbfe5d4a59e0ceb31a447664d`.
- Solve-lane yield: 2 drafts.
- Arc-lane yield: 1 draft.
- Frontier-lane yield: 0.
- Trajectory-lane yield: 0.
- Total yield: 3 drafts.

All five supplied receipts resolved and named the expected problems. The
interleaved rollout sessions were locally dry-run harvested and each cited
turn body was checked:

- a02J05 attempts 2–3 are codex-7 turns 46–47;
- a01A12 is codex-6 turns 48–49.

`e-codexroll-019fa2c1-t046` is already present in the store. The other three
cited turn-rounds must be harvested by ground control before promotion. I did
not use the harvester's `--commit` mode because this pass forbids store writes.

## Yield

Run A yields one construction-level solve memory and one branch-sensitive arc.
The solve memory records the rotate–square–translate–fourth-root decomposition.
The arc records the non-obvious deleted-segment argument: pull hypothetical
real unit-segment membership back through the fourth-power identity.

Run B yields one resolved-frontier solve memory covering the complete Abel
regularization architecture. It proposes a `:uses` edge to the proved
integration-by-parts tail identity and a `:resolves` edge to the original Abel
removal frontier.

## Verdict 1: carrying negative search results forward

This is capturable, but as an amendment to
`e-codexpilot-order-proof-search-by-known-route-components-before-literature`,
not as a parallel search-policy memory.

The existing memory orders the arms according to whether a mathematical route
is already known. a02J05 adds the missing temporal rule: a sufficiently
specific negative result can be cached across attempts and can justify not
rerunning an arm. Without that addition, “fall through to Zulip/arXiv” could be
misread as “repeat the same searches on every attempt.”

The cache must be scoped rather than permanent. Record:

- the exact sources or namespaces searched;
- the query family;
- the observed absence or background-only result;
- the library/archive revision or date;
- the construction stage it failed to supply;
- explicit invalidation triggers: changed target, changed query vocabulary,
  relevant dependency revision, new theorem/PR, or evidence that the earlier
  search was too narrow.

For a02J05 the carried record was specific enough: installed Sinc and
ImproperIntegrals lacked the theorem; Zulip lacked a formalized construction;
the named arXiv papers were background only. Attempts 2 and 3 therefore spent
their budget on component assembly instead of repeating two negative arms.

This is stronger than merely recording that a search was empty. It is a
bounded negative-search cache with rerun conditions.

## Verdict 2: the three-attempt arc

The seven-part frontier-writing hypothesis from pass 30 survives this second
case unchanged:

1. exact residual target;
2. proved prefix separated from the gap;
3. serial dependencies recorded;
4. ordered candidate route in library-level operations;
5. available API and absent packaging named;
6. untested status stated honestly;
7. trigger vocabulary placed in hook and body.

The a02J05 first-run frontier contains all seven. Attempt 2 used it to prove the
convergence/tail-control half while leaving the exact value open; attempt 3 used
the same route and prefix to finish the Abel evaluation. The run also supplies
a persistence property not visible in a one-step closure: a good frontier can
be partially consumed without becoming obsolete, provided its residual status
is updated after each attempt.

This is now a two-case emerging rule across distinct terrains:

- a01A07: local disk convergence, closed one attempt later;
- a02J05: conditional improper integration, carried across three attempts.

I still did not mint a general process memory. Both are positive cases and
there is no contrast showing which field is essential or what a failed
frontier record looks like. The evidence now warrants keeping the checklist as
an explicit candidate rule and adding “update the residual after partial
consumption.” A third terrain or a negative contrast should trigger promotion.

## Hand-applicable amendments

### A. Conditional search-order memory

Target:
`e-codexpilot-order-proof-search-by-known-route-components-before-literature`

Append a `:negative-result-cache` policy:

- a negative result may suppress rerunning an arm only when it records sources,
  query family, date/revision, and the missing construction stage;
- cache entries are invalidated by a changed target, materially new vocabulary,
  dependency/archive revision, relevant new theorem/PR, or evidence that the
  original query was too narrow;
- distinguish `:not-needed`, `:searched-empty`, and
  `:background-only-no-construction-anchor`.

Add the a02J05 observed case:

- attempt 1 receipt `f8586558-7882-4ce9-b2cf-29402de797b1`;
- attempt 2 receipt `1a5d2232-4c2a-4e0c-a31b-155e635899da`;
- closure receipt `00cf1cdd-0bf5-47f3-ab5c-3b93c9ea331c`;
- closure commit `32c53bafff982b6dbfe5d4a59e0ceb31a447664d`;
- effect: Zulip and arXiv were deliberately not rerun, while component work
  carried the proof.

Suggested confidence:
`:multi-case-conditional-policy-with-observed-negative-cache-payoff`.

### B. Resolve the Abel-removal frontier

Target:
`e-codexpilot-remove-Abel-regularization-from-the-Dirichlet-sinc-integral`

- change status from open to resolved;
- retain the original proved prefix and background-only literature result;
- add attempt-2 receipt `1a5d2232-4c2a-4e0c-a31b-155e635899da` and commit
  `0671018683aaccb36e727f0080b577a1bc446347`;
- record attempt 2's partial resolution: uniform Dirichlet tail and existence
  of positive and symmetric improper limits;
- add closure receipt `00cf1cdd-0bf5-47f3-ab5c-3b93c9ea331c` and commit
  `32c53bafff982b6dbfe5d4a59e0ceb31a447664d`;
- replace the untested candidate route with the validated damped Fubini,
  uniform Abel-tail, compact-head dominated-convergence, three-error, and
  uniqueness-of-limits route;
- record `:use-status :carried-across-three-attempts`.

### C. Sinc tail-identity memory

Target:
`e-codexpilot-derive-a-sinc-tail-identity-by-differentiating-cosine-over-x`

Append two downstream uses:

- attempt 2 used the identity to prove
  `abs_integral_sinc_le_two_div` and the Cauchy convergence of finite sinc
  integrals;
- attempt 3 reused the same tail control inside the Abel-removal estimate and
  closed `dirichlet_integral_improper`.

Evidence:

- receipts `1a5d2232-4c2a-4e0c-a31b-155e635899da`,
  `00cf1cdd-0bf5-47f3-ab5c-3b93c9ea331c`;
- commits `0671018683aaccb36e727f0080b577a1bc446347`,
  `32c53bafff982b6dbfe5d4a59e0ceb31a447664d`.

Suggested confidence:
`:multi-attempt-directly-consumed`.

### D. Proof-hole measurement memory

Target:
`e-codexpilot-separate-lexical-sorry-count-from-real-proof-hole-count`

Append a02J05 attempt 2 as the third distinct divergence shape:

- headline sorry delta: 0;
- new axiom-clean declarations: 4;
- newly usable declarations: 4;
- target remained sorried.

Then append attempt 3:

- headline delta: −1;
- newly usable declarations: 12;
- all 17 declarations axiom-clean.

This reinforces the already amended three-axis meter: direct holes,
newly axiom-clean declarations, and declarations unblocked through dependency
closure.

Evidence:

- receipts `1a5d2232-4c2a-4e0c-a31b-155e635899da`,
  `00cf1cdd-0bf5-47f3-ab5c-3b93c9ea331c`;
- commits `0671018683aaccb36e727f0080b577a1bc446347`,
  `32c53bafff982b6dbfe5d4a59e0ceb31a447664d`.

### E. Frontier-drafting candidate rule

Do not promote yet. Append a02J05 as the second confirming case in the
candidate-rule register created from pass 30, and add one item:

8. after a partial attempt, update which prefix is proved and restate only the
   residual gap; do not discard the still-valid route.

The evidence now supports `:two-case-emerging`, not a settled general rule.

## Subject handles

Reused:

- `M-codex-sorry-loop`
- `a01A12`
- `a02J05`
- `math/holomorphic-disk-api`
- `math/measure-integration-api`

Minted: none.

The existing pattern handles were confirmed in the read-only graph export.
Searchable terms—principal `cpow`, argument bounds, slit plane, fourth root,
Dirichlet sinc integral, Abel regularization, Fubini, uniform tail, dominated
convergence, and negative search cache—appear in hooks and bodies.

All three hooks state a usable move beyond their memory names, and every draft
has a nonempty `:how-to-apply`.
