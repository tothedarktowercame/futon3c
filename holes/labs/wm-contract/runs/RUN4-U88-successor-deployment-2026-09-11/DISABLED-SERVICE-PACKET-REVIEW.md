# Disabled historical service packet review

Accepted the structural casting change 387d6592. Materializer casting has exactly
three nonblank string roles, with the author distinct from both reviewers.
The trusted-entry boundary still compares the entire server casting against
the task-pin envelope and refuses mismatches. No request gains authority to
choose actors. The historical selector independently checks the verifier's
author and repair-reviewer identities.

Independent materializer regression: 7 tests / 18 assertions passed. Added
missing-role, author/ordinary-reviewer collision, blank repair reviewer, numeric
author and extra-role controls. Expanded suite: 8 tests / 23 assertions passed.
Lint zero errors/warnings, check-parens and diff check pass.

Packet d3bb791d SHA256
`ddcb28875e40ae44e0f84feecaabf13698a9914f3cb93fa968ca9147fa70ed64`
is accepted as preparation data only. It accurately pins the verified artifact,
casting and proposed identities, but its schema is not the deployment
materializer's template schema. It cannot yet be fed into actual service
construction. No packet-specific async gate has been established by this review;
the prior U88-based fixture does not substitute for it.

Remaining preparation is feasible without changing reviewed Futon2 HEAD:
freeze the manifest, task pin, cohort preregistration and exact deployment
schema under a new Futon3c preparation directory. Use explicit source snapshots
and canonical authority paths; preserve original source provenance and hashes.
Do not edit or repin qualification source HEAD merely to add packet files.
Then exercise those actual frozen inputs in disposable roots through the real
materializer, trusted admission, cohort, repair store, async writer/readers,
observation and duplicate-step path. Test-local path relocation and evidence
copies must be explicit and digest-rebound; they are not production admission.

Historical execution remains unperformed. A later successor link must come
from its actual typed execution receipt. No live capacity, attempt, repair
state, reset, restart, credential or configuration changed in this review.
