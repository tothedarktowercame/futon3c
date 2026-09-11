# V4 teaching exchange implementation

This packet implements the Student plan → independently executed TA judgment →
Student revision → reviewed construction handoff in the existing learning-phase
consumer. It is not a V4 activation or a production-readiness certificate.

## Consumer and authority

`live_learning_phases/build-request` copies an explicit unit
`:v4/teaching-config` into Student dispatch authority before computing its digest.
Absent that configuration, `run-live!` delegates to the existing construction
path. A configured Student runs the teaching prelude first; construction is
not dispatched until its journal contains an accepted TA judgment. The V3 Guide
retains its store/harness-only channel. No generated campaign configuration is
changed in this packet.

Configuration is version 1 with a separately provisioned `:ta-agent-id`,
`:max-revisions` (0–2), and `:job-budget-ms` (positive, at most 900000).
The existing frame Student is the plan author. The TA must have a distinct
registered actor and session. This packet does not provision that TA seat or
assert exclusive ownership of an arbitrary existing agent.

The first plan follows the existing Student workspace reset and Agency session
reset. Agency's actual reset contract clears the session ID; it does not supply
the next one (`agency/registry.clj`, `reset-session!`). Accordingly the initial
plan authority explicitly requires a fresh executed session different from the
prior Student and the TA. The actual completion supplies its session identity;
subsequent revisions pin it. The TA's session is pinned before execution.
These are different guarantees, recorded explicitly rather than treating an
unknown new session as an authenticated pre-execution pin.

Every job uses `job-port` announce/observe/activate and controller-registered
`typed-role-submission`. Registration precedes activation. Admission validates
completion envelopes, exact plan/review ancestry, role, every node, stable IDs,
acyclic obligations, and bounded revisions before accepting immutable payloads.
Student plan and revision submissions require an actual job-bound search
receipt, even if the query returned no relevant pattern. Claimed memory use
must be surfaced and must respect the existing holdout. TA search uses the same
existing authenticated and gated search service, under its distinct role.

Plan nodes carry goals, definitions, explicit conditions and arguments, and
ordinary/pattern/memory/gap warrants. Pattern/memory warrants carry revision
hashes. These are claims for the TA to check, not machine-verified source reads
or mathematical applicability. No citation closes a proof obligation. The TA
can accept a construction plan, require revision, or decline judgment; each
node has a diagnosis and instruction. Acceptance is not mathematical proof.

The accepted exchange is bound into the construction request and its typed
schema. The Student must report used/changed/unused/unknown for every node of
that exact plan. The normal Student receipt retains the exchange and this use
account. The normal proof preservation, compilation, promotion and closure
checks remain in their existing execution path. Use reports are authored
observations, not independent evidence of benefit. Only Student search receipts
are inherited as prior Student exposure; the TA's private search results are
not counted as material served to the Student.

## Durability and limits

A private `PHASE.edn.teaching.edn` journal retains the original construction
request, exact subrequests, observations, revision ancestry and final receipt.
`countdown_control/live-learning-phase-inputs` retrieves that saved request when
ordinary construction has not started, avoiding a new random dispatch on every
tick. Each step takes a file lock and performs one bounded job transition.
There is no internal polling loop. The journal is persisted before activation;
replay observes the same job and never reactivates a completed one. A completed
receipt replays without further Agency calls. Construction repairs retain the
reviewed teaching inputs.

An interruption during workspace/session preparation is explicitly
`:teaching-initialization-reconciliation-required`; the packet does **not**
pretend that it can infer whether an unreceipted reset completed. This must be
resolved through an observable, replayable reset operation before unattended
production activation. Likewise cannot-judge, terminal transport failure or an
exhausted revision budget stops construction with a typed reason. These require
integration with the campaign's apparatus/learning disposition policy rather
than inventing mathematical failure or silently skipping teaching.

The planning role is instructed to work read-only and not read the reference
solution; this is not a new OS sandbox. Canonical memory-service gates apply to
served memories, but free-form model text is not a mechanically verified
information-flow proof. Real separate-actor rehearsal and packet exposure review
remain release gates.

## Validation and remaining work

`validation.json` and namespace output files retain the executed checks. Tests
use real temporary journals, actual typed admission and recorded search
receipts; Agency and search results are substituted fixtures. The reset fixture
now matches the actual nil-session-until-invocation lifecycle. No live model,
Lean invocation, library mutation or shared JVM reload is claimed.

The broader V3 learning test had an existing exact-map expectation that omitted
`:submission/authority-version 2`, introduced in the previously reviewed packet.
The same failure was reproduced in canonical master before updating the expected
map to require the explicit version. No invariant or assertion was removed.

Remaining production work includes independent review of this implementation,
observable reset reconciliation and frame-end dispositions, explicit TA seat
ownership, locked canonical pattern revision publication with index freshness,
a separate next-use receipt, and a real bounded end-to-end rehearsal. In
particular `coined_pattern/publish!` publishes **proposed** entities; it is not a
compare-and-publish operation for reviewed file revisions and is not substituted
for one here. The canonical library owner and all mutation paths need a common
revision boundary before automatic replacement is safe.

## Independent review repairs: session continuity and prior exposure

Review job `invoke-1789157187595-20274-b5588f36` found that construction reset the
Student while crediting planning-session searches (P1), and that revisions did
not inherit earlier searches from their retained session (P2).

Construction now sets `:fresh-session? false` and binds `:session-id` to the
last executed Student planning turn. The actual construction activation callback
checks that exact registered session before any workspace reset or activation.
Terminal validation also checks the executed session. A repair requesting a
fresh session is refused under this authority: it needs explicit redelivery and
new authority, rather than credit for material the new session did not receive.

Each collected planning turn now captures exact search-receipt IDs. The next
revision's immutable authority names prior Student jobs, executed sessions,
submission digests and those receipt IDs. Resolution checks the actual registered
submission, content digest, same actor/frame/problem/exchange/session, and exact
search records. Depositor/holdout gates are reapplied. TA-private searches and
searches added to an old job after its capture are not inherited. Both typed
admission and terminal collection use this verified ancestry.

The teaching exchange journal and authority are now version 2 (plan schema and
configuration remain version 1). Old version-1 journals require explicit
retirement; they are not silently upgraded to exposure guarantees they never
recorded. The regression verifies refusal before effects with the old journal
unchanged. This is a retirement requirement, not an automatic retirement action.

The new tests call the real construction activation callback and verify zero
workspace/session resets, refusal before activation on drift, and terminal
rejection of old-memory use from another session. Revision tests admit an
actually served prior memory after a new empty search, and reject changed
sessions, forged submissions, missing search receipts and withheld content.

Executed validation now passes 165 tests / 851 assertions across the six
namespaces in `validation.json`. Independent confirmation of these repairs is
pending. No V4 code has been loaded into a shared JVM or used for a live trial.
