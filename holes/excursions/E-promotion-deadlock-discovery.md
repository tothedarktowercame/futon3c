# E-promotion-deadlock-discovery

**Opened 2026-08-18 by codex-6 for claude-2. Discovery only. No source or
store state was changed.**

## Finding

The three refusals are real, but they are not three independent policy
failures. D3 and D4 expose one missing operation: the conductor has no way to
apply a separately authored review to an already proposed attachment. Its only
operation, `promote-memory-attachment!`, is an attach-and-self-generate-review
shortcut whose preconditions describe an unattached memory. The current frame
workflow supplies an independent reviewer and often an already attached
proposal, so the shortcut rejects the well-formed cases.

The load-bearing rule must remain: the memory author must not be the review
author. The recommended repair is to make immutable review evidence, not the
guide's claimed identity, authoritative. Merely deleting any author/reviewer
comparison is not an acceptable fix.

## D3 — reviewer is forced to be the conductor actor

### Verified mechanism

`promote-memory-attachment!` receives `:reviewer` from the request and
`:acting-identity` from context (`src/futon3c/peripheral/memory_lifecycle.clj:203-213`).
It first rejects a reviewer different from the acting identity
(`memory_lifecycle.clj:232-237`), then reads the memory's durable
`:evidence/author` as depositor and rejects equality
(`memory_lifecycle.clj:240-253`).

The guide cannot currently pass the scribe as reviewer. At the authenticated
HTTP/conductor boundary, `reviewer-mismatch` rejects any adjudication or direct
promotion whose reviewer differs from the calling agent
(`src/futon3c/apm/conductor_surface.clj:37-50,70-85`). The same boundary stamps
the caller as `:acting-identity` on every promotion
(`conductor_surface.clj:29-35,53-65`). The test explicitly calls this rule
“P14 forbids the guide from impersonating the scribe”
(`test/futon3c/apm/conductor_test.clj:530-540`). Therefore changing only the
request's `:reviewer` cannot work: the surface rejects it before lifecycle code
runs.

There is a second obstruction. `review-attachment!` does correctly verify that
the review evidence author differs from the memory author
(`memory_lifecycle.clj:118-136`), but it also requires the invocation agent and
session to equal the evidence author and session
(`memory_lifecycle.clj:137-146`). The scribe card says the scribe must execute
its own approval (`holes/labs/M-apm-demonstration/role-cards/scribe-v2.md:53-55`),
yet the public conductor operations contain no review operation—only
`:promote-artifact` (`conductor_surface.clj:7-18`)—and the conductor is bound to
the guide session. The documented rule and callable surface therefore disagree.

The existing shortcut manufactures new review evidence using the requested
reviewer but the conductor session (`memory_lifecycle.clj:280-300`), then calls
`review-attachment!` with that reviewer substituted as `:agent-id`
(`memory_lifecycle.clj:307-316`). That is suitable only when the authenticated
actor really is that reviewer; it cannot consume the independent scribe review
reported for frame 10.

### Recommended fix

**Design change, not a one-line relaxation.** Add a distinct “apply existing
review” promotion path taking `memory-id`, exact `pattern-ids`, verdict, and
`review-evidence-id`. Fetch the evidence and derive the reviewer from its
immutable `:evidence/author`; never accept reviewer authorship as a guide-supplied
fact. Validate all existing invariants in `validate-review!`—especially memory
author != review author, exact subject, exact patterns, verdict, provenance and
witness status (`memory_lifecycle.clj:118-180`). The conductor may authorize
the state transition, but it must not impersonate the reviewer. This requires
separating “review evidence authenticity” from “identity allowed to project an
already authenticated review”; today those are conflated at
`memory_lifecycle.clj:137-146`.

An alternative is to expose `review-attachment!` as a scribe-authenticated
conductor action and let the scribe execute it before the guide advances. That
matches the frozen role card most literally, but requires multi-seat authority
over one live conductor binding and is the larger design change.

**If wrong:** removing `reviewer-not-actor`, substituting the scribe's name, or
dropping the depositor comparison would let the guide forge an independent
review or self-review. That destroys the author-not-equal-reviewer property and
must not be done. The proposed evidence-consuming path preserves it because the
reviewer's identity comes from separately persisted evidence and remains
checked against the memory's persisted author.

## D4 — proposed attachments are rejected as “not statusless”

### Verified mechanism

The memory writer extracts pattern subjects into `:roles :patterns`
(`src/futon3c/peripheral/memory_write.clj:181-205`) and deliberately marks any
such attachment `:proposed` (`memory_write.clj:206-222`). Its comment states
that an agent-supplied pattern is a proposal requiring librarian/reviewer
promotion (`memory_write.clj:217-220`). This is the correct safe write: an
unreviewed pattern claim is explicit but cannot masquerade as reviewed.

The promotion shortcut rejects the mere presence of
`:attachment-status`, including `:proposed`
(`memory_lifecycle.clj:255-259`). It then separately rejects any existing
pattern list (`memory_lifecycle.clj:261-266`). Its success path is specifically
for a statusless, patternless edge: it adds the pattern and sets `:proposed`
itself (`memory_lifecycle.clj:267-274`). Thus D4 is confirmed, but the wrong
component is the **promotion dispatcher/precondition**, not `memory_record`.
The latter produces precisely the explicit proposal state that
`review-attachment!` expects: that function checks exact pattern equality and
current state, then changes the status from proposed to reviewed/challenged
(`memory_lifecycle.clj:350-371,388-408`).

### Recommended fix

**Small design change sharing the D3 path.** Dispatch by actual edge state:

- statusless and patternless: attach the requested pattern as proposed;
- `:proposed` with exactly the reviewed pattern set: consume the independent
  review evidence and call the invariant-preserving review transition;
- reviewed/challenged, or a different/nonempty pattern set: refuse or use the
  existing idempotency/conflict rules.

Do not remove `:attachment-status :proposed` from `memory_record`; that would
erase lifecycle information and make a well-formed proposal indistinguishable
from legacy incomplete data. Do not simply allow all non-statusless edges;
that could overwrite challenged/reviewed state or approve a pattern set the
review did not certify. Exact-pattern validation at
`memory_lifecycle.clj:158,363-367` must survive.

**If wrong:** a blanket relaxation of the status gate could mutate a reviewed
or challenged attachment and detach the projected status from its review
history. It would not directly remove author != reviewer, but it would make
that review certify different content, which is equivalent corruption.

## D5 — student deposits use a different domain

### Verdict: CONFIRMED for currently minted APM student seats; REFUTED as a
universal statement about all possible student configurations

The ZAI `memory_record` tool stamps the domain from `ctx :memory-domain`,
defaulting to `:zaif-work` (`src/futon3c/agents/zai_api.clj:601-617`). The ZAI
invoke constructor likewise resolves an explicit `memory-domain`, a provisioned
domain, or `:zaif-work` (`zai_api.clj:1257-1268`) and passes it into the backend
(`zai_api.clj:1277-1284`). A test asserts the default is `:zaif-work`
(`test/futon3c/peripheral/memory_write_test.clj:315-338`).

Frame seats define the student as ZAI (`src/futon3c/agency/frame_seats.clj:6-11`).
The frame-seat preparer passes model, evidence store and session data, but no
memory domain (`src/futon3c/transport/http.clj:2532-2554`), although the local
ZAI constructor supports one (`http.clj:2513-2524`). Therefore the ordinary
minted APM student uses the ZAI default. This is a configuration omission, not
an intrinsic property of student authorship: an explicitly configured ZAI
student can use `:mathematics`.

Promotion always constructs lifecycle context with `:domain :mathematics`
(`src/futon3c/peripheral/problem.clj:1674-1697`). `validate-edge!` throws when
that requested domain differs from the edge domain
(`memory_lifecycle.clj:74-89`), and `problem.clj` catches it as
`:promotion-attachment-review-threw` (`problem.clj:1698-1701`). The reported
failure shape is therefore exactly predicted by source. A live read also
confirmed `e-3a3aed11-214d-4100-a0d8-e3a6016abf65` is authored by
`f10-student`; the domain itself is projected on its memory/assert edge by
`memory-hyperedge` (`memory_write.clj:206-216`).

### Recommended fix

**Needs an operator domain ruling, followed by a small configuration design
change.** If all APM frame knowledge belongs to the mathematics lifecycle,
mint the student seat with `:memory-domain :mathematics`, threaded explicitly
through the seat specification/preparer rather than inferred from an `fN-`
name. If student-private working memory is intentionally `:zaif-work`, then
cross-domain promotion needs an explicit import/re-home operation; weakening
`validate-edge!` is not acceptable.

**If wrong:** globally changing ZAI's default would merge unrelated ZAI work
into mathematics and weaken domain isolation. Allowing the conductor to ignore
the mismatch could review or mutate an edge under the wrong domain policy.
The safe fix is scoped configuration or an auditable cross-domain transition.

## D6 — refusals disappear from the machine trace

### Confirmed mechanism

The cycle engine appends a `:steps` record only on the success branch
(`src/futon3c/peripheral/cycle.clj:361-393`). A backend refusal instead returns
a runner error without a replacement state (`cycle.clj:345-352`). The conductor
does add the refusal to its transient handle log (`src/futon3c/apm/conductor.clj:104-129`),
but `saved-step` checkpoints only successful handles
(`conductor.clj:131-141`). Most decisively, the binding refuses to install a
failed next handle: it returns an error while leaving the authoritative handle,
version and receipt map unchanged (`src/futon3c/apm/conductor_binding.clj:144-180`);
only success resets the handle and advances the version
(`conductor_binding.clj:181-194`). Hence neither `:state :steps` nor the saved
cycle contains D3-D5, and even the transient conductor log is discarded at the
binding boundary.

### Recommended fix

**Design change.** Introduce a durable failed-action receipt that records tool,
sanitized args, structured refusal, action id and step index without applying
the action's state mutation. It should be appended atomically to the cycle
trace (or a machine-owned refusal ledger included by trace derivation), saved,
and versioned, while leaving the conductor usable. Do not “fix” this by
committing the current failed handle: `failure` sets `:ok false`
(`conductor.clj:97-102`), and subsequent `raw-step` calls short-circuit
(`conductor.clj:111-114`), poisoning the frame.

**If wrong:** treating refusal as a successful promotion would contaminate
`:promotion-result`; merely logging prose would recreate the unauditable path;
and storing unsanitized arguments could leak packet/session data. The receipt
must state failure without satisfying phase outputs or promotion counts.

## Smallest-first implementation order

1. **D5 after operator ruling:** thread an explicit frame memory domain. Small
   configuration change; immediately restores the student lane without
   weakening lifecycle checks.
2. **D3/D4 together:** add the missing existing-review transition and proposed
   edge dispatch. This is one coherent design change; implementing either as a
   relaxed comparison is unsafe.
3. **D6:** durable refusal receipts. This is orthogonal but should precede the
   next experiment if failures are expected to be diagnosable from its trace.

## Corrections to `E-APM-f10-defects.md`

- D3's observed deadlock is correct, but “the conductor can pass the actual
  reviewer” is not a small threading fix: the authenticated surface expressly
  forbids it, and the lifecycle also binds invocation session to review-evidence
  session. The system lacks an operation for consuming the already authored
  review.
- D4 is correct about the refusal but should not imply that attaching at write
  time is wrong. `memory_record` intentionally creates a safe proposed edge;
  the attach-only promotion shortcut is being used where the existing review
  transition belongs.
- D5 is confirmed for default minted frame students, not for every conceivable
  student: ZAI already supports an explicit memory domain, but frame minting
  fails to provide it.
- D6 is confirmed and slightly worse than reported: the conductor temporarily
  logs the refusal, but the binding discards the entire failed next handle, so
  even that log does not remain authoritative.
