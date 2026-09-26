# Interpreting a feedback pattern as a local institution

2026-09-10, Codex-17. PROPOSED worked design; no endpoint implemented,
no institution adopted, no live messages sent. This continues the apex
excursion. It does not close the observation-model or institutional-force gaps.

## Joe's direction

Verbatim excerpt from this session:

> So then the question is, how do you interpret that design pattern? Into some kind of locally applicable institution within that particular application area.

> So as you said, who are the participants? And so forth.

> So I think this needs thinking through... Not just in terms of the high-level schematic, but the details of how, what happens if... You know, we try to make it make sense of the requirement.

Interpretation, not an additional ruling: develop a structured interpretation
and execution contract, with local applicability checked. Retrieval or a
system prompt alone does not establish an obligation or its satisfaction.

## What IAD contributes

Ostrom's [1983 rule-configurations paper](https://dlc.dlib.indiana.edu/dlc/items/df548b86-e342-4e90-b217-fbcb6d8cfe62)
identifies participants, positions, actions, outcomes, information, control,
and costs/benefits. These give us concrete questions for specifying the local
situation; they do not uniquely determine its rules.

Corneli's [2016 paper](https://metameso.org/~joe/papers/corneli2016institutional.pdf),
pp. 3–4, connects role-conditioned rules with computational tests, distinguishes
rules/norms/strategies, and discusses participation in rule revision and
distributed monitoring. That supports making both execution and amendment
explicit. The protocol below is a new design proposal, not an implementation
prescribed by those papers.

IAD can guide an institutional interpretation of a pattern; it is not itself
a compiler from prose to a unique institution. An interpretation needs local
bindings, evidence, and authority. A production system can execute some rules;
it cannot by itself establish that people accept them, that information was
understood, or that an institution works socially.

Local basis: `futon2/holes/DESIGN-REQUIREMENTS-apex-2026-09-09.md:42` (R-C);
`futon2/holes/labs/wm-contract/SESSION-C-pattern-institution-duality-2026-09-09.md:21`
(institution/interpreter distinction). Neither warrants collapsing the two.

## Pattern expression, before local interpretation

Illustrative IF/HOWEVER/THEN draft, not a promoted library pattern:

- IF a shared task produces feedback relevant to its participants' subsequent
  work or accountability;
- HOWEVER storing or sending a result does not establish that the affected
  participants received an accessible version;
- THEN identify the affected participants, establish how each can receive and
  respond to the feedback, and retain evidence of delivery and outstanding gaps.

This deliberately leaves open what constitutes a participant, a receipt, an
appropriate deadline, and a response. An interpreter must expose those missing
bindings rather than fill them silently. Applicability is established,
not-applicable, or unknown with evidence and scope; unknown is not permission
to skip. Binding a solo task must not manufacture a team to make this apply.

## One local interpretation: review feedback on a memory caption

Illustrative task: a student drafts a caption; an independent reviewer finds
that its stated applicability is too broad. The revision and its reason should
reach the author and the task owner. A later consumer may need the correction
if it used the superseded revision. These are proposed roles, not assignments
to any live agents.

| Institutional question | Proposed local binding and observable consequence |
|---|---|
| Participants / boundaries | A versioned task roster plus an evidence-backed affected-consumer list. Online presence does not define membership. Record who established the list and why each recipient belongs. |
| Positions | Caption author, independent reviewer, task owner, affected consumer, delivery service, monitor. Bind stable identities; state any permitted role overlap. |
| Choices | Reviewer submits feedback; dispatcher attempts authorized delivery; recipient acknowledges or disputes; owner addresses missing routes. Receipt and agreement are distinct actions. |
| Information | Each recipient gets an authorized view containing the revision, finding, reason, and response route. Record the view digest; access restrictions can block delivery rather than license disclosure. |
| Control / aggregation | The monitor derives delivery coverage from accepted receipts. No author-set success flag. Acknowledgment by one recipient cannot discharge another's obligation. |
| Outcomes / scope | This episode concerns one feedback revision and a frozen membership revision. Delivery, consideration, revision acceptance, and subsequent use are separate outcomes. |
| Costs / responses | Retry only within authorized resource limits. An unavailable recipient creates a visible unresolved delivery obligation. No automatic punishment for transport failure. |
| Amendment | Authorized task governance can change routes or scope with recorded reasons. A change creates a successor episode when obligations change; it does not rewrite the old result as successful. Affected participants can raise a dispute. |

For this worked interpretation, 'reaches' means a receipt from the recipient's
authorized inbox for the specified accessible payload. Transport acceptance
alone is weaker; acknowledgment is stronger. The chosen threshold must be
named in the instance. Neither threshold demonstrates comprehension or use.
If the application lacks verifiable inbox receipts, that is an adapter gap,
not grounds for relabeling a send as a receipt.

A role-conditioned obligation can be represented as: the designated dispatcher
is obliged, when feedback revision F is accepted for task T, to deliver the
permitted view V to every bound recipient by the locally adopted deadline;
otherwise record an overdue obligation and invoke the authorized repair route.
This is a proposed rule with a response, not a new universal sanction policy.

## Structured endpoint contract

These are proposed operations, not claims about existing routes:

1. `POST /institution-interpretations`: pattern revision, task facts, evidence,
   proposed bindings. Returns a draft containing rules, missing bindings,
   applicability findings, and unsupported observation requirements. It cannot
   send messages or adopt its own proposal.
2. `POST /institution-instances`: reviewed interpretation plus adoption warrant
   and participant revision. Checks authorization and required bindings; freezes
   an instance version. Refuses unresolved prerequisites. Standing authority can
   authorize routine instances; a new Joe approval is not required per task.
3. `POST /institution-instances/{id}/events`: typed, authenticated events such
   as feedback-created, delivery-attempted, inbox-receipt, acknowledged,
   disputed, deadline-reached. Validate actor, recipient, payload digest,
   instance version and evidence through the relevant adapter. Event IDs are
   idempotent; conflicting reuse is refused. Client assertion is not proof.
4. `GET /institution-instances/{id}`: derived obligations, accepted evidence,
   pending/overdue/unavailable states, permitted next actions, amendment history,
   and the latest event sequence used for the answer.

Concurrent operations supply their expected instance version. Evidence for an
old revision stays attached to that revision. There is no generic 'mark done'
operation. An application can enforce its adopted completion rule at its own
completion endpoint, consulting this monitor; external acts can still violate
that rule and must remain observable as violations.

Example response after one of two required receipts:

```json
{"status":"pending", "receipt-standard":"authorized-inbox",
 "coverage":{"required":2,"received":1},
 "outstanding":[{"recipient":"task-owner","state":"unavailable"}],
 "delivery-complete":false,"consideration":"unobserved",
 "subsequent-use":"unobserved"}
```

The two names/counts describe a synthetic example, not current campaign data.
An empty recipient set is explicitly vacuous and earns no delivery evidence.
A deadline is a required local binding if the instance claims timeliness; this
proposal invents no universal duration.

## Execution trace and rejecting cases

A reviewer records feedback F at revision 2. The instance requires receipts
from author A and owner O. A's validated inbox receipt makes coverage 1/2.
O is unavailable: coverage remains 1/2, even if the dispatcher retries or O
leaves the live agent registry. A disagrees: receipt remains valid, and a
separate dispute opens. O later receives the correct view: delivery reaches
2/2; neither agreement nor subsequent use is thereby established. A later
caption revision and its review can demonstrate response. A consumer record
naming that revision can demonstrate subsequent consumption.

Acceptance cases for an implementation (specified here, not executed):

- `feedback-delivered:false`, a queued send, or a missing evidence field cannot
  satisfy an inbox-receipt obligation.
- A receipt for the wrong recipient, view digest, or revision cannot count.
- Duplicate receipt events cannot inflate coverage.
- Disconnecting or deleting a live seat cannot shrink the frozen denominator.
- Unauthorized disclosure or an unauthorized agent wake is refused. Joe's
  current prohibition on Claude invocations remains effective even if a draft
  lists a Claude recipient: record the blocked route, do not dispatch.
- A missing participant binding or unknown applicability blocks activation.
- Recipient disagreement counts as receipt, not endorsement; silent receipt
  cannot be counted as subsequent use.
- Deadline expiry records overdue status, never automatic satisfaction.
- An unauthorized amendment cannot erase an outstanding obligation.

## Relation to memory, preferences, and the War Machine

A memory caption helps retrieve a candidate. A statement that it is useful
when continuity holds leaves a separate obligation: establish continuity for
the particular function and domain, using admissible evidence. Likewise,
retrieving this pattern leaves a separate institutional interpretation:
establish the participant set, authority, receipt semantics, and enforceable
operations for this task. LLM suggestions can help produce both drafts;
evidence admission and state transitions require explicit checks.

For the War Machine, an activated instance would supply permitted actions,
obligations, and observations to the task context. Candidate policies could
include delivering a missing view or resolving a dispute. A preference could
rank admissible response strategies once an outcome model exists; it cannot
substitute for delivery receipts or override the prohibition on a send.
No numerical C values or observation-model bridge are supplied by this note.

The smallest meaningful implementation experiment is one caption-review
feedback episode with two bound recipients, one intentionally unavailable
route, later recovery, and observed consumption of the corrected revision.
Run the rejecting cases before claiming the pattern is institutionally
implemented. Independently review the bindings and the execution trace.
This would establish a bounded local protocol, not a general theory of
institutional compliance. Learning can then compare repeated episodes and
propose amendments; it must not silently change the adopted obligations.


## Primer cross-check — 2026-09-10

Joe supplied `/home/joe/iad.txt`, a ChatGPT-assisted operational primer.
Read in full; source SHA256: `b688cd04d5f92362a129655851dd8df652cf35aa59c1faa26a37cff132a17e54`.
This is a design input, not independent scholarly verification. The following
additions make omissions in the worked proposal explicit; they do not adopt
new operational rules.

### Reconstruct before prescribing

The endpoint design above is prescriptive. A separate descriptive record must
state what actors actually did, which information was available at decision
time, and which enforcement or monitoring mechanisms actually operated.
Do not infer a rule-in-use from a desired outcome, an API name, or a declared
rule. Keep declared rule, implemented mechanism, observed conduct, and outcome
evaluation as separately sourced fields. Observation of regular conduct alone
does not establish an obligation.

For the caption-review episode, reconstruct at least two linked action
situations: the author's proposal of a caption and the reviewer's judgment
about its admission. Publication consumes the review result at a further
boundary. Authority to propose is not authority to approve. A delivery receipt
supports neither authority unless the corresponding binding establishes it.

### External conditions and evaluation

The local instance needs three explicit context records:

- Rules-in-use: applicable standing instructions, adoption/amendment authority,
  and evidence of enforcement or accepted practice, with unknowns retained.
- Community attributes: working conventions, shared vocabulary, trust and
  competence assumptions relevant to interpreting feedback. Record assumptions
  as assumptions; an agent roster cannot establish these attributes.
- Material conditions: available transports, durable inboxes, authentication,
  access restrictions, time and token budgets, and outages. An obligation can
  persist when its delivery mechanism is unavailable.

Evaluate separately: delivery coverage, delay and resource cost, distribution
of missed feedback across participants, opportunity to dispute, and later use
of the correction. A fast 2/2 delivery is not evidence of useful feedback or
fair participation. Do not invent numerical preference weights from this list.
Repeated episodes can reveal an inaccessible channel or exclusionary rule;
that evidence can support a proposed amendment. Only authorized adoption
changes the next instance's rules. Learning about a rule is distinct from
changing it.

### Deontic status is not technical capability

For each bound actor/action/context, retain independently evidenced answers
to `can`, `may`, `must`, and `must-not`. These are not four mutually exclusive
labels. An action can be possible and prohibited, or required and currently
impossible. Unknown evidence remains unknown. Conflicting obligations and
prohibitions are reported for resolution, not silently assigned a priority.

In the current setting, an available agent-wake endpoint could make a Claude
invocation technically possible while Joe's instruction prohibits invoking it.
A feedback obligation cannot override that instruction. Record the blocked
route and seek an authorized alternative; do not drop the recipient or spend
the quota. Conversely, a recipient's disconnected state establishes neither a
prohibition nor the absence of an obligation.

Express each proposed regulatory rule with actor/position, deontic status,
action, conditions, and an explicit response to noncompliance where one is
adopted. Distinguish rejection of an invalid certificate from a sanction on its
author: the former need not imply the latter. A response rule itself needs an
actor, authority, and feasible action. Merely writing 'escalate' leaves those
bindings unresolved.

Additional acceptance cases: technically possible but prohibited action is
refused; required but unavailable action remains outstanding; successful
self-authored work cannot serve as independent review; a repeated behavior
without authority evidence is not promoted to a rule; a proposed amendment
cannot change current obligations before adoption. These are specifications,
not newly executed tests.

## Optative preference selects the institutional proposal — 2026-09-10

Joe's verbatim direction:

> the key, I think, will be to relate this back to preferences... because we need to select suitable institutions to match "optative" moods -- not to create busy work.

Status: proposed computational interpretation of this direction. No preference
masses, selection equation, or institution is adopted by this addendum.

### Selection contract before activation

An optative expresses a desired situation, not yet an obligation. Record who
expresses it, whose outcomes matter, its task scope and horizon, and how its
satisfaction could be observed. Example: a correction is available and used
before an affected participant repeats the corrected mistake. Delivery is an
intermediate observation; receipt counts alone cannot establish that outcome.

The proposed interpretation endpoint should return candidate arrangements
before activating any one of them. Each candidate must declare:

- the desired outcome and its observation contract;
- the local participants and applicability evidence;
- the change to permissions, obligations, information access and responses;
- the hypothesized route from that change to the desired outcome;
- the evidence supporting that hypothesis and what remains unknown;
- expected effort, delay, resource use and distribution of burdens;
- existing obligations it preserves, and authority required to adopt it;
- a comparison baseline and a review/expiry condition.

The baseline is the current authorized arrangement, not suspension of its
obligations. Choosing no additional institution is valid. A retrieval match
only nominates a candidate; it does not supply evidence of suitability or
license activation. Lack of evidence of benefit is not evidence of zero
benefit, and administrative cost is not a license to bypass standing rules.

### Concrete comparison for correction feedback

All alternatives below are proposals, not live assignments. Assume an existing
review process already stores corrections and retains its independent-review
requirement. Compare additions against that baseline on the same task and
participant population.

| Candidate | Additional institutional commitment | Expected mechanism to examine | Cost or failure to observe |
|---|---|---|---|
| Retain current arrangement | None | Existing retrieval may already bring the correction into the next task | Missed correction at next use |
| Searchable correction with applicability caption | Assign responsibility for maintaining and reviewing the correction's description | Relevant retrieval exposes the correction when needed | Caption maintenance; wrong or missed retrieval |
| Targeted notification | Bind affected recipients and assign delivery responsibility | Correction arrives before their next relevant decision | Interruptions, access gaps, mistargeting |
| Acknowledgment before a specified dependent action | Add a local prerequisite and a route for resolving unavailable recipients | The dependent action cannot proceed through the controlled endpoint without recorded receipt | Waiting, bottlenecks, perfunctory acknowledgment |

These may be combined if evidence supports the combination; they are not
necessarily a ladder of increasingly good governance. In particular, more
acknowledgments can increase recorded activity without improving subsequent
work. The last candidate does not establish understanding merely by blocking
an action until acknowledgment.

### Where preferences enter the War Machine

Proposed separation of objects:

1. A task-scoped preference concerns outcomes for the named participants.
2. An institutional candidate changes the available actions, information or
   transition conditions under which policies operate.
3. A predictive model estimates consequences of policies under that candidate.
4. Selection compares those consequences and the institution's burdens under
   the adopted evaluation rule, subject to existing authority and constraints.

If represented through AIF, the optative needs a declared outcome domain and
an explicit interpretation into the preference carrier. The institution is
then a candidate intervention in the modeled action situation, not itself a
scalar preference weight. Learning about delivery, use and cost can improve
the predictions used in later selections. It cannot automatically amend the
institution. This design does not supply the outstanding observation bridge,
assert that the runtime already evaluates institutional candidates, or invent
probabilities to make such an evaluation executable.

Until comparable predictions and a warranted preference interpretation exist,
the endpoint returns a comparison with unknowns and reasons, not a fabricated
numeric ranking. It may recommend a bounded evidence-gathering experiment
under existing authority. Selection and authorized adoption remain separately
recorded even if a standing delegation permits both without operator input.

### Consumer and non-busywork acceptance conditions

The selection record names the chosen arrangement, baseline, preference basis,
expected improvement, predicted burden, uncertainty, and adoption authority.
Its next consumer is the task's policy-generation/admission boundary: that
boundary must demonstrably use the selected instance's information and rules.
The later evaluation records downstream correction use and observed burdens,
not only procedural compliance. Increased activity alone cannot satisfy the
improvement claim.

Required future controls (specified, not run):

- Additional receipts with unchanged downstream correction use must not by
  themselves demonstrate preference satisfaction.
- If a candidate offers no additional benefit on the adopted criteria and
  adds burden, the evaluator must not prefer it merely for having more rules.
- A favorable benefit estimate cannot admit a prohibited agent invocation.
- Disconnecting the selected institution from policy generation must fail the
  claim that institutional selection affected execution.
- Unobserved downstream use remains unknown even with complete delivery.
- Burdens and benefits affecting different participants remain visible; a
  population average must not silently settle whose interests take priority.

The next bounded experiment should compare the current arrangement with one
applicable addition on frozen task inputs. Record both its execution cost and
whether a subsequent task actually consumes the correction. A single episode
can demonstrate that path; it cannot establish general superiority. Retain or
revise the arrangement through its declared review process according to what
that evidence supports.
