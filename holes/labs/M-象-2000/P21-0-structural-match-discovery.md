# P21-0 — structural matching and attestation at use

Read-only census and source review, 2026-09-28. Joe's decision is that an
attestation occurs at use, not from graph links; proposer self-citation and echoes
from a shown id list do not count, and the counting rule will be reopened later
(`holes/missions/M-象-2000.md:1091-1104`).

## 1. Proposals now

象 asks an analysis agent to write `RECORD.candidates.json` beside the turn
analysis. The requested candidate has id, title, fragment, context, IF, HOWEVER,
THEN, parent, BECAUSE and `tried`; it remains a proposal and does not enter the
library (`emacs/session-turn-analysis.el:858-880`). It must search proposals before
minting another name (`:884-889`). The file-level `for` and `by` identify the turn
and proposer; individual candidates do not repeat `by`.

A read-only JSON census of all
`~/.emacs-graph/session-turn-analysis/*.candidates.json` found **381 files, 628
candidate occurrences, 602 distinct non-null ids** at this snapshot. Nonempty
field counts were:

| field | occurrences |
|---|---:|
| id, fragment, context, IF, THEN, tried | 628 each |
| title | 576 |
| HOWEVER | 588 |
| BECAUSE | 595 |
| parent | 602 |
| scope | 0 |

Thus these are normally structured clauses, not merely name plus prose, but 40
lack a real HOWEVER, 33 lack BECAUSE, and no proposal has the P21 scope field.
The draft flexiargs are a much smaller promoted subset: 10 files under
`storage/operator-turns/candidates/`. A draft carries the four clauses in the
normal flexiarg form (for example
`storage/operator-turns/candidates/operator/name-the-acceptance-test.flexiarg:14-20`).
The admitted library has the same clause form under `futon3/library/`.

`xlate.py census` reads analysis fragments and the adjacent candidate files,
counting ids rather than comparing clauses (`scripts/xlate.py:221-280`). `find
--with-candidates` indexes the library plus only the 10 draft flexiargs
(`:22-33,57-89,111-119`), so most JSON proposals are counted but not searchable
through that command.

## 2. Matching
No current code structurally merges proposals. `find` is BM25 over title,
keywords, conclusion and context, with title/keywords doubled (`xlate.py:31-33,
57-71,93-108`); it is candidate discovery, not a merge decision. The cascade
mission names automatic comparison/merge/split as future work
(`holes/missions/M-象-cascade.md:132-160`). Its consent-gate survey already shows
seven nearby names without deciding structural identity (`:78-105`).

The smallest decidable matcher should take a closed proposal shape with
`:if :however :then :because :scope`. It Unicode-normalises (NFKC), case-folds
Latin text, collapses whitespace and normalises terminal punctuation, but does
not remove words. It returns a per-field equality/difference vector. **Merge**
requires equality of all five normalised fields, irrespective of name.
Anything differing is **adjacent**, including the same name. Missing clauses or
scope make the pair `:uncomparable`, never a merge. A diagnostic may report
explicit negators (`not`, `never`, `不`, `非`, `无`) in a differing HOWEVER, but
exact field comparison already keeps opposite clauses apart.

This cannot decide paraphrase, entailment, whether two scopes denote the same
world population, or whether negation has semantic scope. An embedding can
retrieve candidates for comparison, but must not decide merge.

## 3. Attestation at use

Recorded or partly recorded use surfaces are:

- A `:pattern-card/selection` stores author, agent, session, pattern id and valid
  time, with stamp/harness when supplied; its endpoints make seat and pattern
  queryable (`agency/pattern_card_record.clj:20-47,79-101`). It identifies who
  selected what and when. It does **not** store the basis turn or ids shown to
  that user, so it cannot currently establish an independent attestation.
- WM has a concrete PSR/PUR flight shape: PSR records chosen pattern,
  candidates, rationale and confidence; PUR records pattern, actions, outcome
  and prediction error (`futon2/holes/flight-log.spec.edn:60-64,92-95`). The
  runner also joins a selected pattern to interpretation and construction
  receipts (`futon2/src/futon2/aif/full_loop_runner.clj:1735-1757`). These can
  prove machine use, but the shown-id set, human attester and basis turn are not
  fields of that PSR/PUR shape. Historical prose PSR/PUR sections are not one
  canonical queryable writer.
- P7b `:artifact/weak-activation` records ranked hits, cited ids and weak ids
  beside a job (`agency/artifact_activation.clj:97-139`). Retrieval is exposure,
  not use, so neither a hit nor weak activation attests the pattern.
- Every ordinary agent turn can carry `Prompt: pattern …`; the header is built
  from exact-seat prompt-line segments (`transport/http.clj:4590-4615,
  4617-4645`). Context retrieval itself is stored with its result list
  (`dev/futon3c/dev.clj:937-960`). A citation after that exposure is an echo
  unless a later use record establishes an independent basis.
- Incoming flexiarg `@how`/`@why` links and the census's descendant count are
  connectivity only (`xlate.py:283-290`), exactly the SEO case Joe excluded.

Proposed immutable `:pattern/attestation` act, written at the use boundary:

```clojure
{:id "act:…" :kind :pattern/attestation :pattern-id "family/name"
 :attester "agent-id" :at "…" :use {:kind :pattern-card|:wm|:psr-pur :ref "…"}
 :basis-turn "evidence-or-job-id" :proposal-author "xiang-or-agent"
 :presentation {:ref "evidence-or-job-id" :shown-pattern-ids ["…"]}
 :disposition :counts|:proposer-warrant|:shown-list-echo}
```

Validation resolves the use and basis records; it does not trust caller-supplied
identity or shown ids. Counting excludes attester = proposal author, 象 citing
its own proposal (`:proposer-warrant`), a pattern in that presentation's shown
list (`:shown-list-echo`), retrieval-only records, and incoming links. Store one
record per use even when excluded, so “why did this not raise the level?” is
answerable. The present prompt header means the shown-list join is required,
not an optional quality flag.

## 4. Acceptance pairs
There is real evidence for the same-id/different-structure hazard:
`operator/pick-an-offered-option` has an empty HOWEVER for “Option A please”
(`~/.emacs-graph/session-turn-analysis/turn-2bpCVY.json.candidates.json:17-25`),
but another occurrence says “He follows the agent's recommendation, so the
choice is the agent's as much as his” and changes THEN/BECAUSE
(`turn-9OKHKi.json.candidates.json:6-14`). These are adjacent under the proposed
matcher despite sharing an id. They are not literally opposite, so I will not
claim they satisfy the sharp bad case.

The corpus has no pair of different ids with all four nonempty clauses exactly
equal, and no observed same-name pair with literally opposite HOWEVER clauses.
The first matcher test should therefore construct:

- same name `operator/proceed`, same IF/THEN/BECAUSE/scope, HOWEVER A “approval
  is required” versus B “approval is never required” => `:adjacent`, with
  `:however` as the differing item;
- names `operator/proceed` and `orchestration/go-ahead`, identical five
  structural fields => `:merge`.

An attestation projection test supplies two uses of the merge candidate, one
whose presentation lists that id and one independently grounded; only the
second raises the count.

## 5. First packet and order
First build one pure `proposal-match` over the closed five-field shape, returning
`:merge`, `:adjacent` or `:uncomparable` plus per-field results. Pin the two
constructed pairs above and the real `pick-an-offered-option` adjacency fixture.
No files move and no proposal is merged in that packet.

Then: (1) define and map the immutable attestation act; (2) add one writer at a
real use boundary, starting with pattern-card selection because its act already
has author/seat/time; (3) join the exact presentation record and implement the
three exclusions; (4) add WM/PSR-PUR adapters only where who/turn/shown-list can
be proved; (5) later let M-象-cascade order gateway patterns from counted
attestations. The later counting revision stays explicit rather than being
hidden in the matcher.
