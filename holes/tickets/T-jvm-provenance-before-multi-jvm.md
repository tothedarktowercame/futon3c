# T-jvm-provenance-before-multi-jvm — a JVM cannot currently say what code it is

**Opened:** 2026-08-28 · claude-13, from Joe: *"one JVM … that was primarily set
up to accommodate my laptop. Now we have a much bigger and better bare metal
server. So I think we could afford to have multiple JVMs — we have /home/joe/ as
well as /home/dionysus/ and /home/apollo/ in case we wanted to use these for a
degree of isolation."*

**Status:** open. The resource premise of the one-JVM rule is void; the
identifiability premise is not, and this ticket is the precondition.

## The one-JVM rule was doing two jobs, and only one has expired

**Job 1 — fit on a laptop. Void.** Measured 2026-08-28:

    32 cores · 249 GiB RAM · 228 GiB available · 21 GiB in use
    /home/dionysus  exists, owned by dionysus, empty
    /home/apollo    exists, owned by apollo,   empty

This machine could carry dozens of JVMs. Two separate accounts are already
provisioned for isolation and unused.

**Job 2 — make "what code is running?" have an answer.** With exactly one JVM
per repo on `master`, the answer is implied by the rule itself. **With several,
there is no answer at all** — and the JVM cannot supply one:

    GET /health              200 — uptime, queue gates, sessions, agents, started-at
    GET /api/alpha/version   404
    GET /api/alpha/health    404
    GET /api/alpha/status    404

**No checkout path, no branch, no commit sha, anywhere.** A running futon3c
cannot tell you which working tree it was loaded from.

## Why that is the whole reason the original incident hurt

`CLAUDE.md` records it: on 2026-08-22/23 a branch 56 commits behind master was
`load-file`d into the shared JVM, and every master-only route answered
`Unknown endpoint`. Joe hit it twice in one morning *"with no way to tell it from
a bug."*

The damage was not the wrong code. **The damage was that the wrong code was
indistinguishable from a bug**, because the process could not be asked what it
was. One-JVM was a crude fix for that: keep the population at one and the
question never arises.

Multiplying JVMs without provenance reinstates the same failure by a new route —
`Unknown endpoint` stops meaning "stale branch" and starts meaning "you asked the
wrong port", which is no easier to diagnose.

## The repair

`GET /api/alpha/provenance` on every JVM, reporting at minimum:

| field | why |
|---|---|
| `checkout` | the absolute path of the working tree it was started from |
| `branch`, `head-sha`, `dirty?` | what code that tree held at start |
| `reloaded-from` | **every** path live-loaded since start, in order — this is the field the 08-22 incident needed and no `git` answer provides, because `load-file` leaves no trace in the tree |
| `port`, `repo` | so a caller can tell two JVMs of the same repo apart |
| `started-at` | already in `/health`; keep it here too |

`reloaded-from` is the one that matters. `checkout` and `head-sha` describe how
the process *started*; a hot-load changes what it *is*, and nothing records that
today.

## The policy this proposes to replace

Not *"one JVM per repo"* but:

> **Every JVM states its provenance, and a caller can ask.** Live-loading code
> from a checkout the JVM does not declare is the violation — not the existence
> of a second JVM.

That is strictly stronger than the rule it replaces: it forbids the thing that
actually went wrong on 08-22 *and* it stays true when there are six JVMs. The
original rule forbade the incident only as a side effect of forbidding the
population.

Isolation by account (`dionysus`, `apollo`) is then a free extra: a JVM running
as another user cannot write into `/home/joe/code` at all, which turns a class of
"writes that go in the wrong place" into a permission error rather than a silent
success.

## The vector, and the heredity relation it induces

*Joe, 2026-08-28: "JVM should have a pinned hash (or vector of these), these
could be part of an overall heredity tree (again, we have multiple git repos) …
and so we can test, model, run — all without getting everything confused."*

Right, and the vector is **nine wide** — `futon3c`'s `deps.edn` carries ten
`:local/root` entries across nine repositories. Measured 2026-08-28:

| repo | branch | sha | tree |
|---|---|---|---|
| futon3c | master | f24ce773 | **DIRTY (17)** |
| futon3b | main | 9795feb | clean |
| futon1b | master | d5a071e | clean |
| futon0 | main | cd010d0 | **DIRTY (1)** |
| futon3 | main | ba6fe3b | **DIRTY (4)** |
| futon2 | main | 6ca85d4 | clean |
| futon3a | main | 58ea67a | clean |
| futon5 | **M-propagators-2026-07-15** | 6f0ce55 | clean |
| futon4 | main | 4139c74 | clean |

**The state of the serving JVM is not nameable today.** Three of nine components
have uncommitted changes, so no sha describes them; and `futon5` is on a feature
branch, which means *"one JVM per repo, running master"* is already not literally
true of the classpath — it is true of `futon3c` and unexamined for the other
eight. That is not an argument against multiple JVMs; it is the state the current
single JVM is in.

### Heredity is a product order, and it has meets by construction

A JVM state is a vector **v ∈ Π(repo → commit)**. Each component is ordered by
git ancestry, which is a partial order; a product of partial orders is a partial
order, ordered componentwise. Two consequences that are useful rather than
decorative:

**Meets exist, componentwise, and `git merge-base` computes them.** The greatest
common ancestor of two JVM states is the vector of per-repo merge-bases. So the
heredity tree over JVM states **is a meet-semilattice by construction** — which
makes it the first structure in this stack where that property is guaranteed
rather than measured and mostly absent. The pattern cascades checked today have a
meet in 2 of 25 cases (`T-strategic-cascade-emits-disconnected-patterns`); this
one has one always, because the product of orders inherits meets from its factors.

**Comparability answers "does this result transfer?".** Two states are comparable
iff *every* component is. So a test result obtained at vector `t` applies to a run
at vector `r` exactly when `t` and `r` are comparable — and when they are not, the
incomparable components name precisely which repos make the test inapplicable.
That is the mechanism for *test, model, run without getting everything confused*:
not discipline, a computable relation.

### The Lean spec is a component of the vector, not a thing outside it

This is the part that pays off elsewhere. If the spec's repo is one of the
components, then **"the spec trails the implementation" stops being an impression
and becomes a distance**: how many commits have landed in the code components
since the spec component's sha was current.

§3.1i measured trailing crudely, as a ratio of commit rates — 42 spec revisions
against 494 in the layer it specifies. With the vector it is exact, per repo, at
any moment, and it is the number that distinguishes Joe's *trailing* (bounded)
from *not keeping up* (growing).

## Why inbox zero is not holding — three categories, one of them unreachable

*Joe, 2026-08-28: "this is where the README-inbox-zero feature is useful —
clearly it isn't working properly yet. If we get to inbox zero, we can push to
prod, pin the heredity, and get set for experiments."*

`futon0/README-inbox-zero.md` states the goal — *no repo carries uncommitted work
older than a day* — and its third cost is exactly the pin problem: **"a dirty tree
is an unreliable narrator, and agents believe it."** A tree with uncommitted
changes has no sha, so it cannot be a component of the vector.

Census of the 22 dirty items, 2026-08-28. They are **three different things and
only two of them are an inbox**:

**1 · Live run-products in tracked paths — 3 items, and these make the goal
unreachable as stated.**

    holes/labs/M-diagramprover/apm-driver/axiom-audit.jsonl        modified 73 minutes ago
    holes/labs/M-diagramprover/apm-driver/campaign-ledger.jsonl    git diff: +1 −0, a pure append
    holes/labs/M-diagramprover/apm-driver/statements-manifest.jsonl

The running machine appends to these. Committing them clears the tree for as long
as it takes the next frame to write a line. **No amount of discipline reaches
inbox zero while a live process writes into tracked paths** — this is a category
error in what is tracked, not a failure to tidy.

**2 · Finished work sitting unstaged — 13 items.** Eleven pattern-library scribe
outputs for frames f37–f49 (codex and zai), a hole-review resolution-witness
`.edn`, and two `.flexiarg` library files in `futon3`. This is the README's cost
#1 verbatim, still happening: *finished work sits unstaged and is forgotten*, and
frame outputs are exactly the artefacts already known to strand
(`rescued-solves-refs`, 2026-08-27).

**3 · Source edits and one stray — 6 items.** `apm-coordinator-enabled.py`,
`substrate/client.clj`, `live-map-pca3-v1.json`, `README-termux.md`,
`patterns-index.tsv`, and a file in `futon3` named **`li`** — almost certainly a
mistyped `ls`, and precisely the noise the README means by an unreliable
narrator.

### What this asks of the pin

The vector wants to pin **source**. The working tree does not distinguish source
from product, so the pin cannot be taken while category 1 exists. The fix is the
one already applied to `futon2/data/` — which is gitignored, and which is why
`futon2` is clean in the table above while `futon3c` is not.

So: **the tracked/untracked boundary is a declared coverage boundary**, and it is
currently undeclared for run-products. That is `declaresCoverage` and
`outsideIsTyped` (`CoverageReport.lean`) applied to a working tree — the same
shape as the census cleanup and the contract's `reserved` list.

Order of work: move category 1 out of tracked paths *first*. Categories 2 and 3
are then a genuine inbox that a person can clear in an afternoon, and the pin
becomes takeable. Clearing 2 and 3 first would feel like progress and would be
undone by the next frame.

**Not mine to clear.** The 13 unstaged items in category 2 are other agents'
frame outputs; committing them is a judgement about their content that belongs to
whoever authored them. Flagged, not staged.

## Acceptance

- `GET /api/alpha/provenance` answers on futon3c, including `reloaded-from`.
- `scripts/proof-eval.sh`'s existing classpath guard reports the JVM's declared
  checkout in its refusal message rather than only refusing.
- **CLAUDE.md's "One JVM per repo" section is rewritten** to the rule above.
  That edit is Joe's to make or approve; this ticket does not touch it.

## Not in scope

Actually standing up a second JVM; migrating anything to `dionysus` or `apollo`;
port allocation policy. Provenance first — a second JVM before the endpoint
recreates the 08-22 diagnosis problem.

## Related

- `CLAUDE.md` — "One JVM per repo, running master" (the section this revises).
- `futon2/holes/missions/M-formal-war-machine.md` §3.1l (timelines by process,
  not namespace), §3.1i (L0: a process states what it is).
- `futon3c/scripts/restore-http-routes.sh`, `scripts/proof-eval.sh`.
