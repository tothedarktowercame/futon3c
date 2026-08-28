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
