# The invoke cap — raised from 30 to 60 minutes as a stopgap

*(Written up 2026-07-27 after a live loss: job `invoke-1785165682706-208-826bd944`.)*

## The failure

A bell was dispatched to `codex-7` carrying a long compute packet (local
causal-state reconstruction over a spacetime field). It ran for **31m52s**
(16:21:23 → 16:53:15 BST) and then:

```
state: failed
mode:  brief
result: (empty)
```

No error, no partial result, no summary on the completion bell. The parked
caller woke with `(no summary)` and had to reconstruct what happened from
file mtimes in the working tree.

**About half the work had actually been done** — the expensive reconstruction
had written its outputs to disk before the kill — but nothing was committed and
nothing was reported. Had the caller trusted the bell payload, that work would
have looked like a total loss.

## The cause

At the time of the incident, the cap was neither removed nor raised server-side:

| where | default |
|---|---|
| `dev/futon3c/dev/agents.clj` — `CODEX_INVOKE_TIMEOUT_MS` | `3600000` (60 min) |
| `dev/futon3c/dev/agents.clj` — `FUTON3C_RELAY_INVOKE_HARD_TIMEOUT_MS` | `3600000` (60 min) |

The defaults were raised to 60 minutes on 2026-08-02 as an explicit stopgap.
Running JVMs retain their old values until restarted; this document still records
the original failure because the architectural repair remains open.

`scripts/agency_send.py` accepts `--timeout-ms`, but it is **opt-in** — the
payload only carries it when the flag is passed:

```python
if a.timeout_ms:
    body["timeout-ms"] = a.timeout_ms
```

Omit the flag and you inherit the server's 30 minutes. The flag's own help text
already documents this exact outcome:

> *"Until the supervised-overrun fix lands, a turn hitting this is abandoned as
> state=failed and its result is lost — set generously for long packets."*

## Why "we fixed the 30-minute cap" is misleading

Supervised-overrun machinery exists and **does** work — `holes/labs/M-zai-learning-loop/bpm-batch-0-results.md:35`
records a wall-clock extension engaging at 35m, with tool-round budget rather
than wall clock as the binding constraint.

But that is the **zai learning-loop path**. A codex relay invoke still dies hard
at the 30-minute mark, as this job did. Do not assume the overrun machinery
covers your route.

## The rule

**Any bell whose packet might run long MUST pass `--timeout-ms` explicitly.**
The default is the setting that loses your results.

```bash
python3 futon3c/scripts/agency_send.py --from <your-id> --to <codex-N> --kind bell \
  --timeout-ms 5400000 \
  --park --park-deadline 6000 --park-payload "<review checklist>"
```

Two things in that line beyond the timeout:

- `--park`, `--park-deadline`, `--park-payload`, `--surface` (lines 37–43) bundle
  dispatch-and-park into one command. This is the path `CLAUDE.md` says to prefer;
  it has landed. Hand-rolling the park with a separate `curl` works but is two
  round-trips and two chances to get the absolute-epoch-ms arithmetic wrong.
- Set the **park deadline longer than the invoke timeout**, so the deadline wake
  is a genuine backstop rather than firing while the job is still legitimately
  running.

## Corollary for the caller

Because a capped job reports `state=failed` with an empty result, **never treat
a failed job as "no work happened."** Check the working tree before deciding to
re-dispatch:

```bash
git -C <repo> status --porcelain
find <repo> -newermt '<dispatch time>' -not -path '*/.git/*' -type f
```

Mind the clock skew when you do: job records are **UTC** (`...Z`), while `ls`
and `find` report **local time**. During BST that is a one-hour offset, which is
easily large enough to make genuine in-window work look like it predates the
dispatch.

## Closed, partly (2026-07-29)

The second of the two options below is now done: `agency_send.py` defaults
`--kind bell` to **4 hours** (`BELL_DEFAULT_TIMEOUT_MS`), far above the server's
30-minute cap. Whistles are unchanged, since a caller is synchronously blocked on
them. `--timeout-ms 0` defers to the server default if you want the old
behaviour.

This was done after the cap took three more handoffs on 2026-07-29 — codex-6 and
codex-5 twice — each time leaving completed work uncommitted in the working tree
with `state=failed` and an empty result. In every case the dispatcher had read
the `--timeout-ms` help text, which describes this exact failure, and not passed
the flag. That is the lesson worth keeping: **an opt-in fix that must be
remembered on every call, under time pressure, is not a fix.** The default is
the fix.

## Open gap

The real repair is still outstanding: extending **supervised overrun** to the
codex relay route, so a turn that exceeds its budget is *harvested* rather than
discarded. The machinery exists and works on the pouch/zai route
(`M-zai-learning-loop`); the codex relay — the route the coding-handoff protocol
makes the default — has never been wired to it. Until that lands, a long enough
timeout only makes the cliff rarer; it does not remove it.

Note also that the cap is not one constant. The invocation-related defaults in
`codex_cli.clj`, `agent_pouch.clj`, `transport/http.clj`,
`transport/ws/invoke.clj`, and `social/whistles.clj` were all raised together,
but still are not derived from a shared value — which is why
"we fixed the 30-minute cap" has been true and unhelpful more than once.
