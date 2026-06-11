# T-typed-bell-arse-write-async — move the typed-bell ArSE write off the bell request thread (WATCH, flag-ON only)

Surfaced by the Claude-owner review of M-typed-bells (`b4ed15f`), 2026-06-11.
**Status: WATCH — not a defect, no action while `FUTON3C_TYPED_BELLS` is OFF.**
Pick up only if/when the flag is live AND typed-bell volume grows.

## What to watch

When `FUTON3C_TYPED_BELLS` is ON, a `type=query`/`type=answer` bell triggers a
**synchronous** ArSE write on the `POST /api/alpha/bell` request thread:
`maybe-typed-bell-arse-bridge!` → `arse-ask!` / `arse-answer!` in
`src/futon3c/transport/http.clj`, each of which does
`arse-load-entities` (read `~/code/storage/arse/entities.json`) +
`arse-save-entities` (write it back) + an evidence POST — all inline, before
`create-invoke-job!` returns.

## Why it's only a watch-item, not a bug

- **Flag-gated OFF by default** — zero effect on the legacy bell path until activated.
- **Q&A-only** — `:assert`/`:request`/etc. hit the inert `case` default (no write).
- **No new risk class** — the ArSE HTTP handlers (`/arse/ask`, `/arse/answer`)
  were *already* synchronous file read+write; the bridge just calls the same
  refactored `arse-ask!`/`arse-answer!` in-process. We didn't add blocking I/O to
  a path that lacked it; we extended an existing synchronous shape onto a busier path.

## Why it could bite later

`entities.json` is a single growing JSON file read-and-rewritten on every Q&A
bell. As ArSE fills (the whole point of typed bells) the read+rewrite cost grows
O(file), and it lands on a **hot coordination path** — the same backpressure
hazard recorded in `blackboard-backpressure` / `feedback_no_server_restart`: a
slow synchronous write on a request thread can cascade when traffic is high.

## Fix direction (when triggered)

Move the ArSE write **off** the bell request thread:
- Enqueue the `:ask`/`:answer` onto an async worker (the durable turn-queue is the
  obvious feeder), return the bell immediately, and stamp the job `:ref` once the
  worker resolves the thread-id; **or**
- Make `arse-save-entities` append-only / indexed so the cost stops scaling with
  file size.
Preserve **TB-5** (job `:ref` == ArSE `in-reply-to`) and **TB-7** (one thread per
query, replay-safe) — async must not reorder these. Re-run
`typed_bells_invariants_test` + the transport typed-bell tests after any change.

## References

- Mission + review checkpoint: `holes/missions/M-typed-bells.md`
- Code: `src/futon3c/transport/http.clj` (`maybe-typed-bell-arse-bridge!`,
  `arse-ask!`, `arse-answer!`)
- Invariants: `src/futon3c/logic/typed_bells_invariants.clj` (TB-5, TB-7)
- Prior-art caution: blackboard-backpressure; `feedback_no_synchronous_heavy_drawbridge_calls`
