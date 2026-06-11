# Bells And Whistles

> **TL;DR — when bells cross, get on a call.** A **bell** is async (fire-and-forget; the
> reply comes back as a *separate, later* turn, so it can cross/interleave with other
> bells — an agent can't always tell a new request from a reply to its own request). A
> **whistle** is synchronous (you block and get the answer in the *same* round-trip —
> crossing-immune, because the request and its response are atomically paired). **So if a
> bell exchange is getting crossed or confused, switch to a whistle to reconcile** — one
> of you whistles, the other answers. Don't *both* whistle at once (synchronous mutual
> calls can deadlock). Why: `holes/excursions/E-crossed-bells.md`.

`bell`, `whistle`, and `whistle-stream` use the same invoke/job engine.
The difference is response contract and caller interaction model.

## Surfaces

| Surface | Endpoint | Contract | Best for |
|---|---|---|---|
| `bell` | `POST /api/alpha/bell` | Immediate accept (`202`) + job polling | Fire-and-forget async handoff |
| `whistle` | `POST /api/alpha/whistle` | Single terminal JSON response | Simple request/response callers |
| `whistle-stream` | `POST /api/alpha/whistle-stream` | NDJSON stream with progress + terminal event | Long-running work with live visibility |

`POST /api/alpha/whistle` also accepts `"stream": true`, which routes to the same streaming behavior as `/api/alpha/whistle-stream`.

## Sending from a shell — use the robust client (don't hand-quote JSON)

Inline `curl -d '{"prompt":"...don't..."}'` **breaks** when the prompt contains an
apostrophe, parens, or unicode — the shell mangles the single-quoted payload. Use
`scripts/agency_send.py`, which reads the prompt from **stdin** (a quoted heredoc =
zero shell interpolation, safe for any characters) and JSON-encodes it in-process:

```bash
python3 futon3c/scripts/agency_send.py --to codex-3 --kind whistle <<'EOF'
Anything goes — apostrophes (don't), parens (f), unicode μ/κ/β/Σ/≥, newlines.
EOF
```

`--kind bell` (async, 202+job-id) or `--kind whistle` (blocking, terminal JSON);
`--dry-run` prints the payload without sending. This is the recommended client for
agents composing bells/whistles in a shell.

Typed bells are available when `FUTON3C_TYPED_BELLS` is enabled. Use `--type`
to declare the bell's speech act; use `--ref` for the ArSE thread or referent.
Allowed types are `query`, `answer`, `assert`, `challenge`, `agree`, `define`,
`retract`, `suggest`, and `request`. Untyped bells behave as `request`; unknown
types are rejected.

```bash
python3 futon3c/scripts/agency_send.py --to claude-6 --from codex-1 \
  --kind bell --type query <<'EOF'
Is the typed-bell ArSE bridge active in this JVM?
EOF

python3 futon3c/scripts/agency_send.py --to codex-1 --from claude-6 \
  --kind bell --type answer --ref ask-1772986500-3 <<'EOF'
Yes. The query bell created the ArSE thread and stamped its id into the job.
EOF
```

## `bell` (async handoff)

Request:

```json
{"agent-id":"codex-1","prompt":"do X and report back"}
```

Typed request:

```json
{"agent-id":"codex-1","caller":"claude-6","prompt":"Is X true?","type":"query"}
```

Typed answer:

```json
{"agent-id":"claude-6","caller":"codex-1","prompt":"Yes, because ...","type":"answer","ref":"ask-1772986500-3"}
```

Immediate response:

```json
{"ok":true,"accepted":true,"job-id":"invoke-...","state":"queued","status-url":"/api/alpha/invoke/jobs/invoke-..."}
```

Then query `GET /api/alpha/invoke/jobs/:id` for terminal state and delivery metadata.

## `whistle` (one-shot)

Request:

```json
{"agent-id":"codex-1","prompt":"status update"}
```

Response (success):

```json
{"ok":true,"response":"...","agent-id":"codex-1","session-id":"..."}
```

Response (error):

```json
{"ok":false,"error":"...","agent-id":"codex-1"}
```

## `whistle-stream` (modem-style long poll)

Request:

```json
{"agent-id":"codex-1","prompt":"run a multi-step task","poll-ms":250,"heartbeat-ms":1000}
```

Stream events (NDJSON, one JSON object per line):

1. `started`
2. `job-event` updates (`accepted`, `running`, terminal event)
3. periodic `heartbeat` while non-terminal
4. terminal `done` with final job snapshot

Example stream:

```json
{"type":"started","job-id":"invoke-...","state":"queued"}
{"type":"job-event","job-id":"invoke-...","event":{"type":"accepted"}}
{"type":"job-event","job-id":"invoke-...","event":{"type":"running"}}
{"type":"heartbeat","job-id":"invoke-...","elapsed-ms":1012}
{"type":"done","ok":true,"job-id":"invoke-...","job":{"state":"done","delivery":{"status":"pending"}}}
```

## Delivery + Reliability Notes

- All three surfaces create/query the same invoke job model.
- `bell` is non-blocking and does not stream by itself; observe progress via job status polling.
- `whistle-stream` gives partial-progress visibility during execution (`job-event` + `heartbeat`), so callers can see work is still active.
- Delivery status is recorded in the job ledger (`delivery-recorded` event) when terminal output is emitted to the caller surface.
