# Bells And Whistles

`bell`, `whistle`, and `whistle-stream` use the same invoke/job engine.
The difference is response contract and caller interaction model.

## Surfaces

| Surface | Endpoint | Contract | Best for |
|---|---|---|---|
| `bell` | `POST /api/alpha/bell` | Immediate accept (`202`) + job polling | Fire-and-forget async handoff |
| `whistle` | `POST /api/alpha/whistle` | Single terminal JSON response | Simple request/response callers |
| `whistle-stream` | `POST /api/alpha/whistle-stream` | NDJSON stream with progress + terminal event | Long-running work with live visibility |

`POST /api/alpha/whistle` also accepts `"stream": true`, which routes to the same streaming behavior as `/api/alpha/whistle-stream`.

## `bell` (async handoff)

Request:

```json
{"agent-id":"codex-1","prompt":"do X and report back"}
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
