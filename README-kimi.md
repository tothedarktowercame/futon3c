# Kimi in the Agency

`:kimi` is an Agency agent type backed by the Kimi For Coding subscription
(`api.kimi.com`). It reuses the Z.AI harness wholesale: same agent loop, same
tool set, same evidence/memory wiring, same interrupt and bellback contracts.

- `src/futon3c/agents/zai_api.clj` — the harness (agent loop, tools, evidence).
- `src/futon3c/agents/kimi_api.clj` — the provider: endpoint, models, key,
  sampling. ~70 lines, because that is all that actually differs.
- `src/futon3c/transport/http.clj` — `:kimi` in the capability table, the
  session-file table, the local invoke-fn builder, the frame-seat timeout
  policy and `auto-bellback-recipient-types`.
- `emacs/kimi-repl.el` — the REPL surface (`M-x kimi-repl`,
  `M-x kimi-repl-attach-agent`).

## Use it

```bash
# spawn a seat
curl -s -X POST http://127.0.0.1:7070/api/alpha/agents/auto \
  -H 'Content-Type: application/json' \
  -d '{"type":"kimi","cwd":"/home/joe/code/futon3c"}'
# -> {"ok":true,"agent-id":"kimi-1",...}

# invoke it
curl -s -X POST http://127.0.0.1:7070/api/alpha/invoke \
  -H 'Content-Type: application/json' \
  -d '{"agent-id":"kimi-1","prompt":"...","surface":"http","caller":"you"}'
```

Bells (`scripts/agency_send.py --to kimi-1 --kind bell --mode work`) and
`/api/alpha/invoke-stream` work exactly as for a zai seat. In Emacs,
`M-x kimi-repl-attach-agent` attaches `*kimi-repl:kimi-1*` to the seat's
server-side session.

## Configuration

| what | default | override |
| --- | --- | --- |
| API key | `~/.kimikey`, then `~/.kimi-key` | `KIMI_API_KEY` |
| endpoint | `https://api.kimi.com/coding/v1` | `KIMI_BASE_URL` |
| model | `k3` | `KIMI_MODEL`, or `model` on register/restore |
| thinking | model default | `KIMI_THINKING_TYPE` |
| reasoning effort | `low` | `KIMI_REASONING_EFFORT` |

A keyless Kimi seat refuses the turn. It deliberately does NOT fall through to
the Z.AI key resolver: that would run the seat on the wrong subscription and
look like it worked.

## Provider facts, verified live 2026-09-23

- Models: `k3` (1M ctx), `k3-256k`, `kimi-for-coding` (K2.8 Preview, 1M ctx),
  `kimi-for-coding-highspeed` (K2.7). `GET /coding/v1/models` lists them.
- **An explicit `temperature` is refused**: `invalid temperature: only 0.6 is
  allowed for this model`, and the single allowed value moves with the
  reasoning effort. The provider therefore omits the field. This is why
  `zai-api`'s sampling block became pluggable — a `nil` in the `:sampling` map
  omits that field from the request body rather than sending a null.
- These models reason by default (effort `high` on k3, `max` on
  `kimi-for-coding`). A multi-round tool loop pays that on every round, so the
  default here is effort `low`: reasoning stays on, the per-round tax does not.
- `GET /coding/v1/me` reports the account and plan level. There is no usage or
  quota endpoint, so Kimi has no voxterm `/usage` panel — unlike Claude, Codex
  and Z.AI, there is nothing to read.
