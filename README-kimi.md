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

## Vision

A kimi seat gets a `view_image` tool: it reads a local image file and hands it
to the model as an `image_url` content part. The point is the feedback loop —
screenshot a page with Playwright, then LOOK at it, rather than inferring what
rendered from the DOM.

```
run_shell   node pw-shot.mjs        # writes /tmp/shot.png
view_image  /tmp/shot.png           # the model sees the pixels
```

- **The image rides in the tool result.** Kimi accepts `image_url` parts inside
  a `tool`-role message (verified live 2026-09-23), which is not something the
  OpenAI dialect guarantees; it means a screenshot arrives as the result of the
  tool that took it, with no synthetic user turn spliced into the loop.
- **Only vision providers are offered it.** `:vision?` gates the tool family, so
  a Z.AI seat's tool list is byte-identical to what it was.
- **PNG, JPEG, GIF, WebP, BMP, HEIC/HEIF. Not SVG** — the vendor rejects SVG as
  image input, so the tool says so and points at `read_file` instead.
- **Oversized images are downscaled, not refused** (4096x2160; past that a
  larger image costs processing time and buys no understanding). Formats the
  JVM decoder cannot open pass through byte-for-byte, unmeasured.
- **Only the two most recent images stay inline.** Earlier ones are replaced by
  their caption (`viewed /tmp/shot.png (1280x720) 9KB — image elided…`). A
  screenshot loop that kept them all would resend every megabyte of every
  screenshot on every later round; the caption keeps the record that the look
  happened. Tune with `:retained-images`, and the per-image cap with
  `:max-image-bytes` (default 8MB).

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

## Documentation

Mirrored locally at `~/code/refs-external/kimi-api-docs` (`./refresh.sh` to
re-fetch): the vendor's own machine-readable docs — `llms.txt` index,
`llms-full.txt` for grepping, `openapi.json`, and every page as markdown.

Read the base URL before trusting a page: those docs describe the
pay-as-you-go platform (`api.moonshot.ai/v1`), not the coding plan we run on.
The request shapes are shared; the constraints are not identical, and the
coding endpoint is the more permissive of the two. The mirror's README records
where they diverge and what that means for this provider.

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
- `GET /coding/v1/me` reports the account and plan level (`user_level_name`,
  e.g. `Ultra`).
- `GET /coding/v1/usages` reports the quota, and voxterm's `/usage` panel reads
  it. Two views arrive in one response and they can disagree: `limits[]` is the
  windowed form (`limit`/`remaining`/`resetTime`) and `usages.*` is a
  convenience ratio that has been seen reporting 0 for an exhausted window
  (MoonshotAI/kimi-code#3951), so `limits[]` wins for any window it covers.
- **Kimi's long window is a MONTH, not a week**: this plan reports a 5-hour
  pool and `limit_month_total` / `limit_month_code` (a 7-day window exists on
  some plans and is preferred when present). The usage strip therefore leads
  with each provider's `lead_*` — longest window it actually bills on, plus its
  name — rather than calling a month a week.
