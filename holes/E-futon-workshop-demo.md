# E-futon-workshop-demo — live #futon workshop, week of 2026-07-06

**Date opened:** 2026-07-04
**Owner:** claude-14 (prep apparatus); **Joe runs the room.**
**Charter:** Joe's workshop workup (abstract + session plan + logistics checklist),
pasted 2026-07-04 — preserved in `holes/futon-workshop-plan.md`. This excursion
is the *assurance* work: make sure the pieces on the logistics checklist are
actually true, fix what isn't, and leave an auditable record of what was verified.

**Bounded scope (E-discipline):** IRC attachability, agent liveness on lucy,
evidence-store mirror query, mesh assurance across the two Linodes, cards.
NOT in scope: Matrix bridge build (see Decisions), new agent-engine integrations
beyond re-pointing a bridge nick, Agency feature work unrelated to the demo.

## Status board (verified live, 2026-07-04 morning)

| Checklist item | State | Evidence |
|---|---|---|
| ngircd on lucy, 6667 + 6697/TLS | **GREEN** | listening both ports; LE cert valid for `172-236-28-208.ip.linodeusercontent.com`, verify rc 0 |
| Server password on card | **GREEN** | `[Global] Password` set in ngircd.conf; probe authenticated with it |
| External TLS join from a fresh client | **GREEN** | probe joined `#futon` from Dionysus over 6697 |
| Bot nicks present in #futon | **GREEN** | `claude codex claude-2 tickle corpus` all in NAMES |
| Bridge command surface (!help !mc !mission !reset !gate/!ungate !todo !patterns !psr !pur !par) | **GREEN** | `ngircd_bridge.py` (2437 lines); `INVOKE_SKIP_WHEN_BUSY` defaults ON |
| @claude → agent accept | **GREEN (fixed today)** | was `[accept failed] Agent not registered: claude-1` — bridge env predated the 2026-06-15 site-prefix migration. Fixed: `NICK_AGENT_MAP claude:lon-claude-1` in `~/.config/futon3c/bridge-futon.env` on lucy (backup `.bak-20260704`), `ngircd-bridge@futon` restarted. Probe #2: `[accepted claude-…] queued`. |
| @claude → actual reply | **RED — Joe-only blocker** | invoke fails in 6s, exit 1: **`claude` auth on lucy is expired** (`401 Invalid authentication credentials`, CLI v2.1.177 at `~/.local/bin/claude`). Fix: `ssh -t lucy-joe claude login`, or `claude setup-token` on the laptop → `CLAUDE_CODE_OAUTH_TOKEN` on lucy. |
| @codex → reply | **RED — decision needed** | lucy's `codex-1` = WS-bridge proxy to the LAPTOP's codex bridge ("ws bridge not connected"). Also Codex quota exhausted until ~2026-07-18. See Decisions. |
| tickle / corpus nicks | **AMBER** | mapped to `tickle-1`/`corpus-1`, neither registered on lucy's roster — mentions will accept-fail. Register or drop from BRIDGE_BOTS before the demo. |
| Evidence mirror query | **RED — parcel in flight** | `GET /api/alpha/evidence?limit=3`: 15s on laptop (near-empty store), >30s timeout on lucy. Fast-path build belled to claude-11 (2026-07-04, job `invoke-…-53de8c1c`); deploy needs a lucy restart window. NOTE: response shape is `{ok,count,entries}` — the plan's jq needs `.entries[]`, not `.[]`. |
| Venue NAT headroom | **AMBER — one sudo line** | `MaxConnectionsIP = 10` in ngircd.conf would bounce half a 20-person room behind one NAT. Needs (Joe, has sudo): `sudo sed -i 's/MaxConnectionsIP = 10/MaxConnectionsIP = 40/' /etc/ngircd/ngircd.conf && sudo pkill -HUP -x ngircd` (backup first). |
| DNS `irc.futonproject.org` | **RED — decision needed** | NXDOMAIN. Card hostname doesn't exist; TLS cert only matches the linodeusercontent name. See Decisions. |
| Mesh across two Linodes | **GREEN (fixed today)** | Root cause of the one-way view: lucy's JVM has NO `FUTON3C_PEERS` configured (hub never pulls workers). Fixed live via Drawbridge `(federation/sync-peer! "http://172.236.108.82:7070")` → `chi-claude-1 :registered`, `lon-claude-1 :skipped-local`. **Cross-node invoke PROVEN**: bell via lucy's API → proxy → metameso CLI → "ok" in <10s (job `invoke-…-9e9b6dc9`). Durability: add `FUTON3C_PEERS=http://172.236.108.82:7070` to lucy's env for the next restart; re-run `sync-peer!` after any metameso (re)boot until periodic re-sync lands. |
| lucy JVM | **GREEN** | up since 2026-06-16 (17.7d), queue-hardening gates all true, 3 agents registered |
| Load test (5 concurrent clients) | **TODO** | scripted probe exists (this excursion); run after lucy claude auth is fixed |
| Cards | **TODO** | blocked on the hostname decision; printable sheet is a quick artifact once decided |
| Projector curl/jq | **TODO** | rewrite for `{ok,count,entries}` shape + `since` param; pre-test after fast-path lands |

## Decisions — TAKEN (Joe, 2026-07-04)

1. **Card hostname/TLS: real domain + fresh cert.** `hyperreal.enterprises` DNS is served from Joe's own box (`ns1/ns2.box.hyperreal.enterprises`) — one A record away. Runbook below. Final hostname to be confirmed before cards print (working assumption: `irc.hyperreal.enterprises`).
2. **@codex seat: run Codex from linode-chicago (metameso).** Joe's own words: "plan to run Codex but from linode-chicago — if I get eager I will *also* add glm". Cross-node invoke path proven today (see mesh row). Runbook below. GLM = optional extra, Joe-driven.
3. **Matrix: SKIP for the workshop.** Rob's agents already federate via #zabuton WS; `T-matrix-federation-proof` stays parked as the post-workshop follow-up (possibly hosted from lucy — different IP may clear the Cloudflare block).

## Runbooks

**R1 — DNS + cert (Joe: DNS panel + sudo on lucy):**
1. Add `A irc.hyperreal.enterprises → 172.236.28.208` on the box DNS.
2. On lucy (nginx is present — use its plugin or webroot, NOT standalone):
   `sudo certbot certonly --nginx -d irc.hyperreal.enterprises`
3. Point ngircd `[SSL] CertFile/KeyFile` at the new lineage, `sudo pkill -HUP -x ngircd`.
4. Verify: `openssl s_client -connect irc.hyperreal.enterprises:6697` → CN matches, then tell claude-14 to regenerate/print-check the cards.

**R2 — Codex on metameso (Joe: install + auth; claude-14: register + rewire + test):**
1. (Joe) Renew the Codex subscription; on metameso: install codex CLI (`npm i -g @openai/codex`; check node first — not verified present) and `codex login`.
2. (claude-14) Register the agent on metameso with an EXPLICIT id `chi-codex-1` via `POST /api/alpha/agents` — do NOT auto-register: the site-prefix rollout covered local claude only, so auto would mint unprefixed `codex-1`, which is a PROTECTED id and gets refused on mirror to lucy.
3. (claude-14) `sync-peer!` on lucy → `chi-codex-1` proxied; bridge env `NICK_AGENT_MAP codex:chi-codex-1`; restart `ngircd-bridge@futon`; IRC probe `@codex ping` end-to-end.

**R3 — before-demo ops on lucy (Joe, sudo/auth):** claude auth fix · `MaxConnectionsIP` 10→40 + HUP · `FUTON3C_PEERS` env for next restart. Then (claude-14): load test with 5+ scripted IRC clients, mirror-phase curl/jq rewrite (`.entries[]`, `since` param) and rehearsal, evidence fast-path deploy at a Joe-blessed restart window.

## Fixes landed this excursion

- 2026-07-04: bridge `NICK_AGENT_MAP` claude→`lon-claude-1` (lucy, env backup kept), service restarted, accept path verified live from an external client.

## Joe's personal checklist (things only Joe can do)

- [ ] Fix Claude auth on lucy (`claude login` there, or setup-token → env).
- [ ] `MaxConnectionsIP` sudo one-liner (above).
- [ ] Decisions 1–3.
- [ ] Venue wifi check + 4G hotspot (from the original checklist).
