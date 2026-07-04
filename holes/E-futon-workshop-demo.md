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
| Mesh across two Linodes | **AMBER** | metameso up (238d), JVM running, roster `chi-claude-1` + mirrored `lon-claude-1`. But lucy does NOT see `chi-claude-1` back — federation reconciles at boot only (known gap, no periodic re-sync). One `sync-peers!` on lucy fixes it for the demo; periodic re-sync remains the tracked follow-up. |
| lucy JVM | **GREEN** | up since 2026-06-16 (17.7d), queue-hardening gates all true, 3 agents registered |
| Load test (5 concurrent clients) | **TODO** | scripted probe exists (this excursion); run after lucy claude auth is fixed |
| Cards | **TODO** | blocked on the hostname decision; printable sheet is a quick artifact once decided |
| Projector curl/jq | **TODO** | rewrite for `{ok,count,entries}` shape + `since` param; pre-test after fast-path lands |

## Decisions (Joe)

1. **Card hostname/TLS** — options: (a) print `172-236-28-208.ip.linodeusercontent.com:6697` (works today, ugly); (b) point a domain Joe controls (e.g. `irc.hyperreal.enterprises`?) at lucy + certbot a matching cert (nicest, ~half a day incl. DNS propagation); (c) IP + port 6667 plaintext on the card (no TLS story).
2. **The second agent seat (@codex)** — options: (a) renew Codex before the demo + keep laptop WS bridge running at the venue (two fragilities); (b) re-point the `codex` nick at `claude-2` on lucy (zero new infra, honest-label question); (c) integrate z.ai GLM behind the codex seat (new integration + new subscription in demo week = risk; better as a post-workshop experiment).
3. **Matrix** — recommendation: NOT this week. Rob's agents already federate via the #zabuton WS bridge; Rob-as-human can join by IRC like everyone else (or Element→his own homeserver, unrelated to our stack). Matrix bridge remains parked (`holes/tickets/T-matrix-federation-proof.md`).

## Fixes landed this excursion

- 2026-07-04: bridge `NICK_AGENT_MAP` claude→`lon-claude-1` (lucy, env backup kept), service restarted, accept path verified live from an external client.

## Joe's personal checklist (things only Joe can do)

- [ ] Fix Claude auth on lucy (`claude login` there, or setup-token → env).
- [ ] `MaxConnectionsIP` sudo one-liner (above).
- [ ] Decisions 1–3.
- [ ] Venue wifi check + 4G hotspot (from the original checklist).
