# M-federated-agency-hardening -- routing + healing per federation point

Date: 2026-06-15
Status: IDENTIFY — diagnosis recorded (Checkpoint 1). Cross-box federation routing
is not yet populating proxies; remote agents currently arrive as local phantoms.
Sibling: `M-agency-hardening.md` (CLOSED 2026-06-12 — the local/IRC layer; named
`M-kangaroo` warm-pouch as a deferred successor). This mission is the **federation**
layer: making Agency correct across boxes.

## Motivation / the principle (Joe, 2026-06-15)

> "Routing and any planned healing should be **per federation point**, not only
> locally. lon-claude-1 was the agent I was trying to reach and they were based on
> the remote box. If they had no pouch there, healing should take place **there**,
> not on my laptop."

The local-layer hardening (M-agency-hardening) assumed one box. Federation breaks
that assumption: an agent has a **home federation point**, and an invoke from
another box must **route there**; a missing/dead warm pouch must be **respawned/healed
at the home box**, never re-created locally. The existing per-box self-heals (e.g.
the session-sentinel reset in `dev/config.clj`, and the kangaroo warm pouch) are the
*local* analogue of what federation needs done *per point*.

## Checkpoint 1 — 2026-06-15 (diagnosis)

**What was done:** root-caused why a federated agent (`lon-claude-1`, hosted on a
remote box) returned empty turns when reached from the laptop, and mapped the
federation-routing gap. (Investigation only; no federation config changed.)

**Symptom:** in the laptop's claude-repl bound to `lon-claude-1` (session
`72c65a23-…`), the agent replied `[no text or tool calls in this turn]`.

**Evidence (live, this session):**
- Warm-pouch snapshot — `lon-claude-1`: `:alive? false`, `:turn-count 1`,
  `:session-bytes nil`, `:stderr ["No conversation found with session ID:
  72c65a23-…"]`. Contrast `claude-3`: `:alive? true`, `:turn-count 24`,
  `:session-bytes 718126` (a real, working local conversation).
- Roster routing — **every** agent (incl. `lon-claude-1`, `chi-claude-1`) is
  `:invoke-route :local`, `invoke-diagnostic "local invoke-fn registered"`,
  `auto-registered? true`, `emacs-socket "server"`, **no `origin-url` / no
  `proxy?`**.
- Federation config (laptop, via Drawbridge): `peers ["http://172.236.28.208:7070"]`
  (the linode hub), **`self-url nil`**, **`proxy-agents []`** (none), `role "laptop"`.

**Root cause:** `lon-claude-1` lives on a remote box, but on the laptop it is a
**local phantom** — auto-registered locally (via the emacs repl-attach path) rather
than as a federation proxy. So the laptop tried to run it itself: `spawn-pouch!`
issued `claude --resume 72c65a23-…`, that session-id has no conversation on the
laptop → the process died ("No conversation found") → the feed got no response →
empty turn, with no fallback. (A real-looking-but-nonexistent UUID also slips past
the `"nil"/"null"` sentinel check in `dev/config.clj`, so neither warm nor cold would
have recovered locally — and recovering locally is the wrong thing anyway.)

**Contrasting case — `chi-claude-1` (reachable, which is the *more dangerous* symptom):**
`chi-claude-1` is the *same* kind of `:local` phantom (no `origin-url`/proxy), yet it
**was reachable by REPL — at least initially** — where `lon-claude-1` was not. Live
now: `chi-claude-1` still holds registry session `1e5abee2-…` but has **no warm pouch**
(`:chi-pouch {}`); `lon-claude-1`'s session is `nil` (reset this session). The only
difference is whether the phantom's session-id resolved to a **real local
conversation**: `chi-claude-1`'s did, so it answered; `lon-claude-1`'s did not ("No
conversation found"). The sharper reading: `chi-claude-1` "working" is **not** evidence
that federation works — a laptop-**local** conversation answered under that name, i.e.
you were talking to a local stand-in, not the real remote `chi-` agent. So accidental
local reachability **masks** the routing gap (silent-wrong is worse than the clean
"No conversation found" failure). The fix is the same for both: route to the home
federation point so the *real* agent answers.

**Why the proxy path didn't engage:** the mechanism exists —
`futon3c.agency.federation` (`announce!` → a peer creates a `make-proxy-invoke-fn`
proxy whose invoke-fn forwards to `origin-url/api/alpha/invoke`) — but nothing is
announcing the remote agents *into* the laptop, so `proxy-agents` is empty and the
only `lon-claude-1` the laptop knows is the local phantom.

**Gaps to close (the mission's work):**
1. **`self-url` unset on the laptop** (and likely per box) — `announce-to-peer!`
   sends `origin-url = self-url`; with it nil the announce can't hand a valid origin.
2. **The home box is not announcing its agents to the laptop** (or the laptop isn't
   in its peer list) — federation must be bidirectional / each-box-knows-peers so
   proxies actually arrive. `peers` on the laptop points only at the linode hub.
3. **emacs repl-attach mints a LOCAL phantom for a remote agent name**, masking the
   proxy. Attaching a repl to a remote agent should **bind to the proxy and route
   remotely**, not auto-register a local agent. (Order matters: the proxy must exist
   first; then attach must not clobber it. See `claude-repl--auto-register` /
   `--restore-agent` and `/api/alpha/agents/auto|restore`'s local-vs-`origin-url`
   branch in `transport/http.clj` ~1903–1964.)
4. **Per-point healing** — once routing is correct, the "no pouch / dead pouch /
   `No conversation found`" self-heal must run at the **home** federation point
   (respawn fresh there), not locally. (The local kangaroo + session-sentinel
   self-heals are the per-box template to lift to per-point.)

**Immediate action taken (noted as the *wrong layer*, kept honest):** reset
`lon-claude-1`'s session + evicted its dead pouch on the laptop. This cleaned the
phantom's broken pouch but did **not** fix routing — the local registration remains,
so a fresh invoke would still run locally. The real fix is to stop running remote
agents locally at all.

**Test state:** n/a (diagnosis checkpoint; no code changed in this mission).

**Next:** Joe is building the announce/peering side across boxes. Candidate first
steps, his call on order: (a) set `FUTON3C_SELF_URL` per box; (b) make each box
announce its agents to its peers (and confirm bidirectional peering); (c) make the
emacs repl-attach proxy-aware so it stops minting local phantoms for remote agents;
(d) lift the warm-pouch self-heal to run at the home point. Each is a candidate
sub-checkpoint here.

## Checkpoint 2 — 2026-06-16 (remote health probe; wedge fixed; firewall claim retracted)

**What was done:** probed the health of the two remote claude agents (`lon-claude-1`
on London, `chi-claude-1` on Chicago) from the laptop and over SSH, and attempted the
Joe-approved no-JVM-restart fix on London. Findings:

**Box / region map (from `~/.ssh/config`, all SSH on :2222):**
- `lucy-joe` = 172.236.28.208 = **London** = the federation hub (laptop's only `peers` entry).
- `linode-chicago` (= `linode-joe`) = 172.236.108.82 = **Chicago**.

**Chicago — `chi-claude-1`: UP ✓.** Box healthy, futon3c JVM serving, 7070 listening
locally; `chi-claude-1` registered, **status idle**, session `042f5d3d-…`, route local.
The *real* Chicago agent is alive and well on its home box. No action needed there.

**London — `lon-claude-1`: agent healthy in-registry, but the box's HTTP API is wedged.**
- JVM alive (pid 2784025), **Drawbridge 6768 responsive**, **`futon-ops` emacs responsive**
  (`(+ 2 2)`→4), load 0.16, **no threads stuck in app code** (only idle `main`/xtdb).
- `lon-claude-1` registered + **idle**, session `72c65a23-…`, and **that conversation
  EXISTS on disk** at `~/.claude/projects/-home-joe-code-futon3c/72c65a23-….jsonl` — so on
  its home box the agent is genuine (contrast the laptop, where the same id was a phantom
  with no conversation, Checkpoint 1).
- **BUT port 7070 won't serve**: localhost curl times out; `ss` shows `LISTEN 51 50` on
  `*:7070` — the accept backlog is full (51 > 50), connections pile up unaccepted. The
  http-kit 7070 listener is wedged (server-loop alive in epoll-wait, but not
  accepting/responding) while the rest of the JVM is fine.

**~~KEY federation-transport finding — the firewall~~ — RETRACTED 2026-06-16, was a
misdiagnosis.** I originally concluded laptop→linode `:7070` was firewalled because curl
timed out. **It was not the firewall — it was the wedged old London JVM not accepting
connections** (the full `LISTEN 51 50` accept backlog). After the clean restart, the laptop
reaches **both** boxes' `:7070` directly: London `200` in ~57 ms, Chicago `200` in ~228 ms,
and the laptop sees each box's real roster over HTTP. There is **no firewall block** on
`:7070` from the laptop. (Joe also corrected the framing: laptop↔linode has long worked via
IRC and bells/whistles over a WebSocket link — reachability was never the wall I painted.)
So Checkpoint 1's gaps #2/#3 stand on their own: the boxes are mutually reachable; what's
missing is the **announce/proxy routing** so the laptop stops running `lon-`/`chi-` as
local phantoms and instead routes to the home box.

**Attempted fix (London, Joe-approved: bounce 7070 via Drawbridge, no JVM restart) — FAILED.**
Rebuilt the handler from live singletons (`!evidence-store`, `(:node @!f1-sys)`; no IRC
system present) and called `(:server @!f3c-sys)` to stop + `http/start-server!` to rebind,
via Drawbridge. The stop signalled, but the wedged server **does not release the listen
socket** (server-loop can't process the close), so the rebind throws
`java.net.BindException: Address already in use`. (Clojure wraps this as a misleading
"Syntax error macroexpanding at (1:1)" — the form actually ran.) Net effect: London's 7070
is in the **same wedged state as before** (still listening, still not responding, registry
intact — no worse), plus a harmless lingering second `server-loop` thread from the failed
binds. **Conclusion: an in-process Drawbridge bounce cannot work while the wedged server
holds the port; freeing it needs either a reflective force-close of the http-kit listen
socket (uncertain, risks further JVM destabilisation) or a JVM restart (frees everything,
reloads current code; drops warm pouches; registry restored via `restore-on-boot!`).**
Deferred to Joe's call. (Originally framed as low-urgency because I believed
`lon-claude-1` was unreachable from the laptop due to a firewall — that belief was wrong;
see the RETRACTED finding above. The restart was in fact the whole fix.)

**Test state:** n/a (probe + one failed in-process bounce; no committed code change).

**Resolution — 2026-06-16 (clean restart on merged code).** Joe's call: bring London
down and clean-`fdev`-start it on the laptop's latest code. Done. London had
**uncommitted federation work** (the `self-url` + `site-qualify` wiring this mission
called for — gaps #1, #3) in `scripts/dev-linode-env` (`FUTON3C_SITE=lon`,
`FUTON3C_SELF_URL=http://172.236.28.208:7070`) and `config.clj`/`agents.clj`; preserved it
(branch `lon-federation`, then cherry-picked onto the laptop's `agency-fixes-2026-06-11`
— only `dev-linode-env` was London-unique; the config/agents site-qualify was already on
the branch). Killed the wedged JVM (SIGTERM ignored → SIGKILL, confirming the wedge) and
ran `fdev --no-attach`. London is back: 7070 serving, roster restored
(`claude-2`/`codex-1`/`lon-claude-1`, all idle), now on current code (incl. the
evidence-query and streaming fixes London was missing) **with `FUTON3C_SELF_URL` and
`FUTON3C_SITE=lon` live** — so gap #1 is closed on London and `claude-1` correctly
registers as `lon-claude-1`. **Post-restart the laptop reaches London `:7070` directly
(200 / ~57 ms) and Chicago `:7070` (200 / ~228 ms) — no firewall.** **Still open (the real
gap):** the laptop runs `lon-claude-1`/`chi-claude-1` as **local phantoms** (`:invoke-route
:local`), so a "say hello" spawns a *local* claude (here with the bare-`~/` cwd bug and a
crossed session that introduced itself as `chi-claude-1`) instead of routing to the home
box — Checkpoint 1's gaps #2/#3, now unblocked since both boxes are reachable. There is
also a **stuck WS connection** laptop→London `:7070` (≈63 KB queued London→laptop, unread)
worth investigating — likely the federation/relay bridge that should carry this routing.
The federation config on London is committed (`53acac4`); pushing it to origin so the
laptop/GitHub also carry the `dev-linode-env` site defaults is a small follow-up (not yet
done).

## Adjacent observations — 2026-06-15 (to fold into later checkpoints)

**(A) No real-time turn throughput from pouched agents.** Watching agents that run
on warm pouches, their turns surface **all-at-once on completion**, not live. Root
cause in code: `agent_pouch.clj/read-turn*` accumulates all `assistant` text into a
`StringBuilder` and only returns `{:result …}` at the stream's `result` event; the
`:on-event` hook surfaces **tool activity**, not assistant text. So nothing of a
turn's prose appears until it finishes. (Same shape on the cold path — both
read-until-`result`; the repl gets the whole turn on return. The "live" feel was only
ever the 5s `*agents*` ticker + `*invoke:*` blackboard refresh, never streamed text.)
M-kangaroo's "Observability parity (2026-06-11)" brought warm to parity with cold on
the *observability contract* (evidence, blackboard, ticker, tool surfacing, interrupt,
pattern retrieval) — but its flagged residual asymmetry is *text concatenation* (last
vs all), **not** incremental streaming. So real-time text throughput is an *un-noted*
limitation of **both** paths. Candidate follow-on: thread incremental `assistant`
events through `:on-event` → blackboard/repl so turns stream as they're produced
(matters more in the federated case, where many agents' turns are watched at once).
[Check before fixing: confirm the warm path's ticker/blackboard *is* firing — if it
went quiet for pouched agents, that's a regression of the parity fix, distinct from
the never-implemented text streaming.]

**(B) Open question: should claude emit auto-bellback to Codex? (may just waste time).**
Observed in the JVM console this session: after a `claude-N` warm turn finishes a bell
job, the **auto-bellback** to `codex-N` triggers a codex invoke that returns a trivial
turn — e.g. `[invoke] codex-2 ok result-len=13 execution-evidence=false tool-events=0`
— i.e. a near-no-op acknowledgement that still costs a full invoke (and, with codex,
real time/tokens). Auto-bellback (from M-agency-hardening: route a completion
notification back to the caller so the mesh edge is recorded and the caller can
resume) may be wasteful in the **claude→codex** direction, or more generally for
*completion-only* notifications that don't warrant a response turn. Candidate
refinement: make completion-bellbacks **non-invoking** (record the mesh edge / notify
without spawning a responder turn), or suppress auto-bellback for directions/types
where the recipient has nothing to do. Not yet investigated — flagged per Joe.

## Observation — 2026-06-16 (`--resume` bifurcates a live session; I-1 violation)

While driving the mark3 Linode run from the Emacs REPL, an **agency-spawned headless
twin of this very session** was found running concurrently: `claude --print
--input-format stream-json --permission-mode bypassPermissions --resume <session-id>`,
parented by the `make dev` JVM under tmux `futon-dev`. Both incarnations shared the
session id and the working tree; the headless twin was autonomously re-executing the
session's background Bash (an IATC reconstruction loop) — racing the interactive
incarnation on shared output files **and** the single GPU, and **regenerating "ghost"
loop/​shell processes on every kill** until the twin's parent `claude` PID was killed at
the source. PIDs that day: `450692` (twin, under `make dev`) vs `466749` (the interactive
session; its shell was the direct child — confirming "466749 is me", do-not-kill).

- **Expectation (Joe, per `futon3/docs/guides/README-peripherals.md`):** stopping input to
  the REPL and `--resume`-ing on the CLI should **capture / hand off** the live session to
  a single inhabitant — not fork it. Observed behavior is **bifurcation**: two live
  inhabitants of one session id.
- **This is a direct I-1 violation** (futon3c CLAUDE.md — "one agent = one session = one
  identity; an agent already running is never represented by spawning a new process").
- **Suspected cause:** the M-kangaroo warm-pouch changes may have altered resume/capture
  semantics (historically, early Agency *did* run bifurcated agents as a known mode).
- **Hardening asks:** (1) `--resume <id>` must detect an already-live inhabitant and
  **attach-or-refuse**, never fork — a per-session-id single-writer lease/lock; (2) the
  agency auto-spawn path (`make dev`) must not headlessly resume a session that has a live
  interactive inhabitant; (3) until (1)/(2) land, concurrent incarnations sharing one cwd
  and output dir cause file races — argues for **per-incarnation git worktrees / output
  dirs** as a stopgap. Also surfaced: an auto-started `vllm serve` was bound to
  `0.0.0.0:8000` (reachable off-box) — agency-spawned dev services should default to
  `127.0.0.1`.

## Phase transition — 2026-06-16: hot-fixes → a logic model of Agency (Joe)

> "I think we may have done just about as much as we can do with 'hot fixes' — the next
> step would be to actually build a logic model of Agency so we can prove that it behaves
> according to the various invariants that we've bumped into without writing them down."

Every failure this mission chased was a symptom of an **unstated invariant that nothing
checks**. The hot-fixes (session sentinel, codex liveness ping, inline codex, the London
restart) each patched one symptom. The durable move is to **formalise the invariants and
prove them**, following the futon logic-model discipline already used elsewhere
(`src/futon3c/agents/tickle_logic.clj` — `core.logic`/`pldb` with `db-rel invoke-readyo`;
the typed-bells logic model TB-1..7 from M-typed-bells). Build an **Agency logic model**:
encode agency state as facts (agents, sessions, connections, routes, peers) and the
invariants below as relations / checkable goals, so `run*` can FIND violations and the
invariants can be PROVEN over the modelled state space. Each invariant gets an id, a
logic-model check, and a test — the VERIFY artifact this mission has been missing.

### Invariants bumped into (evidence-first; candidates AG-1..AG-7 for the model)

Prose invariants I-0..I-5 already live in `futon3c/CLAUDE.md`; these are the ones this
mission surfaced that are **not yet written down or checked**:

- **AG-1 Singular identity & session integrity** (extends I-1). One agent ↔ one session-id
  ↔ one live inhabitant ↔ one conversation. *Violations seen:* `lon-claude-1` resumed a
  conversation cross-contaminated with `chi-claude-1` (identity crossing); `--resume`
  bifurcated one live session into two inhabitants (Observation above). *Property:* no two
  registry agents share a session-id; a session-id has exactly one live writer (lease).
- **AG-2 Home-point routing.** An invoke must reach the agent's home point (where its real
  conversation/worker lives), never run as a local phantom. *Violation:* the laptop ran
  `lon-`/`chi-claude-1` locally (Checkpoint 1). *Property:* a local registration for a
  remote-homed agent is ill-formed; route(agent) → home.
- **AG-3 Connection-state honesty.** `:connected` ⇒ a live underlying channel; a dead
  channel ⇒ eventually `:disconnected` ∧ reconnecting. *Violation:* the codex WS bridge
  sat `:connected` with no socket for up to 10 min (063096f liveness fix). 
- **AG-4 Continuous reachability, not registration-frozen.** Remote-target health is a
  function of current state, not a latch set once at registration. *Violation:* the codex
  reachability gate ran only at registration, so a later hub outage zombied the agent.
- **AG-5 Total, diagnosable invoke-readiness.** ∀ agent: invoke-ready? (route ∈
  {:local,:ws}) ∨ carries a diagnostic explaining why not. *Partially modelled already* by
  `registry/invoke-routing-info`; the model should make it total + provable.
- **AG-6 Session capacity / freshness.** A context-exhausted session must reset to a fresh
  thread, not fail every invoke. *Violation:* codex-1 session `019ecbd7` ("ran out of
  room") failed all invokes until reset (this session).
- **AG-7 Server accept-liveness.** Listening ⇒ accepting (bounded accept latency); a full,
  unserviced accept backlog is a violation. *Violation:* London's http-kit listened with a
  saturated backlog while not responding (the wedge that triggered the restart).

These are candidates, not final — DERIVE should consolidate/rename and check for overlap
(e.g. AG-3 and AG-4 may be one liveness invariant). Next mission step: **MAP** the agency
state into the logic-model fact schema, then **DERIVE** the AG-* relations.

## Checkpoint CP-A — 2026-07-10 (Agency logic model MAP + DERIVE)

Implemented the VERIFY artifact as a pure `core.logic`/`pldb` model in
`src/futon3c/agency/logic.clj`, extending the existing Agency logic namespace rather than
splitting a duplicate federation model. The MAP layer now includes explicit facts for
agent identity/type/status, session ids and live writers, invoke route/readiness/diagnostic,
home point/local point, proxy/origin-url, peer sites/URLs, self URL, local registrations,
peer rosters, connection liveness, remote health, session capacity, and server
accept-liveness. It accepts plain snapshots through `snapshot->db` for deterministic tests
and can also build a read-only live snapshot from `registry/!registry`, `registry-status`,
and `federation/!config` via `build-live-db`.

DERIVE decision: AG-3 and AG-4 are one liveness-honesty invariant in the model. AG-3 is the
transport half (`:connected` implies live channel); AG-4 is the remote-health half
(registered-ready must reflect current reachability). The exported relation is
`ag-3-4-unreachable-connectedo`, with the finder `find-unreachable-connected` preserving
which half fired.

CP-A finders are exported with the mission names:
`find-phantoms`, `find-unpropagated`, `find-session-collisions`, and
`find-unreachable-connected`. Deterministic fixtures in
`test/futon3c/agency/federation_logic_test.clj` reproduce the two recorded violations:
`claude-4` local on `:lon` missing from the `:laptop` roster, and `lon-claude-1` /
`chi-claude-1` locally minted on `:laptop` as remote-home phantoms. A correctly federated
proxy fixture has no non-informational violations. Each AG-1 through AG-7 relation has a
passing and failing fixture.

Gates at checkpoint creation: `clj-kondo --lint src/futon3c/agency/logic.clj
test/futon3c/agency/federation_logic_test.clj` clean; `futon4/dev/check-parens.sh` clean
on the changed Clojure files; `clojure -M:test -n futon3c.agency.federation-logic-test -n
futon3c.agency.logic-test -n futon3c.agency.invariants-test -n
futon3c.agency.federation-test` passed with 53 tests / 122 assertions. The checkpoint is
standalone and changes no runtime behavior; no JVM/server restart was performed.

## Checkpoint CP-B slice 1 — 2026-07-10 (AG-2 registration seam enforcement)

Implemented the first model-driven enforcement slice for AG-2 on the server-side Clojure
registration path. `/api/alpha/agents` registrations with `origin-url` now route through
`futon3c.agency.federation/register-proxy-agent!`, so peer announces create or refresh a
federation proxy with `:proxy? true`, `:remote? true`, `:origin-url`, inferred
`:home-site`, and a `make-proxy-invoke-fn` back to the home Agency. Site-qualified local
phantoms such as `lon-claude-1` are replaced by the proxy when a valid origin announce
arrives, while unqualified protected local lanes remain protected.

The local registration seams now refuse remote-home identities before they can mint a
local invoke-fn. The guard recognizes existing proxies and configured peer site prefixes
(`FUTON3C_PEER_SITES` / `:peer-sites`) for ids like `lon-claude-1` and `chi-claude-1`.
Both `/api/alpha/agents` without `origin-url` and `/api/alpha/agents/restore` return
`remote-home-local-registration-refused` for those ids, so the Emacs restore/attach path
cannot strip proxy metadata and turn a remote agent into a local phantom. `/agents/auto`
also checks the selected id before local session-file mutation.

`announce!` still does not arm federation when peers/self-url are missing, but it no
longer silently no-ops: it logs a structured skip reason such as `:no-peers` or
`:no-self-url`. Valid announces continue to emit `origin-url` and `proxy=true`.

Acceptance oracle: `test/futon3c/agency/federation_registration_test.clj` drives the real
HTTP handlers, then builds a CP-A snapshot from `registry/!registry` and asserts
`logic/find-phantoms` is empty. It also seeds the pre-fix local-phantom state and verifies
CP-A flags it before a valid origin announce heals it. Gates at checkpoint creation:
`clj-kondo --lint src/futon3c/agency/federation.clj src/futon3c/transport/http.clj
test/futon3c/agency/federation_registration_test.clj test/futon3c/agency/federation_test.clj`
clean; `futon4/dev/check-parens.sh` clean on the changed Clojure files; `clojure -M:test
-n futon3c.agency.federation-registration-test -n futon3c.agency.federation-test -n
futon3c.agency.invariants-test -n futon3c.agency.logic-test -n
futon3c.agency.federation-logic-test` passed with 58 tests / 155 assertions.

Deferred: B2 still owns WS transport/reconnection liveness; B3 still owns IRC bridge and
Emacs proxy-binding UX. No server/JVM restart was performed.

## Checkpoint CP-B slice 2 — 2026-07-10 (continuous, liveness-aware peer sync)

Implemented the default-off continuous federation sync layer. The existing one-shot
`sync-peers!` remains in bootstrap, and bootstrap now also calls
`federation/start-sync-daemon!`; with the default `FUTON3C_FED_SYNC_INTERVAL_MS=0` this is
a no-op, preserving current restart behavior. Joe can arm the daemon by setting
`FUTON3C_FED_SYNC_INTERVAL_MS` to a positive millisecond interval; optional
`FUTON3C_FED_SYNC_MAX_BACKOFF_MS` caps retry backoff.

The daemon uses a single-thread daemon `ScheduledExecutorService`, mirroring the
watcher/multi pattern. Its deterministic core is `sync-tick!`, which accepts injected
`:now-ms`, `:fetch-fn`, and `:jitter-fn` for offline tests. Each tick reuses
`sync-peer!` / `register-proxy-agent!`, imports newly appeared peer agents as proxies, and
prunes departed proxies for that peer. Real local agents are never pruned because pruning
only removes records still marked as proxies from the peer origin URL.

AG-3/AG-4 liveness model: each peer has tracked reachability and per-peer backoff. A
failed pull marks only that peer's proxies stale instead of deleting them; their registry
readiness becomes `:invoke-route :none` with a federation-unreachable diagnostic, so they
no longer read as live. CP-A live snapshots now include federation `connection-state` and
`remote-health` facts via `federation/connection-facts` and
`federation/remote-health-facts`, so `logic/find-unreachable-connected` flags stale peer
proxies and clears when the peer recovers and the roster re-syncs.

Tests: `test/futon3c/agency/federation_sync_test.clj` covers the frozen-roster fix
(`claude-4` appears after the next tick), departure pruning, local-agent non-pruning,
AG-3/AG-4 stale/recovery with backoff, and the default-off gate. Gates at checkpoint
creation: `clj-kondo --lint src/futon3c/agency/federation.clj
src/futon3c/agency/registry.clj src/futon3c/agency/logic.clj dev/futon3c/dev/bootstrap.clj
test/futon3c/agency/federation_sync_test.clj test/futon3c/agency/federation_test.clj`
clean; `futon4/dev/check-parens.sh` clean on the changed Clojure files; `clojure -M:test
-n futon3c.agency.federation-sync-test -n futon3c.agency.federation-registration-test -n
futon3c.agency.federation-test -n futon3c.agency.invariants-test -n
futon3c.agency.logic-test -n futon3c.agency.federation-logic-test` passed with 62 tests /
186 assertions.

Deferred: no new WS transport or WS reverse-invoke work; no IRC bridge or roster-driven
IRC bot changes. No server/JVM restart was performed.

## Checkpoint CP-B slice 3 — 2026-07-10 (IRC bots from live Agency roster)

Implemented the bridge-side IRC analogue of B2 in `scripts/ngircd_bridge.py`, default-off.
The pure rule is `desired_bot_nicks(roster_json, type_allowlist)`: select agent ids from
`GET /api/alpha/agents` whose type is in the allowlist (`claude,codex,zai` by default) and
whose roster metadata is not proxy/remote. This enforces the mesh invariant that an
agent's IRC bot is present only at its home bridge; imported federation proxies are
excluded so remote agents do not get duplicate IRC nicks or relay loops.

Runtime reconciliation is gated by `BRIDGE_BOTS_FROM_ROSTER=false` by default, preserving
the existing static `BRIDGE_BOTS` behavior until armed. When Joe sets
`BRIDGE_BOTS_FROM_ROSTER=true` and restarts the bridge unit, the bridge seeds bots from
`BRIDGE_BOTS ∪ desired_bot_nicks(roster)` and periodically refreshes from
`AGENTS_URL`. Newly registered local agents start an `IRCBot` thread and join the bridge's
channels; departed roster agents are cleanly `PART`/`QUIT`ed and removed. Explicit
`BRIDGE_BOTS` entries remain pinned. Roster fetch failures skip reconciliation rather than
interpreting the roster as empty.

Tests: `scripts/test_ngircd_bridge_roster.py` covers the pure selector for new local
agents, proxy/remote exclusion, departure, type filtering, flag-off static behavior, and
flag-on union with pinned bots. Gates at checkpoint creation:
`python3 -m py_compile scripts/ngircd_bridge.py` clean; `python3 -m pytest
scripts/test_ngircd_bridge_roster.py` passed with 6 tests. `ruff` and `pyflakes` were not
installed in this environment.

Deferred: no Agency/Clojure changes, no systemd unit edits, no live bridge restart, no
WS reverse-invoke changes. Running bridges remain untouched until Joe arms the flag and
restarts a bridge unit.

## Cross-references

- `M-agency-hardening.md` — the local/IRC layer (closed); the single-box predecessor.
- `M-kangaroo.md` / `futon3c/src/futon3c/agency/agent_pouch.clj` — the warm-pouch whose
  per-point healing this mission needs.
- `futon3c/src/futon3c/agency/federation.clj` — `announce!`, `announce-to-peer!`,
  `make-proxy-invoke-fn`, `peers`, `self-url`, `sync-peer!`.
- `futon3c/src/futon3c/transport/http.clj` ~1903–1964 — `/agents/auto|restore`
  local-vs-proxy (`origin-url`/`proxy?`) registration branch.
- `futon3c/emacs/claude-repl.el` — `claude-repl--auto-register` / `--restore-agent`
  (the local-phantom-minting path).
- `dev/futon3c/dev/config.clj` (`valid-session-id` session-sentinel self-heal) — the
  per-box analogue of the per-point heal needed here.
- Mesh topology: laptop (role `laptop`) ⇄ linode hub `172.236.28.208`; `lon-*` / `chi-*`
  are remote federation points.
