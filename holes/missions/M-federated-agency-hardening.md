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

## Checkpoint CP-B slice 4 — 2026-07-12 (laptop roster-completeness: arm sync daemon)

**Trigger:** claude-6 (on lucy/London) relayed via IRC that the laptop's roster
was incomplete — London's `claude-6` was missing from the laptop's roster.

**Diagnosis (live, this session):** Compared rosters across all three boxes:

| Agent | Home | Laptop | London | Chicago |
|-------|------|--------|--------|---------|
| claude-6 | London | **MISSING** | ✓ local | ✓ proxy |
| claude-1 | laptop | ✓ local | MISSING | MISSING |
| codex-2 | laptop | ✓ local | MISSING | MISSING |
| zai-2..10 | laptop | ✓ local | MISSING (only zai-1) | MISSING (only zai-1) |

**Root cause — two asymmetric gaps:**

1. **London→laptop (sync-pull):** The laptop peers with London
   (`FUTON3C_PEERS=http://172.236.28.208:7070`) and runs a one-shot
   `sync-peers!` at boot. But `claude-6` appeared on London AFTER the laptop's
   last boot, so it was never pulled. The continuous sync daemon
   (`start-sync-daemon!`, implemented in CP-B slice 2) was default-off
   (`FUTON3C_FED_SYNC_INTERVAL_MS=0`), so no subsequent tick caught it.

2. **Laptop→London (announce):** The laptop has no `FUTON3C_SELF_URL`, so
   `announce!` silently skips every registration with `:no-self-url` (confirmed
   in test output). London never learns about laptop agents `claude-1`,
   `codex-2`, `zai-2..10`. London can't sync-pull from the laptop either because
   the laptop binds `127.0.0.1` (not reachable from remote boxes), and London's
   env only peers with the laptop if `FUTON3C_LAPTOP_URL` is set (it isn't).

**Fix applied (commit `df1d23b`):**

- `scripts/dev-laptop-env`: armed the continuous sync daemon with
  `FUTON3C_FED_SYNC_INTERVAL_MS=60000` (1 min) and
  `FUTON3C_FED_SYNC_MAX_BACKOFF_MS=300000` (5 min backoff cap). This closes gap
  #1 — on next laptop restart, `claude-6` and all future London agents propagate
  within 1 minute of appearing on London. Peer fetch failures skip the tick
  rather than pruning proxies (AG-3/AG-4 liveness model from CP-B slice 2).

- Documented why `FUTON3C_SELF_URL` is intentionally unset on the laptop: it
  binds `127.0.0.1`, so London can't reach it back. The laptop→London announce
  direction (gap #2) requires a network-level path — Joe's topology decision
  (tunnel, public bind, or London peering with the laptop via SSH-forwarded
  port). Until then, London-side agents can't invoke laptop-only agents
  (`claude-1`, `codex-2`, `zai-2..10`) by proxy.

**Gates:** bash syntax check clean (`bash -n`). No Clojure changed (env script
only). `clojure -M:test -n futon3c.agency.federation-sync-test -n
futon3c.agency.federation-registration-test -n futon3c.agency.federation-test`
passed 30 tests / 112 assertions. No JVM/server restart performed — the new env
vars take effect on next `./scripts/dev-laptop-env` boot.

**Deferred:** gap #2 (laptop→London) needs Joe's network decision. London should
also arm its own `FUTON3C_FED_SYNC_INTERVAL_MS` for the London→Chicago and
London→laptop directions (that's London-side work for claude-6). No live restart
performed.

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

## Checkpoint CP-C — 2026-07-12 (roster completeness: AG-8, site-grouped *agents*, two live bugs fixed)

**Directive (Joe):** the *agents* buffer on each box and the laptop "absolutely don't
align". Desired shape: rows grouped by federation point (`chi | codex-2`, `lon |
claude-1`, `oxf | zai-1`), agents addressable locally (`claude-6`) or globally
(`oxf-claude-1`), backed by a completeness criterion: **the roster shows all agents
across all peers at all times** (= AG-8).

**Live diagnosis on lucy (before):** federation config `peers=[]`, `peer-sites=#{}` —
nothing ever synced or announced from the hub, so each box's roster was just "what
registered locally". 11 agents, every one `:invoke-route :local`, zero proxies:
`chi-claude-1`/`chi-codex-1` were AG-2 local phantoms *on the hub*, and a bare
`claude-1` duplicated `lon-claude-1`'s session `72c65a23` (AG-1 violation). Chicago,
by contrast, already carried proper proxies for lucy's agents — the peering was
half-armed in one direction only.

**Two root-cause bugs found and fixed (both with regression tests):**

1. **The phantom-minting machine** — `roster_store.clj/restore-payload` persisted
   *proxy* records with `:proxy?`/`:origin-url`/`:remote?` stripped, so every boot
   laundered legitimate federation proxies into local-looking records that
   `restore-on-boot!` replayed as AG-2 phantoms. That is how chi-claude-1/chi-codex-1
   phantoms got onto lucy. Fix: `roster-snapshot` now excludes proxy records (the sync
   daemon re-imports live proxies from the peer after boot). Test:
   `proxies-are-not-persisted` in `roster_store_test.clj`.
2. **The sync daemon never worked** — in `federation.clj/sync-tick!` the destructured
   local `peers` (nil on daemon ticks) shadowed the `peers` fn, so `(or peers (peers))`
   called nil as a function: NPE on every scheduled tick, caught+printed by the daemon
   wrapper, `tick-count` stuck at 0. CP-B slice 2's tests always injected `:peers`, so
   they stayed green. Fix: fall back to `(:peers @!config)`. Test:
   `sync-tick-uses-configured-peers-when-none-injected` in `federation_sync_test.clj`.

**Live arming on lucy (Drawbridge, no JVM restart):** `configure!` with
peers=[chicago], self-url, peer-sites #{chi oxf}; deregistered the three
phantoms/dupes (+ removed their /tmp session files); `sync-peers!` re-imported
chi-claude-1/chi-codex-1 as proper proxies (`:proxy? true :home-site :chi`);
announced lucy's local agents to Chicago (201s; 409 for the two it already had);
daemon armed at 30s and verified ticking (tick-count advancing) after the NPE fix
was hot-reloaded. Persisted in `scripts/dev-linode-env`: default
`FUTON3C_PEERS=http://172.236.108.82:7070` (merging with FUTON3C_LAPTOP_URL when
set), `FUTON3C_PEER_SITES=chi,oxf`, `FUTON3C_FED_SYNC_INTERVAL_MS=30000`. The
peer-sites config also arms the CP-B slice-1 AG-2 refusal guard on lucy, which was
inert with `peer-sites=#{}` (that is why restore-on-boot resurrected the phantoms
unopposed).

**Site-grouped *agents* renderer:** `blackboard.clj/format-agent-status` now renders
`site | name` rows grouped per federation point (blank line between blocks, sites
sorted): site from proxy `:home-site` metadata, else the id's site prefix, else this
box's FUTON3C_SITE, else `?`; the agent's own site prefix is stripped inside its
block (`lon-claude-1` renders as `lon | claude-1`). Hot-reloaded live; lucy's buffer
now shows a `chi` block (proxies, `remote`) above the `lon` block. Test:
`format-agent-status-groups-by-site` (plus one stale assertion fixed:
`ws-bridge` is metadata, not the route, per the code's own comment).

**AG-8 named in the logic model:** `ag-8-roster-incompleteo` (relational core:
existing `missing-from-peer-rostero`) + finder `find-roster-incomplete`
(= `query-unpropagated-agents`); pass/fail fixtures in `federation_logic_test.clj`.
The buffer *displays* the invariant; the sync daemon + announce path *enforce* it;
the finder *checks* it.

**Cross-box propagation of the fix:** Chicago belled via its (now-correct) proxy
with arming instructions incl. the sync-tick! NPE fix (first bell's job ran >30min
with no result event while Chicago sat idle — the proxy-invoke job lifecycle needs a
look; re-belled compactly). Laptop reached over IRC (its bridge connects to lucy's
ngircd from 161.73.4.62; `zai` is the laptop-bridge bot in #futon): sent
FUTON3C_SITE=oxf + peering + hygiene instructions addressed to zai.

**Side findings:** (a) futon1b :7074 was wedged active-but-not-serving (listening,
established conns piling, HTTP timeouts — a live AG-7 instance on the store JVM;
the bridge's "IRC evidence write failed: timed out" for 90+ min was this).
Restarted per futon1b README remedy. (b) `zai-1` exists both in lucy's registry
(lucy-homed, M-zaif-harness) and as the laptop's zai — a live name collision that
only universal site-qualification resolves. (c) The registration-guard tests in
`federation_registration_test.clj` are FUTON3C_SITE-sensitive: with lon exported
(lucy shell default) they fail because lon-* ids read as local-homed; gates must run
`env -u FUTON3C_SITE` on lucy until the tests pin their own site context.

**Test state:** `clojure -M:test` over blackboard, roster-store, federation-logic,
logic, invariants, federation, federation-sync, federation-registration:
96 tests / 301 assertions / 0 failures (run with `env -u FUTON3C_SITE`, see (c));
clj-kondo clean on all touched files; check-parens OK.

**Deferred (next slices):** universal site-qualification at every local
registration seam + a bare-id→local-site resolver at invoke/bell/attach (renames
live agents, needs claude-repl.el cooperation — the laptop's `oxf` adoption is the
prerequisite); honest route labels for proxies in the roster (they currently render
`local` because the proxy invoke-fn is a local fn — should read `proxy → <origin>`);
laptop reachability for reverse invokes (SSH -R tunnel or the B2 WS reverse-invoke);
proxy invoke job lifecycle (first chi bell never emitted a result event).

**CP-C amendment (same day, after Chicago's bellback):** the first chi bell was not
lost — the job completed (state `done`) after a long turn; the "proxy invoke job
lifecycle" concern softens to "long turns surface no intermediate events" (kin to
Adjacent observation A). chi-claude-1's read-before-write reply exposed the real
Chicago blocker: **version skew** — its checkout predates CP-B slice 2 (no
`start-sync-daemon!`/`sync-tick!`, `configure!` without `:peer-sites`), so
Drawbridge arming was impossible there by any eval. Resolution: CP-C rebased onto
origin (which had gained the laptop's `df1d23b` "dev-laptop-env: arm federation
sync daemon" — the laptop side acted on the IRC instructions) and pushed as
`e506479`; Chicago belled to pull + reload + arm. Chicago's registry was already
clean (zero phantoms, all 8 lucy agents mirrored as proxies). futon1b's :7074
wedge cleared on restart (FTS caught up, text-search serving again).

**CP-C amendment 2 (2026-07-12, after Chicago armed):** Chicago's daemon is live
(tick-count advancing, surgical 4-file checkout onto its master — a full branch
merge there is still owed through normal channels). Its field report surfaced two
roster-completeness gaps: **(1) home-site nil for bare-id proxies** — fixed in
`b9aa6cf`: home-site resolves id-prefix → origin declaration (announce body /
roster metadata) → configured url→site map (`configure!` now retains
`:peer-site-by-url`; `FUTON3C_PEERS` accepts `site=url` entries; lucy reconfigured
live). **(2) `claude-2` import refused as `:skipped-protected-id`** — a London
agent whose bare id collides with a Chicago-protected lane is silently absent from
Chicago's roster; OPEN, deferred to the universal-site-qualification slice (the fix
is renaming at the source, not import-side exceptions). Same-day adjacent fix: WS
handshakes were failing `:invalid-registry` for the laptop's codex-3 because
`AgentType` lacked `:zai` and S-presence validates the whole registry per
handshake (`b436c84`) — codex-3 now registers on lucy (route `:none` pending
ws-availability). The laptop side is active: it committed its own CP-B slice 4
(laptop roster-completeness, `9536ff7`) and armed its dev-laptop-env.

**CP-C amendment 3 (2026-07-12, laptop-side live deploy):** claude-6 (on lucy)
requested the laptop deploy for full roster auto-sync. The laptop's running JVM
predated the NPE fix (`e506479`), so the daemon was armed in env (`df1d23b`) but
never ticked (`tick-count` stuck at 0). Live deploy via Drawbridge:

1. **Manual sync** (`/eval` → `fed/sync-peers!`): immediately pulled London's
   current roster. `claude-6` registered as a proxy (`:action :registered`).
   Roster went from 21→22 agents, 3→4 proxies.
2. **Hot-reloaded `federation.clj`** into the running JVM (the NPE fix lives in
   source but the running code was stale). Stopped the old daemon, restarted with
   60s interval. Verified `tick-count` advanced to 1 after 65s — the daemon is
   genuinely ticking, not silently NPE-ing.
3. **`FUTON3C_PEER_SITES=lon,chi`** added to `dev-laptop-env` (`cf50a89`): arms
   the AG-2 phantom guard on the laptop so the emacs repl-attach path cannot mint
   local phantoms for remote-homed agents. Takes full effect on next boot;
   live-armed via the hot-reload.

Laptop roster now partially complete (London→laptop direction): 4 proxies
(`claude-6`, `lon-claude-1`, `chi-claude-1`, `chi-codex-1`) — but this is only
the non-colliding subset. London's `claude-2…5`, `codex-1`, `zai-1` were silently
skipped (`:skipped-local` / protected-id) because the laptop has its own agents
under those bare names. Under the bare-id regime, two sites that both grew
`claude-2…5` can never fully see each other — this is the strongest evidence yet
for the universal-site-qualification slice (the laptop's `oxf` adoption is the
prerequisite). Same collision hole as Chicago's `claude-2` case (CP-C amendment 2).

The laptop→London announce direction was deferred as "needs network path." A
concrete answer (claude-6, in-thread): a persistent reverse SSH tunnel from the
laptop — `ssh -f -N -R 17070:localhost:7070 -p 2222 joe@172.236.28.208` — then
`FUTON3C_SELF_URL=http://127.0.0.1:17070`. The URL only needs to resolve from
lucy; lucy relays onward. Independent of renaming, though the collision problem
limits laptop→London mirroring equally until ids are site-qualified.

Gates: bash syntax clean. The federation namespace reloaded without error;
daemon ticking verified. No full JVM restart performed (hot-reload + Drawbridge
eval only). Commits pushed to `origin/agency-fixes-2026-06-11` (`ba4f4a0`).

## Checkpoint CP-D — 2026-07-12 (import-seam site-qualification: locals bare, imports qualified)

**Design (Joe):** rosters need not be literally identical — "I like being able to use
the local short form; 'tell codex-1' should be automatically localized." So identity is
(site, local-name) with the split at the IMPORT seam: local agents keep bare ids
(bare addressing always resolves on-box), and imported proxies register site-qualified
(`proxy-local-id`: laptop's codex-1 → oxf-codex-1 on lucy), forwarding invokes with the
remote's own bare id (`:remote-agent-id`). Nothing is renamed at its home box — the
disruptive "rename at the source" variant died here.

**This dissolves the collision gap** (Chicago's claude-2, the 7 mutually-invisible
laptop/London ids): same-named agents on different sites now coexist as qualified
proxies. Guards added: `own-site-reflection?` (a peer's proxy of OUR agent is skipped,
never imported as a loop-back; detected by home-site = our site or entry origin-url =
our self-url) and direct-presence dedup (codex-3's ws-bridge row suppresses the proxy
import of the same agent). Prune compares qualified ids and excludes reflections, which
makes migration self-healing: pre-qualification bare proxies and pre-guard loop-backs
are pruned on the first tick after upgrade.

**Verified live on lucy:** one reload+tick took the roster from 21 to 31 agents with
the full laptop contingent visible — `oxf | claude-1…5, codex-1…3, zai-1…12` — bare
zai proxies migrated to qualified twins, codex-3 single-rowed. Earlier same evening the
laptop had come fully online (zai-3's deploy: tunnel `ssh -R 17070:localhost:7070`,
self-url, announce push — its agents auto-appeared on lucy within seconds of the
announce, initially grouped lon until lucy's `oxf=tunnel` peer entry provided the
url→site mapping; persisted in dev-linode-env `d0c6f33`).

**Commits:** `fbe3376` (qualification + guards), dedup + prune-exclusion follow-ups
(branch tip). Tests: federation sync/registration/logic suites green (38/139 on the
sync+reg subset; run `env -u FUTON3C_SITE`). Chicago belled and laptop IRC'd to pull —
the laptop urgently (its pre-guard code would import loop-backs of its own agents from
lucy's now-qualified roster; self-heals once it upgrades).

**Still open after CP-D:** bare-id resolution for OUTBOUND addressing is implicit
(bare = local registry hit; qualified = proxy hit) — a dedicated resolver at the
bell/invoke seams is only needed if we later want bare ids to fall through to a unique
remote match. Honest route labels for proxies (`proxy → origin` instead of `local`),
codex-3 execution bridge (presence-only), laptop FUTON3C_SITE=oxf adoption (now purely
cosmetic for its own display), tunnel durability (systemd unit / autossh on the laptop).

## Checkpoint CP-E — 2026-07-13 (known needed extensions after CP-D)

The mesh works: three sites, auto-join both directions, site-grouped rosters, import-seam
qualification, "tell codex-1" localizes. What remains, in rough priority order — each is
a candidate slice with its evidence already recorded above:

1. **Honest proxy rows (AG-5 display).** Proxies render `[… local] … — ready` because the
   proxy invoke-fn is a local fn. Rows should read the truth: `proxy → <site>` for the
   route, and invocability distinct from presence (codex-3's presence-only registration
   reads `ready` but refuses invokes by design; the `lane=presence-only` annotation is a
   hand-set stopgap). Includes fixing last-active on the WS ready path (codex-3 read
   "idle 11h ago" at the moment it connected).

2. **Live state sync for proxies.** Roster sync carries identity/type/capabilities, not
   live status: a proxy's `idle (3s ago)` reflects the last sync tick, not the remote
   agent; INVOKING/parked/ws-connected states are invisible off-box. Extend the roster
   payload (or a delta channel) so the *agents* view at any site shows remote agents'
   real state. (Kin to Adjacent observation A — streaming — but at roster granularity.)

3. **AG-8 run continuously.** `find-roster-incomplete` exists but nothing runs it; wire
   it into the sync daemon (or the ticker) and surface violations in the *agents* header
   — e.g. a per-site roster fingerprint so a diverged box is visible at a glance instead
   of by eyeball diff (how this whole mission started).

4. **Cross-site caller identity.** Bells/bellbacks and mesh edges record bare caller ids
   ("zai-3" could be oxf's or a future lon local). Callers crossing sites should be
   recorded site-qualified, reusing proxy-local-id semantics at the invoke/bell seams.

5. **Tunnel durability (oxf).** The laptop's reverse tunnel is a plain backgrounded ssh —
   dies on sleep/reboot. Wants a laptop-side systemd user unit or autossh. Lucy already
   degrades honestly (peer stale + backoff) when it drops.

6. **codex-3 execution bridge.** The direct WS registration is presence-only; invoking a
   laptop codex for real needs the full Codex WS bridge started laptop-side (its helper's
   own error message says as much).

7. **Deploy debts.** lucy: http.clj changes (home-site POST passthrough, cf3d623/b436c84
   shapes fix is loaded but http handler additions are not) land only at next JVM
   boot/careful reload — until then inbound announces to lucy rely on the url→site
   fallback rather than the declared home-site. Chicago: still running a 4-file surgical
   checkout on master; owes a real branch merge (its own standing note). Laptop: verify
   the post-bed535f tick imported lon-claude-2…5/lon-codex-1/lon-zai-1 and migrated its
   stale bare claude-6 proxy (instructed, unverified).

8. **Test-suite site hygiene.** federation-registration tests assume an undecorated box;
   on lucy (FUTON3C_SITE=lon exported) they fail — gates must run `env -u FUTON3C_SITE`.
   Fixtures should pin their own site context so the gate is env-independent.

9. **Evidence-outage data loss.** During the futon1b :7074 wedge (AG-7 instance,
   2026-07-12) the IRC bridge's evidence writes timed out and that window's channel
   traffic is unrecoverable (FTS index-as-of confirms). If IRC evidence matters, the
   bridge needs write buffering/retry across store outages — possibly futon1b-side
   mission rather than this one.

Deliberately NOT extensions (settled design): renaming agents at their home box (CP-D
chose import-seam qualification instead — locals stay bare); literally-identical rosters
(each box renders its own perspective; completeness is same-agents, checked by AG-8).

## Checkpoint CP-F — 2026-07-13 (SPEC: WS federation uplink — NAT-transparent per-point sync)

**Status: ready to build. Scope-bounded handoff (R11); target builder: chi-codex-1 on
the Chicago box, branch `agency-fixes-2026-06-11`.**

**Motivation (Joe, 2026-07-13):** "I don't want to have to think about tunnels." The oxf
reverse SSH tunnel (CP-E item 5) died with a lucy reboot and needed a human to notice.
The durable form of "an autoconnecting line that triggers tunnel recreation" is to make
the line itself the transport: a NAT'd box federates by dialing OUT one WebSocket to a
hub peer; roster announce and invoke forwarding ride that socket; the client's reconnect
loop IS the per-point healing. This supersedes CP-E item 5 long-term (the systemd tunnel
unit `scripts/oxf-lucy-tunnel.service` is the stopgap until CP-F ships).

**What already exists (reuse, do not reinvent — I-4):**
- WS invoke routing: `futon3c.transport.ws.invoke` (`register!`/`invoke!`, per-agent
  pending map keyed by invoke-id) and registry fallback at `registry.clj` ~782.
- Import-seam site-qualification (CP-D): `federation/register-proxy-agent!` with
  `proxy-local-id`, `own-site-reflection?` guard, direct-presence dedup, prune rules.
- Proxy invoke pattern: `federation/make-proxy-invoke-fn` (HTTP form) — CP-F adds the
  WS-uplink analog, same shape.
- Reconnect/backoff pattern: `federation/backoff-delay-ms`, `record-peer-failure!`.
- WS server frame loop: `transport/ws.clj` (`:ready` handshake ~line 195); frame parsing
  in `transport/protocol.clj` `parse-ws-message` (case over `type`).

**Design:**

1. **Client (NAT'd box, e.g. laptop/oxf)** — new ns `futon3c.agency.fed-uplink`
   (`src/futon3c/agency/fed_uplink.clj`), running inside that box's futon3c JVM:
   - `(start-uplink!)` reads `FUTON3C_FED_UPLINK` (`<site>=<ws-url>`, e.g.
     `lon=ws://172.236.28.208:7070/<existing ws mount>`) + `FUTON3C_FED_TOKEN`;
     connects with JDK built-in `java.net.http.HttpClient/newWebSocketBuilder`
     (JDK 21 on all boxes; NO new dependency).
   - On connect: send `:fed-announce` (shape below) with the full local roster;
     re-announce every `FUTON3C_FED_SYNC_INTERVAL_MS` (30s default, idempotent
     full-state, same semantics as the HTTP sync daemon).
   - On disconnect/error: reconnect loop with the federation backoff pattern,
     re-announce on reconnect. `(stop-uplink!)`, `(uplink-status)` for ops.
   - Handles inbound `:fed-invoke` → dispatch `registry/invoke-agent!` with the BARE
     local agent-id on an executor (never the WS read thread) → reply
     `:fed-invoke-result` correlated by `:invoke-id`.
   - Handles inbound `:fed-roster` (hub's push-down) → import each entry through
     `register-proxy-agent!` (same CP-D seam and guards) so the NAT'd box sees
     `lon-*`/`chi-*` without being reachable.

2. **Server (hub, lucy)** — extend `transport/ws.clj` frame loop + `protocol.clj`:
   - New frame `:fed-announce {:site "oxf" :token "…" :roster [{:agent-id "codex-1"
     :type "codex" :capabilities […]} …]}`. Token must equal hub's
     `FUTON3C_FED_TOKEN`; mismatch → error frame + close, register nothing.
   - On valid announce: import each roster entry via `register-proxy-agent!`
     (qualified ids, `own-site-reflection?` guard, dedup — all as in HTTP sync),
     metadata `:federation/transport :ws-uplink`; register/refresh an uplink
     invoke-fn per imported agent: `make-uplink-invoke-fn` sends `:fed-invoke`
     `{:invoke-id (UUID) :agent-id <bare remote id> :prompt … :timeout-ms …}` down
     the uplink socket and parks on a promise in a per-connection pending map,
     resolved by `:fed-invoke-result` (mirror of `make-proxy-invoke-fn`, transport
     swapped). Timeout → typed SocialError, consistent with ws-invoke's.
   - Reply to each announce with `:fed-roster` (hub's exportable roster: locals +
     other-site proxies, minus the announcing site's own agents — reflection rule).
   - On socket close: mark that site's uplink proxies `:federation/stale? true`
     (do NOT prune immediately); prune after 3 missed announce intervals, reusing
     existing prune semantics. Re-announce clears stale.
   - Relay falls out: other HTTP peers (chi) sync lucy's roster and see oxf-*
     entries with peer-url = lucy, so their invokes chain through the hub's uplink
     invoke-fn. Verify opportunistically; not a blocking criterion for this slice.

**Frame shapes (add to `protocol.clj` `parse-ws-message` + renderers):**
`:fed-announce` (site, token, roster), `:fed-roster` (site, roster),
`:fed-invoke` (invoke-id, agent-id, prompt, timeout-ms),
`:fed-invoke-result` (invoke-id, ok, result, session-id, error).

**Files — :in (read first, minimal diff):** `src/futon3c/agency/federation.clj`,
`src/futon3c/agency/registry.clj`, `src/futon3c/transport/ws.clj`,
`src/futon3c/transport/ws/invoke.clj`, `src/futon3c/transport/protocol.clj`.
**:out:** `src/futon3c/agency/fed_uplink.clj` (new), edits to `transport/ws.clj` +
`transport/protocol.clj` (frames), `scripts/dev-laptop-env` + `scripts/dev-linode-env`
(FUTON3C_FED_UPLINK / FUTON3C_FED_TOKEN blocks, commented default off),
`test/futon3c/agency/fed_uplink_test.clj` (new).

**Test expectations (in-process, fake channels as in existing transport tests):**
1. Valid `:fed-announce` → qualified proxies registered (`oxf-codex-1` from bare
   `codex-1`), `:federation/transport :ws-uplink`; announce containing the hub's own
   agents → `:skipped-own-site` (reflection guard holds over uplink).
2. Invoke on an uplink proxy emits `:fed-invoke` with the BARE remote id; a
   `:fed-invoke-result` resolves it; no result within timeout → typed invoke-error.
3. Socket close → proxies stale, not pruned; re-announce → stale cleared; 3 missed
   intervals → pruned.
4. Bad/missing token → closed, nothing registered.
5. Gate: `env -u FUTON3C_SITE clojure -X:test` green (CP-E item 8 hygiene).

**Criteria checklist:**
- [ ] Laptop federates with the tunnel OFF (no 17070 listener anywhere).
- [ ] Rebooting either box requires zero human action: uplink reconnects within
      backoff, roster reappears on the next announce.
- [ ] `oxf=http://127.0.0.1:17070` peer entry removable from dev-linode-env once
      the uplink is live (leave the removal itself to a verified deploy step).
- [ ] Invariants hold: uplink routes to agents that already exist (I-2); no
      subprocess spawning in transport code (grep gate); locals stay bare (CP-D).

**CP-F build-dispatch datapoint (2026-07-13, while the build ran):** the first
cross-site build dispatch surfaced a hardening gap of its own. The bell to
`chi-codex-1` traversed lucy's HTTP proxy invoke-fn; BOTH hops (lucy→chi proxy
call, chi's local invoke job) timed out at exactly 600s while the codex process
kept working underneath (verified on-box by chi-claude-1: JVM-parented PID
alive, CPU advancing, CP-F files landing in a worktree). Fallout: the job
reported `failed`, and lucy's proxy row wedged at `:invoking` (cleared by hand
via Drawbridge; the remote registry had it `:idle` — a CP-E item 2 instance,
proxy state diverging from home-box truth). Lessons for this mission: (a) any
cross-site dispatch expected to exceed ~10min needs a completion-bell contract,
not a synchronous proxy wait — the CP-F uplink invoke path should carry the
caller's timeout end-to-end rather than compounding fixed per-hop caps; (b)
proxy `:invoking` should be leased/TTL'd or reconciled from the home box on
sync, never left to a response that may never arrive.
