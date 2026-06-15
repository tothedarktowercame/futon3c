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
