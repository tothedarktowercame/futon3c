# Federation & the multi-box launcher

How the futon3c mesh runs across several machines, and how to bring a box up
consistently. Three boxes today; the design scales to N.

## The boxes

| Box | hostname | role | site | SSH alias | Agency |
|-----|----------|------|------|-----------|--------|
| Laptop | `Dionysus` | `laptop` | *(undecorated)* | — | local `:7070` |
| linode1 | `lucy` | `linode` | `lon` | `lucy-joe` (port 2222) | `:7070`, public via nginx `:7073→7070`; hosts IRC |
| linode2 | `metameso` | `linode2` | `chi` | `linode-joe` / `linode-chicago` | `:7070` |

The laptop is behind NAT; lucy is the public hub (IRC + nginx TLS). Agents
coordinate over the Agency HTTP API (bells/whistles), not IRC — so only lucy
runs an IRC daemon.

## One launcher, role-detected: `fdev`

There is **one** launcher script, not one-per-box. `~/bin/fdev` detects the
box's role and execs the matching profile:

```
fdev → role detection → fopsd (ensure Emacs daemon) → scripts/dev-<role>-env → make dev
```

Role is resolved in order: `--role <r>` flag › `$FUTON_ROLE` › `~/.futon-role`
file › hostname map (`Dionysus→laptop`, `lucy→linode`, `metameso→linode2`).

- `fdev --dry-run` prints the resolved plan without launching.
- `fdev --no-attach` provisions the tmux session without attaching (for headless
  / SSH bring-up).
- `fopsd` idempotently ensures the single `futon-ops` Emacs daemon is up first
  (so the server's blackboard `emacsclient` calls never block on a missing
  daemon). `FUTON_OPS_PROFILE` selects a chemacs profile (`graph` on the laptop)
  or, when empty, a plain `emacs --daemon` (the linodes have no chemacs).

`fdev` and `fopsd` live in `scripts/` (canonical) and are **copied into place**
on each box.

## Deploying to a box

```bash
# from a checkout of futon3c on the target box:
cp scripts/fdev scripts/fopsd ~/bin/ && chmod +x ~/bin/fdev ~/bin/fopsd
echo <role> > ~/.futon-role        # laptop | linode | linode2  (optional if hostname is mapped)
~/bin/fdev                          # detect role, launch, attach
```

A brand-new box also needs: the futon3c repo + its sibling `:local/root` deps
(`futon0 futon1 futon1a futon2 futon3a futon3b futon4 futon5`), a modern JDK
(21, to match the others), and `claude`/`codex` CLIs if it is to host an agent
(see `CLAUDE_BIN` below).

## The per-role profiles (`dev-<role>-env`)

Each profile is "bake in the role's env, run health checks, then `exec make
dev`". They differ only by what the box needs — features are modular and you can
relaunch with more later.

- **`dev-laptop-env`** — full dev box. Registers a local codex agent, opens the
  outbound WS bridge to lucy, starts file watchers + web sidecars.
- **`dev-linode-env`** (lucy / `linode`) — hub. `FUTON3C_SITE=lon`, registers
  `lon-claude-1`, runs the IRC relay. No built-in IRC (ngircd runs as a separate
  service); no WM-UI window.
- **`dev-linode2-env`** (metameso / `linode2`) — lean worker. `FUTON3C_SITE=chi`,
  registers `chi-claude-1`, federates to lucy (`FUTON3C_PEERS`). No IRC, no
  WM-UI, trimmed watchers.

## One JVM per box (CLAUDE.md I-0)

`pgrep -xc java` should be **1** on every box at rest. The futon3c JVM hosts
everything that serves (Agency, WebArxana, War Machine backends), and CLJS
watches run **embedded inside it** via `dev/futon3c/dev/shadow.clj`
(`#{:war-machine :webarxana}`, dev-http on `:8710` in-process) — *not* a
standalone `npx shadow-cljs`.

`fdev` therefore does **not** spawn a `wm-ui` shadow-cljs window by default
(that was a redundant second JVM). Opt in only if you really need a standalone
watcher: `FUTON_WM_UI_WINDOW=wm-ui fdev`. To rebind `:8710` to the embedded
server after killing a standalone, via Drawbridge:
`(do (futon3c.dev.shadow/stop!) (futon3c.dev.shadow/start! :war-machine :webarxana))`.

## Agent identity & federation

Agent ids are **site-qualified** so they are globally unique across the mesh:
`config/site-qualify` prefixes a box's locally-registered agent ids with
`FUTON3C_SITE` (lucy → `lon-claude-1`, metameso → `chi-claude-1`; the laptop is
undecorated → `claude-3`, `codex-1`). Foreign agents arriving via a bridge
(e.g. lucy's `codex-1`, which is the laptop's) stay undecorated.

Why it matters: a peer refuses to mirror an agent whose id is on the
*protected-continuity-id* list (`claude-1`, `claude-2`, `codex-1`,
`codex-vscode`) — that guard stops a remote proxy from squatting an
operator-facing local id. Plain `claude-1` therefore never crosses a node
boundary; `lon-claude-1` (unique, unprotected) mirrors freely. This is what lets
`lon-claude-1` / `chi-claude-1` show up as proxies on the other boxes.

**Federation reconciles only at boot** (`bootstrap.clj` runs `sync-peers!` once
+ a push-on-register hook, single attempt, no retry). So **bring the hub (lucy)
up first**, then workers. Until a periodic re-sync lands, refresh a stale
cross-node view with a manual `(futon3c.agency.federation/sync-peers!)`.

## Operational gotchas

- **`CLAUDE_BIN`** — on the linodes `claude` lives in `~/.local/bin`, which is
  *not* on the JVM's non-interactive launch PATH. The profiles set
  `CLAUDE_BIN=/home/joe/.local/bin/claude` so agents are invocable; without it
  an invoke fails `Cannot run program "claude"`.
- **Hard restart = SIGKILL.** Plain `pkill`/SIGTERM on the futon3c JVM can hang
  its shutdown (the boundary writer closes while a ticker keeps appending →
  endless `[boundary] I-single-boundary VIOLATION … — closing`), and the
  half-dead JVM keeps `fdev` from respawning. Use `pkill -9 -x java`, then `fdev`.
- **Don't `pkill -f "futon3c.dev"`** (or any pattern your kill command's own argv
  contains, e.g. `clojure`): `pkill -f` self-matches the shell running it and
  kills your SSH session. Match the process *name*: `pkill -x java`.
- **Reach lucy's Agency** over the nginx TLS proxy `https://<lucy>:7073`
  (→ localhost:7070), or an SSH tunnel; raw `:7070` is plain-HTTP. SSH to lucy is
  on port **2222** (via the `lucy-joe` alias).
