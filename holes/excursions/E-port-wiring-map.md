# E-port-wiring-map — which port is what, on which host

Opened 2026-08-14 (Joe: "different hosts are using different ports and it's
confusing!"). Written during `M-dionysus-winddown`, after the confusion cost
several wrong conclusions in a single afternoon.

All figures measured 2026-08-14. Where something is unknown it says so.

---

## The map

| host | site | :7070 | :7072 | :7073 | :7074 | :7075 | :17070 |
|---|---|---|---|---|---|---|---|
| Dionysus | oxf | futon3c Agency | futon1b | futon1b | — | — | *not local* |
| Zone | ams | futon3c Agency | listening | futon1b | — | — | — |
| lucy | lon | futon3c Agency | — | **nginx (HTTPS)** | futon1b | ? | tunnel endpoint |
| chicago | chi | futon3c Agency | listening | — | futon1b | — | — |

**futon3c Agency is always :7070.** That one is stable everywhere.

**futon1b is NOT stable.** `:7073` on Dionysus and Zone, `:7074` on lucy and
chicago. futon1b's own README records why: *"nginx owns :7073 there"* on lucy,
and the two genuine per-box knobs are `--store-dir` and `--port` — "lucy 7074,
chicago 7074, default elsewhere 7073".

### Store directories differ too, and the names mislead

| host | `--store-dir` | size |
|---|---|---|
| Dionysus | `migration-store-21` | **21 GB** |
| Zone | `migration-store-21` | (same *name*, different contents) |
| lucy | `switchover-store` | 4.6 GB, with a 215 MB `fts5-evidence.db` |
| chicago | `chicago-store` | — |

Dionysus and Zone use the *same directory name for different stores*. Nothing
about `migration-store-21` identifies which box it belongs to, and two boxes
having a store of that name does not make them replicas.

---

## Confusions this resolves

### 1. `:17070` is a tunnel, not a broken loopback

`FUTON3C_SELF_URL=http://127.0.0.1:17070` on Dionysus looks like a
misconfiguration — a loopback address advertised to remote peers, and nothing
listens on :17070 locally. It is not a bug. The systemd unit
`oxf-lucy-tunnel.service` is described as:

> futon3c oxf federation tunnel (laptop :7070 -> lucy :17070)

So `127.0.0.1:17070` is correct **from lucy's point of view**: lucy reaches
Dionysus's Agency through the reverse tunnel bound on lucy's own :17070. The
address is right; it is just not interpretable from the box that publishes it.

*(This corrects an earlier reading in the M-dionysus-winddown work, where the
loopback SELF_URL was called out as a defect.)*

### 2. `FUTON1A_URL` points at futon1b

On Dionysus: `FUTON1A_URL=http://127.0.0.1:7073`, `FUTON1A_PORT=0`. The
embedded futon1a is disabled and the name now points at the futon1b server —
an artifact of the 2026-07-12 futon1a→futon1b switchover, which kept the
variable name and changed its meaning.

The same switchover left `scripts/phase_5_signatures.clj` in futon3 defaulting
to the old `:7071`, so a weekly systemd timer was a connection-refused every
run until it was fixed on 2026-08-14. **When a port moves, grep for the old
number rather than for the service name.**

### 3. The evidence dual-write is disabled by a name collision

`FUTON_SUBSTRATE_URL` and `FUTON1B_URL` are both `http://127.0.0.1:7073`.
`futon3c.watcher.file-ingest/post-futon1b!` posts every hyperedge to the
secondary **only when it differs from the primary** — the `self-dual-write?`
guard added 2026-07-11 to stop double-posting into one store. With both
variables equal, the guard fires on every write and the dual-write leg is a
permanent no-op.

Consequence: evidence written on Dionysus goes to the local store and nowhere
else. The mechanism to send it elsewhere is built and tested; it is one
environment variable away from working. Full analysis: `M-dionysus-winddown`.

### 4. One futon1b process binds two ports

On Dionysus, pid 604285 owns **both** `:7072` and `:7073`, and both answer
`{:ok true, :node-open? true}`. Seeing two futon1b health responses on one host
does not mean two nodes.

---

## LIVE DEFECT — two futon1b services on one store

Measured 2026-08-14 16:46 on Dionysus. **Two systemd user units are both
active, both running futon1b against `migration-store-21`:**

```
c7-futon1b-dionysus.service   loaded active running   futon1b substrate :7073 (restored from captured argv)
futon1b-server.service        loaded active running   futon1b XTDB2 store server (E-futon1b-operational-switchover)
```

Two JVMs hold the store open — pid 604285 (since 2026-08-10) and pid 882387
(started 16:45:50, 381% CPU, 1.9 GB RSS). Both have an open fd into
`migration-store-21`.

futon1b's README states the invariant plainly:

> XTDB 2 local stores are **single-process**: while the server runs, no other
> JVM may open the same `--store-dir`.

The `c7-` unit describes itself as "restored from captured argv", i.e. some
restoration mechanism recreated a unit for a server that already had one. The
health endpoint still answers, so this is not currently visibly broken — but it
is a documented invariant violation against the 21 GB store that
`M-dionysus-winddown` exists to preserve.

**Not resolved here.** Deciding which unit is authoritative and stopping the
other is an operator call, and stopping the wrong one on a live store is worse
than the current state. Recorded so it is not discovered as a corruption later.

---

## How to read a host quickly

```bash
ss -ltn | awk '{print $4}' | grep -oE ':(7[0-9]{3}|17[0-9]{3})$' | sort -u
for p in 7070 7072 7073 7074; do
  printf ":%s " $p; curl -s -m 4 "http://127.0.0.1:$p/health"; echo
done
pgrep -af "futon1b|futon3c.dev"          # store-dir and port live in argv
systemctl --user list-units --type=service | grep futon
```

`{"uptime-seconds":…,"queue-hardening":…}` is the Agency.
`{:ok true, :deep false, :node-open? true}` is futon1b.

---

## Open

- lucy `:7075` serves HTML; unidentified.
- lucy `:7074` listens but does not answer `/health` over plain HTTP — likely
  XTDB pgwire rather than the futon1b HTTP server. The futon1b HTTP endpoint on
  lucy was not located, which is what blocked testing a restored dual-write.
- `:7072`'s role on Zone and chicago is unconfirmed; on Dionysus it is the same
  futon1b process as `:7073`.
