# E-evidence-flows-everywhere

**Status:** seeded 2026-08-14, not being solved yet (Joe: "it isn't a critical
issue to solve now"). Attached to `M-dionysus-winddown`.

Seed notes only. Enough to stop the requirement being rediscovered, and to stop
someone building the wrong thing in a hurry.

---

## The requirement

> "in general yes to dual-write to Zone but we need dual-write to a failsafe,
> especially because Zone is going to become the main box when Dionysus goes
> away. So, broadly, evidence should actually flow everywhere." — Joe, 2026-08-14

Point-to-point is not enough, and picking a better partner does not fix it.
**Zone cannot be its own failsafe** once Zone is the main box, so any design
whose answer is "write to Zone" fails at exactly the moment it matters.

Underwritten by the FUTON tenet it serves: *the turns are the primary research
record; the evidence is the work.* A record that exists in one place is not a
record, it is a copy waiting to become an absence.

## Why the current mechanism cannot satisfy it

`futon3c.watcher.file-ingest/post-futon1b!` posts each hyperedge to **one**
secondary, `FUTON1B_URL`, best-effort (failures logged, never fatal), guarded
by `self-dual-write?` so it no-ops when the secondary equals the primary.

Single-target by construction. Not broken — just not the shape the requirement
needs.

## What today demonstrated (2026-08-14)

Three hosts, three unrelated ways of silently failing to carry the record. This
is the argument for fan-out: any single path is one config collapse from being
the only path, and none of these announced themselves.

| host | failure | how long | how discovered |
|---|---|---|---|
| Dionysus | `FUTON_SUBSTRATE_URL` and `FUTON1B_URL` both `127.0.0.1:7073`, so `self-dual-write?` no-ops every write | ~1 month | reading the guard |
| Dionysus | `futon1b-server.service` crash-looping, 291 restarts, ~4.5 min CPU each | ≥2 days | 1,633 unread alerts in a JSONL file |
| lucy | futon3c unstartable — `futon2.aif.memory-contract` exists only on a branch lucy wasn't on; survived only because its JVM had been up 13 days | unknown | stopping it |

## Design constraints

**Governed by `futon0/README-federate.md`.** This is the live-stream sibling of
the file-corpus sync already shipped (`scripts/evidence_mesh_sync.sh`), not a
replacement for it. Relevant invariants:

- **F-0 stable global identity + origin metadata.** Essential here: fan-out
  across N hosts creates A→B→A loops unless each event carries `:origin/site`
  and is not re-fanned by receivers. Get this wrong and the mesh amplifies.
- **F-4 idempotent replay, loud mismatch.** Same event arriving twice by two
  paths must be a no-op, not a duplicate.
- **F-7 replication is not backup.** Fan-out propagates bad writes as
  faithfully as good ones. A point-in-time recovery point is still owed.

**Do not push a fat client onto the laptop's successor.** There is no next
laptop; the client is a DeX phone over mosh/tmux. Whatever this becomes, it
runs server-side.

**Don't copy the mesh-sync design wholesale.** That works because a file corpus
is content-addressed against a manifest, so any peer can serve and correctness
is provable on arrival. A live event stream has no manifest and no closed set —
the properties that made pull-any-peer work there are not available here. The
*posture* transfers (verify on arrival, absence is an alert); the mechanism
does not.

## Sketch, not a decision

Smallest thing that could work: let `FUTON1B_URL` take a list, post to each,
keep the `self-dual-write?` guard per-target, keep failures non-fatal, and tag
each event with `:origin/site` so receivers do not re-fan. That is a small
change to one function and it would have survived every failure in the table
above — no single target's collapse silences the record.

What it does **not** give: ordering, resumability after a receiver outage, or
acknowledgement. `README-federate.md` §4 wants per-origin cursors and pull
replication for exactly those reasons. So the list is a floor, not the design.

## Open questions

1. Push-list or pull-with-cursors? The list is hours; cursors are the design
   the federate doc actually specifies.
2. What happens to writes made while a target is down — dropped (best-effort,
   as now) or queued? Dropping is honest and simple; queueing is what makes it
   a replication system rather than a broadcast.
3. Does the receiving side need to distinguish locally-originated evidence from
   replicated evidence when serving queries? (F-6 provenance survives merging.)
4. Where does the failsafe live once Dionysus is gone — chi, hyperreal, or
   something off-mesh? F-7 says a replica is not a recovery point, so the
   answer is probably "not another live node".
5. Does the interoceptive surface need a per-target limb, i.e. does
   `dual-write-disabled` become `target-N-unreachable`?

## Prior art in-tree

- `scripts/evidence_mesh_sync.sh` — manifest-driven, sha-verified, pull from
  any peer, systemd timer, receipts. The file-corpus analogue, working.
- `scripts/systemd/futon1b-vitality.py` — now carries `dual-write-disabled` and
  `evidence-write-stale` limbs, surfaced into `*agents*`.
- `futon0/README-federate.md` — the governing design.
- `holes/excursions/E-port-wiring-map.md` — which futon1b answers where, and
  why `:7074` is not `:7073`.
