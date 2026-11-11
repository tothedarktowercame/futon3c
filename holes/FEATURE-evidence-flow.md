# [[Evidence Flow]] — feature, turned off

**State:** off (2026-11-11). One evidence store, futon1b on zone (:7073);
every agent on zone writes and reads it. Nothing flows between boxes except
the nightly off-box backups. Recorded by claude-13 on Joe's word, 2026-11-11,
when [E-evidence-flow](excursions/E-evidence-flow.md) closed.

## What the feature is

Evidence written on one Agency reaching another: duplicated, collated,
federated on read, or partitioned by concern (the four designs E-evidence-flow
named in July). It matters as soon as more than one Agency does real work.

## Why it is off, and why it is kept

Off because there is one working Agency. In July 2026 the laptop and lucy
each ran a store and they filled separately, while some readers looked at the
wrong one (E-evidence-flow). Since then the work moved to zone: lucy runs no
store (its :7073 is nginx), metameso's Agency is up but its store is empty
(`count 0`, 2026-11-11), and reads default to the local store (futon3c
`scripts/dev-laptop-env`, 2026-07-18).

Kept because it comes back with **federated agency**. Joe (2026-11-11):
"there's no specific reason that claude-cs would *have to* be on my box, in
principle Murch or Rob or some other collaborator could work with their own
Agency". Federated agency is one of the stronger candidates for co-working
and for bringing in collaborators and contributors; each collaborator's
Agency would write its own evidence, and the question of how those streams
meet returns. Joe: "For my current purposes it isn't relevant to work through
the various issues, notably b/c we have backups being made regularly."

## What exists, if it is turned on

- Bells already cross boxes: the zone JVM has `FUTON3C_PEERS=lon=…,chi=…`
  (cross-site bells need the area code). Routing across boxes is
  [M-federated-agency-hardening](missions/M-federated-agency-hardening.md)
  (OPEN: remote agents arrive as local phantoms).
- [README-federation.md](../README-federation.md) describes the July
  three-box mesh (laptop, lucy, metameso). Its box table is out of date:
  zone is now the working box and lucy's :7070 is gone.
- `xtdb_backend.clj` anticipated "replicated entries that arrive after a
  delay"; no replication was built.
- Off-box copies: `evidence-backup.service` (restic, nightly 03:30) to
  metameso and lucy, checked by `zone-health` `evidence-backup` (PASS,
  21 h, both hubs verified, 2026-11-11).
- Leftover: `dev-laptop-env` still defaults `FUTON2_WM_EMIT_BASE` to lucy's
  dead :7070; it only applies if a laptop JVM is started from that script.

## The question it raises

Do others work this way: collaborators each running their own agent hub,
with records that meet? Logged as a library question in futon7
`holes/argument-graph/questions.edn` (Q-joe-5), for a `bb daily-loop scan
--question` run.
