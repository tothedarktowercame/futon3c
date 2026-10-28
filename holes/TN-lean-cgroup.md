# TN: Lean work charged to the futon3c cgroup throttles Agency (2026-10-28)

claude-11, at Joe's request ("maybe it is due to the APM lean work being
attributed to the fdev cgroup").

## What happened

- Afternoon of 2026-10-28: Joe noticed latency; futon1b was slow; from 19:32Z
  Agency's promise-history writer (single thread, queue 1024) was full, every
  `POST /park` and `GET /followups/ready` answered 500 (1,128 failures in half
  an hour), and the writer then completed about one task per few minutes,
  blocking in `Futon1bBackend._append` (`src/futon3c/evidence/futon1b_backend.clj:475`)
  while futon1b itself served evidence POSTs in ~40 ms.
- The futon3c service cgroup
  (`…/futon.slice/futon-services.slice/futon3c-zone.service`) stood at
  63.5 GiB against `memory.high` = 64 GiB, with 360,563 `high` events and no
  OOM kills. Breakdown: anon 19.9 GiB, file cache 35.6 GiB (active 26.6,
  inactive 9.0), kernel 8.0 GiB.
- The most recent background jobs in that cgroup were APM Lean verification
  sweeps, 17:02–17:28Z (`/tmp/futon3c-bg/bg-1793206605647-235.log`,
  `…-236.log`, `…-237.log`: per-problem `errors= sorryAx= axiomlines=`).
  `futon3c.agency.bg-process` / `scripts/bg.py` re-parent jobs to the serving
  JVM, so the jobs, and the mathlib build-file page cache they read, are
  charged to the futon3c cgroup. The cache stays charged after the jobs end.
  Whenever the cgroup crosses `memory.high`, the kernel throttles everything in
  it, Agency included.

## Relief applied (20:30Z, no restart)

    echo 20G > /sys/fs/cgroup/user.slice/user-1000.slice/user@1000.service/futon.slice/futon-services.slice/futon3c-zone.service/memory.reclaim

Evicts page cache only (process memory untouched; the cost is re-reading from
disk). Result: `memory.current` 63,544 → 38,891 MiB, active file
26.6 GiB → 34 MiB; the writer drained 1,024 → 0 queued (completed
2,553,098 → 2,554,484); Agency `/agents` in 1–2 ms. Use the same line if it
recurs.

## Related fixes the same afternoon

- futon3c `c3fbdc619`: a drain the full writer queue rejects is best-effort
  (`:outbox-rejected`), not a 500 to park/followups.
- futon3c `d65b33073`: at most one queued drain per state atom (each mutation
  had queued a full redundant drain of the outbox).

## Proposed (not done)

1. Run APM Lean jobs in their own cgroup, e.g. `systemd-run --user --scope -p
   MemoryHigh=… -p MemoryMax=…`, instead of as children of the serving JVM, so
   Lean's page cache is charged to Lean (owner: APM code, e.g.
   `src/futon3c/apm/bank_sweep.clj`, `library_lane_adapters.clj`, or the
   bg-process launcher).
2. An outer timeout guard on the append path's http-kit deref, as the GET
   path already has (`futon1b_backend.clj:226-237`), so a lost response cannot
   stall the writer.
3. Watch `memory.events` `high` for the futon3c cgroup; the same throttle
   stalled Agency before (memory note futon3c-slice-memory-high-throttle).
