---
name: futon3c-slice-memory-high-throttle
description: Agency "crawling" with no OOM is usually the futon3c-zone.service cgroup past memory.high=64GB; check memory.pressure, not the Agency logs
metadata:
  type: project
---

The Agency JVM, the `claude --print` seats, and the `codex-linux` processes all
live in one cgroup: `user.slice/user-1000.slice/user@1000.service/futon.slice/futon-services.slice/futon3c-zone.service`
(`memory.high` = 64 GB, `memory.max` = 80 GB). When any process in that slice
balloons past the high watermark, the kernel throttles *every task in the slice*
and forces direct reclaim.

Diagnosis, in order:
- `cat <cg>/memory.pressure` — `full avg10` is the number that matters. 96% means
  every task in the slice is stalled on memory 96% of wall-clock.
- `cat <cg>/memory.events` — a huge `high` count with `oom_kill 0` is the signature.
- Then rank processes: `for p in $(cat <cg>/cgroup.procs); do ...VmRSS... done | sort -rn`
- `/proc/<pid>/fd` names the file a runaway is slurping.
- `ss -ltn | grep 7070` is the fastest tell of all: `LISTEN 11 50` (non-zero
  Recv-Q) means the socket is up and queueing but nothing is accepting — the
  JVM is starved, not dead. A refused connection would mean it actually died.
- `cat /proc/<pid>/wchan` on the suspect reads `mem_cgroup_handle_over_high`,
  and its state is `D`. That names the throttled process without guessing.

**Why:** `memory.high` throttles instead of killing, so this never produces an OOM,
a crash, or anything in the Agency logs — only "everything is slow." Agency HTTP
timeouts and a wedged G1 (old gen pinned ~100%, heap unable to commit toward -Xmx,
`jcmd` unable to reach a safepoint) are downstream symptoms, not the fault.

**How to apply:** when Joe reports Agency crawling, read the cgroup's
`memory.pressure` before touching the JVM. Do NOT dispatch agents to investigate —
they are in the same cgroup and get throttled identically, which reads as a second
problem and wastes a round-trip. Fix by killing the memory hog; the throttle
releases in seconds. Seen 2026-08-31: two `p4ng/detect_drift.py` instances at
47 GB + 11 GB, each `read_bytes()`-ing the 16 GB
`storage/futon1a/default/proof-path.log.edn` referent with no size guard
(`detect_drift.py:143`). Seen again 2026-09-23: `futon6/scripts/normalize-math-prose.py` at 57 GB from
84 KB of input — `split_inline_math_dollar` scanned forever on an escaped `$`,
appending an empty part per pass. Fixed in that function plus the two prose
rules that manufactured the `\$`. Note the JVM needed **no restart** either
time: heap was 1.6 GB of 10.8 GB, no deadlock, and it drained its backlog by
itself seconds after the hog was killed. Check for a deadlock before assuming
the JVM is the problem — the 331% CPU that looks like spinning is `ps` showing
a lifetime average, not current load.

Related: [[futon1b-deep-health-can-oom-a-full-heap]],
[[one-jvm-per-repo-live-load-from-own-checkout]].
