# E-bg-check — verify durable bg-process after a JVM restart

**Owner:** claude-4 · **Date:** 2026-06-27 · **Relates to:** `futon3c.agency.bg-process`, `scripts/bg.py`, commit `e36c400`, futon3c CLAUDE.md "Durable background work".

**Purpose.** A 30-second smoke test to confirm the durable background-process facility is healthy in a freshly-restarted futon3c JVM, and that a launched process is parented to the JVM (so it will survive pouch teardown).

**Run this when:** Joe has just restarted the JVM. Hand it to any agent.

> ⚠️ **A JVM restart kills all bg-process tasks** — they are children of the JVM, which was their durable parent. Their in-memory registry is gone (their `/tmp/futon3c-bg/<id>.log` files remain). After a restart, **re-launch** any durable work you still need; old `bg-…` ids are dead.

## The check (copy-paste)

```bash
cd /home/joe/code/futon3c

# 1. Facility loads in the fresh JVM (require + list) — expect {:ok true …} and a vector
scripts/bg.py list

# 2. Launch a marker job; capture its id + pid
OUT=$(scripts/bg.py launch "echo bg-check-ok; sleep 3; echo bg-check-done" --label bg-check)
echo "$OUT"
ID=$(echo "$OUT" | grep -oE 'bg-[0-9]+-[0-9]+' | head -1)
BGPID=$(echo "$OUT" | grep -oE ':pid [0-9]+' | grep -oE '[0-9]+')

# 3. Durability property: the process's parent is the SERVING JVM, not a pouch
JVM=$(ss -ltnp 2>/dev/null | grep ':6768' | grep -oE 'pid=[0-9]+' | head -1 | cut -d= -f2)
echo "bg pid=$BGPID  parent=$(ps -o ppid= -p "$BGPID" | tr -d ' ')  JVM=$JVM   (parent must == JVM)"

# 4. Let it finish, then confirm clean exit + captured output
sleep 4
scripts/bg.py status "$ID"     # expect :status :exited, :exit 0
scripts/bg.py tail   "$ID" 5   # expect: bg-check-ok / bg-check-done

# 5. Cleanup
scripts/bg.py forget "$ID"; rm -f "/tmp/futon3c-bg/$ID.log"
```

## PASS criteria

1. **Step 1** prints `{:ok true …}` (the ns loads; no compile/classpath error).
2. **Step 2** returns a map with an `:id "bg-…"` and a numeric `:pid`, `:status :running`.
3. **Step 3** — `parent == JVM` (the spawned process is a direct child of the serving JVM; this is the whole point — it will survive pouch eviction).
4. **Step 4** — `status` shows `:status :exited`, `:exit 0`; `tail` shows both `bg-check-ok` and `bg-check-done` (output captured durably to the log).

Any deviation (step 1 errors, parent ≠ JVM, no captured output) means the facility is mis-wired in this JVM — report it; don't rely on bg-process for durable work until fixed.

## Notes

- Reachable equivalently over Drawbridge directly: `(futon3c.agency.bg-process/launch! {:cmd "…" :label "…"})`, `/status`, `/tail`, `/kill!`, `/forget!`.
- This checks the fresh JVM only. It does **not** recover pre-restart tasks (by design — v1 tracks tasks in-memory).
