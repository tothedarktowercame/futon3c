# Traceability: M-operational-readiness

End-to-end evidence chain for the Operational Readiness mission.

```
Mission (M-operational-readiness.md)
  -> PSR (evidence: tag=psr, mission=M-operational-readiness)
    -> GitHub Issue (#N, label=codex)
      -> Codex Commit (SHA range in PUR)
        -> PUR (evidence: tag=pur, mission=M-operational-readiness)
          -> Test Evidence (clojure -X:test, count in checkpoint)
            -> Checkpoint (evidence: tag=checkpoint, mission=M-operational-readiness)
```

## Evidence Query Reference

All evidence for this mission is queryable via the Agency API:

```bash
# All PSRs (pattern selection records)
curl "http://localhost:7070/api/alpha/evidence?tag=psr"

# All PURs (pattern use records)
curl "http://localhost:7070/api/alpha/evidence?tag=pur"

# All checkpoints (gate progress snapshots)
curl "http://localhost:7070/api/alpha/evidence?tag=checkpoint"

# All autowake lifecycle events
curl "http://localhost:7070/api/alpha/evidence?tag=M-operational-readiness"

# Full mission session
curl "http://localhost:7070/api/alpha/evidence?session-id=autowake/M-operational-readiness"
```

## Link Chain

### #10 — ClosedChannelException cleanup

| Link | Artifact |
|------|----------|
| Mission | M-operational-readiness § Scope |
| PSR | evidence `e-6f3ba8d1` (pattern R11, context: test noise) |
| Issue | [#10](https://github.com/tothedarktowercame/futon3c/issues/10) |
| Commits | (pending) |
| PUR | (pending — emitted by autowake on completion) |
| Tests | (pending) |
| Checkpoint | (pending — emitted by autowake after cycle) |

### #16 — Tickle self-exclusion

| Link | Artifact |
|------|----------|
| Mission | M-operational-readiness § Scope |
| PSR | (pre-mission, no formal PSR) |
| Issue | [#16](https://github.com/tothedarktowercame/futon3c/issues/16) (closed) |
| Commits | `4d7f876` |
| PUR | (pre-Layer-2, no formal PUR) |
| Tests | 3 new tests (scan-activity-excludes-self-id, default-self-id, run-scan-cycle-excludes-self) |
| Checkpoint | (pre-Layer-4) |

### #17 — GET /api/alpha/agents/:id

| Link | Artifact |
|------|----------|
| Mission | M-operational-readiness § Scope |
| PSR | (pre-mission, no formal PSR) |
| Issue | [#17](https://github.com/tothedarktowercame/futon3c/issues/17) (closed) |
| Commits | `610c85a` |
| PUR | (pre-Layer-2, no formal PUR) |
| Tests | 2 new tests (agent-get-returns-registered-agent, agent-get-returns-404-for-unknown) |
| Checkpoint | (pre-Layer-4) |

### #18 — DELETE /api/alpha/agents/:id

| Link | Artifact |
|------|----------|
| Mission | M-operational-readiness § Scope |
| PSR | evidence `e-d16c51a4` (pattern R11, context: lifecycle incomplete) |
| Issue | [#18](https://github.com/tothedarktowercame/futon3c/issues/18) |
| Commits | (pending) |
| PUR | (pending — emitted by autowake on completion) |
| Tests | (pending — 4 tests specified in issue) |
| Checkpoint | (pending — emitted by autowake after cycle) |

### #19 — /health per-agent details

| Link | Artifact |
|------|----------|
| Mission | M-operational-readiness § Scope |
| PSR | evidence `e-c54ae926` (pattern R11, context: monitoring visibility) |
| Issue | [#19](https://github.com/tothedarktowercame/futon3c/issues/19) (closed) |
| Commits | `f3114b3` |
| PUR | (pre-Layer-2, completed before PUR emission was added) |
| Tests | 1+ new test |
| Checkpoint | (pre-Layer-4) |

### #20 — :query/author in EvidenceQuery

| Link | Artifact |
|------|----------|
| Mission | M-operational-readiness § Scope |
| PSR | evidence `e-1faabc85` (pattern R11, context: backend consistency) |
| Issue | [#20](https://github.com/tothedarktowercame/futon3c/issues/20) (closed) |
| Commits | `f3114b3` |
| PUR | (pre-Layer-2) |
| Tests | 2+ new tests |
| Checkpoint | (pre-Layer-4) |

### #21 — /health uptime + started-at

| Link | Artifact |
|------|----------|
| Mission | M-operational-readiness § Scope |
| PSR | evidence `e-480944d1` (pattern R11, context: restart detection) |
| Issue | [#21](https://github.com/tothedarktowercame/futon3c/issues/21) (closed) |
| Commits | `a06a38b` |
| PUR | (pre-Layer-2) |
| Tests | 1 new test |
| Checkpoint | (pre-Layer-4) |

### #22 — /health evidence count

| Link | Artifact |
|------|----------|
| Mission | M-operational-readiness § Scope |
| PSR | evidence `e-f32b2785` (pattern R11, context: evidence tracking) |
| Issue | [#22](https://github.com/tothedarktowercame/futon3c/issues/22) (closed) |
| Commits | `020d876` |
| PUR | (pre-Layer-2) |
| Tests | 1 new test |
| Checkpoint | (pre-Layer-4) |

## Bidirectional Navigation

**From an error in production** → find the agent, query evidence by author,
find the PUR for the issue that introduced the change, trace back through
commit → issue → PSR → mission goal.

**From a mission goal** → read the scope table, find each issue's PSR for
rationale, check the PUR for outcome, verify via checkpoint test count.

**From a pattern** → query `?tag=psr&tag=R11` to find all issues using R11,
check their PURs to see if the pattern worked as expected.
