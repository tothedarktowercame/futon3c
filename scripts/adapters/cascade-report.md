# cascade adapter report

Runner: `scripts/adapters/cascade_adapter.py`

Invariant check:

- Imports and calls `scripts/fill.py`; no local ArSE ask/answer implementation.
- Uses `kind=fill.KIND_CASCADE_FEED` for every call.
- Calls `fill(..., None)` for open mined holes, recording I2 honestly with no ArSE write.
- Deterministic selection: first 3 cascade-fed problem nodes, then first 2 unfed problem nodes, sorted by mission-triples filename.

- ArSE manifest before entity_count: 90
- ArSE manifest after entity_count: 93
- observed entity_count delta: 3
- filled holes posted: 3
- open holes recorded locally: 2
- ArSE writes expected from filled holes: 6 (ask + answer per `fill.fill()`)

| status | hole | hungry_for | filler / OPEN | thread-id | question evidence | answer evidence |
|---|---|---|---|---|---|---|
| FILLED | `E-mission-head:problem:cascade-problem` | `:payoff` | `baseline-cyber-ant` (`futon3/library/ants/baseline-cyber-ant.flexiarg`) | `ask-1781553359-90` | `arse-q-ask-1781553359-90` | `arse-a-ask-1781553359-90` |
| FILLED | `M-IRC-stability:problem:cascade-problem` | `:parse` | `loop-failure-signals` (`futon3/library/realtime/loop-failure-signals.flexiarg`) | `ask-1781553359-91` | `arse-q-ask-1781553359-91` | `arse-a-ask-1781553359-91` |
| FILLED | `M-P3-rational-reconstruction:problem:cascade-problem` | `:payoff` | `compose-independent-lemmas` (`futon3/library/math-strategy/compose-independent-lemmas.flexiarg`) | `ask-1781553359-92` | `arse-q-ask-1781553359-92` | `arse-a-ask-1781553359-92` |
| OPEN | `M-agency-forum:problem:cascade-problem` | `:parse` | OPEN | `` | `` | `` |
| OPEN | `M-agency-hardening:problem:cascade-problem` | `:parse` | OPEN | `` | `` | `` |
