# Pre-live War Machine rollback and readiness record — 2026-07-23

**Recorded:** 2026-07-23T22:19:31+01:00  
**Operator decision:** Joe authorised a fully live War Machine click using fresh
`zai-8` (worker) and `codex-6` (independent checker), after a recoverable source
snapshot. The repository scope is the operational Futon stack plus `p4ng` and
`apm-lean`; no other repositories are included.

## Source rollback surface

Every tracked source tree below was committed and pushed. At the preflight
check, each current branch was `0` ahead and `0` behind its upstream and had no
tracked changes.

| Repository | Branch | Pushed commit |
|---|---|---|
| futon0 | main | `4796804707bd6c4b092d1575ab81d40bae7367d0` |
| futon1b | master | `de37a95a5af073e09a91ad03b86732f61ab9069e` |
| futon2 | M-propagators-ant-gate | `edb50edc7bcc1146681212b951599ca3fd1ada5d` |
| futon3a | main | `dab70084de37c0a501670eb96485187e5f5f9983` |
| futon3b | main | `77feffc89c23a7586d649e128721aec46ae95961` |
| futon3c | master | this record's containing `prelive-wm-20260723T210844Z-ready` commit |
| futon4 | main | `5a42376fdda145adc10b45643c0474616d88aee7` |
| futon5 | M-propagators-2026-07-15 | `378913a7518c63e0d8ad3e7d4755fd9c156cd6fa` |
| futon5a | master | `b87525019ef0eb4abbc8177ca1ed713f81661064` |
| futon6 | master | `e7110a030e9c1169a3ee50bc0e70fa573f94c209` |
| futon7 | master | `f7717cbcaf956157c344d201ba33ee00e85b2799` |
| futon7a | master | `503c13c66d5701c2a93c60a34382e3f96290616a` |
| p4ng | master | `3ea830a8439b72cf9153851527df4f84c72c42f8` |
| apm-lean | master | `fe78c85d1a1b4267f6db16fda884ad403128e2df` |

For every repository except `p4ng`, the remote rollback branch
`prelive-wm-20260723T210844Z-final` names the first complete snapshot. A second
remote rollback branch, `prelive-wm-20260723T210844Z-ready`, names the
post-preflight state. Overleaf rejects non-`master` refs, so `p4ng` has no
remote rollback branch: its pushed `master` commit above is the remote anchor,
and a local annotated `prelive-wm-20260723T210844Z-final` tag resolves to it.

## Deliberately excluded runtime/generated files

These untracked files were not presented as source and were not committed:

- `futon1b`: `fts5-evidence.db`, `logs/`, `migration-export-full/`
- `futon5`: five generated paper PDFs under `holes/tech-notes/paper/`
- `futon5a`: eight `.history`, LaTeX-intermediate, and PDF paths under `essays/`
- `p4ng`: thirty LaTeX intermediates, PDFs, backups, logs, and `svg-inkscape/`
- `apm-lean`: `ApmCanaries/Current.lean.bak`

Git rollback protects source. It does **not** roll back the live XTDB stores,
agent transcripts, evidence events, or other append-only runtime state. The
click must therefore preserve outcome/provenance receipts and use compensating
retractions where a runtime write needs reversal; it must not imply that
`git reset` repairs data-plane effects.

## Preflight repair and validation

The initial `GET /api/alpha/war-machine` returned 503 because Futon3c's R14
projection still required the retired `futon2.aif.policy-precision` namespace.
Futon2 had deliberately renamed that engineering controller to
`futon2.aif.selection-gain` to avoid claiming variational policy precision.

Futon3c commit `145fea7d23dc3699ba53774aaefb24ff543bb492`
repairs the projection, reads `:selection-gain`, preserves outward `:gamma` for
client compatibility, and labels the controller honestly. Validation:

- focused test: 1 test, 6 assertions, zero failures;
- `clj-kondo`: zero errors, zero warnings;
- `check-parens`: OK;
- the whole HTTP namespace: 101 tests / 502 assertions, with seven known
  unrelated stale assertions (health evidence count, new minibuffer
  server-timestamps, and an evolved AIF-stack agenda).

The serving JVM was not restarted. The changed namespace was loaded through
Drawbridge. Its direct live projection returned
`:controller-kind :selection-gain`, gain `1.0`, zero outcome samples, and no
trace-read error.

The subsequent live endpoint check returned:

- HTTP `200`;
- caller wall time `4.428668s`;
- `31,248,775` response bytes;
- SHA-256
  `1d74b17f15e4bf03d68828a21b063e3c36e0a862a63fa4678e8108eeb3ca4675`;
- snapshot `as-of` `2026-07-23T21:16:20.159739365Z`;
- scheduler `:last-error nil`, `:error-count 0`.

At that snapshot the top admissible action was
`advance-mission M-expressions-of-interest`, followed by two pattern-authoring
sorries. This ranking is an input to operator confirmation, not autonomous
permission to enact it.

## Live-click control boundary

This is a confirm-to-enact run, not an advice-only run and not bounded
autonomy:

1. the War Machine produces the current recommendation and its reasons;
2. Joe confirms the concrete mission/action;
3. fresh `zai-8` performs normal live work inside that confirmed target;
4. fresh `codex-6` independently checks outcomes and records a separate
   witness;
5. any target change or expansion beyond the confirmed mission returns to Joe.

Stop conditions are: endpoint/scheduler degradation, repository drift outside
the confirmed target, failed independent evidence, an invariant conflict, or a
proposed data-plane operation whose recovery semantics are unknown.
