# Authored giant-component corroboration

| input sha256 | futon3 HEAD | script sha256 | run date |
| --- | --- | --- | --- |
| 9992e2c62e41453d9ae33ba6c660b20195c91006759c84a3154af858436612e5 | 6899c80349a9851fb80e9e17763f0083552eeea9 | daf7b0629a39a442d6db3cf6a6bacd00a99a287c6cbe49be1e7bfc39c8474211 | 2026-10-19 |

The input SHA was verified before parsing. Only use-derived mined kinds (`co-cited`, `rejected-beside`, `next-in-session`, `used-together`) can corroborate; mined `why`/`how` edges are excluded to avoid circular evidence. The citation audit contains **475** distinct cited patterns.

## (a) Authored-edge classification

`edges.tsv` contains one row per authored edge whose endpoints lie in the authored giant, including directive, source file/set, authored line/sha/date, status, and use evidence turn ids.

| directive | total | corroborated | uncorroborated | no-evidence |
| --- | ---: | ---: | ---: | ---: |
| references | 1433 | 49 | 391 | 993 |
| see-also | 305 | 6 | 61 | 238 |
| why | 274 | 1 | 67 | 206 |
| how | 90 | 3 | 9 | 78 |

Unresolved prose/reference tokens were excluded: references=3682, see-also=0, why=7, how=56.

## (b) Giant sizes

| graph | giant size |
| --- | ---: |
| all authored directives | 793 |
| corroborated authored edges only (same authored giant node universe) | 7 |
| references only | 550 |
| see-also only | 122 |
| why only | 257 |
| how only | 26 |

## (c) Top 20 hubs

### `stack-coherence/evidence-ledger` — degree 40; uncorroborated/no-evidence degree 40

| edge | directive | status | set | authored date | authored sha |
| --- | --- | --- | --- | --- | --- |
| `ai4ci/ai-core-function` — `stack-coherence/evidence-ledger` | references | no-evidence | ai4ci | 2026-01-02 | `7941bdaf320bc23cd9bcc50c1c1524a3d26c0025` |
| `ai4ci/collective-intelligence-framing` — `stack-coherence/evidence-ledger` | references | no-evidence | ai4ci | 2025-12-31 | `521967f5c4959f67f858ce422c63cc8feed351bc` |
| `ai4ci/funding-justification` — `stack-coherence/evidence-ledger` | references | no-evidence | ai4ci | 2025-12-31 | `521967f5c4959f67f858ce422c63cc8feed351bc` |
| `ai4ci/project-summary-core` — `stack-coherence/evidence-ledger` | references | no-evidence | ai4ci | 2026-01-02 | `7941bdaf320bc23cd9bcc50c1c1524a3d26c0025` |
| `code-coherence/dead-code-hygiene` — `stack-coherence/evidence-ledger` | references | uncorroborated | code-coherence | 2025-12-31 | `521967f5c4959f67f858ce422c63cc8feed351bc` |
| `devmap-coherence/devmap-scope-discipline` — `stack-coherence/evidence-ledger` | references | no-evidence | devmap-coherence | 2025-12-29 | `07e25277ee94678cb05e051ab698d13f13226f9a` |
| `devmap-coherence/ifr-state-convergence` — `stack-coherence/evidence-ledger` | references | no-evidence | devmap-coherence | 2025-12-29 | `07e25277ee94678cb05e051ab698d13f13226f9a` |
| `enrichment/ARGUMENT` — `stack-coherence/evidence-ledger` | references | no-evidence | enrichment | 2026-03-06 | `ff42e6b668ca71a86910bda0bc9b05984d17ce14` |
| `enrichment/extend-not-rewrite` — `stack-coherence/evidence-ledger` | references | uncorroborated | enrichment | 2026-03-06 | `ff42e6b668ca71a86910bda0bc9b05984d17ce14` |
| `enrichment/layer-as-evidence` — `stack-coherence/evidence-ledger` | references | no-evidence | enrichment | 2026-03-06 | `ff42e6b668ca71a86910bda0bc9b05984d17ce14` |
| `futon-theory/rapid-debugging` — `stack-coherence/evidence-ledger` | references | uncorroborated | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `library-coherence/library-evidence-ledger` — `stack-coherence/evidence-ledger` | references | no-evidence | library-coherence | 2025-12-31 | `521967f5c4959f67f858ce422c63cc8feed351bc` |
| `meta/scaffolding-emergent-clarity` — `stack-coherence/evidence-ledger` | references | uncorroborated | meta | 2025-12-31 | `521967f5c4959f67f858ce422c63cc8feed351bc` |
| `stack-coherence/evidence-ledger` — `devmap-coherence/ifr-state-convergence` | references | no-evidence | stack-coherence | 2026-01-25 | `92850929e8dafc9352fc89dc8cdf62fadced8ff2` |
| `stack-coherence/evidence-ledger` — `devmap-coherence/prototype-maturity-lifecycle` | references | uncorroborated | stack-coherence | 2026-01-25 | `92850929e8dafc9352fc89dc8cdf62fadced8ff2` |
| `stack-coherence/evidence-ledger` — `stack-coherence/ready-blocked-triage` | references | uncorroborated | stack-coherence | 2026-01-25 | `92850929e8dafc9352fc89dc8cdf62fadced8ff2` |
| `stack-coherence/futon1-determinism` — `stack-coherence/evidence-ledger` | references | no-evidence | stack-coherence | 2026-01-25 | `92850929e8dafc9352fc89dc8cdf62fadced8ff2` |
| `stack-coherence/maturity-evidence-audit` — `stack-coherence/evidence-ledger` | references | no-evidence | stack-coherence | 2026-01-25 | `92850929e8dafc9352fc89dc8cdf62fadced8ff2` |
| `stack-coherence/readme-devmap-sync` — `stack-coherence/evidence-ledger` | references | no-evidence | stack-coherence | 2026-01-25 | `92850929e8dafc9352fc89dc8cdf62fadced8ff2` |
| `stack-coherence/ready-blocked-triage` — `stack-coherence/evidence-ledger` | references | uncorroborated | stack-coherence | 2026-01-25 | `92850929e8dafc9352fc89dc8cdf62fadced8ff2` |
| `stack-coherence/stack-blocker-detection` — `stack-coherence/evidence-ledger` | references | uncorroborated | stack-coherence | 2026-01-25 | `92850929e8dafc9352fc89dc8cdf62fadced8ff2` |
| `storage/all-or-nothing-startup` — `stack-coherence/evidence-ledger` | references | no-evidence | storage | 2026-02-07 | `ee9a5e785db2a5ed37986853a1765adcd753cfe6` |
| `storage/canonical-interface` — `stack-coherence/evidence-ledger` | references | uncorroborated | storage | 2026-02-07 | `ee9a5e785db2a5ed37986853a1765adcd753cfe6` |
| `storage/determinism-vs-expansion` — `stack-coherence/evidence-ledger` | references | no-evidence | storage | 2026-02-07 | `ee9a5e785db2a5ed37986853a1765adcd753cfe6` |
| `storage/deterministic-ingest-pipeline` — `stack-coherence/evidence-ledger` | references | uncorroborated | storage | 2026-02-07 | `ee9a5e785db2a5ed37986853a1765adcd753cfe6` |
| `storage/deterministic-substrate` — `stack-coherence/evidence-ledger` | references | no-evidence | storage | 2026-02-07 | `ee9a5e785db2a5ed37986853a1765adcd753cfe6` |
| `storage/durability-first` — `stack-coherence/evidence-ledger` | references | uncorroborated | storage | 2026-02-07 | `ee9a5e785db2a5ed37986853a1765adcd753cfe6` |
| `storage/durability-throughput-gate` — `stack-coherence/evidence-ledger` | references | no-evidence | storage | 2026-02-07 | `ee9a5e785db2a5ed37986853a1765adcd753cfe6` |
| `storage/error-layer-hierarchy` — `stack-coherence/evidence-ledger` | references | no-evidence | storage | 2026-02-07 | `ee9a5e785db2a5ed37986853a1765adcd753cfe6` |
| `storage/graph-memory-contract` — `stack-coherence/evidence-ledger` | references | uncorroborated | storage | 2026-02-07 | `ee9a5e785db2a5ed37986853a1765adcd753cfe6` |
| `storage/guardrails-vs-tooling` — `stack-coherence/evidence-ledger` | references | no-evidence | storage | 2026-02-07 | `ee9a5e785db2a5ed37986853a1765adcd753cfe6` |
| `storage/identity-flex-uniqueness` — `stack-coherence/evidence-ledger` | references | no-evidence | storage | 2026-02-07 | `ee9a5e785db2a5ed37986853a1765adcd753cfe6` |
| `storage/identity-uniqueness` — `stack-coherence/evidence-ledger` | references | no-evidence | storage | 2026-02-07 | `ee9a5e785db2a5ed37986853a1765adcd753cfe6` |
| `storage/invariants-vs-repair` — `stack-coherence/evidence-ledger` | references | no-evidence | storage | 2026-02-07 | `ee9a5e785db2a5ed37986853a1765adcd753cfe6` |
| `storage/open-world-continuity` — `stack-coherence/evidence-ledger` | references | no-evidence | storage | 2026-02-07 | `ee9a5e785db2a5ed37986853a1765adcd753cfe6` |
| `storage/open-world-velocity-validation` — `stack-coherence/evidence-ledger` | references | no-evidence | storage | 2026-02-07 | `ee9a5e785db2a5ed37986853a1765adcd753cfe6` |
| `storage/persistence-speed-mirroring` — `stack-coherence/evidence-ledger` | references | no-evidence | storage | 2026-02-07 | `ee9a5e785db2a5ed37986853a1765adcd753cfe6` |
| `storage/postcommit-materialization-gate` — `stack-coherence/evidence-ledger` | references | no-evidence | storage | 2026-02-08 | `c60985d971db09561747151b4731681373609c79` |
| `storage/rapid-debugging` — `stack-coherence/evidence-ledger` | references | no-evidence | storage | 2026-02-07 | `ee9a5e785db2a5ed37986853a1765adcd753cfe6` |
| `storage/reproducible-mirroring` — `stack-coherence/evidence-ledger` | references | no-evidence | storage | 2026-02-07 | `ee9a5e785db2a5ed37986853a1765adcd753cfe6` |
| `storage/schema-evolution-stability` — `stack-coherence/evidence-ledger` | references | no-evidence | storage | 2026-02-07 | `ee9a5e785db2a5ed37986853a1765adcd753cfe6` |
| `storage/startup-integrity-gate` — `stack-coherence/evidence-ledger` | references | no-evidence | storage | 2026-02-07 | `ee9a5e785db2a5ed37986853a1765adcd753cfe6` |

### `futon-theory/proof-path` — degree 29; uncorroborated/no-evidence degree 29

| edge | directive | status | set | authored date | authored sha |
| --- | --- | --- | --- | --- | --- |
| `coordination/artifact-registration` — `futon-theory/proof-path` | references | no-evidence | coordination | 2026-05-03 | `fb533ce9e56b42f326259a029bd072f128ee52c0` |
| `coordination/assignment-binding` — `futon-theory/proof-path` | references | no-evidence | coordination | 2026-05-03 | `fb533ce9e56b42f326259a029bd072f128ee52c0` |
| `coordination/mandatory-psr` — `futon-theory/proof-path` | references | no-evidence | coordination | 2026-05-03 | `fb533ce9e56b42f326259a029bd072f128ee52c0` |
| `coordination/mandatory-pur` — `futon-theory/proof-path` | references | no-evidence | coordination | 2026-05-03 | `fb533ce9e56b42f326259a029bd072f128ee52c0` |
| `coordination/par-as-obligation` — `futon-theory/proof-path` | references | no-evidence | coordination | 2026-05-03 | `fb533ce9e56b42f326259a029bd072f128ee52c0` |
| `coordination/pattern-search-protocol` — `futon-theory/proof-path` | references | no-evidence | coordination | 2026-05-03 | `fb533ce9e56b42f326259a029bd072f128ee52c0` |
| `coordination/session-durability-check` — `futon-theory/proof-path` | references | no-evidence | coordination | 2026-05-03 | `fb533ce9e56b42f326259a029bd072f128ee52c0` |
| `cycle-machine/single-producer` — `futon-theory/proof-path` | see-also | no-evidence | cycle-machine | 2026-08-25 | `902d6cce6878c59a85513887dbe0252cafce0b48` |
| `futon-theory/agent-contract` — `futon-theory/proof-path` | references | no-evidence | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `futon-theory/baldwin-cycle` — `futon-theory/proof-path` | references | no-evidence | futon-theory | 2026-08-15 | `cbd639917df7a573c84975fd2b5223067fc5e3b5` |
| `futon-theory/curry-howard-operational` — `futon-theory/proof-path` | references | no-evidence | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `futon-theory/durability-first` — `futon-theory/proof-path` | references | no-evidence | futon-theory | 2026-02-07 | `08894ea22c60c1184ee87c4a53622c1c64da7b0d` |
| `futon-theory/event-protocol` — `futon-theory/proof-path` | references | no-evidence | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `futon-theory/four-types` — `futon-theory/proof-path` | references | no-evidence | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `futon-theory/futonic-logic` — `futon-theory/proof-path` | references | no-evidence | futon-theory | 2026-02-10 | `9cfd7a7016b495efab1538e69feb3f46920e6a2a` |
| `futon-theory/minimum-viable-events` — `futon-theory/proof-path` | references | no-evidence | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `futon-theory/proof-path` — `futon-theory/event-protocol` | references | no-evidence | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `futon-theory/proof-path` — `futon-theory/retrospective-stability` | references | no-evidence | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `futon-theory/proof-path` — `futon-theory/symbolic-geodesic` | references | no-evidence | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `futon-theory/rapid-debugging` — `futon-theory/proof-path` | references | no-evidence | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `futon-theory/retrospective-stability` — `futon-theory/proof-path` | references | no-evidence | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `futon-theory/symbolic-geodesic` — `futon-theory/proof-path` | references | no-evidence | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `futon-theory/task-as-arrow` — `futon-theory/proof-path` | references | no-evidence | futon-theory | 2026-02-08 | `f54574b702605340b326be6621ea69e540bce333` |
| `futon-theory/the-woven-form` — `futon-theory/proof-path` | references | no-evidence | futon-theory | 2026-07-10 | `fb51b6256aaca74acf5b84f1d3c15f2c09bfbe79` |
| `futon-theory/wyrd` — `futon-theory/proof-path` | references | no-evidence | futon-theory | 2026-05-27 | `db9d02a09db94d58fb7a761051c3d6fa6ec57530` |
| `orchestration/recorded-handoff` — `futon-theory/proof-path` | references | no-evidence | orchestration | 2026-06-11 | `860a5595a58d62866b1013715934ae23934cae49` |
| `social/ARGUMENT` — `futon-theory/proof-path` | references | no-evidence | social | 2026-02-10 | `6739b149e40ce0d613d09b5d0eb347d211e67113` |
| `social/derive-the-contract` — `futon-theory/proof-path` | references | no-evidence | social | 2026-06-10 | `7ebd00b26ae7caca8b7d13deb98a06f25dcfa235` |
| `social/scope-bounded-handoff` — `futon-theory/proof-path` | references | no-evidence | social | 2026-02-10 | `6739b149e40ce0d613d09b5d0eb347d211e67113` |
| `social/tension-before-code` — `futon-theory/proof-path` | references | no-evidence | social | 2026-02-09 | `193be6ef0edc2dcd404b030ae4e83a9237f4fa3d` |
| `social/verify-before-compose` — `futon-theory/proof-path` | references | no-evidence | social | 2026-02-09 | `193be6ef0edc2dcd404b030ae4e83a9237f4fa3d` |
| `structure/block-as-futonic-revolution` — `futon-theory/proof-path` | references | no-evidence | structure | 2026-05-03 | `bb07cfab71163e1d526843d4799e29dae1084d5c` |

### `problems/devmap-self-consistency-standards` — degree 20; uncorroborated/no-evidence degree 20

| edge | directive | status | set | authored date | authored sha |
| --- | --- | --- | --- | --- | --- |
| `devmap-coherence/baseline-freeze` — `problems/devmap-self-consistency-standards` | why | no-evidence | devmap-coherence | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `devmap-coherence/devmap-scope-discipline` — `problems/devmap-self-consistency-standards` | why | no-evidence | devmap-coherence | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `devmap-coherence/ifr-f0-sati` — `problems/devmap-self-consistency-standards` | why | no-evidence | devmap-coherence | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `devmap-coherence/ifr-f1-dhammavicaya` — `problems/devmap-self-consistency-standards` | why | no-evidence | devmap-coherence | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `devmap-coherence/ifr-f2-viriya` — `problems/devmap-self-consistency-standards` | why | no-evidence | devmap-coherence | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `devmap-coherence/ifr-f3-piti` — `problems/devmap-self-consistency-standards` | why | no-evidence | devmap-coherence | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `devmap-coherence/ifr-f3a-piti-audit` — `problems/devmap-self-consistency-standards` | why | no-evidence | devmap-coherence | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `devmap-coherence/ifr-f4-passaddhi` — `problems/devmap-self-consistency-standards` | why | uncorroborated | devmap-coherence | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `devmap-coherence/ifr-f5-samadhi` — `problems/devmap-self-consistency-standards` | why | no-evidence | devmap-coherence | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `devmap-coherence/ifr-f6-upekkha` — `problems/devmap-self-consistency-standards` | why | no-evidence | devmap-coherence | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `devmap-coherence/ifr-f7-upa-upekkha` — `problems/devmap-self-consistency-standards` | why | no-evidence | devmap-coherence | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `devmap-coherence/ifr-state-convergence` — `problems/devmap-self-consistency-standards` | why | no-evidence | devmap-coherence | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `devmap-coherence/next-steps-to-done` — `problems/devmap-self-consistency-standards` | why | uncorroborated | devmap-coherence | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `devmap-coherence/prototype-alignment-bridge` — `problems/devmap-self-consistency-standards` | why | uncorroborated | devmap-coherence | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `devmap-coherence/prototype-alignment-embedding` — `problems/devmap-self-consistency-standards` | why | uncorroborated | devmap-coherence | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `devmap-coherence/prototype-alignment-role` — `problems/devmap-self-consistency-standards` | why | no-evidence | devmap-coherence | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `devmap-coherence/prototype-alignment-tension` — `problems/devmap-self-consistency-standards` | why | no-evidence | devmap-coherence | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `devmap-coherence/prototype-maturity-lifecycle` — `problems/devmap-self-consistency-standards` | why | uncorroborated | devmap-coherence | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `devmap-coherence/prototype-structure-checklist` — `problems/devmap-self-consistency-standards` | why | uncorroborated | devmap-coherence | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `devmap-coherence/status-keywords` — `problems/devmap-self-consistency-standards` | why | no-evidence | devmap-coherence | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |

### `futon-theory/structural-tension-as-observation` — degree 19; uncorroborated/no-evidence degree 19

| edge | directive | status | set | authored date | authored sha |
| --- | --- | --- | --- | --- | --- |
| `aif/admissibility` — `futon-theory/structural-tension-as-observation` | references | no-evidence | aif | 2026-06-08 | `933061cd93db585250d47caf03b8d6f9899ca086` |
| `aif/niche-construction` — `futon-theory/structural-tension-as-observation` | references | uncorroborated | aif | 2026-05-27 | `81a076ec45c6b81d2771b337323373f1e83c2d47` |
| `futon-theory/futonic-logic` — `futon-theory/structural-tension-as-observation` | references | no-evidence | futon-theory | 2026-02-10 | `9cfd7a7016b495efab1538e69feb3f46920e6a2a` |
| `futon-theory/reverse-morphogenesis` — `futon-theory/structural-tension-as-observation` | references | uncorroborated | futon-theory | 2026-02-11 | `702f7e95eb52c9bbdbec255d094b42e457923535` |
| `futon-theory/structural-tension-as-observation` — `coordination/ARGUMENT` | references | uncorroborated | futon-theory | 2026-02-08 | `f54574b702605340b326be6621ea69e540bce333` |
| `futon-theory/structural-tension-as-observation` — `futon-theory/baldwin-cycle` | references | no-evidence | futon-theory | 2026-02-08 | `f54574b702605340b326be6621ea69e540bce333` |
| `futon-theory/structural-tension-as-observation` — `futon-theory/four-types` | references | no-evidence | futon-theory | 2026-02-08 | `f54574b702605340b326be6621ea69e540bce333` |
| `futon-theory/structural-tension-as-observation` — `futon-theory/retroactive-canonicalization` | references | uncorroborated | futon-theory | 2026-02-08 | `f54574b702605340b326be6621ea69e540bce333` |
| `futon-theory/structural-tension-as-observation` — `futon-theory/task-as-arrow` | references | no-evidence | futon-theory | 2026-02-08 | `f54574b702605340b326be6621ea69e540bce333` |
| `futon-theory/symbolic-individuation` — `futon-theory/structural-tension-as-observation` | references | no-evidence | futon-theory | 2026-02-09 | `213504280720aeb5dda26920111489eeedbd03d6` |
| `futon-theory/the-woven-form` — `futon-theory/structural-tension-as-observation` | references | no-evidence | futon-theory | 2026-07-10 | `fb51b6256aaca74acf5b84f1d3c15f2c09bfbe79` |
| `futon-theory/wyrd` — `futon-theory/structural-tension-as-observation` | references | no-evidence | futon-theory | 2026-05-27 | `db9d02a09db94d58fb7a761051c3d6fa6ec57530` |
| `mission-coherence/logic-model-before-code` — `futon-theory/structural-tension-as-observation` | references | uncorroborated | mission-coherence | 2026-06-12 | `9d73323b453cd3af36dc480ddea3fb97a6d3bbc7` |
| `mmca/verified-but-idle-link` — `futon-theory/structural-tension-as-observation` | references | uncorroborated | mmca | 2026-08-07 | `d5af0a66cd106be9f541566a3ed25d0832d34178` |
| `orchestration/pattern-warranted-choice-point` — `futon-theory/structural-tension-as-observation` | references | uncorroborated | orchestration | 2026-06-08 | `933061cd93db585250d47caf03b8d6f9899ca086` |
| `social/ARGUMENT` — `futon-theory/structural-tension-as-observation` | references | no-evidence | social | 2026-02-10 | `6739b149e40ce0d613d09b5d0eb347d211e67113` |
| `social/tension-before-code` — `futon-theory/structural-tension-as-observation` | references | uncorroborated | social | 2026-02-09 | `193be6ef0edc2dcd404b030ae4e83a9237f4fa3d` |
| `structure/cook-ting` — `futon-theory/structural-tension-as-observation` | references | no-evidence | structure | 2026-05-03 | `f7ea5537a64698b0f71b64a748562d9cf8a002ee` |
| `structure/hinge-point` — `futon-theory/structural-tension-as-observation` | references | no-evidence | structure | 2026-05-03 | `f7ea5537a64698b0f71b64a748562d9cf8a002ee` |

### `problems/snatch-play-theory-gaps` — degree 19; uncorroborated/no-evidence degree 19

| edge | directive | status | set | authored date | authored sha |
| --- | --- | --- | --- | --- | --- |
| `snatch/an-unmodelled-response-stops-the-line` — `problems/snatch-play-theory-gaps` | why | no-evidence | snatch | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `snatch/consult-the-remedy-before-exiting` — `problems/snatch-play-theory-gaps` | why | no-evidence | snatch | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `snatch/escalate-only-as-far-as-you-can-lose` — `problems/snatch-play-theory-gaps` | why | no-evidence | snatch | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `snatch/exchange-when-both-sides-gain` — `problems/snatch-play-theory-gaps` | why | no-evidence | snatch | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `snatch/forced-play-needs-a-loss-floor` — `problems/snatch-play-theory-gaps` | why | no-evidence | snatch | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `snatch/grim-cuts-the-cascade-and-never-widens-it` — `problems/snatch-play-theory-gaps` | why | no-evidence | snatch | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `snatch/have-a-temperament` — `problems/snatch-play-theory-gaps` | why | no-evidence | snatch | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `snatch/institutions-vary-by-position-and-force` — `problems/snatch-play-theory-gaps` | why | no-evidence | snatch | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `snatch/lead-with-the-exchange-rule` — `problems/snatch-play-theory-gaps` | why | no-evidence | snatch | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `snatch/mark-without-force` — `problems/snatch-play-theory-gaps` | why | no-evidence | snatch | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `snatch/non-binding-talk-still-moves-play` — `problems/snatch-play-theory-gaps` | why | no-evidence | snatch | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `snatch/play-the-authored-order-first` — `problems/snatch-play-theory-gaps` | why | no-evidence | snatch | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `snatch/preserve-the-right-to-abstain` — `problems/snatch-play-theory-gaps` | why | no-evidence | snatch | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `snatch/probe-before-committing` — `problems/snatch-play-theory-gaps` | why | no-evidence | snatch | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `snatch/promote-the-remedy-before-the-exit` — `problems/snatch-play-theory-gaps` | why | no-evidence | snatch | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `snatch/protect-the-unprotected-move` — `problems/snatch-play-theory-gaps` | why | no-evidence | snatch | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `snatch/re-enter-after-observed-repair` — `problems/snatch-play-theory-gaps` | why | no-evidence | snatch | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `snatch/revert-then-invert` — `problems/snatch-play-theory-gaps` | why | no-evidence | snatch | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `snatch/widen-the-cascade-only-on-evidence` — `problems/snatch-play-theory-gaps` | why | no-evidence | snatch | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |

### `futon-theory/stop-the-line` — degree 18; uncorroborated/no-evidence degree 18

| edge | directive | status | set | authored date | authored sha |
| --- | --- | --- | --- | --- | --- |
| `coordination/artifact-registration` — `futon-theory/stop-the-line` | references | uncorroborated | coordination | 2026-05-03 | `fb533ce9e56b42f326259a029bd072f128ee52c0` |
| `coordination/assignment-binding` — `futon-theory/stop-the-line` | references | uncorroborated | coordination | 2026-05-03 | `fb533ce9e56b42f326259a029bd072f128ee52c0` |
| `coordination/bounded-execution` — `futon-theory/stop-the-line` | references | uncorroborated | coordination | 2026-05-03 | `fb533ce9e56b42f326259a029bd072f128ee52c0` |
| `coordination/capability-gate` — `futon-theory/stop-the-line` | references | no-evidence | coordination | 2026-05-03 | `fb533ce9e56b42f326259a029bd072f128ee52c0` |
| `coordination/intent-to-mission-binding` — `futon-theory/stop-the-line` | references | uncorroborated | coordination | 2026-05-03 | `fb533ce9e56b42f326259a029bd072f128ee52c0` |
| `coordination/mandatory-psr` — `futon-theory/stop-the-line` | references | no-evidence | coordination | 2026-05-03 | `fb533ce9e56b42f326259a029bd072f128ee52c0` |
| `coordination/mandatory-pur` — `futon-theory/stop-the-line` | references | no-evidence | coordination | 2026-05-03 | `fb533ce9e56b42f326259a029bd072f128ee52c0` |
| `coordination/par-as-obligation` — `futon-theory/stop-the-line` | references | no-evidence | coordination | 2026-05-03 | `fb533ce9e56b42f326259a029bd072f128ee52c0` |
| `coordination/task-shape-validation` — `futon-theory/stop-the-line` | references | uncorroborated | coordination | 2026-05-03 | `fb533ce9e56b42f326259a029bd072f128ee52c0` |
| `futon-theory/agent-contract` — `futon-theory/stop-the-line` | references | uncorroborated | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `futon-theory/all-or-nothing` — `futon-theory/stop-the-line` | references | uncorroborated | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `futon-theory/counter-ratchet` — `futon-theory/stop-the-line` | references | no-evidence | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `futon-theory/event-protocol` — `futon-theory/stop-the-line` | references | uncorroborated | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `futon-theory/local-gain-persistence` — `futon-theory/stop-the-line` | references | no-evidence | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `futon-theory/stop-the-line` — `futon-theory/agent-contract` | references | uncorroborated | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `futon-theory/stop-the-line` — `futon-theory/counter-ratchet` | references | no-evidence | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `futon-theory/stop-the-line` — `futon-theory/error-hierarchy` | references | uncorroborated | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `parsing/audit-the-route-not-only-the-repairs` — `futon-theory/stop-the-line` | see-also | no-evidence | parsing | 2026-10-11 | `31f78e4b34a4bfde53cd92e331f041d145d46c5b` |
| `peripherals/read-existing-seam-before-implementing` — `futon-theory/stop-the-line` | references | uncorroborated | peripherals | 2026-05-27 | `f15b91d40a268c3965f883867d5e5a065243415d` |
| `social/tension-before-code` — `futon-theory/stop-the-line` | references | uncorroborated | social | 2026-02-09 | `193be6ef0edc2dcd404b030ae4e83a9237f4fa3d` |

### `system-coherence/argue-empirically-not-persuasively` — degree 18; uncorroborated/no-evidence degree 17

| edge | directive | status | set | authored date | authored sha |
| --- | --- | --- | --- | --- | --- |
| `system-coherence/argue-empirically-not-persuasively` — `futon-theory/futonic-logic` | references | no-evidence | system-coherence | 2026-05-04 | `f752c5a1cf0fa58fee1fce150af271615859b012` |
| `system-coherence/argue-empirically-not-persuasively` — `futon-theory/mission-lifecycle` | references | uncorroborated | system-coherence | 2026-05-04 | `f752c5a1cf0fa58fee1fce150af271615859b012` |
| `system-coherence/argue-empirically-not-persuasively` — `system-coherence/single-seed-results-need-multi-seed-validation` | see-also | uncorroborated | system-coherence | 2026-08-30 | `be8c7077713b1b2d8c0d0d5d8eafbcda7d33b81c` |
| `system-coherence/argue-empirically-not-persuasively` — `writing-coherence/hedged-lift` | references | uncorroborated | system-coherence | 2026-05-04 | `f752c5a1cf0fa58fee1fce150af271615859b012` |
| `system-coherence/argue-empirically-not-persuasively` — `writing-coherence/unearned-essentially` | references | uncorroborated | system-coherence | 2026-05-04 | `f752c5a1cf0fa58fee1fce150af271615859b012` |
| `system-coherence/bind-open-questions-to-closure-mechanisms` — `system-coherence/argue-empirically-not-persuasively` | references | corroborated | system-coherence | 2026-05-27 | `05487b72790483d01e78d9b25b086cb428c8aa2e` |
| `system-coherence/correct-effect-claims-with-relevant-denominators` — `system-coherence/argue-empirically-not-persuasively` | references | uncorroborated | system-coherence | 2026-05-25 | `472d5d2627bade0ecdd9cd83262fec7cb6c1c2c1` |
| `system-coherence/facet-before-aggregating` — `system-coherence/argue-empirically-not-persuasively` | references | uncorroborated | system-coherence | 2026-05-25 | `1195eb4f37087a782ea798b0368910543ab81628` |
| `system-coherence/name-the-decision-before-the-framework` — `system-coherence/argue-empirically-not-persuasively` | references | uncorroborated | system-coherence | 2026-05-25 | `d76482e09067231ca977390be4f30af437a5f028` |
| `system-coherence/place-capabilities-at-the-scale-of-their-absence` — `system-coherence/argue-empirically-not-persuasively` | references | uncorroborated | system-coherence | 2026-05-25 | `a3d9135384a2e4d648331dbfc1b6a4696fca42cc` |
| `system-coherence/separate-configurable-choices-from-load-bearing-couplings` — `system-coherence/argue-empirically-not-persuasively` | references | no-evidence | system-coherence | 2026-05-25 | `b225e67ad93cad2a75f333a28f0e79ce48b941ee` |
| `system-coherence/single-seed-results-need-multi-seed-validation` — `system-coherence/argue-empirically-not-persuasively` | references | uncorroborated | system-coherence | 2026-05-27 | `05487b72790483d01e78d9b25b086cb428c8aa2e` |
| `system-coherence/tag-evidence-by-timescale` — `system-coherence/argue-empirically-not-persuasively` | references | no-evidence | system-coherence | 2026-05-25 | `38a3bace8eb4c1866ec0b441980dce91000db786` |
| `system-coherence/treat-content-refresh-as-second-order-when-conditions-bind` — `system-coherence/argue-empirically-not-persuasively` | references | no-evidence | system-coherence | 2026-05-25 | `56346494665ce85b7565f7cee67dbb1754d9735d` |
| `system-coherence/turn-design-into-checks` — `system-coherence/argue-empirically-not-persuasively` | references | uncorroborated | system-coherence | 2026-05-27 | `05487b72790483d01e78d9b25b086cb428c8aa2e` |
| `ukrns/calibrate-impact-promises-to-current-indicator-capacity` — `system-coherence/argue-empirically-not-persuasively` | references | uncorroborated | ukrns | 2026-05-25 | `bbbe5f867f0007dbd4d0f90c52adcdee7b0f7fdb` |
| `writing-coherence/citation-density-load-bearing-claim` — `system-coherence/argue-empirically-not-persuasively` | references | no-evidence | writing-coherence | 2026-05-25 | `80980d9418d46fce6b30e26208346c60f9a303e9` |
| `writing-coherence/meet-the-reader-where-they-are` — `system-coherence/argue-empirically-not-persuasively` | why | uncorroborated | writing-coherence | 2026-08-30 | `3db55f3e15639df85142af7d11d82d5e4e153b11` |
| `writing-coherence/stale-reference-after-restructure` — `system-coherence/argue-empirically-not-persuasively` | why | uncorroborated | writing-coherence | 2026-08-30 | `3db55f3e15639df85142af7d11d82d5e4e153b11` |

### `futon-theory/baldwin-cycle` — degree 16; uncorroborated/no-evidence degree 16

| edge | directive | status | set | authored date | authored sha |
| --- | --- | --- | --- | --- | --- |
| `baldwin/ARGUMENT` — `futon-theory/baldwin-cycle` | references | no-evidence | baldwin | 2026-08-01 | `5e7ce2b2491078c389090aff44078f6c92d13f45` |
| `baldwin/two-claims-not-one` — `futon-theory/baldwin-cycle` | references | no-evidence | baldwin | 2026-08-01 | `5e7ce2b2491078c389090aff44078f6c92d13f45` |
| `futon-theory/baldwin-cycle` — `futon-theory/four-types` | references | no-evidence | futon-theory | 2026-08-15 | `cbd639917df7a573c84975fd2b5223067fc5e3b5` |
| `futon-theory/baldwin-cycle` — `futon-theory/interface-loop` | references | no-evidence | futon-theory | 2026-08-15 | `cbd639917df7a573c84975fd2b5223067fc5e3b5` |
| `futon-theory/baldwin-cycle` — `futon-theory/proof-path` | references | no-evidence | futon-theory | 2026-08-15 | `cbd639917df7a573c84975fd2b5223067fc5e3b5` |
| `futon-theory/baldwin-cycle` — `futon-theory/retrospective-stability` | references | no-evidence | futon-theory | 2026-08-15 | `cbd639917df7a573c84975fd2b5223067fc5e3b5` |
| `futon-theory/four-types` — `futon-theory/baldwin-cycle` | references | no-evidence | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `futon-theory/futonic-logic` — `futon-theory/baldwin-cycle` | references | no-evidence | futon-theory | 2026-02-10 | `9cfd7a7016b495efab1538e69feb3f46920e6a2a` |
| `futon-theory/interface-loop` — `futon-theory/baldwin-cycle` | references | no-evidence | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `futon-theory/local-gain-persistence` — `futon-theory/baldwin-cycle` | references | no-evidence | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `futon-theory/retroactive-canonicalization` — `futon-theory/baldwin-cycle` | references | no-evidence | futon-theory | 2026-02-08 | `f54574b702605340b326be6621ea69e540bce333` |
| `futon-theory/structural-tension-as-observation` — `futon-theory/baldwin-cycle` | references | no-evidence | futon-theory | 2026-02-08 | `f54574b702605340b326be6621ea69e540bce333` |
| `futon-theory/the-woven-form` — `futon-theory/baldwin-cycle` | references | no-evidence | futon-theory | 2026-07-10 | `fb51b6256aaca74acf5b84f1d3c15f2c09bfbe79` |
| `futon-theory/theory-as-exotype` — `futon-theory/baldwin-cycle` | references | no-evidence | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `futon-theory/xenotype-portability` — `futon-theory/baldwin-cycle` | references | no-evidence | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `peripherals/inhabitation-feeds-evolution` — `futon-theory/baldwin-cycle` | references | no-evidence | peripherals | 2026-04-02 | `bd9e3fd1c4388fa8a2181257ea9c59a08251d333` |
| `peripherals/pattern-becomes-peripheral` — `futon-theory/baldwin-cycle` | references | no-evidence | peripherals | 2026-04-02 | `bd9e3fd1c4388fa8a2181257ea9c59a08251d333` |
| `social/ARGUMENT` — `futon-theory/baldwin-cycle` | references | no-evidence | social | 2026-02-10 | `6739b149e40ce0d613d09b5d0eb347d211e67113` |

### `futon-theory/futonic-logic` — degree 16; uncorroborated/no-evidence degree 16

| edge | directive | status | set | authored date | authored sha |
| --- | --- | --- | --- | --- | --- |
| `futon-theory/futonic-logic` — `futon-theory/agent-contract` | references | no-evidence | futon-theory | 2026-02-10 | `9cfd7a7016b495efab1538e69feb3f46920e6a2a` |
| `futon-theory/futonic-logic` — `futon-theory/baldwin-cycle` | references | no-evidence | futon-theory | 2026-02-10 | `9cfd7a7016b495efab1538e69feb3f46920e6a2a` |
| `futon-theory/futonic-logic` — `futon-theory/four-types` | references | no-evidence | futon-theory | 2026-02-10 | `9cfd7a7016b495efab1538e69feb3f46920e6a2a` |
| `futon-theory/futonic-logic` — `futon-theory/proof-path` | references | no-evidence | futon-theory | 2026-02-10 | `9cfd7a7016b495efab1538e69feb3f46920e6a2a` |
| `futon-theory/futonic-logic` — `futon-theory/retroactive-canonicalization` | references | no-evidence | futon-theory | 2026-02-10 | `9cfd7a7016b495efab1538e69feb3f46920e6a2a` |
| `futon-theory/futonic-logic` — `futon-theory/structural-tension-as-observation` | references | no-evidence | futon-theory | 2026-02-10 | `9cfd7a7016b495efab1538e69feb3f46920e6a2a` |
| `futon-theory/futonic-logic` — `futon-theory/symbolic-individuation` | references | no-evidence | futon-theory | 2026-02-10 | `9cfd7a7016b495efab1538e69feb3f46920e6a2a` |
| `futon-theory/futonic-logic` — `futon-theory/theory-as-exotype` | references | no-evidence | futon-theory | 2026-02-10 | `9cfd7a7016b495efab1538e69feb3f46920e6a2a` |
| `futon-theory/futonic-logic` — `problems/commitment-temperature-is-instrumented-as-gain` | why | no-evidence | futon-theory | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `futon-theory/futonic-logic` — `problems/satisfied-rungs-are-counted-and-surfaced` | why | no-evidence | futon-theory | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `futon-theory/reverse-morphogenesis` — `futon-theory/futonic-logic` | references | no-evidence | futon-theory | 2026-02-11 | `702f7e95eb52c9bbdbec255d094b42e457923535` |
| `futon-theory/the-woven-form` — `futon-theory/futonic-logic` | references | no-evidence | futon-theory | 2026-07-10 | `fb51b6256aaca74acf5b84f1d3c15f2c09bfbe79` |
| `futon-theory/wyrd` — `futon-theory/futonic-logic` | references | no-evidence | futon-theory | 2026-05-27 | `db9d02a09db94d58fb7a761051c3d6fa6ec57530` |
| `problems/satisfied-rungs-are-counted-and-surfaced` — `futon-theory/futonic-logic` | see-also | no-evidence | problems | 2026-08-30 | `701a729f2bf3a37829e8e3bf5eb5777a5f4db226` |
| `structure/block-as-futonic-revolution` — `futon-theory/futonic-logic` | references | no-evidence | structure | 2026-05-03 | `bb07cfab71163e1d526843d4799e29dae1084d5c` |
| `structure/mana-allostasis` — `futon-theory/futonic-logic` | references | no-evidence | structure | 2026-05-03 | `bb07cfab71163e1d526843d4799e29dae1084d5c` |
| `system-coherence/argue-empirically-not-persuasively` — `futon-theory/futonic-logic` | references | no-evidence | system-coherence | 2026-05-04 | `f752c5a1cf0fa58fee1fce150af271615859b012` |

### `peripherals/inhabitation-feeds-evolution` — degree 16; uncorroborated/no-evidence degree 16

| edge | directive | status | set | authored date | authored sha |
| --- | --- | --- | --- | --- | --- |
| `peripherals/constrained-execution-envelope` — `peripherals/inhabitation-feeds-evolution` | references | no-evidence | peripherals | 2026-04-02 | `bd9e3fd1c4388fa8a2181257ea9c59a08251d333` |
| `peripherals/hot-reload-as-default-fix-path` — `peripherals/inhabitation-feeds-evolution` | references | no-evidence | peripherals | 2026-05-27 | `f15b91d40a268c3965f883867d5e5a065243415d` |
| `peripherals/inhabitation-feeds-evolution` — `aif/structured-observation-vector` | references | no-evidence | peripherals | 2026-04-02 | `bd9e3fd1c4388fa8a2181257ea9c59a08251d333` |
| `peripherals/inhabitation-feeds-evolution` — `ants/baseline-cyber-ant` | references | no-evidence | peripherals | 2026-04-02 | `bd9e3fd1c4388fa8a2181257ea9c59a08251d333` |
| `peripherals/inhabitation-feeds-evolution` — `futon-theory/baldwin-cycle` | references | no-evidence | peripherals | 2026-04-02 | `bd9e3fd1c4388fa8a2181257ea9c59a08251d333` |
| `peripherals/inhabitation-feeds-evolution` — `futon-theory/mission-lifecycle` | references | no-evidence | peripherals | 2026-04-02 | `bd9e3fd1c4388fa8a2181257ea9c59a08251d333` |
| `peripherals/inhabitation-feeds-evolution` — `gauntlet/aif-as-environment-not-instruction` | references | no-evidence | peripherals | 2026-04-02 | `bd9e3fd1c4388fa8a2181257ea9c59a08251d333` |
| `peripherals/inhabitation-feeds-evolution` — `math-informal/transport-across-isomorphism` | references | no-evidence | peripherals | 2026-04-02 | `bd9e3fd1c4388fa8a2181257ea9c59a08251d333` |
| `peripherals/inhabitation-feeds-evolution` — `meta/baldwin-ratchet-defeats-darkroom` | references | no-evidence | peripherals | 2026-04-02 | `bd9e3fd1c4388fa8a2181257ea9c59a08251d333` |
| `peripherals/inhabitation-feeds-evolution` — `peripherals/constrained-execution-envelope` | references | no-evidence | peripherals | 2026-04-02 | `bd9e3fd1c4388fa8a2181257ea9c59a08251d333` |
| `peripherals/inhabitation-feeds-evolution` — `peripherals/surface-earns-inhabitation` | references | no-evidence | peripherals | 2026-04-02 | `bd9e3fd1c4388fa8a2181257ea9c59a08251d333` |
| `peripherals/pattern-becomes-peripheral` — `peripherals/inhabitation-feeds-evolution` | references | no-evidence | peripherals | 2026-04-02 | `bd9e3fd1c4388fa8a2181257ea9c59a08251d333` |
| `peripherals/pilot-plus-ground-control` — `peripherals/inhabitation-feeds-evolution` | references | no-evidence | peripherals | 2026-06-10 | `3836d4b44ae04ee1949d9953320f7da3b752ace6` |
| `peripherals/progress-heartbeat-distinct-from-cycle-completion` — `peripherals/inhabitation-feeds-evolution` | references | no-evidence | peripherals | 2026-05-27 | `f15b91d40a268c3965f883867d5e5a065243415d` |
| `peripherals/read-only-first-then-extend` — `peripherals/inhabitation-feeds-evolution` | references | no-evidence | peripherals | 2026-05-27 | `f15b91d40a268c3965f883867d5e5a065243415d` |
| `peripherals/read-only-first-then-extend` — `peripherals/inhabitation-feeds-evolution` | see-also | no-evidence | peripherals | 2026-08-30 | `be8c7077713b1b2d8c0d0d5d8eafbcda7d33b81c` |
| `peripherals/surface-earns-inhabitation` — `peripherals/inhabitation-feeds-evolution` | references | no-evidence | peripherals | 2026-04-02 | `bd9e3fd1c4388fa8a2181257ea9c59a08251d333` |
| `peripherals/surface-earns-inhabitation` — `peripherals/inhabitation-feeds-evolution` | see-also | no-evidence | peripherals | 2026-08-30 | `be8c7077713b1b2d8c0d0d5d8eafbcda7d33b81c` |
| `structure/cook-ting` — `peripherals/inhabitation-feeds-evolution` | references | no-evidence | structure | 2026-05-03 | `f7ea5537a64698b0f71b64a748562d9cf8a002ee` |
| `structure/hinge-point` — `peripherals/inhabitation-feeds-evolution` | references | no-evidence | structure | 2026-05-03 | `f7ea5537a64698b0f71b64a748562d9cf8a002ee` |

### `problems/tensions-are-navigated-not-eliminated` — degree 16; uncorroborated/no-evidence degree 16

| edge | directive | status | set | authored date | authored sha |
| --- | --- | --- | --- | --- | --- |
| `eight-gates/an-push` — `problems/tensions-are-navigated-not-eliminated` | why | uncorroborated | eight-gates | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `eight-gates/cai-pluck` — `problems/tensions-are-navigated-not-eliminated` | why | uncorroborated | eight-gates | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `eight-gates/elbow-immediate` — `problems/tensions-are-navigated-not-eliminated` | why | uncorroborated | eight-gates | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `eight-gates/ji-press` — `problems/tensions-are-navigated-not-eliminated` | why | no-evidence | eight-gates | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `eight-gates/kao-lean` — `problems/tensions-are-navigated-not-eliminated` | why | no-evidence | eight-gates | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `eight-gates/lean-commit` — `problems/tensions-are-navigated-not-eliminated` | why | no-evidence | eight-gates | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `eight-gates/lie-split` — `problems/tensions-are-navigated-not-eliminated` | why | uncorroborated | eight-gates | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `eight-gates/lu-roll-back` — `problems/tensions-are-navigated-not-eliminated` | why | no-evidence | eight-gates | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `eight-gates/peng-ward-off` — `problems/tensions-are-navigated-not-eliminated` | why | no-evidence | eight-gates | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `eight-gates/pluck-extract` — `problems/tensions-are-navigated-not-eliminated` | why | uncorroborated | eight-gates | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `eight-gates/press-mechanism` — `problems/tensions-are-navigated-not-eliminated` | why | uncorroborated | eight-gates | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `eight-gates/push-warrant` — `problems/tensions-are-navigated-not-eliminated` | why | no-evidence | eight-gates | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `eight-gates/roll-back-hold` — `problems/tensions-are-navigated-not-eliminated` | why | no-evidence | eight-gates | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `eight-gates/split-isolate` — `problems/tensions-are-navigated-not-eliminated` | why | uncorroborated | eight-gates | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `eight-gates/ward-off-boundary` — `problems/tensions-are-navigated-not-eliminated` | why | no-evidence | eight-gates | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `eight-gates/zhou-elbow` — `problems/tensions-are-navigated-not-eliminated` | why | no-evidence | eight-gates | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |

### `equity/ARGUMENT` — degree 15; uncorroborated/no-evidence degree 15

| edge | directive | status | set | authored date | authored sha |
| --- | --- | --- | --- | --- | --- |
| `equity/ARGUMENT` — `equity/confident-adaptation` | references | no-evidence | equity | 2026-03-20 | `f28b756088a299102c79ac93201f48c8d8c5d964` |
| `equity/ARGUMENT` — `equity/contribution-pathways` | references | no-evidence | equity | 2026-03-20 | `f28b756088a299102c79ac93201f48c8d8c5d964` |
| `equity/ARGUMENT` — `equity/protected-interaction` | references | no-evidence | equity | 2026-03-20 | `f28b756088a299102c79ac93201f48c8d8c5d964` |
| `equity/ARGUMENT` — `equity/shared-maintenance` | references | no-evidence | equity | 2026-03-20 | `f28b756088a299102c79ac93201f48c8d8c5d964` |
| `equity/ARGUMENT` — `equity/visible-impact` | references | no-evidence | equity | 2026-03-20 | `f28b756088a299102c79ac93201f48c8d8c5d964` |
| `equity/confident-adaptation` — `equity/ARGUMENT` | references | no-evidence | equity | 2026-03-20 | `f28b756088a299102c79ac93201f48c8d8c5d964` |
| `equity/confident-adaptation-as-published` — `equity/ARGUMENT` | references | no-evidence | equity | 2026-05-03 | `f7ea5537a64698b0f71b64a748562d9cf8a002ee` |
| `equity/confident-adaptation-merged` — `equity/ARGUMENT` | references | no-evidence | equity | 2026-05-03 | `f7ea5537a64698b0f71b64a748562d9cf8a002ee` |
| `equity/contribution-pathways` — `equity/ARGUMENT` | references | no-evidence | equity | 2026-03-20 | `f28b756088a299102c79ac93201f48c8d8c5d964` |
| `equity/contribution-pathways-as-published` — `equity/ARGUMENT` | references | no-evidence | equity | 2026-05-03 | `f7ea5537a64698b0f71b64a748562d9cf8a002ee` |
| `equity/contribution-pathways-merged` — `equity/ARGUMENT` | references | no-evidence | equity | 2026-05-03 | `f7ea5537a64698b0f71b64a748562d9cf8a002ee` |
| `equity/protected-interaction` — `equity/ARGUMENT` | references | no-evidence | equity | 2026-03-20 | `f28b756088a299102c79ac93201f48c8d8c5d964` |
| `equity/protected-interaction-as-published` — `equity/ARGUMENT` | references | no-evidence | equity | 2026-05-03 | `f7ea5537a64698b0f71b64a748562d9cf8a002ee` |
| `equity/protected-interaction-merged` — `equity/ARGUMENT` | references | no-evidence | equity | 2026-05-03 | `f7ea5537a64698b0f71b64a748562d9cf8a002ee` |
| `equity/shared-maintenance` — `equity/ARGUMENT` | references | no-evidence | equity | 2026-03-20 | `f28b756088a299102c79ac93201f48c8d8c5d964` |
| `equity/shared-maintenance-as-published` — `equity/ARGUMENT` | references | no-evidence | equity | 2026-05-03 | `f7ea5537a64698b0f71b64a748562d9cf8a002ee` |
| `equity/shared-maintenance-merged` — `equity/ARGUMENT` | references | no-evidence | equity | 2026-05-03 | `f7ea5537a64698b0f71b64a748562d9cf8a002ee` |
| `equity/visible-impact` — `equity/ARGUMENT` | references | no-evidence | equity | 2026-03-20 | `f28b756088a299102c79ac93201f48c8d8c5d964` |
| `equity/visible-impact-as-published` — `equity/ARGUMENT` | references | no-evidence | equity | 2026-05-03 | `f7ea5537a64698b0f71b64a748562d9cf8a002ee` |
| `equity/visible-impact-merged` — `equity/ARGUMENT` | references | no-evidence | equity | 2026-05-03 | `f7ea5537a64698b0f71b64a748562d9cf8a002ee` |

### `social/ARGUMENT` — degree 15; uncorroborated/no-evidence degree 15

| edge | directive | status | set | authored date | authored sha |
| --- | --- | --- | --- | --- | --- |
| `gauntlet/ARGUMENT` — `social/ARGUMENT` | references | no-evidence | gauntlet | 2026-02-27 | `459cb1744803a26ed3a550f062730154c6851fca` |
| `social/ARGUMENT` — `coordination/ARGUMENT` | references | no-evidence | social | 2026-02-10 | `6739b149e40ce0d613d09b5d0eb347d211e67113` |
| `social/ARGUMENT` — `futon-theory/agent-contract` | references | no-evidence | social | 2026-02-10 | `6739b149e40ce0d613d09b5d0eb347d211e67113` |
| `social/ARGUMENT` — `futon-theory/baldwin-cycle` | references | no-evidence | social | 2026-02-10 | `6739b149e40ce0d613d09b5d0eb347d211e67113` |
| `social/ARGUMENT` — `futon-theory/event-protocol` | references | no-evidence | social | 2026-02-10 | `6739b149e40ce0d613d09b5d0eb347d211e67113` |
| `social/ARGUMENT` — `futon-theory/four-types` | references | no-evidence | social | 2026-02-10 | `6739b149e40ce0d613d09b5d0eb347d211e67113` |
| `social/ARGUMENT` — `futon-theory/interface-loop` | references | no-evidence | social | 2026-02-10 | `6739b149e40ce0d613d09b5d0eb347d211e67113` |
| `social/ARGUMENT` — `futon-theory/mission-lifecycle` | references | no-evidence | social | 2026-02-10 | `6739b149e40ce0d613d09b5d0eb347d211e67113` |
| `social/ARGUMENT` — `futon-theory/proof-path` | references | no-evidence | social | 2026-02-10 | `6739b149e40ce0d613d09b5d0eb347d211e67113` |
| `social/ARGUMENT` — `futon-theory/retroactive-canonicalization` | references | no-evidence | social | 2026-02-10 | `6739b149e40ce0d613d09b5d0eb347d211e67113` |
| `social/ARGUMENT` — `futon-theory/structural-tension-as-observation` | references | no-evidence | social | 2026-02-10 | `6739b149e40ce0d613d09b5d0eb347d211e67113` |
| `social/ARGUMENT` — `social/explicit-exit-over-abandonment` | references | no-evidence | social | 2026-02-10 | `6739b149e40ce0d613d09b5d0eb347d211e67113` |
| `social/ARGUMENT` — `social/scope-bounded-handoff` | references | no-evidence | social | 2026-02-10 | `6739b149e40ce0d613d09b5d0eb347d211e67113` |
| `social/ARGUMENT` — `social/tension-before-code` | references | no-evidence | social | 2026-02-10 | `6739b149e40ce0d613d09b5d0eb347d211e67113` |
| `social/ARGUMENT` — `social/verify-before-compose` | references | no-evidence | social | 2026-02-10 | `6739b149e40ce0d613d09b5d0eb347d211e67113` |

### `structure/hinge-point` — degree 15; uncorroborated/no-evidence degree 15

| edge | directive | status | set | authored date | authored sha |
| --- | --- | --- | --- | --- | --- |
| `capability/capability-vocabulary-v0` — `structure/hinge-point` | references | no-evidence | capability | 2026-07-21 | `a2da67f2c8f0f4586074a0b117d4bbfe584ab082` |
| `capability/hinge-capability-extraction` — `structure/hinge-point` | references | no-evidence | capability | 2026-07-21 | `a2da67f2c8f0f4586074a0b117d4bbfe584ab082` |
| `structure/backwards-induction` — `structure/hinge-point` | references | no-evidence | structure | 2026-05-03 | `f7ea5537a64698b0f71b64a748562d9cf8a002ee` |
| `structure/block-as-futonic-revolution` — `structure/hinge-point` | references | no-evidence | structure | 2026-05-03 | `bb07cfab71163e1d526843d4799e29dae1084d5c` |
| `structure/cook-ting` — `structure/hinge-point` | references | no-evidence | structure | 2026-05-03 | `f7ea5537a64698b0f71b64a748562d9cf8a002ee` |
| `structure/hinge-entry-slots` — `structure/hinge-point` | references | no-evidence | structure | 2026-05-03 | `f7ea5537a64698b0f71b64a748562d9cf8a002ee` |
| `structure/hinge-point` — `agent/state-is-hypothesis` | references | no-evidence | structure | 2026-05-03 | `f7ea5537a64698b0f71b64a748562d9cf8a002ee` |
| `structure/hinge-point` — `futon-theory/structural-tension-as-observation` | references | no-evidence | structure | 2026-05-03 | `f7ea5537a64698b0f71b64a748562d9cf8a002ee` |
| `structure/hinge-point` — `peripherals/inhabitation-feeds-evolution` | references | no-evidence | structure | 2026-05-03 | `f7ea5537a64698b0f71b64a748562d9cf8a002ee` |
| `structure/hinge-point` — `social/scope-bounded-handoff` | references | no-evidence | structure | 2026-05-03 | `f7ea5537a64698b0f71b64a748562d9cf8a002ee` |
| `structure/hinge-point` — `structure/cook-ting` | references | no-evidence | structure | 2026-05-03 | `f7ea5537a64698b0f71b64a748562d9cf8a002ee` |
| `structure/hinge-point` — `war-machine/inhabitation-threshold` | references | no-evidence | structure | 2026-05-03 | `f7ea5537a64698b0f71b64a748562d9cf8a002ee` |
| `structure/mana-allostasis` — `structure/hinge-point` | references | no-evidence | structure | 2026-05-03 | `bb07cfab71163e1d526843d4799e29dae1084d5c` |
| `structure/unresolved-tensions-at-closure` — `structure/hinge-point` | references | no-evidence | structure | 2026-05-03 | `f7ea5537a64698b0f71b64a748562d9cf8a002ee` |
| `structure/what-problem-is-this-actually-solving` — `structure/hinge-point` | references | no-evidence | structure | 2026-05-03 | `f7ea5537a64698b0f71b64a748562d9cf8a002ee` |
| `structure/whose-question-is-this` — `structure/hinge-point` | references | no-evidence | structure | 2026-05-03 | `f7ea5537a64698b0f71b64a748562d9cf8a002ee` |

### `象/言即行` — degree 15; uncorroborated/no-evidence degree 15

| edge | directive | status | set | authored date | authored sha |
| --- | --- | --- | --- | --- | --- |
| `象/两种规格` — `象/言即行` | see-also | no-evidence | 象 | 2026-09-26 | `f03965c8d392422295b5fbc1cea63dcb2c991b34` |
| `象/名分有据` — `象/言即行` | why | no-evidence | 象 | 2026-09-26 | `f03965c8d392422295b5fbc1cea63dcb2c991b34` |
| `象/收回亦是行` — `象/言即行` | why | no-evidence | 象 | 2026-09-26 | `f03965c8d392422295b5fbc1cea63dcb2c991b34` |
| `象/明言假设` — `象/言即行` | why | no-evidence | 象 | 2026-09-26 | `f03965c8d392422295b5fbc1cea63dcb2c991b34` |
| `象/翻译契约` — `象/言即行` | see-also | no-evidence | 象 | 2026-09-23 | `6894eeff89652476ba303acac82a003a7dd3b988` |
| `象/行有定名` — `象/言即行` | why | no-evidence | 象 | 2026-09-27 | `a3514b95260373f33bbc0b27f1e877d7e128e03e` |
| `象/要约与接受` — `象/言即行` | why | no-evidence | 象 | 2026-09-26 | `f03965c8d392422295b5fbc1cea63dcb2c991b34` |
| `象/言即行` — `cascades/declared-skeleton` | see-also | no-evidence | 象 | 2026-09-23 | `10643d94dbe8371035e0a3cc02be1565362822d0` |
| `象/言即行` — `translation/route-the-untranslatable` | see-also | no-evidence | 象 | 2026-09-23 | `10643d94dbe8371035e0a3cc02be1565362822d0` |
| `象/言即行` — `象/两种规格` | how | no-evidence | 象 | 2026-09-27 | `595af63af6582cde0d87c661829449f10afe1090` |
| `象/言即行` — `象/以史为据` | how | no-evidence | 象 | 2026-09-27 | `595af63af6582cde0d87c661829449f10afe1090` |
| `象/言即行` — `象/双时并记` | how | no-evidence | 象 | 2026-09-27 | `595af63af6582cde0d87c661829449f10afe1090` |
| `象/言即行` — `象/名分有据` | how | no-evidence | 象 | 2026-09-27 | `595af63af6582cde0d87c661829449f10afe1090` |
| `象/言即行` — `象/收回亦是行` | how | no-evidence | 象 | 2026-09-27 | `595af63af6582cde0d87c661829449f10afe1090` |
| `象/言即行` — `象/翻译契约` | how | no-evidence | 象 | 2026-09-27 | `595af63af6582cde0d87c661829449f10afe1090` |
| `象/言即行` — `象/行有定名` | how | no-evidence | 象 | 2026-09-27 | `595af63af6582cde0d87c661829449f10afe1090` |
| `象/言即行` — `象/视图出于史` | how | no-evidence | 象 | 2026-09-27 | `595af63af6582cde0d87c661829449f10afe1090` |
| `象/言即行` — `象/诺必践` | how | no-evidence | 象 | 2026-09-27 | `595af63af6582cde0d87c661829449f10afe1090` |
| `象/言即行` — `象/象不忘` | see-also | no-evidence | 象 | 2026-09-23 | `10643d94dbe8371035e0a3cc02be1565362822d0` |
| `象/言即行` — `象/释义非授` | how | no-evidence | 象 | 2026-09-27 | `595af63af6582cde0d87c661829449f10afe1090` |
| `象/诺必践` — `象/言即行` | see-also | no-evidence | 象 | 2026-09-27 | `a3514b95260373f33bbc0b27f1e877d7e128e03e` |
| `象/象不忘` — `象/言即行` | see-also | no-evidence | 象 | 2026-09-23 | `10643d94dbe8371035e0a3cc02be1565362822d0` |
| `象/释义非授` — `象/言即行` | see-also | no-evidence | 象 | 2026-09-27 | `a3514b95260373f33bbc0b27f1e877d7e128e03e` |

### `futon-theory/agent-contract` — degree 14; uncorroborated/no-evidence degree 14

| edge | directive | status | set | authored date | authored sha |
| --- | --- | --- | --- | --- | --- |
| `agency/identifier-separation` — `futon-theory/agent-contract` | references | uncorroborated | agency | 2026-02-08 | `2d434daa4eb506b7222256a9b619b884b63f65c8` |
| `agency/invariants` — `futon-theory/agent-contract` | references | no-evidence | agency | 2026-02-08 | `2d434daa4eb506b7222256a9b619b884b63f65c8` |
| `agency/self-attribution` — `futon-theory/agent-contract` | references | uncorroborated | agency | 2026-02-08 | `e3ec4f59c5e0f5d5c22967119a5c3648cf0e0119` |
| `agency/single-routing-authority` — `futon-theory/agent-contract` | references | uncorroborated | agency | 2026-02-08 | `e3ec4f59c5e0f5d5c22967119a5c3648cf0e0119` |
| `agency/state-atomicity` — `futon-theory/agent-contract` | references | uncorroborated | agency | 2026-02-08 | `e3ec4f59c5e0f5d5c22967119a5c3648cf0e0119` |
| `coordination/intent-to-mission-binding` — `futon-theory/agent-contract` | references | uncorroborated | coordination | 2026-05-03 | `fb533ce9e56b42f326259a029bd072f128ee52c0` |
| `futon-theory/agent-contract` — `futon-theory/event-protocol` | references | uncorroborated | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `futon-theory/agent-contract` — `futon-theory/proof-path` | references | no-evidence | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `futon-theory/agent-contract` — `futon-theory/stop-the-line` | references | uncorroborated | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `futon-theory/event-protocol` — `futon-theory/agent-contract` | references | uncorroborated | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `futon-theory/futonic-logic` — `futon-theory/agent-contract` | references | no-evidence | futon-theory | 2026-02-10 | `9cfd7a7016b495efab1538e69feb3f46920e6a2a` |
| `futon-theory/stop-the-line` — `futon-theory/agent-contract` | references | uncorroborated | futon-theory | 2026-02-07 | `9e20836d14b734d02cda4e431f7ab578a89c4d65` |
| `orchestration/recorded-handoff` — `futon-theory/agent-contract` | references | uncorroborated | orchestration | 2026-06-11 | `860a5595a58d62866b1013715934ae23934cae49` |
| `social/ARGUMENT` — `futon-theory/agent-contract` | references | no-evidence | social | 2026-02-10 | `6739b149e40ce0d613d09b5d0eb347d211e67113` |
| `social/scope-bounded-handoff` — `futon-theory/agent-contract` | references | uncorroborated | social | 2026-02-10 | `6739b149e40ce0d613d09b5d0eb347d211e67113` |
| `social/verify-before-compose` — `futon-theory/agent-contract` | references | uncorroborated | social | 2026-02-09 | `193be6ef0edc2dcd404b030ae4e83a9237f4fa3d` |

### `hygiene/receipt-then-gate` — degree 14; uncorroborated/no-evidence degree 14

| edge | directive | status | set | authored date | authored sha |
| --- | --- | --- | --- | --- | --- |
| `buffer-cleaner/receipt-then-gate` — `hygiene/receipt-then-gate` | references | uncorroborated | buffer-cleaner | 2026-09-16 | `684aa732cee085e0dc07cb3a9d1961651be97f79` |
| `cascade-construction/order-by-what-each-step-needs` — `hygiene/receipt-then-gate` | references | uncorroborated | cascade-construction | 2026-09-17 | `29b77c55fa3f8f4ecd544a7a2971005a8092f93b` |
| `cascade-construction/separate-construction-from-meaning-review` — `hygiene/receipt-then-gate` | see-also | uncorroborated | cascade-construction | 2026-09-17 | `29b77c55fa3f8f4ecd544a7a2971005a8092f93b` |
| `contracts/every-entry-has-a-falsifier` — `hygiene/receipt-then-gate` | see-also | uncorroborated | contracts | 2026-09-16 | `c0d5b62309260d8760d661531644f1767df5e4b3` |
| `hygiene/classify-by-remedy-judgement` — `hygiene/receipt-then-gate` | references | uncorroborated | hygiene | 2026-09-16 | `684aa732cee085e0dc07cb3a9d1961651be97f79` |
| `hygiene/exempt-the-in-use` — `hygiene/receipt-then-gate` | references | uncorroborated | hygiene | 2026-09-16 | `684aa732cee085e0dc07cb3a9d1961651be97f79` |
| `hygiene/receipt-then-gate` — `apparatus/every-wait-has-a-deadline` | see-also | uncorroborated | hygiene | 2026-09-16 | `684aa732cee085e0dc07cb3a9d1961651be97f79` |
| `hygiene/receipt-then-gate` — `buffer-cleaner/receipt-then-gate` | references | uncorroborated | hygiene | 2026-09-16 | `684aa732cee085e0dc07cb3a9d1961651be97f79` |
| `hygiene/receipt-then-gate` — `hygiene/exempt-the-in-use` | references | uncorroborated | hygiene | 2026-09-16 | `684aa732cee085e0dc07cb3a9d1961651be97f79` |
| `hygiene/receipt-then-gate` — `hygiene/route-to-who-can-act` | references | uncorroborated | hygiene | 2026-09-16 | `684aa732cee085e0dc07cb3a9d1961651be97f79` |
| `hygiene/receipt-then-gate` — `inbox-zero/push-the-declared` | references | uncorroborated | hygiene | 2026-09-16 | `684aa732cee085e0dc07cb3a9d1961651be97f79` |
| `hygiene/receipt-then-gate` — `test-registry/judge-adequacy` | references | uncorroborated | hygiene | 2026-09-16 | `684aa732cee085e0dc07cb3a9d1961651be97f79` |
| `hygiene/receipt-then-gate` — `test-registry/register-the-run` | references | uncorroborated | hygiene | 2026-09-16 | `684aa732cee085e0dc07cb3a9d1961651be97f79` |
| `hygiene/route-to-who-can-act` — `hygiene/receipt-then-gate` | references | uncorroborated | hygiene | 2026-09-16 | `684aa732cee085e0dc07cb3a9d1961651be97f79` |
| `inbox-zero/push-the-declared` — `hygiene/receipt-then-gate` | references | uncorroborated | inbox-zero | 2026-09-16 | `684aa732cee085e0dc07cb3a9d1961651be97f79` |
| `pattern-interpretation/compile-guards-exactly` — `hygiene/receipt-then-gate` | see-also | no-evidence | pattern-interpretation | 2026-09-16 | `c0d5b62309260d8760d661531644f1767df5e4b3` |
| `test-registry/judge-adequacy` — `hygiene/receipt-then-gate` | references | uncorroborated | test-registry | 2026-09-16 | `684aa732cee085e0dc07cb3a9d1961651be97f79` |
| `test-registry/outlive-the-process` — `hygiene/receipt-then-gate` | see-also | no-evidence | test-registry | 2026-09-19 | `6a40337a7d6e55544f9e4759c86b186913231df1` |
| `test-registry/register-the-run` — `hygiene/receipt-then-gate` | references | uncorroborated | test-registry | 2026-09-19 | `6a40337a7d6e55544f9e4759c86b186913231df1` |
| `translation/choose-the-equivalence-check` — `hygiene/receipt-then-gate` | see-also | no-evidence | translation | 2026-09-16 | `c0d5b62309260d8760d661531644f1767df5e4b3` |

### `象/翻译契约` — degree 14; uncorroborated/no-evidence degree 14

| edge | directive | status | set | authored date | authored sha |
| --- | --- | --- | --- | --- | --- |
| `translation/revise-the-source-under-pressure` — `象/翻译契约` | see-also | no-evidence | translation | 2026-09-23 | `848a7733e3f24f0fdb12fd83adeea70b5a905641` |
| `象/同构方为同` — `象/翻译契约` | why | no-evidence | 象 | 2026-09-27 | `a3514b95260373f33bbc0b27f1e877d7e128e03e` |
| `象/翻译契约` — `cascades/declared-skeleton` | see-also | no-evidence | 象 | 2026-09-23 | `6894eeff89652476ba303acac82a003a7dd3b988` |
| `象/翻译契约` — `proof-search/typed-hole-as-frontier` | see-also | no-evidence | 象 | 2026-09-23 | `6894eeff89652476ba303acac82a003a7dd3b988` |
| `象/翻译契约` — `translation/bind-to-the-source` | why | no-evidence | 象 | 2026-09-23 | `6894eeff89652476ba303acac82a003a7dd3b988` |
| `象/翻译契约` — `translation/choose-the-equivalence-check` | why | no-evidence | 象 | 2026-09-23 | `6894eeff89652476ba303acac82a003a7dd3b988` |
| `象/翻译契约` — `translation/declare-what-is-lost` | why | no-evidence | 象 | 2026-09-23 | `6894eeff89652476ba303acac82a003a7dd3b988` |
| `象/翻译契约` — `translation/declare-what-is-preserved` | why | no-evidence | 象 | 2026-09-23 | `6894eeff89652476ba303acac82a003a7dd3b988` |
| `象/翻译契约` — `translation/route-the-untranslatable` | why | no-evidence | 象 | 2026-09-23 | `6894eeff89652476ba303acac82a003a7dd3b988` |
| `象/翻译契约` — `translation/test-by-reproducing-behaviour` | why | no-evidence | 象 | 2026-09-23 | `6894eeff89652476ba303acac82a003a7dd3b988` |
| `象/翻译契约` — `象/两种规格` | see-also | no-evidence | 象 | 2026-09-23 | `6894eeff89652476ba303acac82a003a7dd3b988` |
| `象/翻译契约` — `象/同构方为同` | how | no-evidence | 象 | 2026-09-27 | `595af63af6582cde0d87c661829449f10afe1090` |
| `象/翻译契约` — `象/言即行` | see-also | no-evidence | 象 | 2026-09-23 | `6894eeff89652476ba303acac82a003a7dd3b988` |
| `象/翻译契约` — `象/象不忘` | see-also | no-evidence | 象 | 2026-09-23 | `6894eeff89652476ba303acac82a003a7dd3b988` |
| `象/翻译契约` — `象/释义非授` | how | no-evidence | 象 | 2026-09-27 | `595af63af6582cde0d87c661829449f10afe1090` |
| `象/言即行` — `象/翻译契约` | how | no-evidence | 象 | 2026-09-27 | `595af63af6582cde0d87c661829449f10afe1090` |
| `象/象不忘` — `象/翻译契约` | how | no-evidence | 象 | 2026-09-27 | `595af63af6582cde0d87c661829449f10afe1090` |
| `象/释义非授` — `象/翻译契约` | why | no-evidence | 象 | 2026-09-27 | `a3514b95260373f33bbc0b27f1e877d7e128e03e` |

### `象/象不忘` — degree 14; uncorroborated/no-evidence degree 14

| edge | directive | status | set | authored date | authored sha |
| --- | --- | --- | --- | --- | --- |
| `象/以史为据` — `象/象不忘` | why | no-evidence | 象 | 2026-09-26 | `f03965c8d392422295b5fbc1cea63dcb2c991b34` |
| `象/收回亦是行` — `象/象不忘` | why | no-evidence | 象 | 2026-09-26 | `f03965c8d392422295b5fbc1cea63dcb2c991b34` |
| `象/欠与被欠` — `象/象不忘` | why | no-evidence | 象 | 2026-09-26 | `f03965c8d392422295b5fbc1cea63dcb2c991b34` |
| `象/答必真且中的` — `象/象不忘` | see-also | no-evidence | 象 | 2026-09-23 | `b871428be45348763e296c10cba95574d083c19f` |
| `象/翻译契约` — `象/象不忘` | see-also | no-evidence | 象 | 2026-09-23 | `6894eeff89652476ba303acac82a003a7dd3b988` |
| `象/视图出于史` — `象/象不忘` | why | no-evidence | 象 | 2026-09-26 | `f03965c8d392422295b5fbc1cea63dcb2c991b34` |
| `象/言即行` — `象/象不忘` | see-also | no-evidence | 象 | 2026-09-23 | `10643d94dbe8371035e0a3cc02be1565362822d0` |
| `象/象不忘` — `pattern-interpretation/cite-the-source-bytes` | see-also | no-evidence | 象 | 2026-09-23 | `10643d94dbe8371035e0a3cc02be1565362822d0` |
| `象/象不忘` — `translation/bind-to-the-source` | see-also | no-evidence | 象 | 2026-09-23 | `10643d94dbe8371035e0a3cc02be1565362822d0` |
| `象/象不忘` — `象/同构方为同` | how | no-evidence | 象 | 2026-09-27 | `595af63af6582cde0d87c661829449f10afe1090` |
| `象/象不忘` — `象/名分有据` | how | no-evidence | 象 | 2026-09-27 | `595af63af6582cde0d87c661829449f10afe1090` |
| `象/象不忘` — `象/引非读` | how | no-evidence | 象 | 2026-09-27 | `595af63af6582cde0d87c661829449f10afe1090` |
| `象/象不忘` — `象/措施随案` | how | no-evidence | 象 | 2026-09-27 | `595af63af6582cde0d87c661829449f10afe1090` |
| `象/象不忘` — `象/收回亦是行` | how | no-evidence | 象 | 2026-09-27 | `595af63af6582cde0d87c661829449f10afe1090` |
| `象/象不忘` — `象/答必真且中的` | how | no-evidence | 象 | 2026-09-27 | `595af63af6582cde0d87c661829449f10afe1090` |
| `象/象不忘` — `象/翻译契约` | how | no-evidence | 象 | 2026-09-27 | `595af63af6582cde0d87c661829449f10afe1090` |
| `象/象不忘` — `象/要约与接受` | how | no-evidence | 象 | 2026-09-27 | `595af63af6582cde0d87c661829449f10afe1090` |
| `象/象不忘` — `象/言即行` | see-also | no-evidence | 象 | 2026-09-23 | `10643d94dbe8371035e0a3cc02be1565362822d0` |

### `aif/expected-free-energy-scorecard` — degree 13; uncorroborated/no-evidence degree 13

| edge | directive | status | set | authored date | authored sha |
| --- | --- | --- | --- | --- | --- |
| `aif/belief-aware-risk-term` — `aif/expected-free-energy-scorecard` | see-also | no-evidence | aif | 2026-08-30 | `701a729f2bf3a37829e8e3bf5eb5777a5f4db226` |
| `aif/currency-before-merge` — `aif/expected-free-energy-scorecard` | see-also | no-evidence | aif | 2026-08-30 | `701a729f2bf3a37829e8e3bf5eb5777a5f4db226` |
| `aif/expected-free-energy-scorecard` — `problems/catalogue-r5-efe-label-auditability` | why | no-evidence | aif | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `aif/expected-free-energy-scorecard` — `problems/g-over-cascade-is-undefined` | why | no-evidence | aif | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `aif/expected-free-energy-scorecard` — `problems/r5-expected-free-energy-core` | why | no-evidence | aif | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `aif/expected-free-energy-scorecard` — `problems/satisfied-rungs-are-counted-and-surfaced` | why | no-evidence | aif | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `aif/expected-free-energy-scorecard` — `war-room/wr-27-a-loop-is-born-instrumented-for-its-gain` | why | no-evidence | aif | 2026-09-23 | `21a9199ff95e850cdad64e35c81de04eade48256` |
| `aif/free-energy-as-tick-scalar` — `aif/expected-free-energy-scorecard` | see-also | no-evidence | aif | 2026-08-30 | `701a729f2bf3a37829e8e3bf5eb5777a5f4db226` |
| `aif/niche-construction` — `aif/expected-free-energy-scorecard` | references | no-evidence | aif | 2026-05-27 | `81a076ec45c6b81d2771b337323373f1e83c2d47` |
| `aif/placeholder-is-load-bearing` — `aif/expected-free-energy-scorecard` | see-also | no-evidence | aif | 2026-08-30 | `3db55f3e15639df85142af7d11d82d5e4e153b11` |
| `aif/policy-precision-commitment-temperature` — `aif/expected-free-energy-scorecard` | see-also | no-evidence | aif | 2026-08-30 | `701a729f2bf3a37829e8e3bf5eb5777a5f4db226` |
| `aif/shared-kernel-predictive-forward-model` — `aif/expected-free-energy-scorecard` | see-also | no-evidence | aif | 2026-08-30 | `3db55f3e15639df85142af7d11d82d5e4e153b11` |
| `aif/term-to-channel-traceability` — `aif/expected-free-energy-scorecard` | how | no-evidence | aif | 2026-08-30 | `701a729f2bf3a37829e8e3bf5eb5777a5f4db226` |

## (d) Bridges and articulation points

Bridges: 120. Articulation points: 93.

Articulation points: `agent/eat-your-own-tail`, `agent/handoff-preserves-context`, `agent/student-dispatch`, `agent/trail-enables-return`, `ai4ci/ai-core-function`, `ai4ci/collective-intelligence-evaluation`, `ai4ci/collective-intelligence-framing`, `ai4ci/funding-justification`, `ai4ci/pilot-feasibility`, `aif/candidate-pattern-action-space`, `aif/evidence-precision-registry`, `aif/expected-free-energy-scorecard`, `aif/free-energy-as-tick-scalar`, `aif/grounded-actuation-not-reobservation`, `aif/niche-construction`, `aif/policy-precision-commitment-temperature`, `aif/scheduled-observer-entrypoint`, `aif/shared-kernel-predictive-forward-model`, `baldwin/ARGUMENT`, `campaign-coherence/cross-mission-escrow`, `campaign-coherence/shared-standard-has-no-single-owner`, `cascade-construction/borrow-a-sibling-cascade`, `cascade-construction/choose-the-grain-where-state-lives`, `cascade-construction/mine-patterns-from-incidents`, `cascades/declared-skeleton`, `code-coherence/dead-code-hygiene`, `coordination/cross-validation-protocol`, `coordination/pattern-search-protocol`, `devmap-coherence/prototype-alignment-embedding`, `eight-gates/press-mechanism`, `equity/protected-interaction-as-published`, `f6/stratum-bridge`, `forward-model/graduate-strategy-by-outcome`, `gauntlet/modeline-persists-across-worlds`, `gauntlet/placenta-transfer`, `gauntlet/teaching-inversion`, `math-formalization-CA/measure-integration-api`, `math-formalization-CA/riemann-darboux-api`, `math-formalization-CA/series-evaluation-api`, `math-formalization-CA/uniform-continuity-boundedness`, `math-formalization-CV/holomorphic-disk-api`, `math-formalization-GN/connectedness-component-api`, `math-informal-CT/check-it-on-generators`, `math-informal-NA/numerical-scout`, `math-strategy/characterization-result`, `math-strategy/corpus-trust-protocol`, `math-strategy/existence-result`, `math-strategy/proof-architecture`, `math-strategy/property-of-object-result`, `memory/surface-the-closed-set`, `mmca/persistence-over-appearance`, `orchestration/pattern-warranted-choice-point`, `pattern-coherence/internal-coherence`, `pattern-discipline/peripheral-as-sokoban`, `peeragogy/discerning-a-pattern`, `peeragogy/pattern-language`, `peripherals/canonical-typed-event-vs-side-channel`, `peripherals/progress-heartbeat-distinct-from-cycle-completion`, `plos-npt-with-small-n/framework-spine-with-empirical-ribs`, `problems/devmap-self-consistency-standards`, `problems/tensions-are-navigated-not-eliminated`, `sidecar/explicit-promotion-to-facts`, `sidecar/validation-enforcement-gate`, `social/explicit-exit-over-abandonment`, `stack-coherence/evidence-ledger`, `stack-coherence/futon1-storage-coherence`, `translation/revise-the-source-under-pressure`, `ukrns/open-the-triangle`, `ukrns/publication-cadence`, `vsatelier/cluster-as-agenda`, `vsatelier/decision-provenance`, `vsatlas/minimal-linking-pilot`, `vsatlas/non-destructive-relational-layers`, `vsatlas/pilot-offers`, `vsatlas/proof-of-concept-scope`, `vsatlatarium/anthology-as-filter`, `vsatlatarium/cached-layout-computation`, `vsatlatarium/cluster-as-story`, `war-machine/advanceability`, `war-room/wr-0-organise-without-apparatus`, `war-room/wr-10-next-move-surface-is-recursive-closure`, `war-room/wr-16-operationalised-exploit-loops-are-first-class-observation-channels`, `war-room/wr-18-war-machine-is-demonstrated-not-hypothesised`, `war-room/wr-19-tension-must-generate-not-only-rank`, `war-room/wr-25-good-news-gets-the-same-evidence-discipline-as-bad`, `war-room/wr-26-a-capability-switched-off-carries-its-re-arm-condition-in-writing-at-the-switch`, `war-room/wr-27-a-loop-is-born-instrumented-for-its-gain`, `war-room/wr-4-inhabit-before-building`, `war-room/wr-8-typed-files-are-sources-of-truth`, `war-room/wr-9-bites-empirical-or-logical`, `象/同构方为同`, `象/翻译契约`, `象/裁亦有失`.

### Editorial list: uncorroborated/no-evidence bridges

| edge | directive | status | file | authored sha/date | detached piece |
| --- | --- | --- | --- | --- | ---: |
| `agent/eat-your-own-tail` — `agent/trail-enables-return` | why | no-evidence | `agent/eat-your-own-tail.flexiarg` | `32142eb722428298a20af02a13519739688083d3` / 2026-10-02 | 2 |
| `agent/handoff-preserves-context` — `problems/process-conduct-is-unassured` | why | uncorroborated | `agent/handoff-preserves-context.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `agent/trail-enables-return` — `problems/unqueryable-trails-block-recovery` | why | no-evidence | `agent/trail-enables-return.flexiarg` | `6d0efe39ecbc3d1452a0c95d15fc3754be0a6f52` / 2026-09-06 | 1 |
| `ai4ci/ai-core-function` — `problems/unlinked-reasoning-cues-require-rediscovery` | why | no-evidence | `ai4ci/ai-core-function.flexiarg` | `67013612b64d3a479cffdf4924a030f260a71a70` / 2026-09-06 | 1 |
| `ai4ci/collective-intelligence-evaluation` — `vsatlas/festival-model` | references | no-evidence | `ai4ci/collective-intelligence-evaluation.flexiarg` | `521967f5c4959f67f858ce422c63cc8feed351bc` / 2025-12-31 | 1 |
| `ai4ci/collective-intelligence-framing` — `corps/working-where-others-can-see` | references | no-evidence | `ai4ci/collective-intelligence-framing.flexiarg` | `521967f5c4959f67f858ce422c63cc8feed351bc` / 2025-12-31 | 1 |
| `ai4ci/funding-justification` — `problems/unitemized-ai4ci-budget-looks-speculative` | why | no-evidence | `ai4ci/funding-justification.flexiarg` | `67013612b64d3a479cffdf4924a030f260a71a70` / 2026-09-06 | 1 |
| `ai4ci/funding-justification` — `repository-transition/transformation-engine` | references | no-evidence | `ai4ci/funding-justification.flexiarg` | `521967f5c4959f67f858ce422c63cc8feed351bc` / 2025-12-31 | 1 |
| `ai4ci/pilot-feasibility` — `problems/ai4ci-scope-can-outgrow-six-month-pilot` | why | no-evidence | `ai4ci/pilot-feasibility.flexiarg` | `67013612b64d3a479cffdf4924a030f260a71a70` / 2026-09-06 | 1 |
| `aif/candidate-pattern-action-space` — `problems/r6-candidate-action-space-and-selection` | why | no-evidence | `aif/candidate-pattern-action-space.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `aif/evidence-precision-registry` — `exotic/full-lift-registry` | see-also | uncorroborated | `aif/evidence-precision-registry.flexiarg` | `2761dc09a20d43bf896d8fb0a85acf6f51bea431` / 2026-08-30 | 1 |
| `aif/evidence-precision-registry` — `problems/r7-evidence-channel-precision` | why | no-evidence | `aif/evidence-precision-registry.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `aif/expected-free-energy-scorecard` — `problems/catalogue-r5-efe-label-auditability` | why | no-evidence | `aif/expected-free-energy-scorecard.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `aif/expected-free-energy-scorecard` — `problems/g-over-cascade-is-undefined` | why | no-evidence | `aif/expected-free-energy-scorecard.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `aif/free-energy-as-tick-scalar` — `problems/r8-present-fit-mismatch` | why | uncorroborated | `aif/free-energy-as-tick-scalar.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `aif/grounded-actuation-not-reobservation` — `problems/r16-grounded-actuation` | why | no-evidence | `aif/grounded-actuation-not-reobservation.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `aif/policy-precision-commitment-temperature` — `problems/r14-commitment-temperature` | why | uncorroborated | `aif/policy-precision-commitment-temperature.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `aif/scheduled-observer-entrypoint` — `problems/r10-scheduled-entrypoint` | why | no-evidence | `aif/scheduled-observer-entrypoint.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `aif/shared-kernel-predictive-forward-model` — `problems/r4-forward-model` | why | uncorroborated | `aif/shared-kernel-predictive-forward-model.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `baldwin/ARGUMENT` — `eight-gates/press-mechanism` | references | no-evidence | `baldwin/ARGUMENT.flexiarg` | `5e7ce2b2491078c389090aff44078f6c92d13f45` / 2026-08-01 | 17 |
| `campaign-coherence/cross-mission-escrow` — `futon-theory/mission-dependency` | references | uncorroborated | `campaign-coherence/cross-mission-escrow.flexiarg` | `9d73323b453cd3af36dc480ddea3fb97a6d3bbc7` / 2026-06-12 | 1 |
| `campaign-coherence/cross-mission-escrow` — `futon-theory/mission-interface-signature` | references | uncorroborated | `campaign-coherence/cross-mission-escrow.flexiarg` | `9d73323b453cd3af36dc480ddea3fb97a6d3bbc7` / 2026-06-12 | 1 |
| `cascade-construction/borrow-a-sibling-cascade` — `cascades/edges-earn-permanence` | see-also | no-evidence | `cascade-construction/borrow-a-sibling-cascade.flexiarg` | `29b77c55fa3f8f4ecd544a7a2971005a8092f93b` / 2026-09-17 | 1 |
| `cascade-construction/choose-the-grain-where-state-lives` — `cascades/the-slice-is-the-unit-of-use` | see-also | uncorroborated | `cascade-construction/choose-the-grain-where-state-lives.flexiarg` | `29b77c55fa3f8f4ecd544a7a2971005a8092f93b` / 2026-09-17 | 1 |
| `cascade-construction/mine-patterns-from-incidents` — `pattern-mining/probe-the-claimed-property-not-the-acceptance-proxy` | references | no-evidence | `cascade-construction/mine-patterns-from-incidents.flexiarg` | `29b77c55fa3f8f4ecd544a7a2971005a8092f93b` / 2026-09-17 | 1 |
| `cascades/declared-skeleton` — `problems/undeclared-cascades-cannot-be-audited` | why | no-evidence | `cascades/declared-skeleton.flexiarg` | `f6ca479d5dfe12050e51e85ba64711d059481260` / 2026-09-06 | 1 |
| `code-coherence/dead-code-hygiene` — `repository-transition/diagnostics` | references | no-evidence | `code-coherence/dead-code-hygiene.flexiarg` | `521967f5c4959f67f858ce422c63cc8feed351bc` / 2025-12-31 | 1 |
| `code-coherence/dead-code-hygiene` — `repository-transition/strawman` | references | no-evidence | `code-coherence/dead-code-hygiene.flexiarg` | `521967f5c4959f67f858ce422c63cc8feed351bc` / 2025-12-31 | 1 |
| `coordination/cross-validation-protocol` — `futon-theory/coordination-protocol` | references | uncorroborated | `coordination/cross-validation-protocol.flexiarg` | `fb533ce9e56b42f326259a029bd072f128ee52c0` / 2026-05-03 | 1 |
| `coordination/pattern-search-protocol` — `problems/coordination-patterns-derivation` | why | no-evidence | `coordination/pattern-search-protocol.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `devmap-coherence/status-keywords` — `problems/devmap-self-consistency-standards` | why | no-evidence | `devmap-coherence/status-keywords.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `eight-gates/an-push` — `problems/tensions-are-navigated-not-eliminated` | why | uncorroborated | `eight-gates/an-push.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `eight-gates/cai-pluck` — `problems/tensions-are-navigated-not-eliminated` | why | uncorroborated | `eight-gates/cai-pluck.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `eight-gates/elbow-immediate` — `problems/tensions-are-navigated-not-eliminated` | why | uncorroborated | `eight-gates/elbow-immediate.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `eight-gates/ji-press` — `problems/tensions-are-navigated-not-eliminated` | why | no-evidence | `eight-gates/ji-press.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `eight-gates/kao-lean` — `problems/tensions-are-navigated-not-eliminated` | why | no-evidence | `eight-gates/kao-lean.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `eight-gates/lean-commit` — `problems/tensions-are-navigated-not-eliminated` | why | no-evidence | `eight-gates/lean-commit.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `eight-gates/lie-split` — `problems/tensions-are-navigated-not-eliminated` | why | uncorroborated | `eight-gates/lie-split.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `eight-gates/lu-roll-back` — `problems/tensions-are-navigated-not-eliminated` | why | no-evidence | `eight-gates/lu-roll-back.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `eight-gates/peng-ward-off` — `problems/tensions-are-navigated-not-eliminated` | why | no-evidence | `eight-gates/peng-ward-off.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `eight-gates/pluck-extract` — `problems/tensions-are-navigated-not-eliminated` | why | uncorroborated | `eight-gates/pluck-extract.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `eight-gates/press-mechanism` — `problems/tensions-are-navigated-not-eliminated` | why | uncorroborated | `eight-gates/press-mechanism.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 16 |
| `eight-gates/push-warrant` — `problems/tensions-are-navigated-not-eliminated` | why | no-evidence | `eight-gates/push-warrant.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `eight-gates/roll-back-hold` — `problems/tensions-are-navigated-not-eliminated` | why | no-evidence | `eight-gates/roll-back-hold.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `eight-gates/split-isolate` — `problems/tensions-are-navigated-not-eliminated` | why | uncorroborated | `eight-gates/split-isolate.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `eight-gates/ward-off-boundary` — `problems/tensions-are-navigated-not-eliminated` | why | no-evidence | `eight-gates/ward-off-boundary.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `eight-gates/zhou-elbow` — `problems/tensions-are-navigated-not-eliminated` | why | no-evidence | `eight-gates/zhou-elbow.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `equity/protected-interaction-as-published` — `problems/trainers-feel-unequipped-to-deliver` | why | no-evidence | `equity/protected-interaction-as-published.flexiarg` | `81126d0b4519cdf35926769bbc2f4e04f47a1713` / 2026-09-06 | 1 |
| `f6/stratum-bridge` — `hdm/deep-storage-to-active-graph` | references | no-evidence | `f6/stratum-bridge.flexiarg` | `b94d619861b5778476e0e8052d67e3e3b4d944c6` / 2026-02-09 | 1 |
| `forward-model/flow-toward-controlled-vocabulary` — `war-room/wr-8-typed-files-are-sources-of-truth` | why | uncorroborated | `forward-model/flow-toward-controlled-vocabulary.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `forward-model/graduate-strategy-by-outcome` — `war-room/wr-9-bites-empirical-or-logical` | why | no-evidence | `forward-model/graduate-strategy-by-outcome.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 2 |
| `forward-model/inhabit-supplies-labels` — `war-room/wr-4-inhabit-before-building` | why | no-evidence | `forward-model/inhabit-supplies-labels.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `forward-model/price-your-own-moves` — `war-room/wr-10-next-move-surface-is-recursive-closure` | why | uncorroborated | `forward-model/price-your-own-moves.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `forward-model/reward-supersedes-evidence` — `war-room/wr-9-bites-empirical-or-logical` | why | no-evidence | `forward-model/reward-supersedes-evidence.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `forward-model/seal-the-graduation-criteria` — `forward-model/graduate-strategy-by-outcome` | why | no-evidence | `forward-model/seal-the-graduation-criteria.flexiarg` | `21a9199ff95e850cdad64e35c81de04eade48256` / 2026-09-23 | 1 |
| `gauntlet/modeline-persists-across-worlds` — `problems/peripheral-hops-reset-self-regulation` | why | no-evidence | `gauntlet/modeline-persists-across-worlds.flexiarg` | `0cde9d4a9f17f788428374815406216e1371df4d` / 2026-09-06 | 1 |
| `gauntlet/placenta-transfer` — `agent/pause-is-not-failure` | references | uncorroborated | `gauntlet/placenta-transfer.flexiarg` | `459cb1744803a26ed3a550f062730154c6851fca` / 2026-02-27 | 1 |
| `gauntlet/teaching-inversion` — `problems/agent-discoveries-do-not-teach-humans` | why | no-evidence | `gauntlet/teaching-inversion.flexiarg` | `0cde9d4a9f17f788428374815406216e1371df4d` / 2026-09-06 | 1 |
| `library-coherence/library-embedding-refresh` — `devmap-coherence/prototype-alignment-embedding` | references | no-evidence | `library-coherence/library-embedding-refresh.flexiarg` | `521967f5c4959f67f858ce422c63cc8feed351bc` / 2025-12-31 | 1 |
| `library-coherence/library-evidence-ledger` — `stack-coherence/evidence-ledger` | references | no-evidence | `library-coherence/library-evidence-ledger.flexiarg` | `521967f5c4959f67f858ce422c63cc8feed351bc` / 2025-12-31 | 1 |
| `math-formalization-CA/measure-integration-api` — `math-formalization/transport-across-an-instance-diamond` | see-also | no-evidence | `math-formalization-CA/measure-integration-api.flexiarg` | `53ef3cd71198a3d66143bf990359a71770755d9d` / 2026-08-17 | 1 |
| `math-formalization-CA/riemann-darboux-api` — `math-formalization-CA/ae-integral-zero` | see-also | no-evidence | `math-formalization-CA/riemann-darboux-api.flexiarg` | `04ad31526f101893bee461d43b34a629fd8b9bac` / 2026-08-17 | 1 |
| `math-formalization-CA/series-evaluation-api` — `math-formalization-CA/rpow-exponent-limit` | see-also | no-evidence | `math-formalization-CA/series-evaluation-api.flexiarg` | `04ad31526f101893bee461d43b34a629fd8b9bac` / 2026-08-17 | 1 |
| `math-formalization-CA/uniform-continuity-boundedness` — `math-formalization-CA/metric-cauchy-convergence` | see-also | no-evidence | `math-formalization-CA/uniform-continuity-boundedness.flexiarg` | `04ad31526f101893bee461d43b34a629fd8b9bac` / 2026-08-17 | 1 |
| `math-formalization-CV/frontier-bound-from-arc-hypotheses` — `math-formalization-CV/holomorphic-disk-api` | see-also | no-evidence | `math-formalization-CV/frontier-bound-from-arc-hypotheses.flexiarg` | `d90ce135148db1c85a8cd48d71c6584c4d859c1b` / 2026-08-28 | 1 |
| `math-formalization-CV/frontier-bound-from-arc-hypotheses` — `math-formalization-CV/holomorphic-disk-api` | why | no-evidence | `math-formalization-CV/frontier-bound-from-arc-hypotheses.flexiarg` | `d90ce135148db1c85a8cd48d71c6584c4d859c1b` / 2026-08-28 | 1 |
| `math-formalization-GN/connectedness-component-api` — `math-formalization-GN/connected-union-via-common-point` | see-also | no-evidence | `math-formalization-GN/connectedness-component-api.flexiarg` | `04ad31526f101893bee461d43b34a629fd8b9bac` / 2026-08-17 | 1 |
| `math-informal-CT/check-it-on-generators` — `math-informal/try-a-simpler-case` | references | no-evidence | `math-informal-CT/check-it-on-generators.flexiarg` | `034827005e677a82e63131d3d3d3afc55b73b81e` / 2026-08-10 | 1 |
| `math-informal-NA/numerical-scout` — `math-informal/work-examples-first` | references | no-evidence | `math-informal-NA/numerical-scout.flexiarg` | `459cb1744803a26ed3a550f062730154c6851fca` / 2026-02-27 | 1 |
| `math-strategy/characterization-result` — `math-strategy/hypothesis-category-check` | references | no-evidence | `math-strategy/characterization-result.flexiarg` | `067845796f869e7057fad083a5a7f8b15797fcf7` / 2026-08-17 | 1 |
| `math-strategy/corpus-trust-protocol` — `math-strategy/non-circularity-check` | why | no-evidence | `math-strategy/corpus-trust-protocol.flexiarg` | `53ef3cd71198a3d66143bf990359a71770755d9d` / 2026-08-17 | 1 |
| `math-strategy/existence-result` — `math-informal-CO/use-probabilistic-method` | references | no-evidence | `math-strategy/existence-result.flexiarg` | `067845796f869e7057fad083a5a7f8b15797fcf7` / 2026-08-17 | 1 |
| `math-strategy/proof-architecture` — `math-informal/separate-into-independent-pieces` | why | no-evidence | `math-strategy/proof-architecture.flexiarg` | `53ef3cd71198a3d66143bf990359a71770755d9d` / 2026-08-17 | 1 |
| `math-strategy/property-of-object-result` — `math-informal-CA/monotone-approximation` | references | no-evidence | `math-strategy/property-of-object-result.flexiarg` | `067845796f869e7057fad083a5a7f8b15797fcf7` / 2026-08-17 | 1 |
| `memory/name-a-reusable-artifact` — `memory/surface-the-closed-set` | references | no-evidence | `memory/name-a-reusable-artifact.flexiarg` | `7b533bdb251197a1aa9c28f317f05a2d4d1ef312` / 2026-08-04 | 1 |
| `musn/expensive-move-consent` — `aif/niche-construction` | see-also | uncorroborated | `musn/expensive-move-consent.flexiarg` | `be8c7077713b1b2d8c0d0d5d8eafbcda7d33b81c` / 2026-08-30 | 1 |
| `pattern-discipline/peripheral-as-sokoban` — `pattern-coherence/internal-coherence` | references | no-evidence | `pattern-discipline/peripheral-as-sokoban.flexiarg` | `8d8c82074e5b8519a9565ef4a6110b5492914da0` / 2026-05-04 | 25 |
| `peeragogy/discerning-a-pattern` — `pattern-coherence/applicability-signals` | references | uncorroborated | `peeragogy/discerning-a-pattern.flexiarg` | `f7ea5537a64698b0f71b64a748562d9cf8a002ee` / 2026-05-03 | 1 |
| `peeragogy/pattern-language` — `pattern-coherence/internal-coherence` | references | uncorroborated | `peeragogy/pattern-language.flexiarg` | `f7ea5537a64698b0f71b64a748562d9cf8a002ee` / 2026-05-03 | 24 |
| `peripherals/canonical-typed-event-vs-side-channel` — `realtime/structured-events-only` | references | no-evidence | `peripherals/canonical-typed-event-vs-side-channel.flexiarg` | `f15b91d40a268c3965f883867d5e5a065243415d` / 2026-05-27 | 1 |
| `peripherals/progress-heartbeat-distinct-from-cycle-completion` — `realtime/loop-failure-signals` | references | no-evidence | `peripherals/progress-heartbeat-distinct-from-cycle-completion.flexiarg` | `f15b91d40a268c3965f883867d5e5a065243415d` / 2026-05-27 | 1 |
| `process-coherence/refusal-becomes-pattern` — `orchestration/pattern-warranted-choice-point` | references | uncorroborated | `process-coherence/refusal-becomes-pattern.flexiarg` | `ab277bdd6b6aa3f587e09625c750d258d61a7f92` / 2026-09-08 | 1 |
| `process/built-but-not-wired-invisibility` — `war-room/wr-27-a-loop-is-born-instrumented-for-its-gain` | why | uncorroborated | `process/built-but-not-wired-invisibility.flexiarg` | `59a31a5a38aace5f810eb7c24dd43f4b14419a69` / 2026-09-05 | 1 |
| `process/detection-wired-to-single-catastrophic-response` — `war-room/wr-26-a-capability-switched-off-carries-its-re-arm-condition-in-writing-at-the-switch` | why | no-evidence | `process/detection-wired-to-single-catastrophic-response.flexiarg` | `59a31a5a38aace5f810eb7c24dd43f4b14419a69` / 2026-09-05 | 1 |
| `process/fake-done-via-binary-closure` — `war-room/wr-25-good-news-gets-the-same-evidence-discipline-as-bad` | why | uncorroborated | `process/fake-done-via-binary-closure.flexiarg` | `59a31a5a38aace5f810eb7c24dd43f4b14419a69` / 2026-09-05 | 1 |
| `process/recorded-but-not-surfaced` — `war-room/wr-16-operationalised-exploit-loops-are-first-class-observation-channels` | why | uncorroborated | `process/recorded-but-not-surfaced.flexiarg` | `59a31a5a38aace5f810eb7c24dd43f4b14419a69` / 2026-09-05 | 1 |
| `process/reflexivity-failure-in-a-gate` — `war-room/wr-18-war-machine-is-demonstrated-not-hypothesised` | why | no-evidence | `process/reflexivity-failure-in-a-gate.flexiarg` | `59a31a5a38aace5f810eb7c24dd43f4b14419a69` / 2026-09-05 | 1 |
| `process/spec-drift-by-displacement` — `war-room/wr-8-typed-files-are-sources-of-truth` | why | uncorroborated | `process/spec-drift-by-displacement.flexiarg` | `59a31a5a38aace5f810eb7c24dd43f4b14419a69` / 2026-09-05 | 1 |
| `process/spec-silence-pair` — `war-room/wr-9-bites-empirical-or-logical` | why | no-evidence | `process/spec-silence-pair.flexiarg` | `59a31a5a38aace5f810eb7c24dd43f4b14419a69` / 2026-09-05 | 1 |
| `process/watcher-that-observes-and-defers` — `war-room/wr-19-tension-must-generate-not-only-rank` | why | uncorroborated | `process/watcher-that-observes-and-defers.flexiarg` | `59a31a5a38aace5f810eb7c24dd43f4b14419a69` / 2026-09-05 | 1 |
| `sidecar/explicit-promotion-to-facts` — `sidecar/fact-lifecycle-event-types` | see-also | no-evidence | `sidecar/explicit-promotion-to-facts.flexiarg` | `be8c7077713b1b2d8c0d0d5d8eafbcda7d33b81c` / 2026-08-30 | 1 |
| `sidecar/explicit-promotion-to-facts` — `sidecar/validation-enforcement-gate` | how | no-evidence | `sidecar/explicit-promotion-to-facts.flexiarg` | `be8c7077713b1b2d8c0d0d5d8eafbcda7d33b81c` / 2026-08-30 | 2 |
| `social/explicit-exit-over-abandonment` — `futon-theory/mission-scoping` | references | no-evidence | `social/explicit-exit-over-abandonment.flexiarg` | `193be6ef0edc2dcd404b030ae4e83a9237f4fa3d` / 2026-02-09 | 1 |
| `stack-coherence/futon1-storage-coherence` — `sidecar/validation-enforcement-gate` | references | no-evidence | `stack-coherence/futon1-storage-coherence.flexiarg` | `79ccbd19446b1818b630d77825e721f0910bb413` / 2026-01-31 | 3 |
| `storage/deterministic-ingest-pipeline` — `stack-coherence/evidence-ledger` | references | uncorroborated | `storage/deterministic-ingest-pipeline.flexiarg` | `ee9a5e785db2a5ed37986853a1765adcd753cfe6` / 2026-02-07 | 1 |
| `ukrns/cycle-position-publication` — `ukrns/open-the-triangle` | references | no-evidence | `ukrns/cycle-position-publication.flexiarg` | `cfb521524771c795a916c6e767aae84162590655` / 2026-05-25 | 1 |
| `ukrns/publication-cadence` — `problems/undeclared-publication-cadence-obscures-version` | why | no-evidence | `ukrns/publication-cadence.flexiarg` | `7c653bb9912f504b51d0b2ef8da4c32158ceb9ac` / 2026-09-06 | 1 |
| `vsatelier/cluster-as-agenda` — `vsatlatarium/cluster-as-story` | references | no-evidence | `vsatelier/cluster-as-agenda.flexiarg` | `0c928c019e9274b11ada4ebfe7929eb4d54592ac` / 2026-03-28 | 6 |
| `vsatelier/decision-provenance` — `vsatelier/cluster-as-agenda` | references | no-evidence | `vsatelier/decision-provenance.flexiarg` | `0c928c019e9274b11ada4ebfe7929eb4d54592ac` / 2026-03-28 | 4 |
| `vsatelier/decision-provenance` — `vsatlas/stewardship-layer` | references | no-evidence | `vsatelier/decision-provenance.flexiarg` | `0c928c019e9274b11ada4ebfe7929eb4d54592ac` / 2026-03-28 | 1 |
| `vsatelier/federated-cosmoses` — `vsatlatarium/anthology-as-filter` | references | no-evidence | `vsatelier/federated-cosmoses.flexiarg` | `0c928c019e9274b11ada4ebfe7929eb4d54592ac` / 2026-03-28 | 1 |
| `vsatelier/session-as-rehearsal` — `vsatelier/cluster-as-agenda` | references | no-evidence | `vsatelier/session-as-rehearsal.flexiarg` | `0c928c019e9274b11ada4ebfe7929eb4d54592ac` / 2026-03-28 | 1 |
| `vsatlas/pilot-offers` — `vsatlas/minimal-linking-pilot` | references | no-evidence | `vsatlas/pilot-offers.flexiarg` | `07e25277ee94678cb05e051ab698d13f13226f9a` / 2025-12-29 | 2 |
| `vsatlas/pilot-offers` — `vsatlas/offer-ladder` | references | no-evidence | `vsatlas/pilot-offers.flexiarg` | `07e25277ee94678cb05e051ab698d13f13226f9a` / 2025-12-29 | 1 |
| `vsatlatarium/anthology-as-filter` — `vsatlas/minimal-linking-pilot` | references | uncorroborated | `vsatlatarium/anthology-as-filter.flexiarg` | `6d62c1beb152bc01c9412357c6b632b748ead707` / 2026-03-28 | 2 |
| `vsatlatarium/arc-as-typed-relation` — `vsatlas/non-destructive-relational-layers` | references | no-evidence | `vsatlatarium/arc-as-typed-relation.flexiarg` | `6d62c1beb152bc01c9412357c6b632b748ead707` / 2026-03-28 | 1 |
| `vsatlatarium/cached-layout-computation` — `vsatlas/proof-of-concept-scope` | references | no-evidence | `vsatlatarium/cached-layout-computation.flexiarg` | `6d62c1beb152bc01c9412357c6b632b748ead707` / 2026-03-28 | 10 |
| `vsatlatarium/cached-layout-computation` — `vsatlatarium/cluster-as-story` | references | no-evidence | `vsatlatarium/cached-layout-computation.flexiarg` | `6d62c1beb152bc01c9412357c6b632b748ead707` / 2026-03-28 | 9 |
| `vsatlatarium/cluster-as-story` — `vsatlas/non-destructive-relational-layers` | references | no-evidence | `vsatlatarium/cluster-as-story.flexiarg` | `6d62c1beb152bc01c9412357c6b632b748ead707` / 2026-03-28 | 2 |
| `war-room/wr-10-next-move-surface-is-recursive-closure` — `war-room/wr-8-typed-files-are-sources-of-truth` | why | uncorroborated | `war-room/wr-10-next-move-surface-is-recursive-closure.flexiarg` | `882da1e56a06d7aafdd2226ac2776644ffdec9e6` / 2026-09-02 | 3 |
| `war-room/wr-14-predecessor-completion-pull-is-a-fourth-next-move-criterion` — `war-room/wr-10-next-move-surface-is-recursive-closure` | why | uncorroborated | `war-room/wr-14-predecessor-completion-pull-is-a-fourth-next-move-criterion.flexiarg` | `882da1e56a06d7aafdd2226ac2776644ffdec9e6` / 2026-09-02 | 1 |
| `war-room/wr-18-war-machine-is-demonstrated-not-hypothesised` — `war-room/wr-9-bites-empirical-or-logical` | why | no-evidence | `war-room/wr-18-war-machine-is-demonstrated-not-hypothesised.flexiarg` | `882da1e56a06d7aafdd2226ac2776644ffdec9e6` / 2026-09-02 | 2 |
| `war-room/wr-21-geometrization-is-a-shared-cross-mission-dependency` — `campaign-coherence/shared-standard-has-no-single-owner` | references | no-evidence | `war-room/wr-21-geometrization-is-a-shared-cross-mission-dependency.flexiarg` | `39de634bd7da6a32302e99bc036a16da51f7ae4e` / 2026-08-21 | 1 |
| `war-room/wr-23-upstream-trackers-are-stack-surfaces` — `war-room/wr-0-organise-without-apparatus` | why | uncorroborated | `war-room/wr-23-upstream-trackers-are-stack-surfaces.flexiarg` | `882da1e56a06d7aafdd2226ac2776644ffdec9e6` / 2026-09-02 | 1 |
| `war-room/wr-6-daily-scan-is-depositing-heartbeat` — `war-room/wr-0-organise-without-apparatus` | why | no-evidence | `war-room/wr-6-daily-scan-is-depositing-heartbeat.flexiarg` | `882da1e56a06d7aafdd2226ac2776644ffdec9e6` / 2026-09-02 | 1 |
| `象/同构方为同` — `pattern-coherence/facet-integrity` | see-also | no-evidence | `象/同构方为同.flexiarg` | `a3514b95260373f33bbc0b27f1e877d7e128e03e` / 2026-09-27 | 1 |
| `象/翻译契约` — `proof-search/typed-hole-as-frontier` | see-also | no-evidence | `象/翻译契约.flexiarg` | `6894eeff89652476ba303acac82a003a7dd3b988` / 2026-09-23 | 1 |
| `象/裁亦有失` — `orchestration/rule-evolves-from-its-deferrals` | see-also | no-evidence | `象/裁亦有失.flexiarg` | `a3514b95260373f33bbc0b27f1e877d7e128e03e` / 2026-09-27 | 1 |

## Cross-check

This run has 1456 library nodes (-30 against Claude-5), 2472 authored edge records, 342 components (+65), giant 793 (-73), 325 singleton components (+69), and 78 sets in the giant (-19). Claude-5's cited baseline is 1486 nodes, 277 components, giant 866 across 97 sets, and 256 isolated. Differences are reported rather than tuned: this census includes @references and @see-also in addition to @why/@how, while the pinned mined graph itself names 1431 ids; the current library's node universe is 1456. Top-degree comparison is available in the hub table; this report does not assume its directives are identical to Claude-5's graph.

## Headline

- Giant size under corroborated-only authored edges: **7**
- Bridges in the authored giant: **120**
- Uncorroborated/no-evidence bridges: **118**
