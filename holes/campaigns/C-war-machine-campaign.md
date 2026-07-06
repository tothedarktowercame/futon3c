# Campaign: C-war-machine-campaign - finish the War Machine without making it eat itself

**Status:** FORMING / RALLY draft (2026-07-06, operator-directed; awaiting Joe inspection). Coordination shape: **convergent with self-reference quarantine**.
**Coordinator:** Ground Control (codex-1 for this draft; future controller [OPERATOR?]). **Principal:** Joe.
**Home:** `futon3c/holes/campaigns/` per `futon4/holes/campaign-lifecycle.md`.
**Provenance:** Joe, 2026-07-06: stop trying to dogfood individual War-Machine-related missions through the WM while building the WM; survey the related mission field, join the surviving work into one Campaign, and commission a zai team under ground-control discipline. Operator hypothesis to test: "Trying to dogfood the war machine material as we build the machine could account for some of the recurring head-bangs."

---

## HEAD (minimum obligations)

Source of record: `flight-pipeline-cards-ii.html` KNOWN GAPS box, coauthored Joe + Fable, 2026-07-06. These six gaps are the campaign's minimum tension. Each must map to at least one surviving mission or named workstream before RUN.

| Gap | Source text | Campaign mapping |
|---|---|---|
| 1. THE dF LAST INCH | THE dF LAST INCH — first-flights' best lane psi draws F -0.024, 0.024 short of the gate. Named fix: card 3's injection (seat-by-state) + per-clause multi-vector retrieval — a CONSTRUCTOR change, chartered, not built. No library or deposit work closes it. | New workstream `W-constructor-df-last-inch`; member dependencies `M-first-flights`, `E-live-loop-3`, `M-fold-ansatz`, `E-fold-embed-pipeline`. |
| 2. GAMMA UNFED | GAMMA UNFED — the learning feed is live and tested but has never eaten: needs one gated mission with a replayable deposit AND dF > 0 to enact. Blocked by gap 1 or by any mission crossing its gate first. | New workstream `W-gamma-first-meal`; member dependencies `E-live-loop-3`, `M-first-flights`, `M-fold-self-play`. |
| 3. CO-APP FROM USAGE | CO-APP FROM USAGE — the phylogeny miner doesn't scan live cascade usage; authored clusters get day-one edges, used patterns accrue nothing. The pipeline wiring's single remaining known-pending edge. | New workstream `W-coapp-live-usage-miner`; member dependencies `E-mine-mission-transitions`, `M-operational-vocabulary`, `M-G-over-cascades`. |
| 4. CANDIDATES DRIFT | CANDIDATES DRIFT — 101->108 vs the ~93 prediction; worktree/directory-copy re-pollution is the named smell. Needs a scan-root fence or per-drift explanations. | New workstream `W-candidate-drift-fence`; member dependencies `M-bounded-in-flight-state`, `M-autoclock-in`, `M-aif-faithfulness`. |
| 5. THE PROPOSER GAP | THE PROPOSER GAP — the candidate generator proposes only advance moves; close/survey/apply-cascade never exist to price, so portfolio-shaped operator value stays invisible (M-action-vocabulary's kill finding). Routed to M-aif2 slice 1, untouched. | New workstream `W-proposer-portfolio-actions`; context dependencies `M-action-vocabulary`, `M-aif2`; member dependencies `M-evaluate-policies`, `M-G-over-cascades`, `E-rollout-kill-test`. |
| 6. THE DOGFOOD QUESTION | THE DOGFOOD QUESTION — your own census answered the survey half (19/20 self-referential); the campaign's remaining obligation is the RE-SEQUENCING: which members are worked directly by the zai team under GROUND-CONTROL discipline vs through WM enactment, so the machine stops eating itself. | New workstream `W-dogfood-resequence`; campaign standard; all dogfood-risk members. |

### Minimum-obligation workstreams

| Workstream | Why it exists | Acceptance bar | Initial execution mode |
|---|---|---|---|
| `W-constructor-df-last-inch` | No current member owns card 3's seat-by-state injection plus per-clause multi-vector retrieval as a constructor build. | A constructor change is specified, built, and shown to move at least one replayable first-flights lane across the dF gate without library/deposit hand edits. | `:ground-control-direct` |
| `W-gamma-first-meal` | The gamma feed cannot prove learning until one gated mission with replayable deposit and dF > 0 enacts. | First gamma ingestion record from a qualifying mission, with replayable deposit and positive dF witness. | `:ground-control-direct` until one qualifying mission exists, then `:wm-read-only` review [OPERATOR?] |
| `W-coapp-live-usage-miner` | Existing phylogeny mining gives authored clusters day-one edges but does not accrue edges from live usage. | Live cascade usage contributes co-app edges with provenance, and the known-pending pipeline edge is closed. | `:ground-control-direct` |
| `W-candidate-drift-fence` | The 101->108 vs ~93 drift smell is not a mission feature; it is scan-root/directory hygiene. | Candidate count drift is either fenced by scan roots or every drift has a per-path explanation. | `:ground-control-direct` |
| `W-proposer-portfolio-actions` | Closed `M-action-vocabulary` found a proposer shape gap; no live member yet makes close/survey/apply-cascade candidates priceable. | Candidate generator emits and prices advance, close, survey, and apply-cascade action families against operator value. | `:ground-control-direct`; WM may observe only |
| `W-dogfood-resequence` | The census answered scale, not execution order. The campaign must decide direct-zai vs WM enactment per member. | Every campaign item has an execution mode and release dependency before dispatch; no self-referential item routes through WM enactment before release. | `:ground-control-direct` |

---

## 0. RALLY

### Muster

**Draft roster, not dispatched.** No agents have been commissioned by this document.

| Role | Responsibility | Candidate owner |
|---|---|---|
| Ground Control | Commission zai agents, enforce `GROUND-CONTROL.md`, run gates, preserve logs, split transport failures from content failures | codex-1 / future controller [OPERATOR?] |
| Campaign reviewer | Author != reviewer on every landed parcel; campaign-level stop/go calls | claude-16 [OPERATOR?] |
| WM/AIF lane lead | R5/R6/R7/R14/R16/R19 scoring and tick semantics | claude-12 / claude-5 / claude-4 cluster [OPERATOR?] |
| Fold/cascade lane lead | cascade constructor, fold interface, G-over-cascades, fold curriculum | claude-4 / claude-10 [OPERATOR?] |
| Substrate/provenance lane lead | mission registry, held-work ledger, autoclock, bounded state, substrate promotion | claude-1 / claude-2 / claude-8 [OPERATOR?] |
| External-demonstration lane lead | demonstration-foundry and outward-value evidence | TBD [OPERATOR?] |

### Coordination protocol

The campaign runs on `GROUND-CONTROL.md` plus the campaign lifecycle:

- one zai task per bounded parcel, with author != reviewer;
- no live JVM restart and no live `:7071` writes unless a parcel explicitly owns a dry-run/execute gate;
- self-referential War Machine work is **not** dispatched through WM enactment until this campaign explicitly releases that rule;
- every parcel records whether it is dogfood-risk or external-facing before execution;
- transport failures are logged separately from contract failures.

### Operator checkpoints

1. **Membership ratification:** is the surviving-member set in section 2 right?
2. **HEAD ratification:** do the six minimum obligations above match the current operator/Fable HEAD?
3. **Dogfood quarantine:** does Joe agree self-referential members are worked directly by the zai team under ground-control discipline, not by WM self-enactment?
4. **Keystone choice:** is the keystone standard the campaign-level execution/sequence contract below, or should a narrower keystone mission own it? [OPERATOR?]
5. **Lane owners:** assign named reviewers before any RUN/DISPATCH.
6. **Stop/head-banging threshold:** ratify section 6 kill criteria before execution.

**RALLY exit:** operator ratifies membership, lane owners, dogfood quarantine, and stop criteria.

---

## 1. CHARTER

### Joint goal / gap

War-Machine-related work is scattered across missions and excursions that often concern the machinery used to select, score, fold, enact, or observe the work itself. Running those missions one by one through the partially-built WM creates a self-reference trap: the system under repair becomes the judge, proposer, or execution substrate for its own repairs. The observed symptom is repeated head-banging: correct local work, but recurring confusion around scoring, provenance, pins, gates, and whether a tick can be trusted.

The 2026-07-06 HEAD makes that tension concrete: the campaign must close or route the dF last inch, the unfed gamma learning feed, live usage co-app accrual, candidate drift, the proposer portfolio gap, and the dogfood resequencing question. Those are minimum obligations, not optional nice-to-haves.

The joint goal is to finish the surviving War-Machine stack work under a single Campaign that:

1. surveys the remaining WM-related mission field;
2. sequences work by dependency and self-reference risk;
3. executes self-referential repairs through a zai team under Ground Control rather than through WM self-enactment;
4. returns the WM to dogfooding only after the relevant contract is verified.

No single constituent mission owns this because each sees its own slice: scoring, folds, deposits, tick provenance, external demonstrations, or operator surfaces. The shared standard is cross-mission: **when is it legitimate for the War Machine to work on War Machine material, and what must be completed first?**

### Shared standard

**The War Machine campaign execution standard:** a cross-mission contract that classifies each remaining member by dependency and self-reference risk, assigns an execution mode, and releases each member only when the dependencies it consumes are verified.

Execution modes:

- `:ground-control-direct` - zai team works directly from mission docs; WM does not choose/enact the work.
- `:wm-read-only` - WM may inspect/surface/recommend, but not enact or self-credit.
- `:wm-assisted` - WM may propose candidates or supply traces; Ground Control still dispatches.
- `:wm-dogfood-released` - WM may work the class after campaign STANDARD-VERIFY + RUN/DELIVER releases it.
- `:external-facing` - work concerns outward value/fruit rather than WM internals; WM may be a tool only if consent and evidence gates hold.

### Joint completion criterion

The campaign can dissolve when:

1. every surviving member in section 2 has a ratified disposition: complete, superseded, parked with reason, or executed through its own acceptance bar;
2. all dogfood-risk members either finish or have a named follow-on that no longer blocks WM self-use;
3. the self-reference quarantine rule is either retired by evidence or preserved as standing doctrine;
4. at least one formerly self-referential class is safely worked under the verified standard without head-banging recurrence;
5. a campaign closure note states what WM can now dogfood and what remains operator/ground-control-only.

### Membership

Surviving mission-member count in this draft: **20**. Minimum-obligation workstreams: **6**. Total campaign work items: **26**. Dogfood-risk work items: **25**. External-facing work items: **1** (`M-demonstration-foundry`). Several checked items are complete/context and are listed after the member table.

**Keystone:** the campaign standard itself, with `M-aif-faithfulness`, `M-evaluate-policies`, `M-G-over-cascades`, `E-live-loop-3`, and `M-fold-self-play` as the central dependent cluster. This is a campaign-level standard rather than a single mission deliverable [OPERATOR?].

---

## 2. CONSTITUTION

### Step-0 census method

- Primary census: `futon2.aif.mission-registry/load-missions` over `/home/joe/code`, **198 unique** primary-checkout mission docs.
- Supplement: explicitly named top-level mission/excursion docs not scanned by the registry, including `futon2/holes/M-*.md`, `futon2/holes/E-*.md`, `futon0/holes/missions/M-capability-star-map.md`, `futon5a/holes/missions/M-war-machine-wiring.md`, and `futon7/holes/M-demonstration-foundry.md`.
- Selection keywords: `war-machine`, `wm`, `aif`, `fold`, `cascade`, `escrow`, `peradam`, `action-vocabulary`, `first-flights`, `efe`, `policy/policies`, plus docs whose HEAD/status names the WM stack.

**Registry caveat:** the live registry scans `*/holes/missions/M-*.md`, so top-level futon2/futon7 mission docs are not in the 198 unless they also exist under `holes/missions/`. This draft records those additions explicitly rather than treating them as registry hits.

### Surviving member survey

| Member | Repo/path | Status class | What remains | Depends on / feeds | Self-reference |
|---|---|---|---|---|---|
| `M-aif-faithfulness` | `futon2/holes/M-aif-faithfulness.md` | live / active | Bucket work remains across tick provenance, operator switches, mechanical repairs, and structural tails; B-3e/E7 tracking; re-badge/regenerate as each node lands | consumes `M-evaluate-policies`, E-KL refinements, live ticks; feeds all WM scoring claims | dogfood-risk |
| `M-evaluate-policies` | `futon2/holes/M-evaluate-policies.md` | complete with open close-candidate residue | C10 Joe close call and E7 post-merge re-census; cascade scoring boundary still points to `M-G-over-cascades` | feeds R5 honesty, `M-aif-faithfulness`; depends on cascade scoring for final C10 | dogfood-risk |
| `M-G-over-cascades` | `futon2/holes/M-G-over-cascades.md` | live / exploratory DERIVE | define cascade and `G(pi)` over cascades; typed-link/link-edit experiments only if discharge lift warrants | depends on pattern graph, fold engine, discharge ground truth; feeds policy scoring | dogfood-risk |
| `M-fold-self-play` | `futon2/holes/M-fold-self-play.md` | live / IDENTIFY | turn valid deposits into curriculum; blind score against A-next gold; calibrate constructor/fold improvements | depends on deposit corpus, A-next gold, fold-embed/GFlowNet work | dogfood-risk |
| `M-fold-ansatz` | `futon2/holes/M-fold-ansatz.md` | live / MAP complete | Joe ARGUE steer; phase-2 substrate-2 embedding question; GFlowNet seed and held-out link-prediction decisions | depends on A-next gold corpus, CLean/DarkTower, fold embed pipeline | dogfood-risk |
| `E-fold-embed-pipeline` | `futon2/holes/E-fold-embed-pipeline.md` | live / chartered | run local/GPU probes against A-next gold; decide whether embedding fold is worth building | depends on A-next gold, GFlowNet env, fold ansatz | dogfood-risk |
| `E-cascade-sampler-sampler` | `futon2/holes/E-cascade-sampler-sampler.md` | live / in flight, parked-ish | v1/S4 GFlowNet owner-side next; previous flat-reward lesson must inform fold sampling | feeds fold/GFlowNet decisions; depends on reward signal quality | dogfood-risk |
| `E-close-the-loop` | `futon2/holes/E-close-the-loop.md` | live / DERIVE | fold interface realization choices; firing remains operator-gated; LLM/embedding folds plug into same socket | feeds deposit/fold escrow and `M-fold-self-play` | dogfood-risk |
| `E-mine-mission-transitions` | `futon2/holes/E-mine-mission-transitions.md` | live / DERIVE | live wiring/emit mined overlay; provenance quality remains frontier | feeds rollout path and operational vocabulary | dogfood-risk |
| `M-operational-vocabulary` | `futon2/holes/M-operational-vocabulary.md` | live / INSTANTIATE | GPU box run pending; backward/sorry half and promotion paths remain; R14 feed pending | depends on mined turns, substrate-2 promotion, goals/sorries | dogfood-risk |
| `E-live-loop-3` | `futon2/holes/E-live-loop-3.md` | live / open | scheduled tick at sorry grain; fold-turn read path; reference regressions and constructor dilution/card-3 follow-ons | depends on escrowed fold-turns, held-work ledger, live ticks | dogfood-risk |
| `E-rollout-kill-test` | `futon2/holes/E-rollout-kill-test.md` | live / open | run >=15% non-greedy test; kill or keep rollout/proposal path | depends on rollout traces and operator-gated decision | dogfood-risk |
| `M-peradam-mechanization` | `futon2/holes/M-peradam-mechanization.md` | live / P1-P3 dark landed | two operator rulings for 001 issuance; E1/backfill path after structured witnesses exist | depends on seal/score/author-scorer separation, mana/peradam protocol | dogfood-risk |
| `M-first-flights` | `futon3c/holes/missions/M-first-flights.md` | live / Phase A complete | Phase B policy-grade `G(s, pi)` standing obligation, armed by rollout engine | depends on rollout engine/policy-grade G; feeds flight data shapes | dogfood-risk |
| `M-war-machine-wiring` | `futon5a/holes/missions/M-war-machine-wiring.md` | live / VERIFY | close head/manifold contracts; V-1 substrate-2 commit-vertex coverage gap; one-vs-N head decision | depends on live geometric stack, WM head/tuning, substrate exports | dogfood-risk |
| `M-live-efe-map` | `futon6/holes/missions/M-live-efe-map.md` | live / VERIFY complete with post-INSTANTIATE decisions | post-INSTANTIATE operator decisions and overlays; T1/T2 may open later | consumes WM/AIF status, capability-star-map overlays; operator surface | dogfood-risk |
| `M-autoclock-in` | `futon3c/holes/missions/M-autoclock-in.md` | live / INSTANTIATE-1 | confirmation/XTDB witnesses; creation-clock and after-save-clock rules remain | feeds ground-control lineage, mission registry, live loop ticks | dogfood-risk |
| `M-bounded-in-flight-state` | `futon3c/holes/missions/M-bounded-in-flight-state.md` | live / open | transactional discipline over filesystem substrate; disposition surface and pressure calibration | supports all agent/campaign execution reliability | dogfood-risk |
| `M-war-machine-pilot` | `futon3c/holes/missions/M-war-machine-pilot.md` | mostly complete / awaiting operator acceptance | formal v0 close; unresolved v1+/pilot-cycle substrate/consent questions | depends on operator acceptance; feeds pilot-as-agent future | dogfood-risk |
| `M-demonstration-foundry` | `futon7/holes/M-demonstration-foundry.md` | live / DERIVE | issue-probe and dogfood dependency corpus; one operator-selected external fix attempt; outward proof-of-work | depends on daily scan, GitHub probes, outbox/peradam path | external-facing |

### Checked but not surviving members

| Item | Disposition | Reason |
|---|---|---|
| `M-capability-star-map` | closed/context | mission closed 2026-06-10; caveats spun out; its read-contract remains a dependency of live EFE/map work |
| `M-aif2` | closed/context | closed on its own criteria; residue is campaign/run-deliver style, not reopened here unless Joe wants |
| `M-action-vocabulary` | stopped/context | kill criterion fired; successor finding belongs in M-aif2/operator-interest/value-model lane [OPERATOR?] |
| `M-wm-policies` | closed/context | parent that spawned the current R13/R16/R18 work; not a surviving member |
| `E-policy-rollout-engine` | landed/context | technical contract closed; only operator gate remains |
| `E-precision-over-policies` | closed/context | gamma term landed; scalar-only scope-out remains but is not campaign-critical unless Joe reopens |
| `E-have-want-pairs` | closed/context | proof-join remainder spawned elsewhere |
| `M-war-machine` | parked/context | original head is superseded by concrete pilot/tuning/wiring/live-loop members |
| `M-war-machine-tuning` | parked/context [OPERATOR?] | contains valuable criteria, but may be superseded by live-map/faithfulness/doc surfaces; include as reference unless Joe reopens |

### Self-reference census

- Surviving mission members: **20**
- Minimum-obligation workstreams: **6**
- Total campaign work items: **26**
- Dogfood-risk / self-referential work items: **25**
- External-facing work items: **1**
- Strongly self-referential core (do not route through WM enactment until released): `M-aif-faithfulness`, `M-evaluate-policies`, `M-G-over-cascades`, `M-fold-self-play`, `M-fold-ansatz`, `E-fold-embed-pipeline`, `E-close-the-loop`, `E-live-loop-3`, `M-first-flights`, `M-war-machine-wiring`, `M-live-efe-map`.
- Minimum-obligation workstreams requiring direct Ground Control before release: `W-constructor-df-last-inch`, `W-gamma-first-meal`, `W-coapp-live-usage-miner`, `W-candidate-drift-fence`, `W-proposer-portfolio-actions`, `W-dogfood-resequence`.

### Ready vs missing

| Ready | Missing |
|---|---|
| PIN-1B deposit loader and 2f gate; ground-control manual validated by 5-agent test; three new valid deposits | campaign-level work board and agent roster |
| R5/R6/R7/R14/R16/R19 audit material and many dark/live flips | one canonical dogfood-release rule |
| A-next gold corpus and fold-turn examples | larger fold curriculum and scoring loop |
| cascade constructor and cascade-lane path | card 3 seat-by-state injection and per-clause multi-vector retrieval |
| mission registry and held-work ledger | registry/top-level mission split resolved |
| mana/peradam certificate refusal machinery | operator rulings for real certificate issuance |
| live EFE map and WM trace corpus | post-INSTANTIATE decisions and click/achievement follow-through |
| gamma feed live/tested | first positive-dF gated meal |
| candidate generator and phylogeny miner | portfolio action families; live usage co-app; scan-root drift fence |

**CONSTITUTION exit:** operator accepts the member survey, self-reference classification, and governance roles.

---

## 3. ESCROW

### Escrow ledger

```edn
{:from-mission "M-aif-faithfulness"
 :on "campaign dogfood-release standard"
 :requirement "WM scoring/faithfulness repairs must be executed by ground-control/zai until the score/tick provenance contract is verified"
 :status :held}

{:from-mission "M-evaluate-policies"
 :on "M-G-over-cascades"
 :requirement "C10 close requires a real answer to cascade scoring / policy-as-cascade valuation"
 :status :held}

{:from-mission "M-fold-self-play"
 :on "A-next gold + valid deposit corpus + fold scoring standard"
 :requirement "self-play curriculum can train only after deposit validity and blind scoring are mechanically stable"
 :status :held}

{:from-mission "E-live-loop-3"
 :on "deposit/escrow replay contract"
 :requirement "scheduled ticks can trust fold-turns only through PIN-1B + 2f + reference regression"
 :status :contract-released}

{:from-mission "M-first-flights"
 :on "rollout/policy-grade G"
 :requirement "Phase B opens only when policy-grade G(s,pi) is supplied by the rollout/cascade scoring lane"
 :status :held}

{:from-mission "M-war-machine-wiring"
 :on "head/manifold read-write contracts"
 :requirement "WM cannot autonomously navigate stack manifold until read/write/trace contracts are verified"
 :status :held}

{:from-mission "M-demonstration-foundry"
 :on "external-facing consent/peradam/outbox standard"
 :requirement "outward fixes run only through operator-selected, proof-of-work, consent-gated PR path"
 :status :held}

{:from-workstream "W-constructor-df-last-inch"
 :on "card-3 injection + per-clause multi-vector retrieval"
 :requirement "the dF last inch must be closed by constructor change, not by library/deposit edits"
 :status :held}

{:from-workstream "W-gamma-first-meal"
 :on "positive-dF replayable gated mission"
 :requirement "gamma feed must ingest one qualifying mission before learning claims count"
 :status :held}

{:from-workstream "W-coapp-live-usage-miner"
 :on "live cascade usage edges"
 :requirement "used patterns accrue co-app edges, not only authored clusters"
 :status :held}

{:from-workstream "W-candidate-drift-fence"
 :on "scan-root fence or per-drift explanations"
 :requirement "101->108 drift cannot remain unexplained worktree/directory-copy smell"
 :status :held}

{:from-workstream "W-proposer-portfolio-actions"
 :on "M-aif2 slice 1 / action vocabulary successor"
 :requirement "advance, close, survey, and apply-cascade candidates must all be priceable"
 :status :held}

{:from-workstream "W-dogfood-resequence"
 :on "campaign execution-mode table"
 :requirement "each dogfood-risk member names direct-zai vs WM mode before dispatch"
 :status :held}
```

### Two-step release plan

- `:held -> :contract-released`: STANDARD-VERIFY ratifies the dogfood-release rule and per-lane gates.
- `:contract-released -> :satisfied`: RUN/DELIVER executes the member parcel and the member's own acceptance bar passes.

---

## 4. STANDARD-ARGUE

### Why this standard

The alternative is to keep letting the WM work on its own organs. That has produced useful artifacts, but it also conflates three roles: subject, judge, and executor. The 2026-07-06 ground-control test showed a cleaner separation: independently commissioned zai agents can produce valid deposits from a written contract, and Ground Control can review mechanically. That is exactly the separation this campaign needs.

The campaign standard is not "do not dogfood ever." It is: dogfood only when the class of work has earned release. Until then:

- self-referential WM/AIF/fold work is direct ground-control work;
- WM outputs are evidence, not authority;
- the operator's head-banging rule is a formal stop criterion, not a vibes-based frustration signal;
- transport and contract failures are separated, so the campaign does not misdiagnose infrastructure failures as conceptual failure.

### Cross-mission adequacy

| Requirement family | Why the standard fits |
|---|---|
| Scoring/faithfulness | Prevents an unverified score from judging its own repair |
| Fold/deposit/self-play | Keeps deposit validity and blind scoring ahead of curriculum learning |
| dF/gamma constructor path | Forces the dF last inch and gamma first meal through replayable gates rather than hand-edited deposits |
| Usage/proposer/drift path | Makes live usage, portfolio actions, and candidate hygiene first-class work instead of ambient smells |
| Live loop/ticks | Keeps scheduled reads grounded in replayable artifacts |
| WM pilot/wiring/live map | Keeps operator-facing surfaces from becoming action authority before their contracts are verified |
| Peradam/mana | Names fruit/no-fruit accounting and consent before certification |
| Demonstration-foundry | Allows external value work without turning it into autonomous PR farming |

**STANDARD-ARGUE exit:** Joe agrees the standard answers the dogfood hypothesis well enough to verify.

---

## 5. STANDARD-VERIFY

### Logic model before execution

Draft invariants:

1. **No-self-enactment-before-release:** if `self-reference = dogfood-risk` and release state is not `:wm-dogfood-released`, execution mode must not be WM enactment.
2. **Evidence-not-authority:** WM recommendations for dogfood-risk members are advisory until release; Ground Control owns dispatch.
3. **Two denominators:** campaign metrics split transport survival from content validity.
4. **Fruit accounting:** mana/peradam accounting names attempts with no fruit; no silent laundering.
5. **Member acceptance bars:** each member closes only by its own acceptance criteria; campaign does not restate or weaken them.
6. **Stop rule:** repeated head-banging triggers pause and survey, not retry-looping.

Adversarial traces to check before RUN:

- WM recommends a repair to its own `compute-efe`; controller attempts WM enactment -> rejected.
- zai transport fails twice; metric records transport failure, not contract failure.
- agent produces valid artifact but forgets to track ignored data file -> packaging note, not content repair.
- peradam certificate is attempted by author=scorer -> certificate loader rejects; campaign does not override.
- operator reports repeated head-banging on one lane -> lane pauses and returns to MAP/CONSTITUTION.

**STANDARD-VERIFY exit:** invariants encoded as a small checkable model or checklist [OPERATOR?]; Joe ratifies dogfood-release rule.

---

## 6. RUN / DELIVER

### Proposed sequencing

**Rung 0 - stabilize the execution substrate.**

1. `M-bounded-in-flight-state`: ensure ground-control work remains transactionally bounded.
2. `M-autoclock-in`: finish confirmation/XTDB witnesses needed for reliable agent/session/mission lineage.
3. `W-candidate-drift-fence`: install scan-root fence or per-drift explanations before candidate counts are trusted.
4. `W-dogfood-resequence`: create the execution-mode table; every mission/workstream gets direct-zai vs WM mode before dispatch.
5. Campaign board: create a per-member/per-workstream board with owner/reviewer/gates [OPERATOR?].

**Rung 1 - lock the read/evidence path.**

6. `E-live-loop-3`: keep PIN-1B/2f/reference-regression path green; resolve constructor dilution/card-3 issues.
7. `W-constructor-df-last-inch`: build card 3 seat-by-state injection plus per-clause multi-vector retrieval.
8. `W-gamma-first-meal`: enact only after a replayable positive-dF gated mission exists.
9. `M-aif-faithfulness`: finish visibility and badge debt that affects tick interpretation.
10. `M-evaluate-policies` + `M-G-over-cascades`: close C10/E7 boundary by defining cascade valuation enough for the campaign.

**Rung 2 - fold/cascade curriculum.**

11. `M-fold-ansatz` + `E-fold-embed-pipeline` + `E-cascade-sampler-sampler`: decide the embedding/GFlowNet frontier from A-next gold evidence.
12. `W-coapp-live-usage-miner`: add live cascade usage to co-app edge accrual.
13. `E-close-the-loop` + `M-fold-self-play`: only after deposit scoring and corpus are stable.
14. `M-first-flights` Phase B: release once policy-grade G is available.

**Rung 3 - WM surfaces and wiring.**

15. `W-proposer-portfolio-actions`: make close/survey/apply-cascade candidates exist and priceable.
16. `M-war-machine-wiring`: verify head/manifold contracts.
17. `M-live-efe-map`: post-INSTANTIATE decisions and overlays.
18. `M-war-machine-pilot`: formal close/reopen v1 under the new dogfood-release rule.

**Rung 4 - fruit and outward proof.**

19. `M-peradam-mechanization`: operator rulings + first legitimate certificate issuance.
20. `M-operational-vocabulary` / `E-mine-mission-transitions`: box run/promotion path after scoring/read path is stable.
21. `M-demonstration-foundry`: external-facing proof-of-work path; this is the cleanest place to test WM methods against non-self material.

### Zai-team commissioning plan

Use `GROUND-CONTROL.md` as the operating manual. For each parcel:

- Ground Control writes the bell prompt from the member's own acceptance bar.
- One zai author, one non-author reviewer.
- Reviewer reruns gates and records evidence before approval.
- Failed transport gets one continuation; second failure is data, not rescue.
- No WM self-enactment for dogfood-risk members until STANDARD-VERIFY release.

### Campaign-level kill / stop criteria

The operator's head-banging rule:

- If the same lane causes two consecutive review cycles of confusion about what is being measured, who has authority, or whether the artifact is valid, stop that lane and return to CONSTITUTION/ESCROW.
- If a self-referential member tries to use an unverified WM output as authority for its own repair, stop and reclassify the parcel.
- If 2 or more agents fail transport in a commissioning wave, report transport separately and do not infer instruction failure.
- If a member's own acceptance bar is missing or stale, do not execute; first draft the acceptance bar and mark `[OPERATOR?]`.

**RUN/DELIVER exit:** all Rung 0-2 dogfood blockers either closed or explicitly held; at least one class of WM dogfood is released by evidence; external-facing demonstration has a safe path.

---

## 7. DISSOLUTION

At dissolution:

- every member has a durable disposition;
- unresolved escrow entries become ordinary named dependencies;
- the dogfood-release rule is either dissolved because WM can safely self-work, or preserved as standing doctrine;
- closure record names which WM materials may now be worked by WM enactment, which remain ground-control-only, and why.

**DISSOLUTION exit:** the campaign coordinates nothing further, and the War Machine no longer needs a meta-campaign to know when it can work on itself.

---

## 8. Draft inspection notes

- [OPERATOR?] Membership is intentionally broad. A narrower campaign could split external-facing `M-demonstration-foundry` into a sibling Campaign; I kept it because the operator named it and because it tests WM methods on non-self material.
- HEAD gaps forced six named workstreams because no current surviving mission cleanly owned them end to end: gap 1 -> `W-constructor-df-last-inch`; gap 2 -> `W-gamma-first-meal`; gap 3 -> `W-coapp-live-usage-miner`; gap 4 -> `W-candidate-drift-fence`; gap 5 -> `W-proposer-portfolio-actions`; gap 6 -> `W-dogfood-resequence`.
- [OPERATOR?] `M-war-machine-tuning` is treated as context rather than surviving member; reopen it if its operator-surface criteria are not fully absorbed by `M-live-efe-map` / `M-aif-faithfulness`.
- [OPERATOR?] `M-capability-star-map` is closed, but the caveat excursions may need their own member rows if Joe wants the star-map lane active inside this campaign.
- [OPERATOR?] The keystone is campaign-level governance, not a mission. If the operator wants a concrete keystone mission, candidate name: `M-war-machine-dogfood-release-standard`.
