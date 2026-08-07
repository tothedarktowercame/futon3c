# capability-proof-arxiv — review checklist

Working document. **Not part of the paper.** Tracks what the independent
adversarial review (`futon6/holes/TN-codex-review-of-capability-proof-arxiv.md`,
codex-5) found, what has been corrected, and what is outstanding. The paper
states current fact plus explicit *pending rerun* markers; the reasoning about
how it got there lives here.

Status key: **DONE** corrected in the paper · **PENDING RERUN** needs compute ·
**OPEN** needs a fix or decision · **RECLASSIFY** a readiness card is mislabelled.

---

## 0. The finding that changes how to read the rest

**A ledger entry is weaker evidence than the paper treated it as.** The DAG
contract requires each record to carry output, fingerprint, a structured gate
result, reuse flag, timestamp, and run/corpus identity. `ledger_record` writes
four fields:

```json
{"stage":"S5","corpus_id":"…","run_id":"…","gate":"pass"}
```

and it writes that record *after the command exits* — including when the stage's
criterion is only **printed for a human** and never executed, and `--mark-done`
can synthesise the same record with no output at all. So "12/12 ledgered" was
never the integration warrant it sounded like: for several stages it certifies
that a command ran, not that its criterion held.

This is upstream of most of §1 and §3 below. Fixing the ledger record is worth
more than any individual stage repair, because it converts the whole ledger from
a claim into evidence.

---

## 1. Corrections applied to the paper

| # | Finding | Now says | Status |
|---|---|---|---|
| 1 | `e2e-16` ledger held 4 stages, not 12 | A12 **partial**: 12/12 ledgered, 9 executed under this id, 3 pending rerun | **DONE** |
| 2 | A10 id collision (legacy ids → a paper named `math`) | 9 signatures (papers with ≥2 proofs); twin similarity *pending recomputation* | **DONE** |
| 3 | A6 contradicted by its producer's own `wellformed` field | **weak**: 12 objects for 16 papers, 2 false, 33 orphans, output outside RETRIEVE | **DONE** |
| 4 | A3 retry rate | 45.9% (45/98), definition stated | **DONE** |
| 5 | A3 gate selection | qualified: passes when finals are selected explicitly | **DONE** |
| 6 | A8 "95 theorems" not derivable | export = 446 nodes / 286 edges (88 Proof + 358 Step); XTDB cited to its own artifact | **DONE** |
| 7 | A2 substrate wording | stage *checks* a shipped substrate; corpus-fresh rebuild is H1 tier 2 | **DONE** |
| 8 | S4 slots composition omitted a category | shows `slot: 59` | **DONE** |
| 9 | A1/A2 paper counts | 16/16 | **DONE** |
| 10 | S1 census not reproducible as shipped | notes the frozen manifest requirement | **DONE** |

---

## 2. Pending rerun

| # | Item | Needs | Cost |
|---|---|---|---|
| R1 | Clean single-corpus execution of S3/S4/S7 | removes A12's caveat entirely | ~2 days CPU on Zone, hours on a GPU host |
| R2 | Paper-twin similarity recomputation | prior figure used a collapsed partition | seconds, but wants R1's embed to be quotable |
| R3 | A3′ anchor validation at volume | H21 validated only at n=3 | hours CPU, ~30 re-mined proofs |

---

## 3. Open defects, by severity

### 3a. Release blockers for an unattended run

| # | Item |
|---|---|
| B1 | **The stepper exits 0 on refusal, command failure, and gate failure.** Demonstrated: `✗ S1 BLOCKED … PROCESS_EXIT= 0`. An outer scheduler records success while the stepper reports it stopped. |
| B2 | **The ledger record is weaker than the contract** (§0). Add output path, fingerprint, structured gate result, reuse flag, timestamp; stop writing `pass` when the criterion was merely printed. |
| B3 | **The machine DAG and the runnable stages are different pipelines.** `load_deps()` reads `superpod-dag-contract.md`, whose block still uses *old* semantics (S4 clean, S5 strategy, S6 comprehension, S7 embedding) against OPS's corrected ones (S4 expository, S5 comprehension, S6 paper graph, S7 CLean). Consequences: S5 depends only on S1; S6 omits its S4 dependency; S7 depends on old S4; **S10–S12 have no dependencies at all**. The completeness guard proves completion of a graph the runner does not execute. |
| B4 | **`.rung2.edn` reports are globbed as proof graphs** in the S3 post-gate, the S3 wrapper's eval tail, S5, and S7's inputs. One root cause, four symptoms; it is why S3's directory gate fails on its own sidecars and why S5 emits 98 spurious `no-structure` rows. |
| B5 | **S9 masks its first sub-command with `;`** — the same class as the S11 defect already fixed. Audit every stage command for `;` between sub-steps. |

### 3b. Stage-level gaps (from the review's stage table)

| Stage | Gap |
|---|---|
| S2 | Corpus-fresh substrate **producers never invoked**: `build_term_prior.py`, `build_concept_encyclopedia.py`, `sfc_concept_index.py`. H1 tier 2 is genuinely open. |
| S4 | Region cap still out-of-band (H10); no stage-level gate command. |
| S5 | Sub-stages absent as executables: `iatc_semcheck.bb`, `cas_select.py`, `cas_checks.py`, `cas_cert.py`, `sfc_symbol_grounding.py`, `rung3_residue_llm.py`, `warrant_normalize.py`. Criterion printed, not checked. |
| S6 | Does not consume exposition or concepts; writes `data/paper-graphs` instead of `data/iatc-paper-graphs/$RUN_ID`; returns 0 with `wellformed:false`; no gate. |
| S7 | Omits `clean_argcheck.bb`; does not pass run-specific output dirs. |
| S8 | `clean_to_lean.py` and export/load smoke tests not invoked; no gate. |
| S10 | Reground outputs still not persisted as run artifacts; lift criterion not executable. |
| S11 | `sfc_struct_canon` can emit a *refusal* artifact and exit successfully — no gate distinguishes measured from refused. |
| S12 | Injected outside the machine DAG, no dependencies; `rising` printed rather than enforced. |
| render / RETRIEVE | `render_run.py` and `build_proofcheck_demo.py` exist but no stage invokes them; S6's real output path is not in the retrieval manifest. |

### 3c. Documentation / hygiene

| # | Item |
|---|---|
| D1 | Hazard close-count not mechanically derivable — `E-superpod-hardening.md` is prose with sub-hazards and stale headings. Make it a state table or stop quoting a count. |
| D2 | Source/PDF hash drift — record both together on any handoff. |
| D3 | The launch playbook should record the **actual served model name** (`/v1/models`) and export `FUTON6_EPRINTS`; preflight correctly reported 7/9 in a clean environment for exactly these two. |

---

## 4. Readiness-card reclassification

The dashboard's 2 `build` + 7 `partial` cards are mostly mislabelled. Codex's
classification, which matches the operator's RAW-CTL intuition:

| Card | Actual class | Note |
|---|---|---|
| **RAW-CTL** | **evidence already exists** | Not a build *and not even a run*: `data/exp-20260618/loop-run-70b-raw` exists — ten papers, **12.5% warrant grounding raw vs 21.4% enriched**. The old report miscounted graphs and rung-2 EDN together. Needs a modern finals-only re-analysis, no model tokens. |
| **CAS-SEL** | needs wiring | Selector, registry, checks, segmenter, certificate all exist; OPS runs only the segmenter. "build" is stale. |
| **SFC2b** | wiring, then evidence | Works per formula; needs a batch adapter and a run-scoped output contract. |
| **rung-3** | needs evidence | Deterministic half wired and producing the 818-move census; the bounded LLM-on-residue pass needs a run. |
| **RENDER** | needs wiring | Both renderers exist; `render_run.py --all` already expresses the loop but paths are hardcoded to legacy runs. |
| **STRAT-REC** | needs evidence / calibration | Executed inside `clean_comprehension`; needs a measured recall/error report. |
| **WARRANT-NORM** | needs wiring | Exists, invoked by nothing; defaults to the global tree and a shared demo path. |
| **PASS3-HARVEST** | needs *correct* wiring | Present in S9 but after `;`, reading the global tree, writing a shared path. |
| **LEAN-NL** | split | Core validation is done at 0.71 recall — reclassify **READY**; register per-step attribution and hidden-layer attachment as a *new* build card. |

---

## 5. Workplan before the Superpod window

Ordered by value/cost. Everything in 1–10 is CPU or bounded local-model work;
none needs the eight-GPU allocation.

| # | Work | Cost | Done when |
|---|---|---|---|
| 1 | Make failure loud: nonzero exit on refusal/command/gate failure; S9 `;`→`&&`; exit-status regression tests | very low | a refused dry run exits nonzero |
| 2 | Reconcile the DAG source of truth with S4–S9 semantics; give S10–S12 real dependencies and inputs | low | `--plan` audit matches the contract; S5 refused without S2/S3/S4 |
| 3 | Exclude `*.rung2.edn` everywhere proof graphs are globbed; make S6 consume its specified inputs, write the retrieved path, and fail on unattached proofs | low | replay no longer sees 196 "graphs"; S6 lands in RETRIEVE |
| 4 | Re-run the existing RAW-CTL analytic with finals-only readers; update the stale card | very low | frozen report, identical paper set, modern gates |
| 5 | Make S9 run-scoped: wire `warrant_normalize` + `clean_hole_harvest` with explicit paths | low CPU | normalized-hole vocabulary + pass-3 map under `$RUN` |
| 6 | Wire the deterministic CAS chain (`cas_select` → checks → `cas_cert`) | low CPU | per-proof select + certificate for all 98 finals |
| 7 | SFC2b batch adapter, run over the 16-paper sample | medium, bounded LLM | run-scoped symbol files, support/unsupported rates |
| 8 | rung-3 residue pass + strategy-recognizer miss scoring | medium | question artifact; before/after recogniser recall |
| 9 | Parameterize and invoke `render_run --all` as a post-S8 tail | low–medium CPU | render count = eligible papers; artifacts in RETRIEVE |
| 10 | Split LEAN-NL: core READY, rest a new build card | not window-blocking | new card with one end-to-end CLean example |
| 11 | **Only then:** clean 12–16 paper rehearsal in a fresh run namespace, rebuilding S2 rather than checking it | hours | `replay_e2e --through S12` 11/11, 12/12 same-corpus ledger with hashes, no `adhoc` metrics, clean RETRIEVE, source/PDF hashes recorded |

---

## 6. The acceptance criterion, in the reviewer's words

> The decisive acceptance criterion is not another component count. It is one
> fresh run for which the stepper's process status, stage ledger, output paths,
> replay harness, and paper all refer to the same corpus and agree.

Worth keeping because it reframes what remains: the census is strong and
reproduced independently. What is missing is not capability but *agreement
between the instruments that report on it* — which is the same class of problem
as every hazard in the ledger, one level up.

---

## 7. Findings from the rung-2 measurement (2026-08-07)

Opened while working W6. Corrects a claim, and adds three items.

**Corrected.** Rung-2 was reported here and in `design-dag.md` as failing
corpus-wide. That was the apparatus: `iatc_semcheck.bb` invoked bare `python3`,
whose interpreter lacks `edn_format`, so R2d raised and the composer failed
every graph for the same reason. Fixed (`futon6@82171d8`). Rung-2 passes on
**49/98**. The heuristic worth keeping: *a heterogeneous corpus does not fail
uniformly, so a uniform failure is evidence about the harness first.*

| sub-check | PASS | FAIL |
|---|---:|---:|
| R2a anchor-faithfulness | 84 | 14 |
| R2b closure | 58 | 40 |
| R2c warrant-resolution | 98 | 0 |
| R2d concept-coverage | 92 | 0 (6 `NA`, deliberate) |

| # | item | cost | done when |
|---|---|---|---|
| 12 | R2c is un-failable by configuration (`:warrant-floor 0.0`, report-only until calibrated) yet prints in the same PASS column as three gating rungs, so any aggregate over that column mixes them. Print report-only checks as `REPORT`, and have headline rates quote their denominator. R2d's 6 `NA` are correct and deliberate (`:na-not-fail`) — they only mean R2d's denominator is 92 | low | a rung summary in which report-only and NA are distinct from PASS |
| 13 | Incremental checkpointing for long LLM passes. One malformed response discarded a 98-paper run's completed verdicts; the parse is now hardened, but the payload is still only written at the end | low–medium | kill a run mid-pass, restart, and have it resume rather than restart |
| 14 | R2b closure — 40/98 graphs carry orphan nodes. This is a *finding about extraction*, not a defect, and belongs in the census rather than the gate backlog: it is the `missing-warrant` rate seen from the other side. Decide whether closure should gate at all, or only report | design call | a stated position on whether an unreachable node is a failure or a datum |

Item 14 is the one to think about before the window. If closure gates, roughly
half the corpus fails S3 at rung 2 and the run stops; if it reports, the corpus
is admitted with a measured orphan rate. The census reading is the more
defensible one — an unattached claim is a real property of the literature, not
obviously an extractor error — but it should be chosen rather than defaulted.
