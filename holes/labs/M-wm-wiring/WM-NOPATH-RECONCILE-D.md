# WM-NOPATH-RECONCILE-D: the 17 no-path edges, reconciled

claude-11, 2026-09-26. Discovery, read-only. This reconciles the 17 edges
WM-SUPERSET-D (futon3c 67db43d4) classed "no path" with the corrections made
since, so that ⟨2⟩2d is not read as 17 independent implementation jobs.

Sources read:

| what | revision |
|---|---|
| registry | futon2 0a7ea5f2, its `:holes` |
| code | futon2 4dcb6aa2 |
| WM-PROVER-POSITIONAL-D | futon3c 5f249090 |
| H-A-CONSUMER-D | futon2 proof2/packets |
| node packets | deae6e99, e6e48e22 |

`WM-NOPATH-RECONCILE-D.edn` holds the same content machine-readably.

## Counts

| final class | edges | what it means for ⟨2⟩2d |
|---|---|---|
| reclassified, not a code wire | **7** | a registry correction or a map declaration; no code change |
| live code wire | **0** | none remains: the one candidate (measured A) was already fixed by H-A-CONSUMER-I |
| dead path (F_π's chain) | **5** | moot until F_π is computed on the live path; the packet is that path (F1) |
| consumer not written | **5** | the consuming equation has no code; the packet is a component, not a wire |

So ⟨2⟩2d is at most **four code packets**:
- **F1**, which also needs a ruling first;
- **C-R3s**, the state update;
- **C-R3as**, the state prediction error;
- **C-R17**, the Dirichlet accumulation.

Everything else is **one registry packet** of corrections, plus map
declarations.

## The 17, one line each

| edge | final | what changed / what it now is |
|---|---|---|
| R2→R1 (o) | reclassified | RC7+RC8 (futon2 9c4cc59a): o reaches μ through R3's update, not at R1's site (`reconcile-belief-carry` takes only fresh and `:mu-post`). It is carried by R2→R3a→R7→R3 (declared by the node packets) and then R3→R1 (E2, positional). Registry note: `:enters-through {:o :s-next}` |
| R16→R1 (u) | reclassified | Same correction. But u enters **no** belief update in the tick: `apply-arena-belief-events`' events come from the weighted errors only. The formal's B_{u_t} belongs to `:state-belief-update`, already recorded as the hole `[[R2 R3] [R4 R3]]`, so this adds no separate work |
| R4→R1 (A, B) | reclassified | As R16→R1: covered by the `:state-belief-update` hole |
| R4→R2 (A, B) | reclassified | RC7+RC8 recorded it **by design**: `:observe`'s formal is the generative process, and the observation reads the world. It stays a hole; ⟨2⟩2d's ACCEPT must exempt it |
| R1→R4 (μ) | reclassified | Pointer. The live R4/R5 path reads the **token** belief, as rank-actions' `:cascade-belief` (joint-q0 from the token belief carry), while R1's `:code` names only the entity belief. Once R1 names the token belief's site, or is split in two, this is an existing keyed wire |
| R6→R16 (Q(π), π) | reclassified | The path-dependence no longer holds on the live path. `enact/close-loop!` (enact.clj:323), which enacted the first passing act gate, has **no caller** in `src` or `scripts` at 4dcb6aa2. The runner enacts `selected-entry` (full_loop_runner.clj:1356), and the flight enacts `:enacted-steps` from `select-action-cascades`. What remains is an in-function wire, posterior → `bayes-choice`. Close the hole with this evidence, and declare the wire on the map |
| R4→R7 (A) | reclassified | Pointer. `:likelihood-precision` has `:code nil`, but `likelihood_precision/tempered-rates` is called on the live path at cascade_model_manifest.clj:830 (`horizon-g-sparse*`), tempering the token rates that carry A by ζ. The wire is live and is the identity at the declared ζ = 1 |
| R8→R6, R2→R8, R4→R8, R6→R8 | dead path | F_π is not computed on the live path. Group 1 |
| R8→R3 (F_π) | dead path | Also consumer-not-written (`:state-belief-update` has no code): it needs both F1 and C-R3s |
| R2→R3 (o), R4→R3 (A, B) | consumer not written | `:state-belief-update` has `:code nil`; in Lean it is `ExactBeliefTrajectory.exactUpdate` |
| R4→R3a (A, B) | consumer not written | `:state-prediction-error` has `:code nil`; in Lean it is `StatePredictionError.statePredictionError` |
| R1→R17 (μ), R2→R17 (o) | consumer not written | a4a accumulates a different model (capability × mission, from the substrate). Dirichlet accumulation over the tick's (o, μ) (Da Costa eq. 21) is not written |

## Group 1: F_π on the live path (packet F1)

**The stamp.** `policy-prefix-evidence/production-ranked`
(policy_prefix_evidence.clj:56-70) removes `:f` from every ranked entry and
stamps `:f-prefix {:status :not-supplied :reason :no-admitted-policy-prefix
:pending-dependency :d-conditioning-consumption-and-policy-prefix-admission}`.

**Three suppliers, and which is intended:**

| supplier | status |
|---|---|
| `policy_prefix_evidence/evaluate-synthetic`, the observed-prefix F per policy | **intended**. The namespace says "Production remains not-supplied until D owns and admits the policy/execution/observation join". It needs D's conditioning consumption and a policy-prefix admission, neither of which exists |
| `cascade_free_energy/policy-free-energy`, called inside `efe/rank-cascade-actions` (efe.clj:1193) | **present, switched off**. It is computed only when `:f-prefix-production?` is false, and war_machine sets it true at both call sites (:5908 base-opts, :6592 `cascade-decision-admitted`). With the flag off, efe.clj :1339-1360 attaches `:f {:status :computed …}` |
| `war_machine/f-pi-dark-readback` → `policy_free_energy/f-pi-vector` | **not the supplier**: no production caller (tests only); a readback of an earlier F definition |

**A ruling comes first.** It must say which F the registry's
`:policy-free-energy` means: the observed-prefix F, or the cascade F over q0
and the observed tokens. F1 becomes one behaviour only after that. With the
second, F1 is "stop setting `:f-prefix-production?`" together with deciding
what the prefix stamp then says. With the first, F1 waits on D's admission.

**What reads the stamp**, i.e. everything that would change once F is
supplied:
- war_machine.clj :5908 and :6592 set the flag.
- efe.clj :1193 skips the computation; :1339-1344 writes the certificate's
  `:f {:status :not-supplied …}`.
- policy.clj :236 (`selection-candidate` reads `:f-prefix` and takes f from
  it), :255, and :297, which carries it into the record.
- cascade_selection.clj:
  - :88 and :100 admit not-supplied as finite;
  - **:112, where the −F term is 0.0 when not supplied**;
  - :182-183, :200 and :226, where the law receipt records F as absent;
  - :255 and :284-285, the contribution attribution.
- The run record's per-tick F-absence stamp (futon2 f39bf0a1).

F1 closes the hole `[[R2 R8] [R4 R8] [R6 R8] [R8 R6] [R8 R3]]`, except R8→R3,
which also needs C-R3s.

## Group 2: measured A's consumer

**The site:** `observation_rates/token-likelihood-rates` (the defn at :156).

**The early return H-A-CONSUMER-D named** (at futon2 0358d4c5): the `(=
:checkable (:kind cls))` arm returned the zero kernel before reading the
label-derived rates. Every production class (C3, C4, C5, C6, C8, all
`:checkable`) therefore scored zero, whatever the ledger said.

**Status: fixed.**
- futon2 7ba427ab (H-A-CONSUMER-I, 2026-09-25) reads a measured rate first
  (the cond's first arm, :197-203). The zero kernel is taken only for a
  `:checkable` class that is **unmeasured** (:206-208), and is marked
  `:measurement :absent`.
- 64f005d4 then refuses a declared-not-measured rate.

**Not checked here:** whether the live R5 call (war_machine :5898, labels
from `(:subjects observation-labels)`) actually receives measured cells for
the production classes. ζ stays at 1, so `tempered-rates` is the identity on
the live path.

The one no-path edge in this group, R4→R7, is a pointer correction (above).

## Group 3: the rest, in dependency order

1. **RC-registry** (no code):
   - `:belief-state :enters-through {:o :u :A :B → :s-next}`;
   - a `:code` site for the token belief, or a split of the R1 row;
   - close `[R6 R16]` with the close-loop! evidence;
   - `:likelihood-precision :code` = `tempered-rates`.
2. **C-R3s:** the exact state update in code, consuming o, A and B (and F_π
   once F1 lands). Closes `[[R2 R3] [R4 R3]]`, and `[R8 R3]` together with F1.
3. **C-R3as:** the state prediction error in code, consuming A and B. Closes
   `[R4 R3a]`. It comes after C-R3s because they share A's and B's carriers.
4. **C-R17:** Dirichlet accumulation over the tick's (o, μ). Closes `[[R2
   R17] [R1 R17]]`.

## Every `:holes` entry, and what closes it (⟨2⟩2d's ACCEPT, checkable)

| hole (0a7ea5f2) | status | closed by |
|---|---|---|
| `[R6 R16]` | path-dependent | RC-registry, with the evidence that close-loop! is uncalled and the live paths enact the recorded selection |
| `[[R2 R17] [R1 R17]]` | not-realised | C-R17 |
| `[[R2 R8] [R4 R8] [R6 R8] [R8 R6] [R8 R3]]` | not-realised | F1 (`[R8 R3]`: F1 + C-R3s) |
| `[[R2 R3] [R4 R3]]` | not-realised | C-R3s |
| `[R4 R3a]` | not-realised | C-R3as |
| `[R4 R7]` | not-realised | RC-registry (the pointer to `tempered-rates`; the wire is live) |
| `[R4 R2]` | not-realised | **none: by design**. ACCEPT must exempt it |
| `:no-consumer` (per-tick F read by nothing) | no-consumer | none: a diagnostic, not an edge; outside ⟨2⟩2d |

## Not done

- No code, no map edit, no registry edit, nothing run.
- Line numbers are at futon2 4dcb6aa2.
- Not checked:
  - whether production labels carry measured cells (group 2);
  - which F is meant (group 1): that is a ruling.
