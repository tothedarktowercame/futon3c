# Chip-board witness status vocabulary

New certificates from inbox-zero and cascade-verifier now distinguish the
completed Lean proof from unproved runtime correspondence:

```clojure
:lean/status
{:status :witnessed-under-declared-registry
 :scope :lean-model
 :witness {:repository "mathlib4"
           :commit "<exact immutable witness commit>"
           :module "<Lean module path>"
           :theorem "<fully qualified theorem>"}
 :assumption :registered-verbs-implement-declared-semantics
 :runtime-correspondence :not-proven
 :registry {:digest "<this run's verbs/digest>"
            :scope :in-process-function-identity
            :semantic-approval? false}}
```

Inbox points to `a9a24a3b9e070550ede41ebd10621c5bb9b843f0`, theorem
`DarkTower.WarMachine.ChipBoardWitness.Repaired.repairedHazard`.
Cascade points to `e407ec20cbedb8667070dee89cd45f27a52207a0`, theorem
`DarkTower.WarMachine.CascadeVerifierBoardWitness.cascadeHasNoActEffects`.
Both witnesses were already elaborated without sorryAx, with independent
runtime readbacks retained alongside their README files. This metadata change
does not rerun Lean per certificate or certify the new run as the pinned
historical fixture.

`:verified?` still means replay of trace/effects/end and registry digest.
It is not semantic approval. A new unsafe registry can self-replay; its Lean
status still explicitly states model scope and unproved correspondence.
The run's registry digest is recorded as identity provenance, not as a proof
that its functions implement the model. No status map claims safety of an
arbitrary external effect handler.

Validation: one focused namespace, **2 tests / 21 assertions**, zero failures
or errors. Tests cover both producer witness bindings and an isolated unsafe
registry whose true replay result must not imply proved runtime semantics.
Clojure lint 0/0, parens OK, diff check clean. Existing consumers in src/test
were checked for pending-keyword assumptions; none required a migration.
The two offline checker reports use the same vocabulary. Older transcripts,
validation logs and pending-status reports are unchanged historical evidence.

No runtime execution behavior, registry API, snapshot logic, live namespace,
service or stored certificate was modified. The shared-snapshot correction
`e4a8bc18` remains in place. These are newly generated certificate metadata
values; no historical result is relabelled.
