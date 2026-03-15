; Draft seed for M-structural-law Phase 1 (IDENTIFY)
(structural-law-inventory
  (thesis
    "The FUTON stack is not only a collection of repos, devmaps, and missions; it is also a collection of invariants. Structural law should inventory both operational invariants and candidate invariants until the stack can describe itself in those terms.")

  (classification-notes
    ((note :id foundational-vs-gateable
           :summary "A true invariant is foundational and always-on. A gate that can be disabled is still useful, but it is not the same kind of thing."
           :implication "The inventory should distinguish substrate invariants from optional validation gates.")
     (note :id failure-locality-as-foundational
           :summary "The most valuable foundational invariants often make failure locality obvious."
           :implication "futon1a's layered stop-the-line model is evidence for a stack-wide aspirational family around failure locality and rapid diagnosis.")
     (note :id law-vs-implementation-maturity
           :summary "A structural law may be real at the conceptual level even when present implementations are partial, weak, or inconsistent."
           :implication "The inventory should distinguish between candidate laws that recur across the stack and the current maturity of their enforcement.")))

  (registry-schema
    (entry-dimensions
      ((field :key :scope
              :required true
              :values (:local :repo :cross-repo :stack)
              :meaning "How widely the law is meant to bind.")
       (field :key :status
              :required true
              :values (:operational :operational-but-bypassable :operational-when-enabled :candidate :violated)
              :meaning "Current maturity and binding force.")
       (field :key :kind
              :required true
              :values (:foundational :gate :mission-law :reporting-law :process-law :structural-law)
              :meaning "What sort of law this is.")
       (field :key :home
              :required true
              :values (:repo :mission :peripheral :subsystem)
              :meaning "Where the law is primarily owned or expressed.")
       (field :key :implemented-in
              :required false
              :meaning "Code or artifact locations where the law is enforced.")
       (field :key :enforced-at
              :required false
              :meaning "Entry points, hooks, or runtime surfaces where the law actually bites.")
       (field :key :evidenced-by
              :required false
              :meaning "Tests, docs, or traces that justify the entry.")))
    (interpretation-rules
      ((rule :id operational-means-cannot-bypass-in-normal-use
             :summary "Use :operational only when ordinary live use cannot silently bypass the law inside its declared scope.")
       (rule :id bypassable-means-real-but-not-binding
             :summary "Use :operational-but-bypassable when a law is genuinely enforced in some path, but normal stack behavior can still route around it.")
       (rule :id enabled-gates-are-not-foundational
             :summary "Use :operational-when-enabled for checks that become real gates only under explicit runtime configuration.")
       (rule :id candidate-means-law-without-sufficient-enforcement
             :summary "Use :candidate when the law is conceptually recurring but not yet strong enough to count as binding.")
       (rule :id violated-means-claimed-law-broken-by-evidence
             :summary "Use :violated when the stack or mission claims the law, but current evidence shows the claim does not hold."))))

  (inspiration
    (artifact :path "futon3/holes/holistic-argument-sketch.sexp"
              :role "Whole-stack compact description by repo/devmap/mission")
    (note "This inventory does not modify the holistic argument sketch. It builds a parallel invariant description inspired by its form."))

  (operational-families
    ((family :id graph-symmetry
             :status operational
             :question "If A points to B, is the inverse structural relation present?"
             :implemented-in ("portfolio/logic.clj"
                              "agents/tickle_logic.clj"
                              "agency/logic.clj"
                              "peripheral/proof_logic.clj"))
     (family :id status-discipline
             :status operational
             :question "Are state/status labels legal and evidence-consistent?"
             :implemented-in ("agents/tickle_logic.clj"
                              "agency/logic.clj"
                              "peripheral/proof_logic.clj"
                              "peripheral/mission_backend.clj"
                              "peripheral/mission_shapes.clj"))
     (family :id phase-ordering
             :status operational
             :question "Do states advance in a valid order?"
             :implemented-in ("agents/tickle_logic.clj"
                              "peripheral/proof_logic.clj"
                              "peripheral/mission_backend.clj"))
     (family :id required-outputs
             :status operational
             :question "Does each phase produce its required artifacts?"
             :implemented-in ("peripheral/proof_logic.clj"
                              "peripheral/mission_backend.clj"
                              "peripheral/mission_shapes.clj"))
     (family :id existence
             :status operational
             :question "Do referenced entities actually exist?"
             :implemented-in ("agency/logic.clj"
                              "peripheral/proof_logic.clj"
                              "peripheral/mission_backend.clj"))
     (family :id dependency-satisfaction
             :status operational
             :question "Are completed things backed by completed prerequisites?"
             :implemented-in ("portfolio/logic.clj"
                              "peripheral/proof_logic.clj"))
     (family :id startup-contracts
             :status operational
             :question "Does startup require explicit policy and fail loudly when the policy is underspecified?"
             :implemented-in ("futon1a/system.clj"))
     (family :id layered-error-hierarchy
             :status operational
             :question "Do failures surface at the layer that caused them, with stable status/context?"
             :implemented-in ("futon1a/core/pipeline.clj"
                              "futon1a/api/errors.clj"))
     (family :id authorization-and-identity-discipline
             :status operational
             :question "Are write authority and external identity uniqueness enforced before durable write?"
             :implemented-in ("futon1a/auth/penholder.clj"
                              "futon1a/core/identity.clj"
                              "futon1a/core/entity.clj"
                              "futon1a/core/pipeline.clj"))))

  (candidate-families
    ((family :id atomic-inspectable-units
             :status candidate
             :summary "Work should happen in bounded, inspectable units rather than diffusing across repos, stashes, and unowned files."
             :candidate-invariants
             ((invariant :id home-repo
                         :summary "Every active work item has one canonical repo.")
              (invariant :id single-live-copy
                         :summary "A tool or artifact should not have competing live copies in multiple repos without an explicit variant relation.")
              (invariant :id checkout-before-work
                         :summary "Active work starts from an explicit checked-out unit.")
              (invariant :id checkin-on-exit
                         :summary "A unit ends only as committed, parked, abandoned-with-reason, or escalated.")
              (invariant :id inspectable-boundary
                         :summary "A work unit has a stable identifier and readable evidence trail.")))
     (family :id artifact-custody
             :status candidate
             :summary "Outputs should land where the stack expects them to live."
             :candidate-invariants
             ((invariant :id artifact-locality
                         :summary "Derived artifacts belong in the canonical repo for the work domain.")
              (invariant :id scratch-marking
                         :summary "Provisional artifacts are explicitly marked as scratch or bookmark material.")
              (invariant :id generated-vs-source-separation
                         :summary "Generated files must not masquerade as source-of-truth assets.")
              (invariant :id narrow-ignore-exceptions
                         :summary "Tracked exceptions in ignored trees must be explicit and narrow.")))
     (family :id repo-role-clarity
             :status candidate
             :summary "A repo should say what kind of thing it is, and its root should not contradict that claim."
             :candidate-invariants
             ((invariant :id declared-role
                         :summary "Each repo has an explicit role statement.")
              (invariant :id root-legibility
                         :summary "Top-level layout matches the repo's role.")
              (invariant :id main-branch-coherence
                         :summary "Main/master does not silently mix canon, experiments, generated output, and local runtime clutter.")
              (invariant :id legacy-surface-marking
                         :summary "Retained legacy tooling is marked as legacy, compatibility, or canon.")))
     (family :id archaeology-control
             :status candidate
             :summary "Latent work should not accumulate as invisible operational debt."
             :candidate-invariants
             ((invariant :id stash-debt-bounded
                         :summary "Stash count remains below a reasonable threshold, or older stashes must be classified.")
              (invariant :id recover-or-drop
                         :summary "A stash is revived, parked on a branch, or explicitly dropped.")
              (invariant :id obsolescence-recognition
                         :summary "Superseded autostashes do not remain in the live queue once equivalent commits have landed.")))
     (family :id peripheral-custody
             :status candidate
             :summary "A peripheral session should carry enough structure that work cannot silently drift across domains."
             :candidate-invariants
             ((invariant :id domain-id-required
                         :summary "Each peripheral session operates on an explicit domain id or work id.")
              (invariant :id home-repo-in-context
                         :summary "Peripheral context includes canonical repo or workspace root when domain work is repo-owned.")
              (invariant :id hop-preserves-custody
                         :summary "Peripheral hops preserve problem/mission/task identity unless an explicit handoff occurs.")
              (invariant :id exit-produces-fruit
                         :summary "Stopping a peripheral yields fruit that can be mapped to check-in outcomes.")))
     (family :id human-visible-inspectability
             :status candidate
             :summary "State should be visible enough that the human operator can tell what is going on without folklore."
             :candidate-invariants
             ((invariant :id readable-surface
                         :summary "Key outputs are inspectable in human-readable form, not only raw logs.")
              (invariant :id state-projection
                         :summary "Peripheral and mission state project to evidence, blackboards, or reports.")
              (invariant :id evidence-over-folklore
                         :summary "Operationally important claims are queryable or documented in stable artifacts.")))
     (family :id failure-locality
             :status candidate
             :summary "Failures should surface near the layer, subsystem, or entry point that caused them, with enough structure that diagnosis is quick."
             :candidate-invariants
             ((invariant :id fail-at-source-layer
                         :summary "Errors report the layer or subsystem that caused them rather than collapsing into generic failure.")
              (invariant :id stop-the-line-ordering
                         :summary "Higher-layer validation failures prevent lower layers from mutating state.")
              (invariant :id diagnosis-with-bounded-search
                         :summary "A defect should be localizable without folklore or broad repo archaeology.")))
     (family :id budgeted-action-selection
             :status candidate
             :summary "Action selection should be constrained by available budget or license, not by priority alone."
             :candidate-invariants
             ((invariant :id no-costly-action-without-budget
                         :summary "Actions above a cost threshold require sufficient budget before they may proceed.")
              (invariant :id priority-within-budget
                         :summary "The next action is chosen from what is both high-priority and currently affordable.")
              (invariant :id spend-must-be-recorded
                         :summary "Budget expenditure should be attached to explicit actions or transitions, not inferred after the fact.")
              (invariant :id depletion-forces-deferral
                         :summary "Budget depletion blocks, reroutes, or defers action rather than allowing silent overextension.")
              (invariant :id replenishment-has-evidence
                         :summary "Budget replenishment should follow explicit signals, evidence, or policy rather than vibes.")
              (invariant :id budget-state-visible
                         :summary "Current budget state should be inspectable by the operator or peripheral.")))
     (family :id cross-store-agreement
             :status candidate
             :summary "When one operational story is mirrored across registry, ledger, and announcement stores, the mirrors should agree about identity, continuity, and canonical backing."
             :candidate-invariants
             ((invariant :id canonical-ledger-backs-public-claim
                         :summary "Public announcements should point to canonical jobs or receipts.")
              (invariant :id mirrored-actor-agreement
                         :summary "Mirrored records naming the same work item should agree about the responsible agent or actor.")
              (invariant :id session-continuity-alignment
                         :summary "Running work mirrored across stores should agree on the active session or continuity token.")
              (invariant :id aggregate-count-backed-by-ledger
                         :summary "Registry-level running or queued counts should be backed by the canonical job ledger rather than drifting independently.")))))

  (repo-seeds
    ((devmap :id futon0
             :role "workspace hygiene, stack interface, reporting and coordination tools"
             :candidate-invariants
             ((invariant :id clean-entry-and-exit
                         :family atomic-inspectable-units
                         :status candidate
                         :summary "Work begins and ends with a clean repo unless an explicit resumed work unit is checked out."
                         :likely-surface "scripts/futon-sync.clj"
                         :checkout-model
                         ((rule :id clean-entry
                                :summary "A session begins from a clean repo or an explicitly resumed checked-out unit.")
                          (rule :id dirty-requires-custody
                                :summary "Any dirty worktree must be attached to an active checked-out work unit.")
                          (rule :id clean-exit
                                :summary "A session ends only when claimed repos are clean.")
                          (rule :id bounded-exit-paths
                                :summary "If dirty near session end, the only legal exits are commit, park, abandon-with-reason, or handoff."))
                         :note "This would effectively turn ~/code into a constrained workspace peripheral rather than an unconstrained directory."))
              (invariant :id reporting-pipeline-legibility
                         :family human-visible-inspectability
                         :status candidate
                         :summary "Derivative reports should remain readable and inspectable from stable local artifacts.")))

     (devmap :id futon1a
             :role "deterministic substrate with unresolved devmap relationship"
             :discovered-invariants
             ((invariant :id allowed-penholders-required-by-default
                         :family startup-contracts
                         :scope :repo
                         :status operational
                         :kind :foundational
                         :home (:repo "futon1a")
                         :summary "System startup requires an explicit penholder allowlist unless the operator opts out explicitly."
                         :implemented-in ("futon1a/system.clj")
                         :enforced-at ("futon1a.system/start!")
                         :evidenced-by ("futon1a/system_test.clj"
                                        "futon1a/README.md"))
              (invariant :id compat-penholder-must-be-allowed
                         :family startup-contracts
                         :scope :repo
                         :status operational
                         :kind :foundational
                         :home (:repo "futon1a")
                         :summary "Configured compat penholder must be a member of the allowed-penholders set."
                         :implemented-in ("futon1a/system.clj")
                         :enforced-at ("futon1a.system/start!")
                         :evidenced-by ("futon1a/system_test.clj"))
              (invariant :id internals-hidden-by-default
                         :family startup-contracts
                         :scope :repo
                         :status operational
                         :kind :foundational
                         :home (:repo "futon1a")
                         :summary "Raw XTDB node/store handles are hidden unless explicitly exposed."
                         :implemented-in ("futon1a/system.clj")
                         :enforced-at ("futon1a.system/start!")
                         :evidenced-by ("futon1a/system_test.clj"))
              (invariant :id strict-layer-order-l4-to-l0
                         :family layered-error-hierarchy
                         :scope :repo
                         :status operational
                         :kind :foundational
                         :home (:subsystem "futon1a/core/pipeline")
                         :summary "Writes run in strict L4 -> L3 -> L2 -> L1 -> L0 order, and lower layers do not run when higher layers fail."
                         :implemented-in ("futon1a/core/pipeline.clj")
                         :enforced-at ("futon1a.core.pipeline/run-write!"
                                       "futon1a.core.pipeline/run-open-world!")
                         :evidenced-by ("futon1a/cross_layer/pipeline_order_test.clj"))
              (invariant :id layer-specific-error-surface
                         :family layered-error-hierarchy
                         :scope :repo
                         :status operational
                         :kind :foundational
                         :home (:subsystem "futon1a/api")
                         :summary "Each layer reports its own error layer, status, and context through the HTTP surface."
                         :implemented-in ("futon1a/api/errors.clj"
                                          "futon1a/api/routes.clj")
                         :enforced-at ("futon1a.api.routes/write")
                         :evidenced-by ("futon1a/cross_layer/error_hierarchy_test.clj"
                                        "futon1a/cross_layer/interface_loop_test.clj"
                                        "futon1a/README.md"))
              (invariant :id penholder-and-tooling-allowlists
                         :family authorization-and-identity-discipline
                         :status operational
                         :summary "Writes require an allowed penholder, and tooling identities must pass an explicit tooling allowlist."
                         :implemented-in ("futon1a/auth/penholder.clj"
                                          "futon1a/core/pipeline.clj")
                         :evidenced-by ("futon1a/layer3/penholder_test.clj"
                                        "futon1a/api/write_test.clj"))
              (invariant :id external-identity-uniqueness
                         :family authorization-and-identity-discipline
                         :status operational
                         :summary "External identity mappings are canonicalized and conflicting source/external-id pairs are rejected."
                         :implemented-in ("futon1a/core/identity.clj"
                                          "futon1a/core/pipeline.clj")
                         :evidenced-by ("futon1a/cross_layer/pipeline_order_test.clj"))
              (invariant :id relation-endpoints-must-exist
                         :family authorization-and-identity-discipline
                         :status operational
                         :summary "Entity/relation validation rejects relations whose endpoints are missing from the entity set."
                         :implemented-in ("futon1a/core/entity.clj")
                         :evidenced-by ("futon1a/cross_layer/error_hierarchy_test.clj"))
             (invariant :id descriptor-declared-invariants-run-against-live-data
                         :family required-outputs
                         :scope :repo
                         :status operational
                         :kind :structural-law
                         :home (:subsystem "futon1a/model/verify")
                         :summary "Model descriptors can declare invariants that are verified against live XTDB state by scope."
                         :implemented-in ("futon1a/model/verify.clj")
                         :enforced-at ("futon1a.model.verify/verify-scope")))
             :candidate-invariants
             ((invariant :id devmap-scope-clarity
                         :family repo-role-clarity
                         :scope :repo
                         :status candidate
                         :kind :process-law
                         :home (:repo "futon1a")
                         :summary "The active repo scope should remain distinguishable from broader upstream devmap aspirations.")))

     (devmap :id futon3b
             :role "task and glacial coordination boundary with typed proof-paths"
             :discovered-invariants
             ((invariant :id first-failing-gate-wins
                         :family failure-locality
                         :scope :repo
                         :status operational
                         :kind :foundational
                         :home (:subsystem "futon3/gate/pipeline")
                         :summary "All coordinated task work traverses G5 -> G0 in order, and the first failing gate determines the result."
                         :implemented-in ("futon3b/src/futon3/gate/pipeline.clj")
                         :enforced-at ("futon3.gate.pipeline/run")
                         :evidenced-by ("futon3b/test/futon3/gate/pipeline_test.clj"))
              (invariant :id typed-proof-path-boundary
                         :family human-visible-inspectability
                         :scope :repo
                         :status operational
                         :kind :structural-law
                         :home (:subsystem "futon3/gate/shapes")
                         :summary "Gate records and proof-path events must fit small typed closed shapes."
                         :implemented-in ("futon3b/src/futon3/gate/shapes.clj")
                         :enforced-at ("futon3.gate.shapes/validate!")
                         :evidenced-by ("futon3b/src/futon3/gate/pipeline.clj"))
              (invariant :id rejection-still-persists-proof-path
                         :family failure-locality
                         :scope :repo
                         :status operational
                         :kind :foundational
                         :home (:subsystem "futon3/gate/pipeline")
                         :summary "Even rejected traversals persist a minimal proof-path so the glacial loop can observe failure."
                         :implemented-in ("futon3b/src/futon3/gate/pipeline.clj"
                                          "futon3b/src/futon3b/query/relations.clj")
                         :enforced-at ("futon3.gate.pipeline/run")
                         :evidenced-by ("futon3b/test/futon3/gate/pipeline_test.clj"))
              (invariant :id canonicalization-requires-recurrence-threshold
                         :family required-outputs
                         :scope :repo
                         :status operational
                         :kind :structural-law
                         :home (:subsystem "futon3/gate/canon")
                         :summary "Glacial canonization requires recurrence/context/evidence thresholds before new patterns are admitted."
                         :implemented-in ("futon3b/src/futon3/gate/canon.clj")
                         :enforced-at ("futon3.gate.canon/select-candidate")
                         :evidenced-by ("futon3b/test/futon3/gate/level1_test.clj"))
              (invariant :id no-silent-library-overwrite
                         :family artifact-custody
                         :scope :repo
                         :status operational
                         :kind :structural-law
                         :home (:subsystem "futon3/gate/canon")
                         :summary "Glacial canalization refuses to clobber an existing library pattern."
                         :implemented-in ("futon3b/src/futon3/gate/canon.clj")
                         :enforced-at ("futon3.gate.canon/canalize!")
                         :evidenced-by ("futon3b/test/futon3/gate/level1_test.clj")))
             :candidate-invariants
             ((invariant :id task-work-must-traverse-gates
                         :family atomic-inspectable-units
                         :scope :cross-repo
                         :status operational-but-bypassable
                         :kind :process-law
                         :home (:repo "futon3b")
                         :summary "Task-significant work should normally traverse futon3b's gate boundary, but ordinary live entrypoints still bypass it.")
                         :evidenced-by ("futon3/holes/war-room.md"
                                        "futon3/holes/missions/M-futon3x-e2e.md"
                                        "futon3c/emacs/agent-chat.el"))
              (invariant :id proof-paths-should-flow-to-evidence-landscape
                         :family artifact-custody
                         :scope :cross-repo
                         :status candidate
                         :kind :process-law
                         :home (:cross-repo "futon3b->futon3c")
                         :summary "Proof-paths should become evidence-landscape entries on the normal live path, not only in isolated integrations.")
                         :evidenced-by ("futon3/holes/war-room.md"
                                        "futon3/holes/war-bulletin-2.md"))))

     (devmap :id futon3
             :role "canon layer plus retained legacy tooling"
             :candidate-invariants
             ((invariant :id root-legibility-at-top-level
                         :family repo-role-clarity
                         :status candidate
                         :summary "Top-level layout should make canon, tooling, and legacy surfaces legible.")
              (invariant :id generated-builds-not-tracked-as-source
                         :family artifact-custody
                         :status candidate
                         :summary "Generated browser bundles or equivalent artifacts should not dominate the tracked source surface.")
              (invariant :id stash-archaeology-bounded
                         :family archaeology-control
                         :status candidate
                         :summary "Large hidden stash stacks should not persist indefinitely on main.")))

     (devmap :id futon3c
             :role "coordination/runtime layer with live peripherals and invariant logic"
             :discovered-invariants
             ((invariant :id mission-cycle-blocker-must-exist
                         :family existence
                         :scope :repo
                         :status operational
                         :kind :mission-law
                         :home (:peripheral "mission")
                         :summary "A mission cycle may begin only for an obligation already present in the mission obligation set."
                         :implemented-in ("futon3c/src/futon3c/peripheral/mission_backend.clj")
                         :enforced-at ("futon3c.peripheral.mission-backend/tool-cycle-begin")
                         :evidenced-by ("futon3c/test/futon3c/peripheral/mission_backend_test.clj"))
              (invariant :id mission-cycle-phase-ordering
                         :family phase-ordering
                         :scope :repo
                         :status operational
                         :kind :mission-law
                         :home (:peripheral "mission")
                         :summary "Mission cycles advance only along the declared observe -> propose -> execute -> validate -> classify -> integrate -> commit -> gate-review -> completed order."
                         :implemented-in ("futon3c/src/futon3c/peripheral/mission_backend.clj"
                                          "futon3c/src/futon3c/peripheral/mission_shapes.clj")
                         :enforced-at ("futon3c.peripheral.mission-backend/tool-cycle-advance")
                         :evidenced-by ("futon3c/test/futon3c/peripheral/mission_test.clj"))
              (invariant :id mission-phase-required-outputs
                         :family required-outputs
                         :scope :repo
                         :status operational
                         :kind :mission-law
                         :home (:peripheral "mission")
                         :summary "Each mission phase must emit its declared outputs before the cycle may advance."
                         :implemented-in ("futon3c/src/futon3c/peripheral/mission_backend.clj"
                                          "futon3c/src/futon3c/peripheral/mission_shapes.clj")
                         :enforced-at ("futon3c.peripheral.mission-backend/tool-cycle-advance")
                         :evidenced-by ("futon3c/test/futon3c/peripheral/mission_backend_test.clj"))
              (invariant :id mission-obligation-status-discipline
                         :family status-discipline
                         :scope :repo
                         :status operational
                         :kind :mission-law
                         :home (:peripheral "mission")
                         :summary "Mission obligations must use legal statuses, :assertion evidence cannot justify :done, and :abandoned is terminal."
                         :implemented-in ("futon3c/src/futon3c/peripheral/mission_backend.clj"
                                          "futon3c/src/futon3c/peripheral/mission_shapes.clj")
                         :enforced-at ("futon3c.peripheral.mission-backend/tool-obligation-upsert"
                                       "futon3c.peripheral.mission-backend/tool-status-validate")
                         :evidenced-by ("futon3c/test/futon3c/peripheral/mission_backend_test.clj"
                                        "futon3c/test/futon3c/peripheral/mission_test.clj"))
              (invariant :id mission-save-emits-readable-snapshot
                         :family human-visible-inspectability
                         :scope :repo
                         :status operational
                         :kind :reporting-law
                         :home (:peripheral "mission")
                         :summary "Durable mission saves emit a summarized mission snapshot into the evidence store so state remains inspectable outside the live session."
                         :implemented-in ("futon3c/src/futon3c/peripheral/mission.clj")
                         :enforced-at ("futon3c.peripheral.mission/state-snapshot")
                         :evidenced-by ("futon3c/test/futon3c/peripheral/mission_test.clj")))
             :candidate-invariants
             ((invariant :id peripheral-session-custody
                         :family peripheral-custody
                         :status candidate
                         :summary "Peripheral sessions should carry enough custody information to prevent repo/domain drift.")
              (invariant :id repl-turns-emit-evidence
                         :family human-visible-inspectability
                         :status candidate
                         :summary "Claude and Codex REPL turns should emit session-chained evidence entries on the normal live path rather than remaining only in transient Emacs buffers.")
              (invariant :id violations-become-obligations
                         :family atomic-inspectable-units
                         :status candidate
                         :summary "Structural-law violations should map to inspectable work units rather than ambient debt.")
              (invariant :id peripheral-budgeted-action
                         :family budgeted-action-selection
                         :status candidate
                         :summary "Peripheral routing should eventually choose next actions within explicit budget or license constraints, not by urgency alone.")))

     (devmap :id futon4
             :role "Arxana workspace and media/browser surface"
             :discovered-gates
             ((gate :id ui-focal-leftmost-managed
                    :family failure-locality
                    :scope :repo
                    :status operational-when-enabled
                    :kind :gate
                    :home (:subsystem "futon4/arxana-ui")
                    :summary "Exactly one managed window should be focal, and it should be the leftmost managed window."
                    :implemented-in ("futon4/dev/arxana-ui.el"
                                     "futon4/dev/arxana-window-constraints.el")
                    :enforced-at ("window-configuration-change-hook"
                                  "arxana-ui-refresh")
                    :evidenced-by ("futon4/test/arxana-browse-test.el"
                                   "futon0/contrib/futon-config.el"))
              (gate :id docbook-and-hud-layout-checks
                    :family failure-locality
                    :scope :repo
                    :status operational-when-enabled
                    :kind :gate
                    :home (:subsystem "futon4/arxana-window-constraints")
                    :summary "Docbook/browser/HUD/window-side layouts are validated against explicit Reazon relations."
                    :implemented-in ("futon4/dev/arxana-window-constraints.el")
                    :enforced-at ("arxana-window-constraints-validate-*")
                    :evidenced-by ("futon4/docs/docbook/futon4/futon4-00eed2b34c80.org"
                                   "futon0/contrib/futon-config.el"))
              (gate :id arxana-data-shape-checks
                    :family failure-locality
                    :scope :repo
                    :status operational-when-enabled
                    :kind :gate
                    :home (:subsystem "futon4/arxana-data-constraints")
                    :summary "Strategy, link, surface-form, hyperedge, inclusion, flexiarg, and bounce inputs are validated at interactive entry points."
                    :implemented-in ("futon4/dev/arxana-data-constraints.el")
                    :enforced-at ("arxana-links"
                                  "arxana-store"
                                  "arxana-media"
                                  "arxana-inclusion"
                                  "arxana-browser-patterns")
                    :evidenced-by ("futon4/dev/arxana-links.el"
                                   "futon4/dev/arxana-store.el"
                                   "futon4/dev/arxana-media.el"
                                   "futon0/contrib/futon-config.el")))
             :candidate-invariants
             ((invariant :id coherent-live-batches
                         :family atomic-inspectable-units
                         :status candidate
                         :summary "Live work should split into coherent browser/docs vs media/bounce batches rather than mixed drift.")
              (invariant :id old-arxana-stashes-classified
                         :family archaeology-control
                         :status candidate
                         :summary "Older Arxana work should be committed, parked, or dropped rather than left in stash debt.")))

     (devmap :id futon5
             :role "pattern/exotype/math tooling, but not canonical home for FrontierMath work"
             :candidate-invariants
             ((invariant :id math-tool-home-repo
                         :family atomic-inspectable-units
                         :status candidate
                         :summary "Problem-specific mathematical solver code should live in the canonical math repo, not in adjacent pattern repos.")
              (invariant :id mana-gated-work
                         :family budgeted-action-selection
                         :status candidate
                         :summary "Some proposed work should be selectable only when enough budget is available, but this remains weakly implemented.")))

     (devmap :id futon6
             :role "canonical home for FrontierMath and first-proof mathematics work"
             :candidate-invariants
             ((invariant :id frontiermath-artifact-locality
                         :family artifact-custody
                         :scope :repo
                         :status candidate
                         :kind :process-law
                         :home (:repo "futon6")
                         :summary "FrontierMath solver outputs and scratch artifacts should land in futon6 under an explicit harness home.")
              (invariant :id frontiermath-tool-consolidation
                         :family atomic-inspectable-units
                         :scope :repo
                         :status candidate
                         :kind :process-law
                         :home (:repo "futon6")
                         :summary "Problem-specific FrontierMath tools should consolidate here unless explicitly declared as shared infrastructure.")
              (invariant :id mathematically-important-terms-defined
                         :family human-visible-inspectability
                         :scope :cross-repo
                         :status candidate
                         :kind :structural-law
                         :home (:repo "futon6")
                         :summary "Operationally important mathematical terms should be defined, linked to a canonical definition, or explicitly marked as provisional."
                         :evidenced-by ("futon6/README-mentor.md"
                                        "futon6/data/first-proof/latex/sprint-review.tex"))
              (invariant :id convention-bridges-must-be-explicit
                         :family human-visible-inspectability
                         :scope :cross-repo
                         :status candidate
                         :kind :process-law
                         :home (:repo "futon6")
                         :summary "When a proof or argument crosses between non-equivalent definitions or notational conventions, the bridge must be stated and proved explicitly."
                         :evidenced-by ("futon6/README-mentor.md"
                                        "futon6/data/first-proof/latex/sprint-review.tex"
                                        "futon6/holes/missions/M-P7-rational-reconstruction.md"
                                        "futon6/holes/missions/M-P8-rational-reconstruction.md"))
              (invariant :id falsification-cycle-before-commitment
                         :family failure-locality
                         :scope :repo
                         :status candidate
                         :kind :process-law
                         :home (:repo "futon6")
                         :summary "Before committing to a proof direction or YES answer, perform at least one explicit falsification cycle asking what the simplest obstruction or NO direction would be."
                         :evidenced-by ("futon6/data/first-proof/latex/sprint-review.tex"))
              (invariant :id failure-patterns-become-reusable-checks
                         :family failure-locality
                         :scope :cross-repo
                         :status candidate
                         :kind :reporting-law
                         :home (:repo "futon6")
                         :summary "Failures should be reconstructed into named reusable check patterns so the same class of mistake becomes harder to repeat."
                         :evidenced-by ("futon6/README-mentor.md"
                                        "futon6/holes/missions/M-P7-rational-reconstruction.md"
                                        "futon6/holes/missions/M-P8-rational-reconstruction.md"))
              (invariant :id done-means-recoverable-math-conclusions
                         :family human-visible-inspectability
                         :scope :cross-repo
                         :status candidate
                         :kind :mission-law
                         :home (:repo "futon6")
                         :summary "Completed mathematical missions and retrospectives should leave conclusions that can be quickly recovered and reused, not only buried prose.")
              (invariant :id capability-bearing-math-work
                         :family budgeted-action-selection
                         :scope :repo
                         :status candidate
                         :kind :process-law
                         :home (:repo "futon6")
                         :summary "Mathematical work should expand reproducible mathematical capability, not merely produce isolated solver wins or one-off outputs.")))

  (handoff-1-2-working-set
    (operational-family-map
      ((map :family graph-symmetry
            :domains (portfolio tickle agency proof)
            :firm-exemplar (:repo futon3c
                            :artifact "src/futon3c/peripheral/proof_logic.clj")
            :gap "Mission intentionally stops short of a graph-symmetry layer today; Codex also leaves adjacent cross-store agreement pressure rather than a clean paired-edge projection.")
       (map :family status-discipline
            :domains (tickle agency proof mission codex futon1a)
            :firm-exemplar (:repo futon3c
                            :artifact "src/futon3c/peripheral/mission_logic.clj")
            :gap "Shared status helpers exist now; the remaining open pressure is Codex session continuity, which promoted into the candidate family below.")
       (map :family phase-ordering
            :domains (tickle proof mission)
            :firm-exemplar (:repo futon3c
                            :artifact "src/futon3c/peripheral/mission_logic.clj")
            :gap "Mission now has a logic-layer projection; the next question is whether any additional live domains should adopt the same prefix check.")
       (map :family required-outputs
            :domains (proof mission futon1a)
            :firm-exemplar (:repo futon3c
                            :artifact "src/futon3c/logic/structural_law.clj")
            :gap "The shared combinator now exists; remaining work is deciding where else explicit phase-output discipline belongs.")
       (map :family existence
            :domains (agency proof mission codex futon1a)
            :firm-exemplar (:repo futon3c
                            :artifact "src/futon3c/peripheral/mission_logic.clj")
            :gap "Mission blocker existence is now projected; full obligation dependency/reference existence is still not an always-on law.")
       (map :family dependency-satisfaction
            :domains (portfolio proof)
            :firm-exemplar (:repo futon3c
                            :artifact "src/futon3c/peripheral/proof_logic.clj")
            :gap "Mission has obligation DAG structure but no operational dependency-satisfaction law yet.")
       (map :family startup-contracts
            :domains (futon1a)
            :firm-exemplar (:repo futon1a
                            :invariant allowed-penholders-required-by-default)
            :gap "Recovered core family with one strong substrate exemplar so far.")
       (map :family layered-error-hierarchy
            :domains (futon1a)
            :firm-exemplar (:repo futon1a
                            :invariant strict-layer-order-l4-to-l0)
            :gap "Strong recovered substrate family; no second repo exemplar yet.")
       (map :family authorization-and-identity-discipline
            :domains (futon1a)
            :firm-exemplar (:repo futon1a
                            :invariant penholder-and-tooling-allowlists)
            :gap "Operational and well-evidenced in futon1a, but not yet mirrored elsewhere.")))
    (candidate-family-watchlist
      ((family :id human-visible-inspectability
               :best-current-exemplar typed-proof-path-boundary
               :repo futon3b
               :strength operational
               :note "Already has a fully firmed-up local exemplar.")
       (family :id failure-locality
               :best-current-exemplar first-failing-gate-wins
               :repo futon3b
               :strength operational
               :note "Candidate as a stack family, but already strong in futon3b and futon1a.")
       (family :id artifact-custody
               :best-current-exemplar no-silent-library-overwrite
               :repo futon3b
               :strength operational
               :note "Good example of a local custody law that could promote into a wider family.")
       (family :id atomic-inspectable-units
               :best-current-exemplar task-work-must-traverse-gates
               :repo futon3b
               :strength operational-but-bypassable
               :note "The shape is real, but normal live work can still route around it.")
       (family :id repo-role-clarity
               :best-current-exemplar none-yet
               :repo cleanup-pass
               :strength candidate
               :note "Strong pressure from cleanup evidence, but no fully operational exemplar yet.")
       (family :id archaeology-control
               :best-current-exemplar none-yet
               :repo cleanup-pass
               :strength candidate
               :note "Important workspace pressure, still lacking a firm enforced structure.")
       (family :id peripheral-custody
               :best-current-exemplar none-yet
               :repo futon3c
               :strength candidate
               :note "Mission and proof peripherals show the shape, but the custody law is not yet first-class.")
       (family :id budgeted-action-selection
               :best-current-exemplar none-yet
               :repo futon5
               :strength candidate
               :note "Discussed and partially sketched, but no fully operational exemplar yet.")
       (family :id cross-store-agreement
               :best-current-exemplar session-continuity-alignment
               :repo futon3c
               :strength candidate-with-live-exemplar
               :note "Codex Code proves the pressure is real: existence and status discipline cover part of the surface, but session continuity and mirrored metadata still form their own candidate family."))))

  (cleanup-evidence
    ((repo :id futon0
           :evidence "mixed HUD, systemd, podcast, and rhythm work had to be separated into coherent commits"
           :implicates (atomic-inspectable-units repo-role-clarity))
     (repo :id futon1a
           :evidence "README needed to distinguish active substrate from unresolved devmap story"
           :implicates (repo-role-clarity))
     (repo :id futon3
           :evidence "root was cluttered; generated JS bundle was tracked; stash archaeology obscured current state"
           :implicates (repo-role-clarity artifact-custody archaeology-control))
     (repo :id futon3c
           :evidence "active reliability work was coherent, but peripheral and bridge stashes accumulated heavily"
           :implicates (archaeology-control peripheral-custody))
     (repo :id futon4
           :evidence "live work split cleanly into browser/docs vs media/bounce, but old Arxana stashes lingered; Arxana also has real Reazon-backed validation gates when enabled"
           :implicates (atomic-inspectable-units archaeology-control failure-locality))
     (repo :id futon5
           :evidence "FM-001 tool landed in the wrong repo; hybrid wiring change was recoverable from stash; mana-gated work was proposed but not fully wired"
           :implicates (atomic-inspectable-units artifact-custody budgeted-action-selection))
     (repo :id futon6
           :evidence "FM-001b artifacts needed a canonical harness home; proof and superpod stashes required parking; First Proof retrospectives surfaced definition drift, problem substitution, and missing falsification as recurring mathematical failure risks"
           :implicates (artifact-custody archaeology-control human-visible-inspectability failure-locality))))

  (immediate-questions
    ((question :id Q1
               :text "Should atomic inspectable units become a first-class structural-law family?")
     (question :id Q2
               :text "Should every mission/problem/peripheral session carry a canonical home-repo?")
     (question :id Q3
               :text "Should cleanup conditions like stash debt and main-branch coherence become structural-law checks rather than hygiene folklore?")
     (question :id Q4
               :text "Should peripheral stop events normalize into explicit check-in outcomes such as :committed, :parked, :abandoned, or :escalated?")
     (question :id Q5
               :text "Which candidate invariants deserve promotion to foundational always-on invariants, and which should remain optional gates?")))

  (next-step
    (plan
      "Treat this inventory as the candidate-invariant ledger and proto-registry for Phase 1. Classify each family as operational, candidate, gate, or rejected; keep a firm exemplar for each active family; and only then decide which ones deserve logic-layer extraction or eventual promotion into a dedicated invariant surface."))
