;; Peripheral AIF Terminal Vocabularies
;;
;; Each peripheral that agents inhabit gets an AIF head. This file is the
;; canonical registry of what each head observes, believes, selects, and
;; acts on — the "feature grid" per peripheral.
;;
;; Format follows structural-law-inventory.sexp: machine-readable,
;; regenerable narrative companion planned.
;;
;; Relationship to existing terminal vocabularies:
;;   core-terminal-vocabulary.md    = abstract AIF/GFE structure (the schema)
;;   cyberants-terminal-vocabulary  = ants domain instantiation
;;   fulab-terminal-vocabulary      = agent session instantiation
;;   nonstarter-terminal-vocabulary = policy simulation instantiation
;;   joe-terminal-vocabulary        = personal life instantiation
;;   THIS FILE                      = per-peripheral instantiation for futon3c
;;
;; The three conceptual pillars map onto AIF roles:
;;   Argument (holistic-argument-sketch)     → generative model content
;;   Invariants (structural-law-inventory)   → precision landscape
;;   Futonic Missions (futonic-missions.md)  → action policy structure

;; =============================================================================
;; PORTFOLIO INFERENCE (the slow-timescale head)
;; =============================================================================
;;
;; Status: OPERATIONAL (futon3c/src/futon3c/portfolio/)
;; Timescale: slow (runs between mission cycles, ~hours)
;; AIF template: futon2 FulabAdapter (softmax/G/tau)
;; Missing: effect loop (action → state changes)

(:peripheral/id :portfolio-inference
 :peripheral/status :operational
 :peripheral/timescale :slow
 :peripheral/pillar-connections
 {:argument   :partial  ;; doesn't yet read support claims
  :invariants :none     ;; doesn't consult structural law inventory
  :missions   :direct}  ;; reads mission counts, phases, blockers

 ;; ── Observables (o) ──────────────────────────────────────────────
 ;; 15 channels, all normalized to [0,1]
 ;; Source: portfolio/observe.clj
 ;; ABI: portfolio/observe/channel-keys

 :observation-channels
 [{:id :mission-complete-ratio
   :source :mc-backend
   :type :ratio
   :range [0.0 1.0]
   :precision 1.0
   :pillar :missions
   :description "Fraction of missions in :complete status"}

  {:id :coverage-pct
   :source :mc-backend
   :type :percentage
   :range [0.0 1.0]
   :precision 1.0
   :pillar :missions
   :description "Average devmap coverage across repos"}

  {:id :coverage-trajectory
   :source :mc-backend
   :type :slope
   :range [0.0 1.0]  ;; rescaled from [-1,1]
   :precision 0.6
   :pillar :missions
   :description "Coverage slope: improving (>0.5) or declining (<0.5)"}

  {:id :mana-available
   :source :mc-backend
   :type :ratio
   :range [0.0 1.0]
   :precision 0.8
   :pillar :none  ;; M-cyder territory
   :description "Budget headroom (pool balance / cap)"}

  {:id :blocked-ratio
   :source :mc-backend
   :type :ratio
   :range [0.0 1.0]
   :precision 1.0
   :pillar :missions
   :description "Fraction of missions in :blocked status"}

  {:id :evidence-velocity
   :source :evidence-store
   :type :rate
   :range [0.0 1.0]  ;; capped at 20/day
   :precision 0.5
   :pillar :argument  ;; evidence accumulates toward support claims
   :description "Evidence entries per day (normalized)"}

  {:id :dependency-depth
   :source :mc-backend
   :type :count
   :range [0.0 1.0]  ;; capped at 10
   :precision 0.7
   :pillar :missions
   :description "Max blocked-by chain length across missions"}

  {:id :gap-count
   :source :mc-backend
   :type :count
   :range [0.0 1.0]  ;; capped at 120
   :precision 0.9
   :pillar :missions
   :description "Uncovered gaps in devmaps"}

  {:id :stall-count
   :source :mc-backend
   :type :ratio
   :range [0.0 1.0]
   :precision 0.7
   :pillar :missions
   :description "Missions with no recent progress (blocked + unknown)"}

  {:id :spinoff-pressure
   :source :mc-backend
   :type :count
   :range [0.0 1.0]  ;; capped at 40
   :precision 0.4
   :pillar :missions
   :description "Gaps suggesting new missions needed"}

  {:id :pattern-reuse
   :source :placeholder
   :type :ratio
   :range [0.0 1.0]
   :precision 0.3
   :pillar :argument  ;; reuse validates the pattern library
   :description "Pattern reuse ratio (PLACEHOLDER: always 0)"}

  {:id :review-age
   :source :evidence-store
   :type :duration
   :range [0.0 1.0]  ;; capped at 14 days
   :precision 1.0
   :pillar :missions
   :description "Days since last portfolio review"}

  ;; Heartbeat-derived channels (T-7)
  {:id :effort-prediction-error
   :source :heartbeat
   :type :error
   :range [0.0 1.0]
   :precision 0.6
   :pillar :missions
   :description "How wrong effort estimates were (heartbeat)"}

  {:id :bid-completion-rate
   :source :heartbeat
   :type :ratio
   :range [0.0 1.0]
   :precision 0.7
   :pillar :missions
   :description "Fraction of planned actions actually taken"}

  {:id :unplanned-work-ratio
   :source :heartbeat
   :type :ratio
   :range [0.0 1.0]
   :precision 0.5
   :pillar :missions
   :description "Fraction of work that was unplanned"}]

 ;; ── Beliefs (μ) ──────────────────────────────────────────────────
 ;; Source: portfolio/perceive.clj

 :belief-state
 {:sens  "Per-channel predictions (same shape as observation)"
  :mode  "Inferred portfolio mode: :BUILD, :CONSOLIDATE, :REVIEW, :WAIT"
  :focus "Current mission focus (nil = no focus)"
  :urgency "Scalar [0,1]: how urgently action is needed"}

 :perception
 {:method :predictive-coding
  :micro-steps 3
  :alpha 0.55        ;; sensory learning rate
  :beta 0.08         ;; urgency learning rate
  :free-energy "0.5 × mean(precision × raw²)"}

 ;; ── Preferences (C) ──────────────────────────────────────────────
 ;; Currently implicit in policy weights; should become explicit
 ;; via structural law inventory consultation

 :preferences
 {:current-source :implicit-in-policy-weights
  :target-source  :structural-law-inventory
  :preferred      "High coverage, low gaps, high evidence velocity"
  :avoided        "High blocked-ratio, high stall-count, stale review"
  :mode-prior     {:BUILD 0.5 :CONSOLIDATE 0.2 :REVIEW 0.2 :WAIT 0.1}}

 ;; ── Policies (π) / Actions (a) ──────────────────────────────────
 ;; Source: portfolio/policy.clj

 :action-arena
 {:arena/id :portfolio-management
  :arena/participants [:human :claude :codex]
  :arena/actions [:work-on :review :consolidate :upvote :wait]
  :arena/rules {:mode-gate true :mana-gate true :upvote-decay 0.10}}

 :efe-weights
 {:pragmatic 0.6    ;; goal progress
  :epistemic 0.4    ;; uncertainty reduction
  :upvote    0.3    ;; collective desire signal
  :effort    0.2}   ;; effort penalty

 :policy
 {:method :softmax
  :temperature :tau       ;; from perception
  :abstain-threshold 0.55 ;; below this τ → :wait
  :selection :argmax}     ;; deterministic default; :rng for stochastic

 ;; ── Free Energy (G) ─────────────────────────────────────────────

 :free-energy
 {:formula "G(a) = λ_effort·effort(a) - λ_pragmatic·pragmatic(a) - λ_epistemic·epistemic(a) - λ_upvote·upvote(a)"
  :pragmatic "Channel error magnitudes weighted by action relevance"
  :epistemic "Low-precision channel count / information gain potential"
  :upvote    "PLACEHOLDER: 0 until Nonstarter integration"
  :effort    "Static cost modulated by effort-prediction-error"}

 ;; ── Evidence emission ───────────────────────────────────────────

 :evidence-types
 [:portfolio/observation   ;; raw observation snapshot
  :portfolio/belief        ;; mu + precision after perceive
  :portfolio/policy]       ;; action selection + G scores

 ;; ── Gaps (what M-aif-head must build) ───────────────────────────

 :gaps
 [{:id :effect-loop
   :description "Portfolio produces :action but nothing consumes it"
   :blocks [:default-mode :automated-dispatch]}

  {:id :pillar-connection-argument
   :description "No channel reads holistic argument support claims"
   :blocks [:model-adequacy]}

  {:id :pillar-connection-invariants
   :description "No channel consults structural law inventory"
   :blocks [:preference-exogeneity]}

  {:id :pattern-reuse-channel
   :description "Pattern reuse ratio is always 0 (placeholder)"
   :blocks [:epistemic-accuracy]}])

;; =============================================================================
;; MISSION PERIPHERAL (the deliberative head — M-aif-head Phase 1)
;; =============================================================================
;;
;; Status: NO AIF HEAD (has cycle engine, obligations, gates, but no
;;         generative model, no prediction error, no default mode)
;; Timescale: fast-medium (within mission cycles, ~minutes to hours)
;; AIF template: to be designed (this mission)
;; Missing: everything in the AIF column

(:peripheral/id :mission
 :peripheral/status :no-aif-head
 :peripheral/timescale :fast-medium
 :peripheral/pillar-connections
 {:argument   :none
  :invariants :none
  :missions   :structural}  ;; IS the mission infrastructure

 ;; ── Observables (o) ──────────────────────────────────────────────
 ;; PROPOSED — does not exist yet
 ;; These are the channels the Mission Peripheral AIF head would need

 :observation-channels
 [{:id :phase-progress
   :source :cycle-engine
   :type :ordinal
   :range [0.0 1.0]  ;; 0=observe, 1=completed (9 phases)
   :precision 1.0
   :pillar :missions
   :description "Current phase as fraction of 9-phase cycle"}

  {:id :obligation-satisfaction
   :source :mission-backend
   :type :ratio
   :range [0.0 1.0]
   :precision 1.0
   :pillar :missions
   :description "Fraction of obligations in :done or :proved status"}

  {:id :required-outputs-present
   :source :mission-shapes
   :type :ratio
   :range [0.0 1.0]
   :precision 1.0
   :pillar :missions
   :description "Fraction of required phase outputs present"}

  {:id :structural-law-compliance
   :source :logic/inventory
   :type :ratio
   :range [0.0 1.0]
   :precision 0.9
   :pillar :invariants
   :description "Fraction of applicable structural laws satisfied"}

  {:id :prediction-divergence
   :source :cross-phase-compare
   :type :error
   :range [0.0 1.0]
   :precision 0.7
   :pillar :missions
   :description "Divergence between :propose predictions and :execute actuals"}

  {:id :evidence-for-completion-criteria
   :source :evidence-store
   :type :count
   :range [0.0 1.0]  ;; normalized by criteria count
   :precision 0.8
   :pillar :argument
   :description "Evidence entries supporting mission completion criteria"}

  {:id :gate-readiness
   :source :gate-pipeline
   :type :ratio
   :range [0.0 1.0]
   :precision 0.9
   :pillar :missions
   :description "How many gate checks (G0-G5) would currently pass"}

  {:id :argument-claim-coverage
   :source :holistic-argument
   :type :ratio
   :range [0.0 1.0]
   :precision 0.6
   :pillar :argument
   :description "How many support claims this mission addresses have evidence"}

  {:id :cycle-count
   :source :cycle-engine
   :type :count
   :range [0.0 1.0]  ;; normalized, e.g. cap at 10
   :precision 0.5
   :pillar :missions
   :description "Number of cycles completed for this mission"}

  {:id :days-since-last-activity
   :source :evidence-store
   :type :duration
   :range [0.0 1.0]  ;; capped at 14 days
   :precision 0.8
   :pillar :missions
   :description "Staleness: days since last evidence for this mission"}]

 ;; ── Beliefs (μ) ──────────────────────────────────────────────────
 ;; PROPOSED

 :belief-state
 {:sens "Per-channel predictions"
  :mode "Inferred mission mode: :PROGRESSING, :STUCK, :COMPLETING, :IDLE"
  :phase "Current cycle phase (from cycle engine)"
  :urgency "Scalar [0,1]: how urgently the next phase transition is needed"
  :confidence "Scalar [0,1]: confidence that current approach will succeed"}

 ;; ── Preferences (C) ──────────────────────────────────────────────
 ;; PROPOSED — sourced from structural law inventory + completion criteria

 :preferences
 {:current-source :none
  :target-source  [:structural-law-inventory :completion-criteria]
  :preferred      "High obligation satisfaction, law compliance, gate readiness"
  :avoided        "High prediction divergence, staleness, low compliance"
  :mode-prior     {:PROGRESSING 0.6 :COMPLETING 0.2 :STUCK 0.1 :IDLE 0.1}}

 ;; ── Actions (a) ──────────────────────────────────────────────────
 ;; PROPOSED — maps onto cycle engine operations

 :action-arena
 {:arena/id :mission-cycle
  :arena/participants [:agent]
  :arena/actions [:advance-phase :revise-approach :request-review
                  :save-state :signal-blocked :signal-complete]
  :arena/rules {:phase-gate true :obligation-gate true :law-gate true}}

 ;; ── Gaps ────────────────────────────────────────────────────────

 :gaps
 [{:id :no-observation-layer
   :description "No normalized observation vector exists for mission state"
   :blocks [:perception :prediction-error]}

  {:id :no-generative-model
   :description "No explicit model of what mission progress looks like"
   :blocks [:model-adequacy :refusal]}

  {:id :no-default-mode
   :description "Cycle completion → dead stop, no inter-cycle behavior"
   :blocks [:compositional-closure]}

  {:id :no-prediction-error
   :description ":propose produces narrative, not structured predictions"
   :blocks [:cross-phase-divergence]}

  {:id :no-refusal-surface
   :description "Structural laws not consulted before transitions"
   :blocks [:preference-exogeneity]}])

;; =============================================================================
;; JOE (the sovereign as participant — exogenous agent, observable effects)
;; =============================================================================
;;
;; Status: OBSERVABLE (joe-terminal-vocabulary.md defines the events)
;; Timescale: slow-glacial (days to weeks)
;; AIF template: none — Joe is not "run" by the engine; the engine
;;   models Joe's participation and generates prediction error when
;;   behavioral traces diverge from what the stack needs.
;;
;; Key distinction: Joe cannot be "inhabited" by an LLM agent. But Joe
;; IS inhabited by a human consciousness, and the stack instantiates
;; into Joe's wetware as another engine that emits affect signals,
;; makes commitments, and produces behavioral traces. The AIF head
;; for the Joe peripheral doesn't control Joe — it models what Joe's
;; participation looks like when the stack is healthy, and detects
;; when it isn't.
;;
;; Why this matters beyond philosophy: Joe's behavioral constraints
;; are not verified with core.logic, but their DOWNSTREAM EFFECTS are.
;; "Joe didn't upload recordings" → evidence-velocity drops →
;; portfolio inference sees stale review-age → same diagnostic path
;; as "agent didn't emit PUR." The metaphor becomes computational.
;;
;; I4 implication: The sovereign is also a participant in the model.
;; Preference exogeneity applies to Joe too — that's what "I'm sorry,
;; Joe" means. The structural law inventory is the constitution;
;; the AIF head enforces it against all actors.

(:peripheral/id :joe
 :peripheral/status :observable
 :peripheral/timescale :slow-glacial
 :peripheral/agent-type :exogenous-human  ;; unique: not LLM-inhabited
 :peripheral/pillar-connections
 {:argument   :embodied  ;; Joe IS the argument's author; traces are indirect
  :invariants :sovereign ;; Joe sets invariants but is also constrained by them
  :missions   :direct}   ;; Joe creates, prioritizes, and works on missions

 ;; ── Observables (o) ──────────────────────────────────────────────
 ;; Source: joe-terminal-vocabulary.md events + downstream effects
 ;; These channels are NOT read from Joe's internal states (unknowable)
 ;; but from behavioral traces the stack can observe.

 :observation-channels
 [{:id :evidence-emission-rate
   :source :evidence-store
   :type :rate
   :range [0.0 1.0]  ;; capped at evidence-per-day-cap
   :precision 0.8
   :pillar :argument
   :description "Evidence entries per day attributed to Joe's sessions"
   :joe-vocab-terminal "life/commit, life/coding-session"}

  {:id :recording-currency
   :source :life-events
   :type :duration
   :range [0.0 1.0]  ;; 0=stale (>7 days), 1=fresh (today)
   :precision 0.7
   :pillar :argument  ;; recordings feed the generative model (Joe's thinking)
   :description "Freshness of uploaded recordings (ZoomR4 → evidence)"
   :joe-vocab-terminal "life/recording"}

  {:id :review-cadence
   :source :evidence-store
   :type :duration
   :range [0.0 1.0]  ;; normalized by review-age-cap (14 days)
   :precision 1.0
   :pillar :missions
   :description "Days since last portfolio review (inverse: 1=fresh)"
   :joe-vocab-terminal "implicit — portfolio review evidence"}

  {:id :bid-completion-rate
   :source :heartbeat
   :type :ratio
   :range [0.0 1.0]
   :precision 0.7
   :pillar :missions
   :description "Fraction of committed bids actually completed"
   :joe-vocab-terminal "life/bid, life/clear"}

  {:id :unplanned-work-ratio
   :source :heartbeat
   :type :ratio
   :range [0.0 1.0]
   :precision 0.5
   :pillar :missions
   :description "Fraction of work that wasn't bid (reactive vs planned)"
   :joe-vocab-terminal "life/commit without matching bid"}

  {:id :morning-protocol-adherence
   :source :life-events
   :type :binary
   :range [0.0 1.0]  ;; 0=skipped, 1=complete
   :precision 0.6
   :pillar :none  ;; personal viability, not stack-specific
   :description "Did the morning protocol happen (recording + be-human)?"
   :joe-vocab-terminal "life/recording, life/be-human"}

  {:id :mission-scoping-discipline
   :source :git-events
   :type :binary
   :range [0.0 1.0]
   :precision 0.9
   :pillar :missions
   :description "Did implementation work have a preceding IDENTIFY→DERIVE?"
   :joe-vocab-terminal "implicit — git commits vs mission phase"}

  {:id :preference-stability
   :source :structural-law-inventory
   :type :rate
   :range [0.0 1.0]  ;; 0=no changes, 1=high churn
   :precision 0.8
   :pillar :invariants
   :description "Rate of structural law changes (I4: shouldn't be fast)"
   :joe-vocab-terminal "implicit — inventory.sexp git history"}

  {:id :agent-engagement
   :source :evidence-store
   :type :count
   :range [0.0 1.0]  ;; normalized by agent count
   :precision 0.5
   :pillar :missions
   :description "Are agents being dispatched and producing evidence?"
   :joe-vocab-terminal "implicit — conductor dispatch + agent evidence"}

  {:id :quadrant-balance
   :source :life-events
   :type :distribution
   :range [0.0 1.0]  ;; entropy of Q1-Q4 allocation
   :precision 0.4
   :pillar :none  ;; personal viability
   :description "Evenness of time across quadrants (Q1-Q4)"
   :joe-vocab-terminal "life/coding-session, life/meeting, etc."}]

 ;; ── Beliefs (μ) ──────────────────────────────────────────────────

 :belief-state
 {:sens "Per-channel predictions of Joe's behavioral traces"
  :mode "Inferred engagement mode: :ACTIVE, :REVIEWING, :PLANNING, :AWAY"
  :focus "Current mission or quadrant focus (nil = diffuse)"
  :urgency "Scalar [0,1]: how urgently Joe's attention is needed"
  :viability "Scalar [0,1]: is the morning protocol / life admin on track?"}

 ;; ── Preferences (C) ──────────────────────────────────────────────
 ;; These are what the stack needs from Joe, expressed as preferred
 ;; observation ranges. NOT what Joe wants — what the stack requires.

 :preferences
 {:current-source :joe-terminal-vocabulary
  :target-source  [:structural-law-inventory :bid-clear-mechanics]
  :preferred      "High evidence rate, fresh reviews, bids completed, morning done"
  :avoided        "Dark work (no evidence), stale reviews, preference churn, Q4-only"
  :mode-prior     {:ACTIVE 0.5 :REVIEWING 0.2 :PLANNING 0.2 :AWAY 0.1}}

 ;; ── Actions (a) ──────────────────────────────────────────────────
 ;; The "actions" here are the stack's responses to Joe's behavior,
 ;; not actions Joe takes. The stack nudges, alerts, and refuses.

 :action-arena
 {:arena/id :joe-engagement
  :arena/participants [:stack]  ;; stack acts toward Joe, not vice versa
  :arena/actions [:nudge :alert :refuse :acknowledge :recalibrate]
  :arena/rules {:reflex-gate true   ;; hard alerts for critical failures
                :default-mode true   ;; gentle nudges for drift
                :i4-enforcement true}} ;; "I'm sorry, Joe"

 ;; ── Three-tier responses ────────────────────────────────────────

 :tiers
 {:reflex    "No evidence in 7 days → hard alert (infrastructure broken?)"
  :default   "Review-age climbing, stall-count rising → gentle nudge"
  :deliberative "Effort-prediction-error high → recalibrate bid expectations"}

 ;; ── Gaps ────────────────────────────────────────────────────────

 :gaps
 [{:id :no-life-event-ingestion
   :description "joe-terminal-vocabulary events not yet flowing into evidence store"
   :blocks [:recording-currency :morning-protocol :quadrant-balance]}

  {:id :no-downstream-verification
   :description "Behavioral constraints not checked via core.logic on effects"
   :blocks [:mission-scoping-discipline :preference-stability]}

  {:id :no-nudge-mechanism
   :description "Stack has no channel to signal Joe (no push notifications)"
   :blocks [:nudge :alert]}])

;; =============================================================================
;; TEMPLATE for remaining peripherals (Phase 2)
;; =============================================================================
;;
;; Each peripheral that agents inhabit will get an entry following this shape.
;; Phase 2 peripherals share the reusable AIF head interface from Phase 1
;; but need domain-specific observation channels.
;;
;; Peripherals needing entries (agent-inhabited):
;;   :chat, :explore, :edit, :deploy, :reflect, :mentor,
;;   :proof, :discipline, :test-runner, :alfworld

;; (:peripheral/id :TEMPLATE
;;  :peripheral/status :no-aif-head
;;  :peripheral/timescale :TBD
;;  :peripheral/pillar-connections {:argument :TBD :invariants :TBD :missions :TBD}
;;  :observation-channels [...]
;;  :belief-state {...}
;;  :preferences {...}
;;  :action-arena {...}
;;  :gaps [...])
