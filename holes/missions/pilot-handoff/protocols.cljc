(ns futon7a.placemat.storyboard.protocols
  "The storyboard's domain-specific language.

  This namespace defines the vocabulary in which scenes are written.
  Every scene-file requires this namespace and writes its scene-function
  using the records, protocols, and helpers defined here.

  Design discipline: HtDP-skeleton. Contracts and purposes come first;
  stubs return placeholder values that satisfy the contract; real bodies
  are filled in by Claude Code against the live futon-stack machinery.

  Cross-references (to be wired up by implementer):
  - depth-frames correspond to ~/code/futon3/docs/four-frame-correspondence.md (TODO: confirm path)
  - substrate-elements correspond to ~/code/futon7a/data/grand-unified-placemat.edn :substrate-layer
  - patterns correspond to the Flexiarg pattern library in futon3/library/
  - field-cleaning vocabulary corresponds to the third-jhana phenomenology
    described in the Operator's Foreword (TODO: link)
  - AIF events correspond to the canonical event taxonomy
    (TODO: confirm location; predecessor mission is M-interest-network-coupling)")

;; =====================================================================
;; CORE TYPES
;; =====================================================================

;; A Depth is one of four canonical frames at which observation occurs.
;; Canonical traversal order: :pheno → :geno → :exo → :xeno.
;; Cross-ref: the four-frame correspondence / futon5 canon
;;   :pheno → rūpa     (body / observed level of code)
;;   :geno  → saṅkhāra (mental formations / observed AIF signals)
;;   :exo   → citta    (mind / observed textual distillation)
;;   :xeno  → dhammas  (phenomena / observed semantic distillation)
;;
;; Each depth is a *register* — an observational surface at which
;; movement becomes detectable when the surface is sufficiently quiet.

(def depths
  "The four canonical depths, in descent order. Ascent is the reverse."
  [:pheno :geno :exo :xeno])

;; A FieldState records whether a depth's register is clean enough
;; for movement to be detectable. The third-jhana intuition: the cleaner
;; the field, the more visible the movement against it.
;; Values: :noisy | :clearing | :clean

(defrecord FieldState [depth state evidence])

;; A DepthMove records a transition between depths.
;; - direction is :descend or :ascend
;; - from and to are adjacent in `depths`
;; - field-state captures the destination's state on arrival

(defrecord DepthMove [direction from to field-state])

;; A SubstrateCheck records the result of putting a candidate move
;; through the coherence-rejection mechanism at a particular depth.
;; Each depth has a characteristic substrate:
;;   :pheno → compiler + types + tests
;;   :geno  → AIF + active inference machinery
;;   :exo   → Arxana Essay format + writing-coherence patterns
;;   :xeno  → Flexiarg pattern library + futonic-mission format

(defrecord SubstrateCheck [depth substrate-id candidate result rejection-reason])

;; A PatternInstance records a recognized instance of a Flexiarg pattern
;; (or other named pattern) being instantiated by the scene.

(defrecord PatternInstance [pattern-id at-depth instance-note])

;; A Scene is the unit of storyboard authorship.
;; It carries enough structured data that any of the renderers
;; (war-machine playbook, browser level, slide deck, REPL printer)
;; can produce its mode-specific rendering without further input.

(defrecord Scene
  [;; Identity
   id                 ; keyword, e.g. :scene-04-pilot-phase-3
   title              ; human-readable string
   world              ; one of :futon-stack :war-machine :vsatarcs :placemat
   stack-position     ; int 1..4, matches the reader-stack levels
   predecessor        ; scene-id this one builds on, or nil

   ;; Phenomenological structure — the depth-traversal
   opening-field      ; map of depth → FieldState before play begins
   moves              ; ordered seq of DepthMove + SubstrateCheck + PatternInstance
   closing-field      ; map of depth → FieldState after play
   resolution         ; map: :resolution :commercial-consequence :evidence-emitted

   ;; Mode-specific payloads (each renderer may use a subset)
   pheno-data         ; the EDN-shape this scene contributes to the placemat
   geno-functions     ; seq of fn-syms; the dispositional structure
   exo-narration      ; map of :short and :long narration strings
   xeno-patterns      ; seq of PatternInstance; what patterns this scene exhibits
   pilot-cycle        ; optional 4-card frame for stepping the scene operationally
   ])

(def ^:private depth->index
  (zipmap depths (range)))

(defn- adjacent-depth?
  [from-depth to-depth step]
  (= (+ (get depth->index from-depth -99) step)
     (get depth->index to-depth -1)))

(defn- initial-field-state
  [depth]
  (->FieldState depth :clearing "Opening field defaults to :clearing."))

(defn- initial-field-map
  []
  (into {}
        (map (fn [depth]
               [depth (initial-field-state depth)]))
        depths))

(defn- current-field-state
  [scene depth]
  (or (get (:closing-field scene) depth)
      (get (:opening-field scene) depth)
      (initial-field-state depth)))

(defn- append-move
  [scene move]
  (update scene :moves (fnil conj []) move))

(defn- rejected-depth-move
  [direction from-depth to-depth reason]
  {:move/type :depth-move-rejected
   :direction direction
   :from from-depth
   :to to-depth
   :reason reason})

;; =====================================================================
;; RENDERING PROTOCOL
;; =====================================================================

(defprotocol SceneRenderer
  "A SceneRenderer takes a Scene and produces a mode-specific artifact.

  Each renderer lives in its own namespace under renderers/.
  Renderers are *the only* code that should know about presentation.
  Scenes are pure data; renderers are the transports.

  Mode A — war-machine pilot inhabitation: render-as-playbook
  Mode B — playable browser level:         render-as-level
  Mode C — slide deck:                     render-as-slide
  Mode D — REPL reference document:        render-as-repl-output"

  (render-as-playbook    [renderer scene]
    "Returns an enactable mission-plan for the war-machine pilot.
     Output shape: TODO — confirm against M-war-machine-pilot peripheral spec.")

  (render-as-level       [renderer scene]
    "Returns a Reagent-compatible component spec for browser play.
     Output shape: hiccup-style vector, or component descriptor.
     CLJS-only renderer; stub on JVM side.")

  (render-as-slide       [renderer scene]
    "Returns a slide specification — title, body, transitions, speaker-notes.
     Output shape: TODO — pick slide-format target (reveal.js? Marp?
     custom Reagent? raw EDN that downstream tools render?)")

  (render-as-repl-output [renderer scene]
    "Returns a string suitable for println at the REPL.
     The simplest renderer; proves the multi-mode story end-to-end."))

;; =====================================================================
;; SCENE-CONSTRUCTION HELPERS
;; =====================================================================
;;
;; These are the verbs scene-authors use. Each helper has a contract
;; and a stub. Real bodies are filled in by Claude Code.
;;
;; The naming intent: reading a scene-function aloud should feel like
;; reading the methodology being applied. The helpers are named in the
;; methodology's own vocabulary.

(defn open-field
  "Begins a scene by establishing initial field-states at every depth.

  Contract: opts → Scene (in progress)
  - opts :: {:level keyword, :stack-position int, :predecessor keyword-or-nil,
             :id keyword, :title string, :world keyword}
  - returns :: a Scene record with opening-field populated and moves [].

  Field-state defaults: TODO — by default treat all depths as :clearing,
  but allow opts to override (e.g. a scene that begins mid-mission inherits
  the predecessor's closing-field as its opening-field).

  Example:
    (open-field {:id :scene-04-pilot-phase-3
                 :title \"The pilot lands Phase 3\"
                 :world :war-machine
                 :stack-position 2
                 :predecessor :scene-03-pilot-phase-2})"
  [opts]
  (let [field-map (initial-field-map)]
    (->Scene (:id opts) (:title opts) (:world opts) (:stack-position opts)
             (:predecessor opts)
             field-map [] field-map nil
             nil nil nil nil nil)))

(defn clean-register
  "Records a field-cleaning operation at a depth.

  Contract: Scene × depth × evidence-string → Scene
  - Appends a FieldState change to :moves, transitioning that depth
    from :noisy or :clearing toward :clean.
  - evidence-string explains *what* makes the field cleaner — the
    observation, articulation, or recognition that quieted the register.

  This is the load-bearing methodological move. A depth cannot be
  meaningfully observed until its register is clean enough; cleaning
  is itself an act, and the act must be evidenced.

  Example:
    (clean-register scene :pheno
      \"WM UI compiles; anchor-flip tool wired; Playwright probes pass\")"
  [scene depth evidence-string]
  (let [field-state (->FieldState depth :clean evidence-string)]
    (-> scene
        (assoc-in [:closing-field depth] field-state)
        (append-move field-state))))

(defn descend
  "Records a depth-traversal downward.

  Contract: Scene × from-depth × to-depth → Scene
  - to-depth must be the next depth below from-depth in `depths`.
  - Adds a DepthMove with direction :descend.
  - Precondition: from-depth's current FieldState must be :clean.
    If not, return scene unchanged with a :substrate-rejection note
    in :moves. (This is the substrate-rejection mechanic for descent:
    you can't descend through a noisy register.)

  Example:
    (descend scene :pheno :geno)"
  [scene from-depth to-depth]
  (cond
    (not (adjacent-depth? from-depth to-depth 1))
    (append-move scene
                 (rejected-depth-move :descend from-depth to-depth
                                      "Descent requires adjacent canonical depths."))

    (not= :clean (:state (current-field-state scene from-depth)))
    (append-move scene
                 (rejected-depth-move :descend from-depth to-depth
                                      "Cannot descend through a non-clean register."))

    :else
    (append-move scene
                 (->DepthMove :descend from-depth to-depth
                              (current-field-state scene to-depth)))))

(defn ascend
  "Records a depth-traversal upward.

  Contract: Scene × from-depth × to-depth → Scene
  - to-depth must be the next depth above from-depth in `depths`.
  - Adds a DepthMove with direction :ascend.
  - Ascent does *not* require the from-depth to be clean; you can
    ascend from a noisy depth carrying a pattern you've recognized
    deeper down, and the carrying is itself part of what cleans the
    upper depths on return.

  Example:
    (ascend scene :xeno :exo)"
  [scene from-depth to-depth]
  (if (adjacent-depth? from-depth to-depth -1)
    (append-move scene
                 (->DepthMove :ascend from-depth to-depth
                              (current-field-state scene to-depth)))
    (append-move scene
                 (rejected-depth-move :ascend from-depth to-depth
                                      "Ascent requires adjacent canonical depths."))))

(defn observe-tension
  "Records that a tension has become visible at the current depth.

  Contract: Scene × tension-map → Scene
  - tension-map :: {:at depth, :pattern keyword-or-string, :saying string}
  - The :saying field is the operator's (or agent's) articulation of
    what the tension *is*, in the depth's native vocabulary.
  - Adds the observation to :moves.

  Example:
    (observe-tension scene
      {:at :geno
       :pattern :peripherals/read-only-first-then-extend
       :saying \"write capability landing after observation revealed
                what writes need to look like\"})"
  [scene tension-map]
  (append-move scene
               (assoc tension-map :move/type :tension)))

(defn substrate-check
  "Submits the current move to the coherence-rejection substrate
  at a depth, and records the result.

  Contract: Scene × check-map → Scene
  - check-map :: {:at depth, :substrate keyword, :candidate any, :result keyword}
  - :result is :passes or :rejects.
  - If :rejects, check-map must include :rejection-reason string.
  - The substrate-id should match an entry in the placemat's :substrate-layer.

  Example:
    (substrate-check scene
      {:at :exo
       :substrate :S-arxana-essay
       :candidate \"<articulation of the consent-gate decision>\"
       :result :passes})"
  [scene check-map]
  (append-move scene
               (->SubstrateCheck (:at check-map)
                                 (:substrate check-map)
                                 (:candidate check-map)
                                 (:result check-map)
                                 (:rejection-reason check-map))))

(defn recognize-pattern
  "Records the recognition of a named pattern instance at a depth.

  Contract: Scene × pattern-map → Scene
  - pattern-map :: {:at depth, :pattern keyword, :instance string}
  - The pattern must exist in the Flexiarg pattern library
    (or be a meta-pattern from the placemat).
  - Appends a PatternInstance to scene.xeno-patterns *and* to scene.moves
    (the latter for trace ordering; the former for renderer convenience).

  Example:
    (recognize-pattern scene
      {:at :xeno
       :pattern :MP-substrate-provision
       :instance \"Phase 3 is the substrate adding a new coherence-rejection
                   at the agent-action scale\"})"
  [scene pattern-map]
  (let [pattern (->PatternInstance (:pattern pattern-map)
                                   (:at pattern-map)
                                   (:instance pattern-map))]
    (-> scene
        (update :xeno-patterns (fnil conj []) pattern)
        (append-move pattern))))

(defn close-field
  "Closes the scene; sets closing-field state and resolution.

  Contract: Scene × resolution-map → Scene
  - resolution-map :: {:resolution keyword,
                       :commercial-consequence string-or-nil,
                       :evidence-emitted seq-of-evidence-records}
  - The :commercial-consequence field is optional but recommended;
    when present it names what changes in the Hyperreal commercial
    surface as a result of this scene landing.
  - Renderers (especially the slide-deck renderer) may surface
    :commercial-consequence as the slide's takeaway.

  Example:
    (close-field scene
      {:resolution :phase-3-tripwires-pass
       :commercial-consequence
       \"The 'tantalisingly close' Foreword passage changes tense\"
       :evidence-emitted [...]})"
  [scene resolution-map]
  (assoc scene :resolution resolution-map))

;; =====================================================================
;; INSPECTION / VALIDATION HELPERS
;; =====================================================================
;;
;; These run over completed scenes to check well-formedness.
;; They are themselves substrate-checks at the meta-level — the
;; storyboard's own substrate-rejection mechanism.

(defn scene-valid?
  "Returns true iff the scene is well-formed.

  Checks: TODO — Claude Code to enumerate against the live constraints,
  but at minimum:
  - All four depths appear in opening-field and closing-field.
  - moves form a valid trace (descents only between adjacent depths,
    no descent through a non-clean register, every recognized pattern
    referenced in xeno-patterns also appears in moves).
  - resolution is non-nil.
  - id matches the namespace's file name (convention check).

  This function is called by every renderer before rendering."
  [scene]
  (and (every? #(contains? (:opening-field scene) %) depths)
       (every? #(contains? (:closing-field scene) %) depths)
       (map? (:resolution scene))
       (keyword? (:id scene))
       (string? (:title scene))))

(defn scene-trace
  "Returns the scene's depth-traversal as a printable seq.

  Output: ordered seq of move-summary strings. Used by the REPL renderer.

  Example output:
    [\"open-field at :war-machine, predecessor :scene-03\"
     \"clean :pheno: 'WM UI compiles; anchor-flip wired'\"
     \"descend :pheno → :geno\"
     ...]"
  [scene]
  (mapv (fn [move]
          (cond
            (instance? FieldState move)
            (str "clean " (:depth move) ": " (:evidence move))

            (instance? DepthMove move)
            (str (name (:direction move)) " " (:from move) " -> " (:to move))

            (instance? SubstrateCheck move)
            (str "substrate-check " (:depth move) " " (:substrate-id move)
                 " " (:result move))

            (instance? PatternInstance move)
            (str "recognize " (:at-depth move) " " (:pattern-id move))

            (= :tension (:move/type move))
            (str "tension " (:at move) " " (:pattern move))

            (= :depth-move-rejected (:move/type move))
            (str "rejected " (name (:direction move)) " " (:from move)
                 " -> " (:to move) " (" (:reason move) ")")

            :else
            (pr-str move)))
        (:moves scene)))
