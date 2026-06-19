(ns futon7a.placemat.storyboard.renderers.repl-printer
  "Mode D: REPL renderer.

  The simplest of the four renderers. Takes a Scene and produces a
  string suitable for println at the REPL — pretty-printed depth-
  traversal trace, substrate-check outcomes, recognized patterns,
  resolution, commercial consequence.

  This is the renderer that proves the multi-mode story end-to-end:
  if a scene can be authored once and pretty-printed by this renderer,
  the same scene-data can be passed to the other renderers
  (war-machine playbook, browser level, slide deck) to produce their
  mode-specific outputs without any change to the scene-file itself.

  Design intent: the REPL output should read aloud as a single
  methodological round. Reading the output is itself a use of the
  storyboard — the citta-register receiving the scene's trace as
  language-in-context.

  HtDP discipline: contracts and stubs; Claude Code fills in formatting
  details against any house style for the futon stack's REPL output
  (TODO: confirm by inspecting any existing pretty-printers in
  futon3 or futon5a — e.g. wm-trace pretty-printing, AIF-event
  formatting).")

(require '[clojure.string :as str]
         '[futon7a.placemat.storyboard.protocols :as p])

(declare render-header
         render-opening-field
         render-move
         render-closing-field
         render-pilot-cycle
         render-resolution
         render-field-block
         compact-text)

;; =====================================================================
;; RENDERER IMPLEMENTATION
;; =====================================================================

(defrecord ReplPrinter []
  p/SceneRenderer

  (render-as-playbook [_ _scene]
    ;; Not this renderer's job; stub returns nil.
    nil)

  (render-as-level [_ _scene]
    ;; Not this renderer's job; stub returns nil.
    nil)

  (render-as-slide [_ _scene]
    ;; Not this renderer's job; stub returns nil.
    nil)

  (render-as-repl-output [_ scene]
    (if-not (p/scene-valid? scene)
      (str "INVALID SCENE\n"
           "  " (pr-str (select-keys scene [:id :title :world])))
      (str/join
       "\n\n"
       [(render-header scene)
        (render-opening-field scene)
        (str "TRACE\n"
             (str/join "\n" (map render-move (:moves scene))))
        (render-closing-field scene)
        (render-pilot-cycle scene)
        (render-resolution scene)]))))

;; =====================================================================
;; CONVENIENCE
;; =====================================================================

(defn print-scene
  "Convenience wrapper. Pretty-prints the given Scene to *out*.

  Example usage at the REPL:
    (require '[futon7a.placemat.storyboard.worlds.world-2-war-machine.scene-04-pilot-phase-3 :as s4])
    (require '[futon7a.placemat.storyboard.renderers.repl-printer :as repl])
    (repl/print-scene (s4/scene))"
  [scene]
  (println (p/render-as-repl-output (->ReplPrinter) scene)))

;; =====================================================================
;; SECTION-LEVEL HELPERS (private; for the renderer's internal use)
;; =====================================================================
;;
;; Each helper takes the scene (or a slice of it) and returns a string.
;; The main render-as-repl-output method composes these.
;; HtDP: stubs that return placeholder strings of the right shape.

(defn- render-header
  "Returns the header line(s) — scene id, title, world, stack-position.

  Example output:
    ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    SCENE 04 · The pilot lands Phase 3
    world: :war-machine  ·  stack-position: 2/4
    ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  [scene]
  (let [scene-name (or (some-> (:id scene) name (str/replace #"^scene-" "SCENE "))
                       "SCENE")
        pos (or (:stack-position scene) "?")]
    (str "========================================\n"
         scene-name " :: " (:title scene) "\n"
         "world: " (:world scene) "  stack-position: " pos "/4\n"
         "predecessor: " (or (:predecessor scene) "-") "\n"
         "========================================")))

(defn- render-opening-field
  "Returns the opening field-states, one line per depth.

  Example output:
    OPENING FIELD
      :pheno  · clearing  (Phase 2 transport-wiring complete)
      :geno   · clearing  (AIF trace accumulated)
      :exo    · clearing  (operator-pilot articulation drafted)
      :xeno   · noisy     (pattern not yet recognized at scale)"
  [scene]
  (render-field-block "OPENING FIELD" (:opening-field scene)))

(defn- render-move
  "Returns a one-line representation of a single move.

  Move types and example renderings:
    FieldState       → '  :pheno · clean: WM UI compiles; anchor-flip wired'
    DepthMove        → '  ↓ :pheno → :geno'
    SubstrateCheck   → '  ✓ :exo substrate-check (:S-arxana-essay): passes'
    PatternInstance  → '  ◆ :xeno recognize :MP-substrate-provision'"
  [move]
  (cond
    (= :tension (:move/type move))
    (str "  [tension] " (:at move) " " (:pattern move) " :: "
         (compact-text (:saying move)))

    (= :depth-move-rejected (:move/type move))
    (str "  [reject] " (name (:direction move)) " " (:from move)
         " -> " (:to move) " :: " (:reason move))

    (and (contains? move :state) (contains? move :evidence))
    (str "  [clean] " (:depth move) " -> " (:state move) " :: "
         (compact-text (:evidence move)))

    (contains? move :direction)
    (str "  [" (name (:direction move)) "] " (:from move) " -> " (:to move)
         " (arrive " (-> move :field-state :state) ")")

    (contains? move :substrate-id)
    (str "  [substrate] " (:depth move) " " (:substrate-id move)
         " :: " (name (:result move))
         (when-let [reason (:rejection-reason move)]
           (str " :: " (compact-text reason))))

    (contains? move :pattern-id)
    (str "  [pattern] " (:at-depth move) " " (:pattern-id move) " :: "
         (compact-text (:instance-note move)))

    :else
    (str "  " (pr-str move))))

(defn- render-closing-field
  "Symmetric with render-opening-field; shows closing states."
  [scene]
  (render-field-block "CLOSING FIELD" (:closing-field scene)))

(defn- render-resolution
  "Returns the resolution + commercial-consequence section.

  Example output:
    ─── RESOLUTION ─────────────────────────────────
    :phase-3-tripwires-pass

    Commercial consequence:
      The 'tantalisingly close' Foreword passage changes tense.
      [...]

    Evidence emitted:
      · phase-tripwire-passed :pilot-I1
      · phase-tripwire-passed :pilot-I2
      · phase-tripwire-passed :pilot-I3
      · substrate-extension :consent-gate-event-type
      · commercial-surface-update :foreword-tense"
  [scene]
  (let [{:keys [resolution commercial-consequence evidence-emitted]}
        (:resolution scene)]
    (str "RESOLUTION\n"
         "  " resolution "\n"
         "  commercial: " (compact-text commercial-consequence) "\n"
         "  evidence:\n"
         (if (seq evidence-emitted)
           (str/join "\n"
                     (map (fn [ev]
                            (str "    - " (compact-text (pr-str ev))))
                          evidence-emitted))
           "    - <none>"))))

(defn- compact-text
  [s]
  (-> (or s "")
      str
      str/trim
      (str/replace #"\s+" " ")))

(defn- render-field-block
  [label field-map]
  (str label "\n"
       (str/join
        "\n"
        (for [depth p/depths
              :let [{:keys [state evidence]} (get field-map depth)]]
          (str "  " depth
               " :: " (or state :missing)
               " :: " (compact-text evidence))))))

(defn- render-pilot-cycle
  [scene]
  (let [pilot-cycle (:pilot-cycle scene)
        cards (:cards pilot-cycle)]
    (if (seq cards)
      (str "PILOT CYCLE FRAME\n"
           "  " (compact-text (:stepping-note pilot-cycle)) "\n"
           (str/join
            "\n"
            (map (fn [{:keys [slot label repl-stage foundation r-cluster prompt scene-witness]}]
                   (str "  CARD " slot " :: " label
                        " | repl=" repl-stage
                        " | foundation=" foundation
                        " | r=" (pr-str r-cluster) "\n"
                        "    prompt: " (compact-text prompt) "\n"
                        "    witness: " (compact-text scene-witness)))
                 cards)))
      "PILOT CYCLE FRAME\n  <none>")))
