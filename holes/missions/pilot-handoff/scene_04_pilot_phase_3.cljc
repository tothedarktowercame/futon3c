(ns futon7a.placemat.storyboard.worlds.world-2-war-machine.scene-04-pilot-phase-3
  "Scene 4: The war-machine pilot lands Phase 3.

  This is the prototype scene for the storyboard. It is the first scene
  in which all four depths are present and operating, and it is the
  commercially load-bearing scene for the next ten days — it dramatises
  the moment at which the 'tantalisingly close' Foreword passage
  changes tense.

  World: :war-machine (level 2 of 4 in the reader-stack)
  Predecessor: :scene-03-pilot-phase-2 (transport wiring lands)
  Successor: :scene-05-friction-audit (Phase 4 — earn inhabitation)

  Reader for this scene at multiple modes:
  - Mode A (war-machine pilot): the pilot enacts this scene as the
    Phase 3 mission-plan; substrate-checks become tripwire tests.
  - Mode B (browser level): the player plays through the depth-traversal,
    encountering the consent-gate substrate-check as a level encounter.
  - Mode C (slide deck): one slide titled 'Phase 3: write capability
    earned'; commercial-consequence becomes the takeaway.
  - Mode D (REPL): trace pretty-prints for inspection.")

(require '[futon7a.placemat.storyboard.protocols :as p
           :refer [open-field clean-register descend ascend
                   observe-tension substrate-check recognize-pattern
                   close-field]])

(declare pheno-data geno-functions exo-narration xeno-patterns pilot-cycle-cards)

;; =====================================================================
;; THE SCENE
;; =====================================================================

(defn scene
  "Returns the Scene record for war-machine-pilot Phase 3.

  Reading the function body should feel like reading a methodological
  round: open the field, clean each depth as we descend, observe the
  tension at the relevant depth, run the substrate-check, recognize the
  xeno-pattern, ascend carrying the recognition, close the field with
  the resolution and its commercial consequence."
  []
  (-> (open-field
        {:id :scene-04-pilot-phase-3
         :title "The pilot lands Phase 3"
         :world :war-machine
         :stack-position 2
         :predecessor :scene-03-pilot-phase-2})

      ;; Pheno: the body of the system. What compiles, what runs.
      ;; The Phase 3 write-tools have landed in code; the question is
      ;; whether they cohere with the WM-I4 invariant ("the war machine
      ;; does not act"). The pilot inhabiting the WM peripheral is what
      ;; resolves the apparent tension: the pilot acts; the WM displays.
      (clean-register :pheno
        "Phase 3 write-tools land in code:
         :anchor-flip, :coherence-row-author, :pilot-action all wired;
         consent-gate machinery in place;
         5 fidelity tripwires authored as test suite")

      (descend :pheno :geno)

      ;; Geno: the dispositional structure underneath. The AIF signals
      ;; show what the system *tends toward* given the new capabilities.
      ;; The tension here is read-only-first-then-extend: we're now in
      ;; the *extend* step, and the active inference signals tell us
      ;; what writes should look like because observation has revealed it.
      (clean-register :geno
        "AIF trace shows:
         - 6 days of WM observation accumulated
         - consent-gate pressure rising (operator wants the writes to land)
         - the peripheral's capability envelope is well-specified
         The geno-register is clean enough to observe what the
         dispositional shape of 'write capability' actually is.")

      (observe-tension
        {:at :geno
         :pattern :peripherals/read-only-first-then-extend
         :saying "Write capability lands NOW, after observation revealed
                  what writes need to look like. The pattern's load-bearing
                  reason for phasing — 'observation reveals what writes
                  need to look like' — is what is being cashed at this step."})

      (descend :geno :exo)

      ;; Exo: the textual articulation. The operator and the pilot
      ;; (and claude-N agents) converge on a shared articulation of
      ;; what the consent-gate IS. This is the citta-register: language
      ;; landing in context, mind showing up at the language layer.
      ;; The Markov boundary is around the whole system — operator +
      ;; pilot + agents + WM peripheral together.
      (clean-register :exo
        "Shared articulation lands cleanly:
         - consent-gate is intent-handshake-shaped, NOT free-form-bell-shaped
         - per-substantive-action :consent-gate-event-id citation enforced
         - the typed event taxonomy is EXTENDED rather than side-channeled
         The exo-register is quiet enough that this articulation registers
         as movement against the cleaned field.")

      (substrate-check
        {:at :exo
         :substrate :S-arxana-essay
         :candidate "Articulation of consent-gate as canonical-typed-event
                     extending the existing taxonomy"
         :result :passes
         :rejection-reason nil})

      (descend :exo :xeno)

      ;; Xeno: the pattern as such. What is being instantiated here
      ;; is not just 'Phase 3 of M-war-machine-pilot' but a recurring
      ;; structural move — the substrate-provision meta-pattern from the
      ;; placemat, applied at a new scale (agent-action coherence).
      ;; Recognizing this is what unlocks the *reusability* of the move
      ;; across future missions and across the commercial offer.
      (clean-register :xeno
        "The xeno-register clears: this is not a one-off Phase 3 landing,
         it is the meta-pattern :MP-substrate-provision being exhibited at
         a new scale. The same architectural move that produced the
         compiler+types+tests substrate for code, and the Arxana-essay
         substrate for prose, now produces the consent-gate substrate
         for agent action.")

      (recognize-pattern
        {:at :xeno
         :pattern :MP-substrate-provision
         :instance "Phase 3 IS the substrate adding a new coherence-rejection
                    at the agent-action scale — consent-gate rejects writes
                    that bypass the intent-handshake. The substrate's form
                    is medium-specific (agent action != code != prose); the
                    structural requirement is medium-invariant."})

      ;; Ascent carrying the recognition: as we come back up through
      ;; the depths, the pattern-recognition at xeno makes each upper
      ;; depth land differently. The pheno is no longer 'we shipped
      ;; some code'; it is 'we instantiated the meta-pattern at agent
      ;; scale and have new evidence for its scope'.
      (ascend :xeno :exo)
      (ascend :exo  :geno)
      (ascend :geno :pheno)

      (close-field
        {:resolution :phase-3-tripwires-pass
         :commercial-consequence
         "The 'tantalisingly close' Foreword passage changes tense:
          'tantalisingly-close possible realisation' becomes
          'operational realisation, here is the demo'. The Hyperreal
          commercial offer acquires a demonstrable claim — methodology-
          as-running-apparatus, not methodology-as-research-project.
          VSAT EoIs can now refer to a working pilot. The pitch becomes:
          'I build production software with an apparatus that catches
          its own errors; the apparatus is itself software, which means
          it improves with use, and which means I can show it to you.'"
         :evidence-emitted
         [;; TODO: Claude Code to wire these against the actual
          ;; AIF event taxonomy. Placeholder shapes:
          {:kind :phase-tripwire-passed :id :pilot-I1}
          {:kind :phase-tripwire-passed :id :pilot-I2}
          {:kind :phase-tripwire-passed :id :pilot-I3}
          {:kind :substrate-extension   :substrate :consent-gate-event-type}
          {:kind :commercial-surface-update
           :surface :foreword-tense
           :from "tantalisingly close" :to "operational"}]})

      (assoc :pheno-data (pheno-data)
             :geno-functions (geno-functions)
             :exo-narration (exo-narration)
             :xeno-patterns (xeno-patterns)
             :pilot-cycle (pilot-cycle-cards))))

;; =====================================================================
;; PAYLOAD SECTIONS — populated incrementally
;; =====================================================================
;;
;; The scene-function above produces the depth-traversal trace and the
;; resolution. The payload sections below are populated by additional
;; helper-calls (or by post-processing) to supply mode-specific content
;; that renderers may use.
;;
;; HtDP discipline: contracts present, stubs return shapes that satisfy
;; the contract, Claude Code fills against the live stack.

(defn pheno-data
  "Returns the EDN-shape this scene contributes to the placemat.

  Cross-ref: should integrate cleanly with the existing
  grand-unified-placemat.edn — specifically as an :inter-edge or as
  a new :maturity-snapshot entry showing :V-orgs advancing along I0→I4."
  []
  ;; STUB
  {:contribution :TODO
   :integration-target :grand-unified-placemat-edn})

(defn geno-functions
  "Returns the seq of fn-syms that constitute this scene's dispositional
  structure — the helpers actually called in `scene`. The war-machine
  playbook renderer uses these to compile the scene into an enactable
  mission-plan."
  []
  ;; STUB — should auto-derive from the scene-fn body if possible
  '[open-field clean-register descend observe-tension substrate-check
    recognize-pattern ascend close-field])

(defn exo-narration
  "Returns the human-readable narration for slide-deck and reference
  renderings. Two registers: :short (for slides) and :long (for the
  reference document / Foreword companion text)."
  []
  ;; STUB
  {:short "Phase 3 lands: the pilot can now write to the WM, mediated by
           a consent-gate that extends the canonical event taxonomy."
   :long  "TODO — Claude Code to draft against the Operator's Foreword
           passage; should land as a section that could be lifted into
           the Foreword's next revision."})

(defn xeno-patterns
  "Returns the seq of PatternInstance records exhibited in this scene.
  Convenience accessor — duplicates what `recognize-pattern` adds to
  the scene's :xeno-patterns field, for renderers that want patterns
  without traversing the full move-trace."
  []
  ;; STUB
  [{:pattern-id :MP-substrate-provision
    :at-depth :xeno
    :instance-note "Substrate-provision at agent-action scale"}])

(defn pilot-cycle-cards
  "Returns a concrete 4-card operational frame for stepping this scene.

  The frame is deliberately hybrid:
  - episode labels come from the four-segment card discipline
  - REPL stages say what the pilot does operationally
  - foundations say what kind of attention each card trains
  - R-clusters say what makes the card load-bearing as apparatus"
  []
  {:frame-id :pilot-cycle/scene-04-phase-3
   :stepping-note
   "Treat the four cards as an operational stack frame: step the scene with
    one question at a time, then hand the resulting trace to the REPL printer."
   :cards
   [{:slot 1
     :label "Recognise"
     :repl-stage :read
     :foundation :kaya
     :r-cluster [:R1 :R2 :R3]
     :prompt
     "Name one concrete write-capability that has landed in code and one
      tripwire that now makes it inspectable rather than merely claimed."
     :scene-witness
     "Phase 3 write-tools land in code: :anchor-flip, :coherence-row-author,
      :pilot-action; 5 fidelity tripwires authored as test suite."}
    {:slot 2
     :label "Represent"
     :repl-stage :evaluate
     :foundation :vedana
     :r-cluster [:R4 :R5 :R6 :R7]
     :prompt
     "What is exerting pressure right now: where is the pull to write, what is
      the risk, and what constraint keeps that pressure from collapsing into a
      reckless action?"
     :scene-witness
     "Consent-gate pressure is rising; the active pattern is
      :peripherals/read-only-first-then-extend."}
    {:slot 3
     :label "Reset"
     :repl-stage :print
     :foundation :citta
     :r-cluster [:R8 :R9]
     :prompt
     "State the move in sharable language: what exactly is the consent-gate,
      what event must be cited, and what typed artefact should be emitted so
      another inhabitant can inspect the same judgement?"
     :scene-witness
     "Shared articulation lands: intent-handshake-shaped consent-gate,
      per-substantive-action :consent-gate-event-id, typed-event taxonomy
      extended rather than side-channeled."}
    {:slot 4
     :label "Recap"
     :repl-stage :loop
     :foundation :dhamma
     :r-cluster [:R10 :R11 :R12]
     :prompt
     "What pattern was this an instance of, what commercial consequence now
      follows, and what does this turn seed for the next inhabitant or next
      scene?"
     :scene-witness
     "Recognize :MP-substrate-provision at agent-action scale; the Foreword's
      tense changes from tantalisingly close to operational realisation."}]})
