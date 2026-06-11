(ns futon3c.aif.repl-trace
  "Trace emission for the REPL differential-operator spec (holes/specs/repl.spec.edn).

   A run of the pilot REPL accumulates TURN-RECORDS into a trace γ and
   persists a FRAME: envelope metadata + the γ payload. The frame mirrors
   the futon7 daily scan-frame shape (top-level metadata + a payload vector,
   `futon7/data/daily/<date>.edn`). Per Joe (2026-05-29): the trace IS the
   data the full LOOP tooling is built off of (or a pointer to it).

   The verifier `scripts/repl_spec_verify.clj` consumes a frame's :trace
   (or a bare γ vector). Frames are written to a CANONICAL data dir
   (`futon3c/data/repl-traces/`), NOT `.state` — unlike night-shift frames
   these are meant to be ingestible run-records.

   Pure + dependency-light (edn / io / pprint / java.time) so it loads under
   bb for offline emission and round-trips through the verifier; the live
   pilot loop calls `record-step!` once per turn and `write-frame!` at LOOP.

   Cross-refs:
   - holes/specs/repl.spec.edn — :operator :turn-record is the field schema
   - scripts/repl_spec_verify.clj — the V1..V5 checker over a γ
   - war_machine_pilot.clj run-observe-cycle — the loop that will call this"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]))

(def spec-id :futon-repl)
(def spec-version "0.1")

;; ---------------------------------------------------------------------------
;; turn-record — normalise one step's lifecycle into the spec shape
;; ---------------------------------------------------------------------------

(defn turn-record
  "Build a spec-shaped turn-record from a step map.

   Unsupplied fields are left ABSENT (never defaulted to 0/false) so the
   verifier can tell 'turn not yet reached' from 'recorded as zero'.
   Accepts the ∇-flag as :delta-∇? or ascii :delta-grad?. If both
   :predicted-discharge and :realised-discharge are present but
   :prediction-error is not, it is computed as |realised − predicted|
   (so emitted records satisfy V3 by construction)."
  [m]
  (let [{:keys [step p dT-snapshot v v-attribution predicted-discharge cg-id
                artefact new-edge-family p' realised-discharge
                prediction-error independent? evidence-ref realised-source]} m
        dg (cond (contains? m :delta-∇?)    (:delta-∇? m)
                 (contains? m :delta-grad?) (:delta-grad? m)
                 :else ::absent)
        pe (cond
             (some? prediction-error) prediction-error
             (and (number? predicted-discharge) (number? realised-discharge))
             (Math/abs (double (- realised-discharge predicted-discharge)))
             :else nil)]
    (cond-> {}
      (some? step)                (assoc :step step)
      (some? p)                   (assoc :p p)
      (some? dT-snapshot)         (assoc :dT-snapshot dT-snapshot)
      (some? v)                   (assoc :v v)
      (some? v-attribution)       (assoc :v-attribution v-attribution)
      (some? predicted-discharge) (assoc :predicted-discharge predicted-discharge)
      (some? cg-id)               (assoc :cg-id cg-id)
      (some? artefact)            (assoc :artefact artefact)
      (not= dg ::absent)          (assoc :delta-∇? dg)
      (some? new-edge-family)     (assoc :new-edge-family new-edge-family)
      (some? p')                  (assoc :p' p')
      (some? realised-discharge)  (assoc :realised-discharge realised-discharge)
      (some? pe)                  (assoc :prediction-error pe)
      ;; realised-on-merge: ONLY executed actions may carry the independence
      ;; tag — the calibration verdict counts exclusively these pairs (the
      ;; gate that diagnoses degeneracy must not be clearable by more
      ;; degeneracy). Absent unless supplied, like every other field.
      (some? independent?)        (assoc :independent? independent?)
      (some? evidence-ref)        (assoc :evidence-ref evidence-ref)
      (some? realised-source)     (assoc :realised-source realised-source)
      ;; observational field-delta (Option C, 2026-06-11): movement of the
      ;; WHOLE differential across the executed cycle — recorded for the
      ;; future field-delta realised-semantics design, NOT verdict-counted.
      (some? (:field-delta m))    (assoc :field-delta (:field-delta m)))))

;; ---------------------------------------------------------------------------
;; accumulator — what the live loop drives, one record per turn-cycle
;; ---------------------------------------------------------------------------

(defn new-trace
  "Start an empty trace accumulator (atom holding a vector)."
  []
  (atom []))

(defn record-step!
  "Normalise STEP-MAP and append it to the accumulator. Returns the atom.
   The pilot loop calls this once per completed R→E→P→L step."
  [trace-atom step-map]
  (swap! trace-atom conj (turn-record step-map))
  trace-atom)

;; ---------------------------------------------------------------------------
;; frame — envelope + payload, mirroring the daily scan-frame shape
;; ---------------------------------------------------------------------------

(defn now-iso [] (str (java.time.Instant/now)))

(defn learning
  "Build a :learning evidence block (LOOP :autonomy, repl.spec.edn §:turns
   :LOOP :autonomy). This is the learning the operator-B→A channel would
   supply, internalised into the frame so it survives non-interactive runs.

   v0: the derivations are SUPPLIED by the inhabiting agent (this fn just
   normalises + types them). Automating the derivation — reading the
   transcript/γ to score patterns and mine sorries — is M-pattern-application-
   diagnostic + M-a-sorry-enterprise, the next build. Marked :derivation
   honestly so a reader can tell agent-supplied from auto-mined.

   :patterns-applied — [{:pattern <lib-path> :role <str> :applied? bool}]
                       (M-pattern-application-diagnostic: which library
                        patterns this cycle applied / should have)
   :sorries-mined    — [{:id <kw> :title <str> :kind <kw> :rationale <str>}]
                       (M-a-sorry-enterprise: new gaps discovered this run)"
  [{:keys [patterns-applied sorries-mined transcript-source derivation notes]}]
  {:learning/v        0
   :derivation        (or derivation :agent-supplied)
   :patterns-applied  (vec patterns-applied)
   :sorries-mined     (vec sorries-mined)
   :transcript-source transcript-source
   :notes             notes})

(defn frame
  "Wrap a γ vector in frame-envelope metadata. META supplies :run-id,
   :agent, :date (caller-supplied so emission stays deterministic/testable).
   Optional LEARNING block (from `learning`) is attached under :learning —
   the LOOP-autonomy evidence the operator-B→A channel would otherwise carry."
  ([meta gamma] (frame meta gamma nil))
  ([{:keys [run-id agent date]} gamma learning-block]
   (cond-> {:spec         spec-id
            :spec-version spec-version
            :run-id       run-id
            :agent        agent
            :date         date
            :step-count   (count gamma)
            :trace        (vec gamma)}
     learning-block (assoc :learning learning-block))))

(defn add-learning!
  "Read a frame file, attach/replace its :learning block, rewrite. The reusable
   LOOP write-back primitive: a learning pass enriches an already-emitted frame
   in place (also used to back-fill frames emitted before the schema upgrade)."
  [path learning-block]
  (let [fr (edn/read-string (slurp path))
        fr' (assoc fr :learning learning-block)]
    (spit path (with-out-str (pp/pprint fr')))
    path))

(defn gamma
  "Extract the γ payload from a frame map, or pass a bare vector through."
  [frame-or-gamma]
  (if (map? frame-or-gamma)
    (or (:trace frame-or-gamma) (:gamma frame-or-gamma) [])
    (vec frame-or-gamma)))

(def default-dir "data/repl-traces")

(defn write-frame!
  "Persist FRAME-MAP to <dir>/<run-id>.edn (default data/repl-traces,
   relative to repo root). Returns the path written."
  ([frame-map] (write-frame! frame-map default-dir))
  ([frame-map dir]
   (let [path (str dir "/" (:run-id frame-map) ".edn")]
     (io/make-parents path)
     (spit path (with-out-str (pp/pprint frame-map)))
     path)))

(defn read-frame [path] (edn/read-string (slurp path)))

;; ---------------------------------------------------------------------------
;; demo / smoke — reconstructs the spec's live witness via the emission API,
;; proving the per-turn call shape the pilot will use. `bb` it, then run the
;; verifier on the written frame.
;; ---------------------------------------------------------------------------

(defn emit-witness-demo!
  "Drive the emission API the way the pilot loop will, for the cg-5b03db29
   witness, and write a frame. Returns the path."
  ([] (emit-witness-demo! default-dir))
  ([dir]
   (let [t (new-trace)]
     ;; one R→E→P→L step, as the loop would record it at LOOP-close:
     (record-step! t
       {:step 0
        :p "p-pre-tick"
        :dT-snapshot [{:action {:type :address-sorry :target "sorry/wm-aif-substrate-addressability"} :g-total -4.99 :rank 0}
                      {:action {:type :address-sorry :target "sorry/r3a-likelihood-loop-health"}      :g-total -4.39 :rank 1}]
        :v {:type :address-sorry :target "sorry/wm-aif-substrate-addressability"}
        :v-attribution :pilot-autonomous
        :predicted-discharge -4.99
        :cg-id "cg-5b03db29-b636-4368-88ee-a7c8df23ef55"
        :artefact {:kind :address-sorry :target "sorry/wm-aif-substrate-addressability"}
        :delta-grad? false
        :p' "p-post-tick"
        :realised-discharge -4.99})       ; prediction-error auto-computed
     (write-frame! (frame {:run-id "demo-cg-5b03db29"
                           :agent  "claude-1"
                           :date   "2026-05-25"}
                          @t)
                   dir))))

(defn -main [& args]
  (let [path (emit-witness-demo! (or (first args) default-dir))]
    (println "emitted frame ->" path)))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
