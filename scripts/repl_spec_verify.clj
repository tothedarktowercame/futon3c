(ns scripts.repl-spec-verify
  "Verifier for the futon REPL differential-operator spec.

   Reads a REPL turn-trace γ (a vector of STEP-RECORDS, each bundling one
   Read→Eval→Print→Loop lifecycle per `repl.spec.edn` §:operator :turn-record)
   and asserts the five invariants V1..V5. V1/V2/V3/V4 are hard (pass/fail);
   V5 is a measured metric (skill path-integral).

   Run standalone:
     bb scripts/repl_spec_verify.clj holes/specs/traces/trace-witness-cg-5b03db29.edn

   With no arg it verifies the bundled witness trace. Exit code is 0 iff the
   trace conforms (all hard invariants pass), 1 otherwise.

   Spec: holes/specs/repl.spec.edn (:futon-repl v0.1). The trace keys mirror
   the spec verbatim, including the unicode markers :⊥ (false-floor g) and
   :delta-∇? (connection deformed). Pure `verify` is reusable; lift into
   src/ when the pilot emits traces live."
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; small helpers
;; ---------------------------------------------------------------------------

(defn- false-floor? [g] (= g :⊥))
(defn- a-num? [x] (number? x))
(defn- abs* [x] (Math/abs (double x)))

(defn- printed?    [s] (some? (:artefact s)))
(defn- looped?     [s] (some? (:p' s)))
(defn- moved?      [s] (and (:p' s) (not= (:p' s) (:p s))))
(defn- action-type [a] (:type a))

(defn- basis-types
  "The set of admissible action-types disclosed by a step's dT-snapshot."
  [s]
  (->> (:dT-snapshot s) (map (comp action-type :action)) (remove nil?) set))

(defn- label [s] (str "step " (:step s) " (p=" (:p s) ")"))

;; ---------------------------------------------------------------------------
;; V1 — READ-completeness: slope disclosed in every direction; no silent zero
;; ---------------------------------------------------------------------------

(defn- check-v1 [trace]
  (let [viols
        (for [s trace
              v (concat
                 (when (empty? (:dT-snapshot s))
                   [(str (label s) ": empty dT-snapshot (READ disclosed nothing)")])
                 (for [e (:dT-snapshot s)
                       :let [g (:g-total e)]
                       :when (not (or (a-num? g) (false-floor? g)))]
                   (str (label s) ": action " (pr-str (:action e))
                        " has g-total " (pr-str g)
                        " — must be a number or explicit :⊥ (silent-false-floor)"))
                 (when-let [va (:v s)]
                   (when-not (contains? (basis-types s) (action-type va))
                     [(str (label s) ": chosen v " (pr-str va)
                           " is not in the disclosed action-basis (uncovered direction)")])))]
          v)]
    {:pass (empty? viols) :violations (vec viols)}))

;; ---------------------------------------------------------------------------
;; V2 — no-teleport: no state change without a recorded, attributed v
;; ---------------------------------------------------------------------------

(defn- check-v2 [trace]
  (let [viols
        (for [s trace
              :when (or (moved? s) (printed? s))
              v (cond
                  (nil? (:v s))
                  [(str (label s) ": TELEPORT — state moved/printed with no chosen v "
                        "(the de-skilling failure mode)")]
                  ;; Car-3 seam-c: :operator-directed is a legitimate attribution —
                  ;; begin-live-cycle! emits it when the operator directs a :target (the
                  ;; operator's direction IS the consent; not a teleport).
                  (not (#{:operator :operator-directed :pilot-autonomous} (:v-attribution s)))
                  [(str (label s) ": v present but :v-attribution "
                        (pr-str (:v-attribution s)) " ∉ #{:operator :operator-directed :pilot-autonomous}")]
                  (nil? (:cg-id s))
                  [(str (label s) ": v present but no :cg-id minted at EVAL")]
                  :else nil)
              :when v]
          v)]
    {:pass (empty? viols) :violations (vec viols)}))

;; ---------------------------------------------------------------------------
;; V3 — prediction-error tracked: realised compared to predicted each LOOP
;; ---------------------------------------------------------------------------

(defn- check-v3 [trace]
  (let [looped (filter looped? trace)
        viols
        (for [s looped
              v (let [p (:predicted-discharge s)
                      r (:realised-discharge s)
                      e (:prediction-error s)]
                  (cond
                    (not (a-num? p)) [(str (label s) ": LOOP missing numeric :predicted-discharge")]
                    (not (a-num? r)) [(str (label s) ": LOOP missing numeric :realised-discharge")]
                    (not (a-num? e)) [(str (label s) ": LOOP missing numeric :prediction-error")]
                    (> (abs* (- e (abs* (- r p)))) 1e-6)
                    [(str (label s) ": :prediction-error " e
                          " ≠ |realised − predicted| = " (abs* (- r p)))]
                    :else nil))
              :when v]
          v)
        sum-err (reduce + 0.0 (keep :prediction-error looped))]
    {:pass (empty? viols)
     :violations (vec viols)
     :sum-prediction-error sum-err
     :note "lower Σ over a session = belief-update converging (informational, not pass/fail). V3 sign/units are calibration-pending."}))

;; ---------------------------------------------------------------------------
;; V4 — niche-construction legible: ∇-deformation flagged + named; basis growth
;;      must be attributable to a flagged PRINT (no stealth niche construction)
;; ---------------------------------------------------------------------------

(defn- check-v4 [trace]
  (let [per-step
        (for [s trace
              v (cond
                  (not (contains? s :delta-∇?))
                  [(str (label s) ": PRINT missing :delta-∇? flag")]
                  (and (true? (:delta-∇? s)) (nil? (:new-edge-family s)))
                  [(str (label s) ": :delta-∇? true but no :new-edge-family named (unnamed niche construction)")]
                  :else nil)
              :when v]
          v)
        ;; cross-step: a new admissible action-type in step n+1 must have been
        ;; constructed by step n's PRINT (delta-∇? true).
        cross
        (for [[a b] (partition 2 1 trace)
              :let [grew (set/difference (basis-types b) (basis-types a))]
              :when (and (seq grew) (not (true? (:delta-∇? a))))]
          (str (label a) ": basis grew by " (pr-str grew)
               " at next step but this PRINT was not flagged :delta-∇? (stealth-niche-construction)"))
        viols (concat per-step cross)]
    {:pass (empty? viols) :violations (vec viols)}))

;; ---------------------------------------------------------------------------
;; V5 — skill is a path integral (measured metric, not pass/fail)
;; ---------------------------------------------------------------------------

(defn- check-v5 [trace]
  (let [contrib (fn [pred s]
                  (reduce + 0.0
                          (for [x trace
                                :when (and (pred (:v-attribution x))
                                           (some? (:v x))
                                           (a-num? (:predicted-discharge x)))]
                            (:predicted-discharge x))))
        teleports (count (filter #(and (or (moved? %) (printed? %)) (nil? (:v %))) trace))]
    {:metric true
     :skill-operator (contrib #(= % :operator) nil)
     :pilot-autonomous-discharge (contrib #(= % :pilot-autonomous) nil)
     :teleports teleports
     :note "skill_operator = Σ D_v T over operator-attributed steps; autopen/teleport contribute 0. Raw g-total sign convention defers to V3 calibration."}))

;; ---------------------------------------------------------------------------
;; top-level
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; V6 — learning-evidence (LOOP :autonomy). Soft/metric: autonomous frames
;; SHOULD carry a :learning block (patterns derived + sorries mined), since the
;; operator-B→A learning channel collapses non-interactively. Never fails
;; interactive/demo frames; flags an autonomous frame missing learning.
;; ---------------------------------------------------------------------------

(defn- check-v6-learning [frame]
  (if (nil? frame)
    {:metric true :learning :n/a :note "bare γ (no frame envelope)"}
    (let [l (:learning frame)
          autonomous? (= :substantive (:mode frame))]
      (cond
        (nil? l)
        {:metric true :learning :absent
         :warn (if autonomous?
                 "AUTONOMOUS frame with no :learning — LOOP :autonomy requires it (spec §turns :LOOP :autonomy)"
                 "no :learning (ok for interactive/demo)")}
        (and (empty? (:patterns-applied l)) (empty? (:sorries-mined l)))
        {:metric true :learning :empty
         :warn "learning block present but empty (no patterns-applied / sorries-mined)"}
        :else
        {:metric true :learning :present
         :patterns (count (:patterns-applied l))
         :sorries (count (:sorries-mined l))
         :derivation (:derivation l)}))))

(defn verify
  "Verify a trace γ (vector of step-records). Optional FRAME envelope enables
   the V6 learning-evidence check. Returns a report map."
  ([trace] (verify trace nil))
  ([trace frame]
   (let [v1 (check-v1 trace) v2 (check-v2 trace)
         v3 (check-v3 trace) v4 (check-v4 trace) v5 (check-v5 trace)
         v6 (check-v6-learning frame)]
     {:spec "futon-repl v0.1"
      :steps (count trace)
      :invariants {:V1-read-completeness v1
                   :V2-no-teleport v2
                   :V3-prediction-error-tracked v3
                   :V4-niche-construction-legible v4
                   :V5-skill-path-integral v5
                   :V6-learning-evidence v6}
      :conforms? (every? :pass [v1 v2 v3 v4])})))

(defn- format-report [report file]
  (let [lines (atom [])
        emit #(swap! lines conj %)]
    (emit (str "REPL spec verifier — " (:spec report)))
    (emit (str "trace: " file "  (" (:steps report) " step(s))"))
    (emit "")
    (doseq [[k {:keys [pass violations metric] :as r}] (:invariants report)]
      (emit (str (cond metric "📊" pass "✅" :else "❌") "  " (name k)
                 (cond
                   metric (str "  " (pr-str (dissoc r :metric :note)))
                   pass "  ok"
                   :else (str "  " (count violations) " violation(s)"))))
      (doseq [v (or violations [])]
        (emit (str "      - " v)))
      (when (and (= k :V3-prediction-error-tracked) (:sum-prediction-error r))
        (emit (str "      Σ prediction-error = " (:sum-prediction-error r)))))
    (emit "")
    (emit (str (if (:conforms? report) "CONFORMS ✅" "DOES NOT CONFORM ❌")
               " (hard invariants V1–V4)"))
    (str/join "\n" @lines)))

(defn- read-gamma
  "Read a trace file. Accepts a bare γ vector OR a frame map (envelope +
   :trace payload, per futon3c.aif.repl-trace / the daily scan-frame shape)."
  [file]
  (let [v (edn/read-string (slurp file))]
    (if (map? v) (or (:trace v) (:gamma v) []) v)))

(defn -main [& args]
  (let [default "holes/specs/traces/trace-witness-cg-5b03db29.edn"
        file (or (first args) default)
        raw (edn/read-string (slurp file))
        frame (when (map? raw) raw)
        trace (if (map? raw) (or (:trace raw) (:gamma raw) []) raw)
        report (verify trace frame)]
    (println (format-report report file))
    (System/exit (if (:conforms? report) 0 1))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
