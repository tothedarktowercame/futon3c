;; M-repl-circus cascade job: validate the two occurrence squares, check the
;; glue witness with executable-cascade's own rule, then see how far
;; build-structural / validate-cascade get. Run in futon2:
;;   java -cp "$(clojure -Spath -M:test)" clojure.main /tmp/validate_cascade_178.clj
(require '[clojure.edn :as edn]
         '[futon2.aif.three-halves-square :as square]
         '[futon2.aif.wm.executable-cascade :as ec]
         '[futon2.aif.interpretation-evidence :as evidence])
(import '[java.security MessageDigest])

(defn sha256 [^String s]
  (let [d (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8"))]
    (apply str (map #(format "%02x" %) d))))

(def doc (edn/read-string (slurp "/tmp/squares-178.edn")))
(def prov (edn/read-string (slurp "/tmp/prov-178.edn")))
(def squares (:squares doc))

(println "=== step 3: three-halves-square/validate per occurrence")
(def validated
  (into {}
        (for [s squares]
          (let [v (square/validate s)]
            (println (:pattern s) "->" (:status v)
                     (when (:missing-evidence v) (:missing-evidence v)))
            [(:pattern s) v]))))

(def occ-id
  {"cascade-construction/read-what-exists-first"
   "occ/read-what-exists-first@ed69fb38-turn-178"
   "forward-model/the-forward-model-is-a-pattern-cascade"
   "occ/forward-model-is-a-pattern-cascade@ed69fb38-turn-178"})

(def o1 (occ-id "cascade-construction/read-what-exists-first"))
(def o2 (occ-id "forward-model/the-forward-model-is-a-pattern-cascade"))

(println "=== step 4: glue witness, checked with #'witness-conforming?")
;; The glue: shared situation element :voxterm-diagram-tab-start-incomplete,
;; read-what-exists-first's G object ~ forward-model's I2 object.  The witness
;; is hand-stated for the check (no producer exists; T-overlap-modes finding):
;; kind :consistency, a real sha256 over the shared element and both verbatim
;; situation quotes, endpoints the two occurrences.
(def shared-quote "We had some start on something like this in the Voxterm \"Diagram\" tab but it wasn't complete yet.")
(def witness
  {:kind :consistency
   :digest (sha256 (str "voxterm-diagram-tab-start-incomplete|" shared-quote))
   :objects [o1 o2]})
(println "witness:" witness)
(println "conforming:" (#'ec/witness-conforming? witness o1 o2))
(println "non-conforming control (bad digest):"
         (#'ec/witness-conforming? (assoc witness :digest "abc") o1 o2))

(println "=== step 5: build-structural")
(def candidate
  {:id "policy/kimi-2-manual-2026-11-11"
   :target "session ed69fb38 turn 178 (2026-11-11T16:15:24Z)"
   :arrangement
   {:nodes [{:occurrence-id o1 :pattern "cascade-construction/read-what-exists-first"}
            {:occurrence-id o2 :pattern "forward-model/the-forward-model-is-a-pattern-cascade"}]
    :edges [{:from o1 :to o2 :kind :overlap :witness witness}]
    :precedence [[o1 o2]]}})
(def readings
  (into {}
        (for [[p v] validated]
          [p {:square v
              :provenance {:reading-record-id "kimi-2-manual-mapping-2026-11-11 (not a 象 record)"
                           :span (get-in prov [:spans p])
                           :source-text-digest (:turn-text-digest prov)
                           :analysis-digest (:analysis-digest prov)
                           :source-sha256 (get-in v [:objects :I1 :source :sha256])
                           :square-digest (evidence/value-digest v)}}])))
(def readings-by-occ
  (into {} (map (fn [[p r]] [(occ-id p) r]) readings)))
(def structural (ec/build-structural candidate readings-by-occ {}))
(println "build-structural keys/status:"
         (or (:refusal structural) (select-keys structural [:schema :digest])))
(println "gluing edges:" (:gluing structural))
(println "shared-name-candidates:" (:shared-name-candidates structural))
(println "census:" (:census structural))

(println "=== step 5b: validate-cascade")
(def result (ec/validate-cascade {:structural structural}))
(println "validate-cascade:" result)
(when (:digest structural)
  (let [full {:schema :wm/executable-cascade-v1
              :structural structural
              :structural-digest (:digest structural)}]
    (println "validate-cascade (with structural-digest):"
             (ec/validate-cascade full))))

(println "=== step 5c: pushing to the first substantive clause")
(when (:digest structural)
  (let [base {:schema :wm/executable-cascade-v1
              :structural structural
              :structural-digest (:digest structural)}
        with-digest (assoc base :digest "225a722f49fbee08814b478655b0123be61414c268b29d2c36556a41ef6832c1")]
    (println (ec/validate-cascade with-digest))))

(println "=== step 5d: adding the census projection digest")
(when (:digest structural)
  (let [r (ec/validate-cascade
           {:schema :wm/executable-cascade-v1
            :structural structural
            :structural-digest (:digest structural)
            :digest "225a722f49fbee08814b478655b0123be61414c268b29d2c36556a41ef6832c1"
            :relations {:projection {:census-digest (get-in structural [:census :census-digest])}}})]
    (println r)))

(println "=== step 5e: digest fixed-point + census binding, next clauses")
(when (:digest structural)
  (let [census (:census structural)
        base {:schema :wm/executable-cascade-v1
              :structural structural
              :structural-digest (:digest structural)
              :census-digest (:census-digest census)
              :census (dissoc census :census-digest)}]
    (loop [r base i 0]
      (let [r2 (assoc r :digest (ec/digest r))
            v (ec/validate-cascade r2)]
        (println "clause:" (:clause v) "repair:" (pr-str (:repair v)))
        (when (and (= :identity-digest (:clause v)) (< i 3))
          (recur r2 (inc i)))))))
