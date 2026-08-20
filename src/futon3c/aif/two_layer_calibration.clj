(ns futon3c.aif.two-layer-calibration
  "Pure R12 reporting gate for internal and independently witnessed evidence.

  Layer 1 describes the system's own prediction and realisation.  Layer 2 is
  a substrate or reviewer witness.  Layer 2 is verdict-eligible only when its
  provenance explicitly says that it is independently controlled and names a
  witness different from Layer 1's producer.  Nothing is inferred from a
  successful Layer 1 result.")

(def report-schema :futon3c.aif/two-layer-calibration-v1)

(defn- layer-2-independent?
  [layer-1 layer-2]
  (let [producer-id (get-in layer-1 [:layer-1/provenance
                                     :provenance/producer-id])
        witness-id (get-in layer-2 [:layer-2/provenance
                                    :provenance/witness-id])]
    (and (true? (:layer-2/independent? layer-2))
         (= :independent
            (get-in layer-2 [:layer-2/provenance :provenance/control]))
         (some? producer-id)
         (some? witness-id)
         (not= producer-id witness-id))))

(defn- gate-result
  [layer-1 layer-2 layer-2-present?]
  (cond
    (nil? layer-1)
    {:gate/clear? false
     :gate/verdict :fail
     :gate/reason :layer-1-missing}

    (not= :pass (:layer-1/verdict layer-1))
    {:gate/clear? false
     :gate/verdict :fail
     :gate/reason :layer-1-not-passed}

    (not layer-2-present?)
    {:gate/clear? false
     :gate/verdict :fail
     :gate/reason :layer-2-missing}

    (not (layer-2-independent? layer-1 layer-2))
    {:gate/clear? false
     :gate/verdict :fail
     :gate/reason :layer-2-independence-unproven}

    (not= :pass (:layer-2/verdict layer-2))
    {:gate/clear? false
     :gate/verdict :fail
     :gate/reason :layer-2-not-passed}

    :else
    {:gate/clear? true
     :gate/verdict :pass
     :gate/reason :independently-confirmed}))

(defn two-layer-report
  "Build a deterministic R12 gate report.

  Input fields:

    :layer-1/evidence
      #:layer-1{:prediction ... :realisation ... :verdict :pass|:fail
                :provenance #:provenance{:producer-id ...}}

    :layer-2/witness
      #:layer-2{:verdict :pass|:fail :independent? true|false
                :provenance #:provenance{:witness-id ...
                                         :control :independent|:author}}

  The returned value embeds both input layers unchanged, copies their typed
  provenance into an audit field, and gives a fail-closed gate verdict.  No
  clocks, I/O, generated identifiers, or ambient state participate, so an
  identical trace input always produces an identical report."
  [evidence]
  (let [layer-1 (:layer-1/evidence evidence)
        layer-2-present? (contains? evidence :layer-2/witness)
        layer-2 (:layer-2/witness evidence)]
    (merge
     {:report/schema report-schema
      :report/layer-1 layer-1
      :report/layer-2 layer-2
      :report/provenance
      {:layer-1/provenance (:layer-1/provenance layer-1)
       :layer-2/provenance (:layer-2/provenance layer-2)}}
     (gate-result layer-1 layer-2 layer-2-present?))))
