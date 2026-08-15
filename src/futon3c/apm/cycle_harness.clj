(ns futon3c.apm.cycle-harness
  "Stageable APM cycle harness.  Frame 1, not this namespace's tests, is the
  first end-to-end run.  Tests exercise storage/projection stages and refusals."
  (:require [clojure.java.io :as io]
            [futon3c.apm.preregistration :as prereg])
  (:import [java.security MessageDigest]))

(defn sha256-bytes [bytes]
  (let [digest (doto (MessageDigest/getInstance "SHA-256") (.update bytes))]
    (format "%064x" (BigInteger. 1 (.digest digest)))))

(defn file-bytes [path]
  (java.nio.file.Files/readAllBytes
   (if (instance? java.nio.file.Path path)
     path
     (.toPath (io/file path)))))

(defn memory-store []
  (let [state (atom {})]
    {:write! (fn [id entity] (swap! state assoc id entity) entity)
     :read! (fn [id] (get @state id))
     :state state}))

(defn persist-roundtrip!
  "Persist one entity and require exact read-back before continuing."
  [store id entity]
  ((:write! store) id entity)
  (let [read-back ((:read! store) id)]
    (when-not (= entity read-back)
      (throw (ex-info "entity round-trip mismatch"
                      {:id id :written entity :read-back read-back})))
    read-back))

(defn persist-registration! [store registration-id registration-path]
  (let [bytes (file-bytes registration-path)
        entity {:registration/id registration-id
                :registration/bytes (vec bytes)
                :registration/sha256 (sha256-bytes bytes)}]
    (persist-roundtrip! store registration-id entity)))

(defn emit-frame!
  "Emitter-side F1 gate.  Hashes both Main.lean files before persistence and
  refuses a scaffold-identical closing frame."
  [store {:frame/keys [id] :as frame} scaffold-path closing-path]
  (let [scaffold-hash (sha256-bytes (file-bytes scaffold-path))
        closing-hash (sha256-bytes (file-bytes closing-path))]
    (when (= scaffold-hash closing-hash)
      (throw (ex-info "refusing scaffold-identical frame"
                      {:failure :f1-scaffold-identical-frame
                       :frame/id id :hash scaffold-hash})))
    (persist-roundtrip! store id
                        (assoc frame
                               :frame/scaffold-hash scaffold-hash
                               :frame/closing-hash closing-hash))))

(defn entity-id [entity]
  (some entity [:cycle/id :frame/id :disp/id :ev/id :offer/id :use/id
                :meas/id :attempt/id :snap/id :rprobe/id :cprobe/id
                :promo/id :probe/id :gate/id]))

(defn persist-entities! [store entities]
  (mapv (fn [entity]
          (let [id (entity-id entity)]
            (when-not id
              (throw (ex-info "entity has no schema identity" {:entity entity})))
            (persist-roundtrip! store id entity)))
        entities))

(defn for-cycle [entities cycle-id key]
  (filter #(= cycle-id (get % key)) entities))

(defn one [entities predicate label]
  (let [matches (filter predicate entities)]
    (when-not (= 1 (count matches))
      (throw (ex-info "expected exactly one entity"
                      {:label label :count (count matches)})))
    (first matches)))

(defn derive-trace
  "Derive every validator trace projection from persisted entity content."
  [registration cycle-id entities]
  (let [cycle (one entities #(= cycle-id (:cycle/id %)) :cycle)
        frame (one entities #(= cycle-id (:frame/cycle %)) :frame)
        gate (one entities #(= cycle-id (:gate/cycle %)) :launch-gate)
        snapshot (one entities #(= cycle-id (:snap/cycle %)) :snapshot)
        containment (one entities #(= (:frame/id frame) (:cprobe/frame %))
                         :containment-probe)
        measurement (one entities #(= cycle-id (:meas/cycle %)) :measurement)
        attempts (sort-by :attempt/seq
                          (for-cycle entities cycle-id :attempt/cycle))
        dispositions (for-cycle entities cycle-id :disp/cycle)
        offers (for-cycle entities cycle-id :offer/cycle)
        uses (filter (set (map :offer/id offers)) (map :use/offer entities))
        retrievals (for-cycle entities cycle-id :rprobe/cycle)
        probes (for-cycle entities cycle-id :probe/cycle)
        promotions (for-cycle entities cycle-id :promo/cycle)]
    {:problem (:problem registration)
     :frame {:scaffold-hash (:frame/scaffold-hash frame)
             :closing-hash (:frame/closing-hash frame)}
     :launch-gate-refused-without-witness?
     (:gate/refused-without-witness? gate)
     :cycle-closed? (some? (:cycle/closed-at cycle))
     :disposition-ids (mapv :disp/id dispositions)
     :memory-offers (mapv #(select-keys % [:offer/id :offer/memory-id]) offers)
     :memory-disposition-offer-ids (vec uses)
     :stratum-frozen-at (:cycle/stratum-frozen-at cycle)
     :assigned-at (:cycle/assigned-at cycle)
     :cycle/attempts
     (mapv #(select-keys % [:cycle/regime :cycle/store-revision
                            :cycle/harness-revision
                            :cycle/environment-checkout
                            :cycle/environment-revision
                            :cycle/runner-freshness])
           attempts)
     :cycle/mode (:cycle/mode cycle)
     :cycle/deposit-state (:cycle/deposit-state cycle)
     :cycle/paired-with (:cycle/paired-with cycle)
     :cycle/store-snapshot-id (:snap/id snapshot)
     :cycle/store-snapshot-memory-ids (:snap/memory-ids snapshot)
     :cycle/window {:opened-at (:cycle/opened-at cycle)
                    :closed-at (:cycle/closed-at cycle)}
     :denominator-declared? (boolean (seq (:required-measurement-fields
                                          registration)))
     :denominator-inferred-from-corpus? false
     :available-artifact-ids (vec (mapcat :rprobe/available-ids retrievals))
     :need-probe-retrieved-ids (vec (mapcat :rprobe/retrieved-ids retrievals))
     :containment-claimed? (:cprobe/claimed? containment)
     :containment-probe-recorded? (:cprobe/recorded? containment)
     :containment-probe-passed? (:cprobe/passed? containment)
     :capability-probes
     (mapv (fn [probe]
             {:capability (:probe/capability probe)
              :evidence-id (:probe/evidence-id probe)
              :recorded? (:probe/recorded? probe)}) probes)
     :required-measurement-fields (:required-measurement-fields registration)
     :measurement (select-keys measurement [:meas/values :meas/unset])
     :promoted-artifact-ids (mapv :promo/artifact-id promotions)
     :importable-promoted-artifact-ids
     (mapv :promo/artifact-id (filter :promo/importable? promotions))
     :need-tagged-promoted-artifact-ids
     (mapv :promo/artifact-id (filter #(seq (:promo/need-tags %)) promotions))}))

(defn run-cycle!
  "Live entry point.  `entities` are stage outputs supplied by the live cycle;
  `authorize!` is the existing authorization writer adapter.  No solver is
  embedded here."
  [{:keys [store registration-path registration-id cycle-id frame
           scaffold-path closing-path entities lean-repo agency-endpoint
           solver-seat authorize!]}]
  (let [registration (prereg/read-edn registration-path)]
    (persist-registration! store registration-id registration-path)
    (let [stored-frame (emit-frame! store frame scaffold-path closing-path)
          stored (into [stored-frame] (persist-entities! store entities))
          trace (derive-trace registration cycle-id stored)
          report (prereg/report registration trace lean-repo agency-endpoint
                                solver-seat)]
      (when-not (:launchable? report)
        (throw (ex-info "cycle refused by registration gate" {:report report})))
      {:trace trace :report report :authorization (authorize! registration trace report)})))
