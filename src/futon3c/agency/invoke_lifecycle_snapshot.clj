(ns futon3c.agency.invoke-lifecycle-snapshot
  "Isolated atomic source producer. All provider reads participate in one owned
  boundary. It emits source bytes and a coverage subject, never completeness
  approval and never restart authorization."
  (:import (java.nio.charset StandardCharsets)
           (java.security MessageDigest)
           (java.util.concurrent.locks ReentrantLock)))

(def source-schemas
  {:controller :agency/ingress-controller-snapshot-v1
   :hot-ledger :agency/invoke-hot-ledger-snapshot-v1
   :accepted-queue :agency/accepted-queue-snapshot-v1
   :execution :agency/execution-snapshot-v1
   :final-delivery :agency/final-delivery-snapshot-v1
   :deferred :agency/deferred-resume-snapshot-v1})

(defn- refuse! [code data]
  (throw (ex-info (name code) (assoc data :refusal code))))
(defn- sha256 [^bytes bs]
  (apply str (map #(format "%02x" (bit-and 255 %))
                  (.digest (doto (MessageDigest/getInstance "SHA-256") (.update bs))))))
(defmacro ^:private with-lock [b & body]
  `(let [^ReentrantLock lock# (:lock ~b)]
     (.lock lock#)
     (try ~@body (finally (.unlock lock#)))))

(defn boundary
  [{:keys [owner-id generation scope]}]
  (when-not (and (string? owner-id) (not-empty owner-id)
                 (integer? generation) (= :isolated-fixture scope))
    (refuse! :snapshot/boundary-config-invalid {}))
  {:owner-id owner-id :scope scope :lock (ReentrantLock.)
   :generation (atom generation) :providers (atom {})})

(defn register-provider!
  "Register one source whose mutation and capture are owned by BOUNDARY.
  REVISION returns an independently maintained provider revision before/after
  capture; CAPTURE returns the schema record at the boundary generation."
  [b kind {:keys [owner-id capture revision] :as provider}]
  (with-lock b
    (when-not (contains? source-schemas kind)
      (refuse! :snapshot/provider-kind-unknown {:kind kind}))
    (when-not (and (= (:owner-id b) owner-id) (fn? capture) (fn? revision))
      (refuse! :snapshot/provider-uncoordinated {:kind kind}))
    (when (contains? @(:providers b) kind)
      (refuse! :snapshot/provider-duplicate {:kind kind}))
    (swap! (:providers b) assoc kind provider)
    true))

(defn mutate!
  "Only supported provider mutation boundary. F runs while capture is excluded;
  generation advances after F returns."
  [b f]
  (with-lock b
    (let [result (f)] (swap! (:generation b) inc) result)))

(defn capture!
  "Capture all six records under the common lock. Production refuses until its
  writers actually participate. Returned coverage is a subject for an external
  authority, not an approval."
  [b]
  (with-lock b
    (let [providers @(:providers b)
          missing (set (remove (set (keys providers)) (keys source-schemas)))]
      (when (seq missing) (refuse! :snapshot/providers-incomplete {:missing missing}))
      (let [generation @(:generation b)
            before (into {} (map (fn [[k p]] [k ((:revision p))]) providers))
            records (into {} (map (fn [[kind p]] [kind ((:capture p) generation)]) providers))
            after (into {} (map (fn [[k p]] [k ((:revision p))]) providers))]
        (when-not (= before after)
          (refuse! :snapshot/concurrent-provider-change {:before before :after after}))
        (doseq [[kind record] records]
          (when-not (and (= (source-schemas kind) (:schema record))
                         (= generation (:generation record))
                         (= (:scope b) (:scope record))
                         (= (:owner-id b) (get-in record [:provenance :owner-id])))
            (refuse! :snapshot/provider-record-invalid {:kind kind})))
        (let [sources (into {}
                            (map (fn [[kind record]]
                                   (let [bytes (.getBytes (pr-str record) StandardCharsets/UTF_8)]
                                     [kind {:bytes bytes :sha256 (sha256 bytes)
                                            :record record}]))) records)
              jobs (get-in records [:hot-ledger :jobs])
              universe (mapv (fn [[id job]] {:job-id id :trace-id (:trace-id job)})
                             (sort-by key jobs))]
          {:schema :agency/atomic-lifecycle-capture-v1
           :scope (:scope b) :owner-id (:owner-id b) :generation generation
           :sources sources
           :coverage-subject {:generation generation
                              :source-digests (into {} (map (fn [[k v]] [k (:sha256 v)]) sources))
                              :job-universe universe}
           :completeness-authority :absent
           :restart-authorized? false})))))

(defn capture-resolvers
  "Adapt one immutable capture to the reconciliation resolver interface."
  [capture]
  (into {} (map (fn [[kind {:keys [bytes sha256]}]]
                  [kind (fn [] {:bytes bytes :expected-sha256 sha256
                                :path (str "atomic-capture:" (name kind))})])
                (:sources capture))))
